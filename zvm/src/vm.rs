use std::{collections::HashMap, fmt::Display, rc::Rc, str::FromStr};

use anyhow::{bail, Context};
use zeal_ast::{expr::OperatorType, passes::rune::RuneTablePass, Ast};
use zeal_core::rune::RuneTable;

use crate::{
    chunk::{Chunk, FuncChunk},
    compiler::Archon,
    err::RuntimeError,
    native::zvm_println,
    opcode::{Op, Opcode},
    stack::Stack,
    val::{NativeFunc, Val},
};

// TODO: 10/17/2024 :: Execution fails when encountering println in the loops.zl test script.
//

pub const STACK_MAX: usize = 255; //u16::MAX as usize / 2;
pub const CALL_STACK_MAX: usize = STACK_MAX * 2;
#[derive(Debug, Clone)]
pub struct VM {
    stack: VMStack,
    frames: Stack<CALL_STACK_MAX, CallFrame>,
    // chunks: Vec<Chunk>,
    // chunk: Chunk,
    /// program counter for current running chunk. (chunk counter)
    // pc: usize,
    globals: HashMap<String, Val>, // runes: RuneTable,
                                   // runes: RuneTable,
}

pub type VMStack = Stack<STACK_MAX, Val>;

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            frames: Stack::new(),
            globals: HashMap::new(),
            // runes: RuneTable::empty(),
        }
    }

    pub fn load_entrypoint(path: &str) -> anyhow::Result<Self> {
        let ast = Ast::from_file(path)?;

        let runes = RuneTablePass::dopass(&ast)?;
        let f = Archon::compile_entrypoint(&ast)?;

        let stack_fn = Rc::new(f);
        let mut stack = Stack::new();
        stack.push(Val::Func(Rc::clone(&stack_fn)));

        let arity = stack_fn.arity as isize;
        let beg = stack.top_index().unwrap() as isize - arity - 1;
        let start_slot = std::cmp::min(0, beg) as usize;
        let cf = CallFrame {
            func: Some(Rc::clone(&stack_fn)),
            ip: 0,
            start_slot,
        };
        let mut frames = Stack::new();

        let globals = {
            let mut gs: HashMap<String, Val> = HashMap::new();
            let n = NativeFunc {
                func: zvm_println,
                name: Rc::from("println"),
                arity: 255,
            };
            gs.insert(String::from("println"), Val::NativeFunc(n));
            gs
        };

        frames.push(cf);

        let s = Self {
            stack,
            frames,
            globals,
            // runes,
        };
        Ok(s)
        // let f = self.compile_source(&src)?;
    }

    pub fn dump_stack(&self) -> String {
        self.stack.to_string()
    }

    pub fn dump_chunk(&self) -> String {
        self.try_frame()
            .expect("Cannot dump when no frames in CallStack!!!")
            .to_string()
        // self.chunk_top().unwrap().to_string()
    }

    pub fn dump(&self) -> String {
        let mut dump = String::from("\n ----- DUMP -----\n");
        let stack = self.stack.to_string();
        let bcode = self.dump_chunk();
        dump.push_str(&stack);
        dump.push_str(&bcode);
        dump.push_str(" ----- DUMP END ------\n");

        dump
    }

    // #[inline]
    /// Compiles string of zeal source code into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_source(&mut self, src: &str) -> anyhow::Result<FuncChunk> {
        let ast = Ast::from_str(src)?;
        self.compile_ast(&ast)
    }

    /// Compiles ast into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_ast(&mut self, ast: &Ast) -> anyhow::Result<FuncChunk> {
        Archon::compile_entrypoint(ast)
    }

    // TODO: pub fn test_opcode(&self, opcode: Opcode) -> anyhow::Result<OpEffect> {}
    // ------------------------------------------------------------------------------------
    // something fn like above that doesnt mutate the VM, but reocrds what the
    // effects of running given opcode WOULD have on the VM. Can be used to test if an opcode is
    // okay/safe to run, or used for debugging ,ect
    //
    //
    //
    // pub fn exec_opcode(&mut self, opcode: Opcode) -> anyhow::Result<()> {}

    #[inline]
    pub fn try_frame(&self) -> Option<&CallFrame> {
        self.frames.peek_top()
    }

    #[inline]
    /// Get current Frame being executed.
    pub fn try_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.peekn_mut(0)
    }

    #[inline]
    fn frame(&self) -> &CallFrame {
        self.try_frame()
            .expect("Tried to get executing callframe but Call Stack is empty!!!")
    }

    #[inline]
    fn frame_mut(&mut self) -> &mut CallFrame {
        self.try_frame_mut()
            .expect("Tried to get executing callframe but Call Stack is empty!!!")
    }

    /// Pushes ZBalue::Func onto stack memory, also crates a new CallFrame
    /// and pushes that onto Call Stack.
    pub fn call(&mut self, unit: Rc<FuncChunk>) {
        let stack_fn = Rc::clone(&unit);
        self.stack.push(Val::Func(stack_fn));

        let cf = self.new_frame(unit);

        self.frames.push(cf);
    }

    pub fn call_native(&mut self, nv: &NativeFunc, nargs: usize) -> anyhow::Result<()> {
        // ensure!("")
        let arg_end = self.stack.top_index().unwrap() as isize;
        let arg_start = arg_end - nargs as isize + 1;
        let arg_start = std::cmp::max(0, arg_start) as usize;
        let arg_end = arg_end as usize;

        let args = &self.stack[arg_start..=arg_end];
        let retval = (nv.func)(args)?;

        for _ in 0..nargs + 1 {
            let v = self.stack.expect_pop();
        }

        self.stack.push(retval);

        Ok(())
    }

    fn next_opcode(&mut self) -> Option<Opcode> {
        let frame = self.try_frame_mut()?;

        frame.next_opcode()
    }

    #[inline]
    pub fn frame_constants(&self) -> &[Val] {
        let f = self.frame();
        f.constants()
    }

    // TODO: implement debug opcodes with actual runtime values so
    // i can debug and verify compilation to bytecode.
    pub fn exec(&mut self) -> anyhow::Result<Val> {
        while let Some(opcode) = self.next_opcode() {
            match opcode.op() {
                Op::Return => {
                    let retval = self.stack.expect_pop();
                    if let Some(frame) = self.frames.pop() {
                        self.stack.cursor.set(frame.start_slot);
                        self.stack.push(retval);
                    } else {
                        // let _ = self.stack.pop();
                        break;
                    }
                }
                Op::Println => {
                    let v = self.stack.peek_top().expect("cannot peek an empty stack!");
                    println!("{v}");
                    self.stack.expect_pop();
                }

                Op::Print => {
                    let v = self.stack.peek_top().expect("cannot peek an empty stack!");
                    print!("{v}");
                    self.stack.expect_pop();
                }

                Op::Pop => {
                    let _ = self.stack.expect_pop();
                }
                Op::PopN => {
                    let param = opcode.try_param().unwrap();
                    let n = param.to_u32() as usize;
                    for _ in 0..n {
                        let _ = self.stack.expect_pop();
                    }
                }
                Op::Add => {
                    self.binary_op_float(OperatorType::Add);
                    // let right = self.stack.pop().expect_float64();
                    // let left = self.stack.pop().expect_float64();
                    // let res = ZValue::Number(left + right);
                    // self.stack.push(res);
                }
                Op::Sub => {
                    self.binary_op_float(OperatorType::Sub);
                }
                Op::Div => self.binary_op_float(OperatorType::Div),
                Op::Mul => self.binary_op_float(OperatorType::Mul),
                Op::Neg => {
                    let val = self.stack.expect_pop().expect_float64();
                    self.stack.push(Val::Float(-val));
                }
                Op::Not => {
                    let val = self.stack.expect_pop();
                    let res = !val.is_truthy();
                    self.stack.push(Val::Bool(res));
                }
                Op::Nil => {
                    let _ = self.stack.push(Val::Unit);
                }
                Op::True => {
                    let _ = self.stack.push(Val::Bool(true));
                }
                Op::False => {
                    let _ = self.stack.push(Val::Bool(false));
                }
                Op::Concat => todo!(),
                Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                    // let param = opcode.param.unwrap();
                    // let id = param.to_u32() as usize;
                    // let v = constants[id].clone();
                    // let v = self.chunk().try_read_const(opcode).unwrap();
                    let v = self.read_constant(&opcode).expect("Empty Call Frame!");

                    self.stack.push(v.clone());
                }
                Op::Unknown => todo!(),

                // NOTE: ----- DEFINE GLOBAL -----
                Op::DeclareGlobal8 | Op::DeclareGlobal16 | Op::DeclareGlobal32 => {
                    let name = self
                        .read_constant(&opcode)
                        .expect("Failed to read constant global!!!");
                    if let Val::Rune(s) = name.clone() {
                        let val = self.stack.expect_pop();
                        self.globals.insert(s.to_string(), val);
                    }
                }
                Op::GetGlobal8 | Op::GetGlobal16 | Op::GetGlobal32 => {
                    if let Some(Val::Rune(name)) = self.read_constant(&opcode) {
                        if let Some(val) = self.globals.get(name.as_ref()) {
                            self.stack.push(val.clone());
                        } else {
                            bail!(
                                "{}",
                                RuntimeError::VMUnknownIdentifier {
                                    name: name.clone(),
                                    opcode,
                                    constants: self.frame_constants().to_vec(),
                                    globals: self.globals.clone()
                                }
                            )
                        }
                    } else {
                        panic!("Attempted to get Global, but its value in VM constants is not ZValue::Ident.")
                    }
                }

                Op::SetGlobal8 | Op::SetGlobal16 | Op::SetGlobal32 => {
                    if let Some(Val::Rune(name)) = self.read_constant(&opcode) {
                        let top = self.stack.peek_top().expect("Nothing in stack to peek!!!");

                        if self.globals.contains_key(name.as_ref()) {
                            self.globals.insert(name.to_string(), top.clone());
                        } else {
                            bail!(
                                "{}",
                                RuntimeError::VMUnknownIdentifier {
                                    name: name.clone(),
                                    opcode,
                                    constants: self.frame_constants().to_vec(),
                                    globals: self.globals.clone(),
                                }
                            )
                        }
                    }
                }
                // NOTE: ----- END DEFINE GLOBAL -----
                Op::Eq => self.binary_op_float(OperatorType::Eq),
                Op::Gt => self.binary_op_float(OperatorType::Gt),
                Op::Lt => self.binary_op_float(OperatorType::Lt),
                Op::Ge => self.binary_op_float(OperatorType::Ge),
                Op::Le => self.binary_op_float(OperatorType::Le),
                Op::NotEq => self.binary_op_float(OperatorType::NotEq),

                // NOTE: ----- GET LOCAL -----
                Op::GetLocal8 | Op::GetLocal16 | Op::GetLocal32 => {
                    if let Some(local) = self.read_local(&opcode) {
                        self.stack.push(local.clone());
                    } else {
                        bail!("Unable to call GetLocal*")
                    }
                }
                // NOTE: ----- END GET LOCAL -----

                // NOTE: ----- SET LOCAL -----
                Op::SetLocal8 | Op::SetLocal16 | Op::SetLocal32 => todo!(),
                Op::JumpFalse => {
                    if let Some(top) = self.stack.peek_top() {
                        if top.is_falsey() {
                            self.jump_to(&opcode);
                            // let jumpto = opcode
                            //     .try_param()
                            //     .expect("JumpFalse16 Op has no parameter!!!");
                            //
                            // frame.ip = jumpto.to_u32() as usize;
                        }
                    }
                }

                Op::LongJumpFalse => todo!(),
                Op::Jump => {
                    // let jumpto = opcode.try_param().expect("Jump16 Op has no parameter!!!");
                    self.jump_to(&opcode);
                    // frame.ip = jumpto.to_usize();
                }
                Op::LongJump => todo!(),
                Op::JumpTrue => {
                    if let Some(top) = self.stack.peek_top() {
                        if top.is_truthy() {
                            self.jump_to(&opcode);
                            // let jumpto = opcode
                            //     .try_param()
                            //     .expect("JumpTrue16 Op has no parameter!!!");
                            //
                            // frame.ip = jumpto.to_u32() as usize;
                        }
                    }
                }
                Op::LongJumpTrue => todo!(),
                Op::Call => {
                    let nargs = opcode
                        .try_param()
                        .expect("Op::Call requires paramter and received none!!!")
                        .to_u32() as usize;
                    // .read_constant(&opcode)
                    // .expect(&format!("Failed to read constant from Frame: {:?}", frame));
                    // TODO: Fix this. we need to have function in stack memory BEFORE we compile
                    // its arguments and then emit Op::Call
                    if let Some(callable) = self.stack.peekn_mut(nargs) {
                        match &callable.clone() {
                            Val::Func(f) => {
                                self.call(Rc::clone(f));
                            }
                            Val::NativeFunc(nv) => self.call_native(nv, nargs)?,
                            _ => todo!(),
                        }
                    }
                }
                Op::Rune8 | Op::Rune16 | Op::Rune32 | Op::Rune64 => {}
            };
        }

        let top = if let Some(t) = self.stack.peek_top() {
            t.clone()
        } else {
            Val::Unit
        };
        Ok(top)
    }

    fn jump_to(&mut self, jump_op: &Opcode) {
        let jumpto = jump_op
            .try_param()
            .unwrap_or_else(|| panic!("{} has no parameter!!!", jump_op.op()));
        let cf = self.frame_mut();
        cf.ip = jumpto.to_usize();
    }

    fn read_constant(&self, opcode: &Opcode) -> Option<&Val> {
        let f = self.try_frame()?;
        f.read_constant(opcode)
    }

    fn binary_op_float(&mut self, ty: OperatorType) {
        let right = self.stack.expect_pop().expect_float64();
        let left = self.stack.expect_pop().expect_float64();

        let res: Val = match ty {
            OperatorType::Gt => (left > right).into(),
            OperatorType::Lt => (left < right).into(),
            OperatorType::Ge => (left >= right).into(),
            OperatorType::Le => (left <= right).into(),
            OperatorType::And => panic!("No opcode for And"),
            OperatorType::Or => panic!("No opcode for Or"),
            OperatorType::Eq => (left == right).into(),
            OperatorType::NotEq => (left != right).into(),
            // OperatorType::BitAnd => todo!(),
            // OperatorType::BitOr => todo!(),
            // OperatorType::Xor => todo!(),
            OperatorType::Concat => todo!(),
            OperatorType::Add => (left + right).into(),
            OperatorType::Sub => (left - right).into(),
            OperatorType::Mul => (left * right).into(),
            OperatorType::Div => (left / right).into(),
            OperatorType::Negate => {
                panic!("Cannot run unary operator Negate as a binary float operator!")
            }
            OperatorType::Not => {
                panic!("Cannot run unary operator Not as a binary float operator!")
            }
            OperatorType::Modulo => todo!(),
            OperatorType::Unknown => panic!("Unknown Operator Type."),
        };

        self.stack.push(res);
    }

    pub fn exec_file(&mut self, path: &str) -> anyhow::Result<Val> {
        let source = std::fs::read_to_string(path)?;

        self.exec_source(&source)
    }

    pub fn new_frame(&self, func: Rc<FuncChunk>) -> CallFrame {
        let arity = func.arity as isize;
        let beg = self.stack.top_index().unwrap() as isize - arity - 1;
        let start_slot = std::cmp::min(0, beg) as usize;
        CallFrame {
            func: Some(func),
            ip: 0,
            start_slot,
        }
    }

    pub fn exec_source(&mut self, src: &str) -> anyhow::Result<Val> {
        let f = self.compile_source(src)?;
        let f = Rc::new(f);
        self.call(f);

        self.exec()
    }
    #[inline]
    pub fn frame_memory(&self, frame: &CallFrame) -> &[Val] {
        &self.stack[frame.start_slot..]
    }

    fn read_local(&self, opcode: &Opcode) -> Option<&Val> {
        let frame = self
            .try_frame()
            .expect("Cannot read local from empty CallFrame!!!");
        if let Some(param) = opcode.try_param() {
            let slot = param.to_usize();
            let slot = frame.start_slot + slot;
            assert!(slot < self.stack.len());
            let v = &self.stack[slot];
            Some(v)
        } else {
            None
        }
    }

    pub fn dump_bytecode(&self) -> String {
        let mut s = self.clone();
        s.dump_bytecode_mut()
    }

    pub fn dump_bytecode_mut(&mut self) -> String {
        let mut n = 0;
        let mut bstr = String::new();
        while let Some(opcode) = self.next_opcode() {
            let s = match opcode.op() {
                Op::Return => String::from("RETURN"),
                Op::Println => String::from("PRINTLN"),
                Op::Print => String::from("PRINT"),
                Op::Pop => String::from("POP"),
                Op::PopN => {
                    let param = opcode.try_param().unwrap();
                    let n = param.to_u32() as usize;
                    format!("POPN => {n}")
                }
                Op::Add => String::from("ADD"),
                Op::Sub => String::from("SUB"),
                Op::Div => String::from("DIV"),
                Op::Mul => String::from("MUL"),
                Op::Neg => String::from("NEGATE"),
                Op::Not => String::from("NOT"),
                Op::Nil => String::from("NIL"),
                Op::True => String::from("TRUE"),
                Op::False => String::from("FALSE"),
                Op::Concat => todo!(),
                Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                    let v = self.read_constant(&opcode).expect("Empty Call Frame!");

                    format!("CONST => {v}")
                }
                Op::Unknown => todo!(),

                // NOTE: ----- DEFINE GLOBAL -----
                Op::DeclareGlobal8 | Op::DeclareGlobal16 | Op::DeclareGlobal32 => {
                    let name = self
                        .read_constant(&opcode)
                        .expect("Failed to read constant global!!!")
                        .clone();
                    self.globals.insert(name.to_string(), name.clone());
                    format!("DECLARE_GLOBAL => {name}")
                }
                Op::GetGlobal8 | Op::GetGlobal16 | Op::GetGlobal32 => {
                    if let Some(Val::Rune(name)) = self.read_constant(&opcode) {
                        if let Some(val) = self.globals.get(name.as_ref()) {
                            format!("GET_GLOBAL => {val}")
                        } else {
                            format!("GET_GLOBAL => {name}")
                        }
                    } else {
                        panic!("Attempted to get Global, but its value in VM constants is not Val::Rune.")
                    }
                }

                Op::SetGlobal8 | Op::SetGlobal16 | Op::SetGlobal32 => {
                    if let Some(Val::Rune(name)) = self.read_constant(&opcode) {
                        format!("SET_GLOBAL => {name}")
                    } else {
                        String::from("SET_GLOBAL => Failed to read constant!")
                    }
                }
                // NOTE: ----- END DEFINE GLOBAL -----
                Op::Eq => String::from("EQ"),
                Op::Gt => String::from("GT"),
                Op::Lt => String::from("LT"),
                Op::Ge => String::from("GE"),
                Op::Le => String::from("LE"),
                Op::NotEq => String::from("NOT_EQ"),

                // NOTE: ----- GET LOCAL -----
                Op::GetLocal8 | Op::GetLocal16 | Op::GetLocal32 => {
                    if let Some(local) = self.read_local(&opcode) {
                        format!("GET_LOCAL => {local}")
                    } else {
                        String::from("GET_LOCAL => Unable to call GetLocal*")
                    }
                }
                // NOTE: ----- END GET LOCAL -----

                // NOTE: ----- SET LOCAL -----
                Op::SetLocal8 | Op::SetLocal16 | Op::SetLocal32 => todo!(),
                Op::JumpFalse => {
                    format!("JUMP_FALSE => {}", opcode.try_param().unwrap().to_usize())
                }

                Op::LongJumpFalse => todo!(),
                Op::Jump => {
                    format!("JUMP => {}", opcode.try_param().unwrap().to_usize())
                }
                Op::LongJump => todo!(),
                Op::JumpTrue => {
                    format!("JUMP_TRUE => {}", opcode.try_param().unwrap().to_usize())
                }
                Op::LongJumpTrue => todo!(),
                Op::Call => {
                    let nargs = opcode
                        .try_param()
                        .expect("Op::Call requires paramter and received none!!!")
                        .to_u32() as usize;
                    format!("CALL => {nargs}")
                }
                Op::Rune8 | Op::Rune16 | Op::Rune32 | Op::Rune64 => {
                    let arg = opcode
                        .try_param()
                        .expect("Op::Rune requires paramter and received none!!!")
                        .to_usize();
                    format!("RUNE => {arg}")
                }
            };

            n += 1;
            bstr.push_str(&format!("{n}: {s}\n"));
        }
        bstr
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = self.clone();
        let d = s.dump_bytecode_mut();
        write!(f, "{d}")
    }
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    // TODO: For now this is an Rc, but when i implement my
    // dynanuc stack for the VM, this will changed since the entire Func will live on the stack
    func: Option<Rc<FuncChunk>>,
    ip: usize,
    /// index offset from top of stack that
    /// this call frames variables live at.
    start_slot: usize,
}

impl Default for CallFrame {
    fn default() -> Self {
        Self::zeroed()
    }
}

impl CallFrame {
    pub fn zeroed() -> Self {
        Self {
            func: None,
            ip: 0,
            start_slot: 0,
        }
    }

    pub fn code(&self) -> &Chunk {
        self.try_code().expect("Cannot get code from empty Chunk!")
    }

    pub fn try_code(&self) -> Option<&Chunk> {
        if let Some(c) = self.func.as_ref() {
            Some(&c.chunk)
        } else {
            None
        }
    }

    /// Unconditional jump to location index in bytecode.
    /// panics if location out of range.
    pub fn jump_to(&mut self, location: usize) {
        assert!(
            location < self.bytecode_len(),
            "Access violation! Got: {location} when bytecode length is: {}!",
            self.bytecode_len()
        );
        self.ip = location;
    }

    pub fn try_constants(&self) -> Option<&[Val]> {
        let c = self.try_code()?;
        Some(&c.constants)
    }

    pub fn constants(&self) -> &[Val] {
        let c = self.code();
        &c.constants
    }

    pub fn read_constant(&self, opcode: &Opcode) -> Option<&Val> {
        let param = opcode.try_param()?;
        let id = param.to_usize();
        let cs = self.try_constants()?;
        Some(&cs[id])
    }

    pub fn bytecode_len(&self) -> usize {
        if let Some(c) = self.try_code() {
            c.len()
        } else {
            0
        }
    }

    /// Gets opcode at current self.ip, then increments self.ip to the next opcode.
    pub fn next_opcode(&mut self) -> Option<Opcode> {
        let opcode = self.opcode_at(self.ip).ok()?;
        self.ip += opcode.offset();
        Some(opcode)
    }

    pub fn opcode_at(&self, index: usize) -> anyhow::Result<Opcode> {
        if let Some(f) = self.func.as_ref() {
            let opcode = self.code().opcode_at(index).context(format!(
                "Opcode index out of range! Got: {index} but chunk length is only {}!",
                self.code().len()
            ))?;
            Ok(opcode)
        } else {
            bail!("Tried to get Opcode from an Option<Func> that is None!!")
        }
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = if let Some(func) = &self.func {
            let name = func.name();
            let arity = func.arity as usize;
            let stack_mem = self.start_slot;
            let ip = self.ip;
            format!("CallFrame :: fn {name}/{arity}\n\t=> Call Frame Indicies: {stack_mem:?}\n\t=> ip: {ip}")
        } else {
            String::from("CallFrame::func == None")
        };
        write!(f, "{s}")
    }
}
