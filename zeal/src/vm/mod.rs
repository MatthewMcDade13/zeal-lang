use std::{collections::HashMap, fmt::Display, mem::MaybeUninit, ops::Index, rc::Rc, str::FromStr};

use anyhow::{bail, Context};
use stack::Stack;

use crate::{
    ast::{Ast, BinaryOpType},
    compiler::{
        chunk::Chunk,
        func::FuncChunk,
        opcode::{Op, Opcode},
        Archon,
    },
    core_types::{str::ZIdent, val::ZValue},
    err::core::RuntimeError,
};
pub mod stack;

macro_rules! num_binary_op {
    ($stack: expr, $op: tt) => {

        {
        // TODO: Inequalities dont work... not sure why but left and right seems to
        // get switch up somewhere. OOF
            let right = $stack.expect_pop().expect_float64();
            let left = $stack.expect_pop().expect_float64();
            let res = left $op right;
            let op = stringify!($op);

            let res = ZValue::from(res);
            $stack.push(res);



        }

    };
}

pub const STACK_MAX: usize = u16::MAX as usize;
pub const CALL_STACK_MAX: usize = STACK_MAX * 8;
#[derive(Debug, Clone)]
pub struct VM {
    stack: VMStack,
    frames: Stack<CALL_STACK_MAX, CallFrame>,
    // chunks: Vec<Chunk>,
    // chunk: Chunk,
    /// program counter for current running chunk. (chunk counter)
    // pc: usize,
    globals: HashMap<ZIdent, ZValue>, // runes: RuneTable,
}

pub type VMStack = Stack<STACK_MAX, ZValue>;

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            frames: Stack::new(),
            globals: HashMap::new(),
            // runes: RuneTable::empty(),
        }
    }

    pub fn dump_stack(&self) -> String {
        self.stack.to_string()
    }

    pub fn dump_chunks(&self) -> String {
        todo!()
        // self.chunk_top().unwrap().to_string()
    }

    pub fn dump(&self) -> String {
        let mut dump = String::from("\n ----- DUMP -----\n");
        let stack = self.stack.to_string();
        let bcode = self.dump_chunks();
        dump.push_str(&stack);
        dump.push_str(&bcode);
        dump.push_str(" ----- DUMP END ------\n");

        dump
    }

    #[inline]
    /// Compiles string of zeal source code into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_source(&mut self, src: &str) -> anyhow::Result<FuncChunk> {
        let ast = Ast::from_str(src)?;
        self.compile_ast(&ast)
    }

    /// Compiles ast into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_ast(&mut self, ast: &Ast) -> anyhow::Result<FuncChunk> {
        Archon::compile(&ast)
    }

    #[inline]
    pub fn frame_top(&self) -> Option<&CallFrame> {
        self.frames.peek_top()
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

    pub fn top_frame(&self) -> Option<&CallFrame> {
        self.frames.peek_top()
    }

    pub fn top_frame_mut(&mut self) -> Option<&mut CallFrame> {
        self.frames.peekn_mut(0)
    }

    pub fn call(&mut self, func: FuncChunk) {
        let cf = self.new_frame(func);
    }

    pub fn exec(&mut self) -> anyhow::Result<ZValue> {
        todo!();
        let frame = &mut self.frames[0];
        // let frame = self
        // .top_frame_mut()
        // .context("Cannot exec an empty Call Stack!!!")?;
        while frame.ip < frame.bytecode_len() {
            let opcode = frame.next_opcode()?;
            match opcode.op() {
                Op::Return => {}
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
                    self.binary_op_num(BinaryOpType::Add);
                    // let right = self.stack.pop().expect_float64();
                    // let left = self.stack.pop().expect_float64();
                    // let res = ZValue::Number(left + right);
                    // self.stack.push(res);
                }
                Op::Sub => {
                    self.binary_op_num(BinaryOpType::Sub);
                }
                Op::Div => self.binary_op_num(BinaryOpType::Div),
                Op::Mul => self.binary_op_num(BinaryOpType::Mul),
                Op::Neg => {
                    let val = self.stack.expect_pop().expect_float64();
                    self.stack.push(ZValue::Number(-val));
                }
                Op::Not => {
                    let val = self.stack.expect_pop();
                    let res = !val.is_truthy();
                    self.stack.push(ZValue::bool(res));
                }
                Op::Nil => {
                    let _ = self.stack.push(ZValue::Nil);
                }
                Op::True => {
                    let _ = self.stack.push(ZValue::bool(true));
                }
                Op::False => {
                    let _ = self.stack.push(ZValue::bool(false));
                }
                Op::Concat => todo!(),
                Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                    // let param = opcode.param.unwrap();
                    // let id = param.to_u32() as usize;
                    // let v = constants[id].clone();
                    // let v = self.chunk().try_read_const(opcode).unwrap();
                    let v = frame
                        .read_constant(&opcode)
                        .expect("Failed to read Constant!");

                    self.stack.push(v.clone());
                }
                Op::Unknown => todo!(),

                // NOTE: ----- DEFINE GLOBAL -----
                Op::DeclareGlobal8 | Op::DeclareGlobal16 | Op::DeclareGlobal32 => {
                    let name = frame
                        .read_constant(&opcode)
                        .expect("Failed to read constant global!!!");
                    if let ZValue::Ident(s) = name {
                        let val = self.stack.expect_pop();
                        self.globals.insert(s.clone(), val);
                    }
                }
                Op::GetGlobal8 | Op::GetGlobal16 | Op::GetGlobal32 => {
                    if let Some(ZValue::Ident(name)) = frame.read_constant(&opcode) {
                        if let Some(val) = self.globals.get(name) {
                            self.stack.push(val.clone());
                        } else {
                            bail!(
                                "{}",
                                RuntimeError::VMUnknownIdentifier {
                                    name: name.clone(),
                                    opcode,
                                    constants: frame.constants().to_vec(),
                                    globals: self.globals.clone()
                                }
                            )
                        }
                    } else {
                        panic!("Attempted to get Global, but its value in VM constants is not ZValue::Ident.")
                    }
                }

                Op::SetGlobal8 | Op::SetGlobal16 | Op::SetGlobal32 => {
                    if let Some(ZValue::Ident(name)) = frame.read_constant(&opcode) {
                        let top = self.stack.peek_top().expect("Nothing in stack to peek!!!");

                        if self.globals.contains_key(name) {
                            self.globals.insert(name.clone(), top.clone());
                        } else {
                            bail!(
                                "{}",
                                RuntimeError::VMUnknownIdentifier {
                                    name: name.clone(),
                                    opcode,
                                    constants: frame.constants().to_vec(),
                                    globals: self.globals.clone(),
                                }
                            )
                        }
                    }
                }
                // NOTE: ----- END DEFINE GLOBAL -----
                Op::Eq => self.binary_op_num(BinaryOpType::Equals),
                Op::Gt => self.binary_op_num(BinaryOpType::Gt),
                Op::Lt => self.binary_op_num(BinaryOpType::Lt),
                Op::Ge => self.binary_op_num(BinaryOpType::Ge),
                Op::Le => self.binary_op_num(BinaryOpType::Le),
                Op::NotEq => self.binary_op_num(BinaryOpType::NotEquals),

                // NOTE: ----- GET LOCAL -----
                Op::GetLocal8 | Op::GetLocal16 | Op::GetLocal32 => {
                    if let Some(local) = self.read_local(&opcode) {
                        let l = local.clone();
                        self.stack.push(local.clone());
                    } else {
                        bail!("Unable to call GetLocal* to get Local")
                    }
                }
                // NOTE: ----- END GET LOCAL -----

                // NOTE: ----- SET LOCAL -----
                Op::SetLocal8 | Op::SetLocal16 | Op::SetLocal32 => {}

                Op::JumpFalse => {
                    if let Some(top) = self.stack.peek_top() {
                        if top.is_falsey() {
                            let jumpto = opcode
                                .try_param()
                                .expect("JumpFalse16 Op has no parameter!!!");

                            frame.ip = jumpto.to_u32() as usize;
                        }
                    }
                }

                Op::LongJumpFalse => todo!(),
                Op::Jump => {
                    let jumpto = opcode.try_param().expect("Jump16 Op has no parameter!!!");
                    frame.ip = jumpto.to_usize();
                }
                Op::LongJump => todo!(),
                Op::JumpTrue => {
                    if let Some(top) = self.stack.peek_top() {
                        if top.is_truthy() {
                            let jumpto = opcode
                                .try_param()
                                .expect("JumpTrue16 Op has no parameter!!!");

                            frame.ip = jumpto.to_u32() as usize;
                        }
                    }
                }
                Op::LongJumpTrue => todo!(),
                Op::NoOp => {} // NOTE: ----- END SET LOCAL -----
            };
        }

        let top = if let Some(t) = self.stack.peek_top() {
            t.clone()
        } else {
            ZValue::Nil
        };
        Ok(top)
    }

    fn binary_op_num(&mut self, ty: BinaryOpType) {
        let right = self.stack.expect_pop().expect_float64();
        let left = self.stack.expect_pop().expect_float64();

        let res: ZValue = match ty {
            BinaryOpType::Gt => (left > right).into(),
            BinaryOpType::Lt => (left < right).into(),
            BinaryOpType::Ge => (left >= right).into(),
            BinaryOpType::Le => (left <= right).into(),
            BinaryOpType::And => panic!("No opcode for And"),
            BinaryOpType::Or => panic!("No opcode for Or"),
            BinaryOpType::Equals => (left == right).into(),
            BinaryOpType::NotEquals => (left != right).into(),
            BinaryOpType::BitAnd => todo!(),
            BinaryOpType::BitOr => todo!(),
            BinaryOpType::Xor => todo!(),
            BinaryOpType::Concat => todo!(),
            BinaryOpType::Add => (left + right).into(),
            BinaryOpType::Sub => (left - right).into(),
            BinaryOpType::Mul => (left * right).into(),
            BinaryOpType::Div => (left / right).into(),
        };

        self.stack.push(res);
    }
    // for opcode in self.chunk.iter() {
    // Ok(opcode.offset())
    // vm_exec_opcode(&mut self.stack, &chunk.constants, opcode)?;
    // }

    // pub fn exec_chunks(&mut self, chs: &[Chunk]) -> anyhow::Result<()> {
    //     for c in chs {
    //         self.exec(*c)?;
    //     }
    //     Ok(())
    // }
    //
    pub fn exec_file(&mut self, path: &str) -> anyhow::Result<ZValue> {
        let source = std::fs::read_to_string(path)?;

        self.exec_source(&source)
    }

    pub fn new_frame(&self, func: FuncChunk) -> CallFrame {
        let arity = func.arity() as isize;
        let beg = self.stack.top_index().unwrap() as isize - arity - 1;
        let start_slot = std::cmp::min(0, beg) as usize;
        CallFrame {
            func: Some(Rc::new(func)),
            ip: 0,
            start_slot,
        }
    }

    pub fn exec_source(&mut self, src: &str) -> anyhow::Result<ZValue> {
        let f = self.compile_source(src)?;
        let cf = self.new_frame(f);
        // SAFETY: new_frame gaurantees that CallFrame::func is not None,
        // so this is safe.
        let zfunc = unsafe { cf.func.as_ref().unwrap_unchecked() };
        let zfunc = Rc::clone(zfunc);
        self.stack.push(ZValue::Func(zfunc));

        self.frames.push(cf);
        self.exec()
    }

    #[inline]
    pub fn frame_memory(&self, frame: &CallFrame) -> &[ZValue] {
        &self.stack[frame.start_slot..]
    }

    //
    // pub fn bytecode(&self) -> &Bytecode {
    //     self.chunk().code()
    // }

    fn read_local(&self, opcode: &Opcode) -> Option<&ZValue> {
        if let Some(param) = opcode.try_param() {
            let slot = param.to_usize();
            let v = self.stack.peekn(slot)?;
            Some(v)
        } else {
            None
        }
    }

    // #[inline]
    // pub fn memfor(&self, frame: &CallFrame) -> &[ZValue] {
    //     &self.stack[frame.mem_slots.clone()]
    // }
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d = self.dump();
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

    pub fn constants(&self) -> &[ZValue] {
        let f = self
            .func
            .as_ref()
            .expect("Cannot get constant slice from a Func::NoOp!!!");
        let c = f
            .try_chunk()
            .expect("Cannot read constants from a Func::NoOp!!!");
        &c.constants
    }

    pub fn read_constant(&self, opcode: &Opcode) -> Option<&ZValue> {
        let func = self.func.as_ref()?;
        let param = opcode.try_param()?;
        let id = param.to_usize();
        let c = func.try_chunk()?;
        Some(&c.constants[id])
    }

    pub fn bytecode_len(&self) -> usize {
        if let Some(f) = &self.func {
            f.try_chunk()
                .expect("Invalid read on a Func::NoOp!!!")
                .len()
        } else {
            0
        }
    }

    /// Gets opcode at current self.ip, then increments self.ip to the next opcode.
    pub fn next_opcode(&mut self) -> anyhow::Result<Opcode> {
        let opcode = self.opcode_at(self.ip)?;
        self.ip += opcode.offset();
        Ok(opcode)
    }

    pub fn opcode_at(&self, index: usize) -> anyhow::Result<Opcode> {
        if let Some(f) = self.func.as_ref() {
            let opcode = match f.as_ref() {
                FuncChunk::NoOp => Opcode::no_op(),
                FuncChunk::Fn { chunk, .. } | FuncChunk::Script { chunk } => {
                    let o = chunk.opcode_at(index);
                    o.context(format!(
                        "Opcode index out of range! Got: {index} but chunk length is only {}!",
                        chunk.len(),
                    ))?
                }
            };
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
            let arity = func.arity();
            let stack_mem = self.start_slot;
            let ip = self.ip;
            format!("CallFrame :: fn {name}/{arity}\n\t=> Call Frame Indicies: {stack_mem:?}\n\t=> ip: {ip}")
        } else {
            String::from("CallFrame::func == None")
        };
        write!(f, "{s}")
    }
}
