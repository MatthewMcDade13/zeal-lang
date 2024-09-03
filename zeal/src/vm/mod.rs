use std::{collections::HashMap, fmt::Display, ops::Index};

use anyhow::bail;
use stack::Stack;

use crate::{
    ast::Ast,
    compiler::{
        chunk::Chunk,
        opcode::{Bytecode, Op, Opcode},
        Archon,
    },
    core_types::{
        num::ZBool,
        str::{RuneTable, ZIdent},
        val::ZValue,
    },
    err::core::RuntimeError,
};

pub mod stack;
macro_rules! logical_binary_op {
    ($stack: expr, $op: tt) => {

        {
            let right = $stack.expect_pop().expect_float64();
            let left = $stack.expect_pop().expect_float64();

            let res = ZValue::Bool(ZBool::new(left $op right));
            $stack.push(res);



        }

    };
}

macro_rules! num_binary_op {
    ($stack: expr, $op: tt) => {

        {


            let right = $stack.expect_pop().expect_float64();
            let left = $stack.expect_pop().expect_float64();

            let res = ZValue::Number(left $op right);
            $stack.push(res);



        }

    };
}

pub const VM_STACK_MAX: usize = u16::MAX as usize;
#[derive(Debug, Clone)]
pub struct VM {
    stack: VMStack,
    // chunks: Vec<Chunk>,
    chunk: Chunk,
    /// program counter for current running chunk. (chunk counter)
    pc: usize,

    globals: HashMap<ZIdent, ZValue>, // runes: RuneTable,
}

pub type VMStack = Stack<VM_STACK_MAX, ZValue>;

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            chunk: Chunk::default(),
            pc: 0,
            globals: HashMap::new(),
            // runes: RuneTable::empty(),
        }
    }

    pub fn dump_stack(&self) -> String {
        self.stack.to_string()
    }

    pub fn dump_chunks(&self) -> String {
        self.chunk.to_string()
    }

    pub fn dump(&self) -> String {
        let mut dump = String::from("\n ----- DUMP -----\n");
        let stack = self.stack.to_string();
        let bcode = self.dump_chunks();
        dump.push_str(&stack);
        dump.push_str(&bcode);
        dump.push_str(&format!("pc: {}", self.pc));
        dump.push_str(" ----- DUMP END ------\n");

        dump
    }

    #[inline]
    /// Compiles string of zeal source code into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_source(&mut self, src: &str) -> anyhow::Result<usize> {
        let ast = Ast::from_str(src)?;
        self.compile_ast(&ast)
    }

    /// Compiles ast into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_ast(&mut self, ast: &Ast) -> anyhow::Result<usize> {
        let new = Archon::compile(&ast)?;
        self.chunk = new;
        let depth = self.chunk.len() - 1;
        Ok(depth)
    }

    #[inline]
    pub fn chunk(&self) -> &Chunk {
        &self.chunk
    }

    pub fn opcode_at(&self, index: usize) -> Option<Opcode> {
        let c = &self.chunk;
        c.opcode_at(index)
    }

    pub fn expect_opcode_at(&self, index: usize) -> Opcode {
        self.chunk.opcode_at(index).unwrap()
    }

    pub fn exec_all(&mut self) -> anyhow::Result<()> {
        let _ = self.exec_chunk()?;
        Ok(())
    }

    pub fn exec_chunk(&mut self) -> anyhow::Result<ZValue> {
        self.pc = 0;
        while self.pc < self.chunk.len() {
            if let Some(opcode) = self.chunk.opcode_at(self.pc) {
                self.pc += opcode.offset();
                match opcode.op() {
                    Op::Return => {}
                    Op::Println => {
                        let v = self.stack.peek_top().expect("cannot peek an empty stack!");
                        println!("{}", v.to_string());
                        self.stack.expect_pop();
                    }

                    Op::Print => {
                        let v = self.stack.peek_top().expect("cannot peek an empty stack!");
                        print!("{}", v.to_string());
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
                        num_binary_op!(self.stack, +);
                        // let right = self.stack.pop().expect_float64();
                        // let left = self.stack.pop().expect_float64();
                        // let res = ZValue::Number(left + right);
                        // self.stack.push(res);
                    }
                    Op::Sub => {
                        num_binary_op!(self.stack, -);
                    }
                    Op::Div => num_binary_op!(self.stack, /),
                    Op::Mul => num_binary_op!(self.stack, *),
                    Op::Neg => {
                        let val = self.stack.expect_pop().expect_float64();
                        self.stack.push(ZValue::Number(-val));
                    }
                    Op::Not => {
                        let val = self.stack.expect_pop();
                        let res = if val.is_truthy() { false } else { true };
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
                        let v = read_constant(&opcode, &self.chunk.constants)
                            .expect("Failed to read Constant!");

                        self.stack.push(v.clone());
                    }
                    Op::Unknown => todo!(),

                    // NOTE: ----- DEFINE GLOBAL -----
                    Op::DeclareGlobal8 | Op::DeclareGlobal16 | Op::DeclareGlobal32 => {
                        let name = read_constant(&opcode, &self.chunk.constants)
                            .expect("Failed to read constant global!!!");
                        if let ZValue::Ident(s) = name {
                            let val = self.stack.expect_pop();
                            self.globals.insert(s.clone(), val);
                        }
                    }
                    Op::GetGlobal8 | Op::GetGlobal16 | Op::GetGlobal32 => {
                        if let Some(ZValue::Ident(name)) =
                            read_constant(&opcode, &self.chunk.constants)
                        {
                            if let Some(val) = self.globals.get(name) {
                                self.stack.push(val.clone());
                            } else {
                                bail!(
                                    "{}",
                                    RuntimeError::VMUnknownIdentifier {
                                        name: name.clone(),
                                        opcode: opcode,
                                        constants: self.chunk.constants.to_owned(),
                                        globals: self.globals.clone()
                                    }
                                )
                            }
                        } else {
                            panic!("Attempted to get Global, but its value in VM constants is not ZValue::Ident.")
                        }
                    }

                    Op::SetGlobal8 | Op::SetGlobal16 | Op::SetGlobal32 => {
                        if let Some(ZValue::Ident(name)) =
                            read_constant(&opcode, &self.chunk.constants)
                        {
                            let top = self.stack.peek_top().expect("Nothing in stack to peek!!!");

                            if self.globals.contains_key(name) {
                                self.globals.insert(name.clone(), top.clone());
                            } else {
                                bail!(
                                    "{}",
                                    RuntimeError::VMUnknownIdentifier {
                                        name: name.clone(),
                                        opcode: opcode,
                                        constants: self.chunk.constants.clone(),
                                        globals: self.globals.clone(),
                                    }
                                )
                            }
                        } else {
                        }
                    }
                    // NOTE: ----- END DEFINE GLOBAL -----
                    Op::Eq => logical_binary_op!(self.stack, ==),
                    Op::Gt => logical_binary_op!(self.stack, >),
                    Op::Lt => logical_binary_op!(self.stack, <),
                    Op::Ge => logical_binary_op!(self.stack, >),
                    Op::Le => logical_binary_op!(self.stack, <),
                    Op::NotEq => logical_binary_op!(self.stack, !=),

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
                                self.pc = jumpto.to_u32() as usize;
                            }
                        }
                    }

                    Op::LongJumpFalse => todo!(),
                    Op::Jump => {
                        let jumpto = opcode.try_param().expect("Jump16 Op has no parameter!!!");
                        self.pc = jumpto.to_usize();
                    }
                    Op::LongJump => todo!(),
                    Op::JumpTrue => todo!(),
                    Op::LongJumpTrue => todo!(), // NOTE: ----- END SET LOCAL -----
                };
            }
        }

        let top = if let Some(t) = self.stack.peek_top() {
            t.clone()
        } else {
            ZValue::Nil
        };
        Ok(top)
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

    pub fn exec_source(&mut self, src: &str) -> anyhow::Result<ZValue> {
        self.compile_source(src)?;
        self.exec_chunk()
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
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d = self.dump();
        write!(f, "{d}")
    }
}

fn read_constant<'a>(opcode: &Opcode, constants: &'a [ZValue]) -> Option<&'a ZValue> {
    if let Some(param) = opcode.try_param() {
        let id = param.to_usize();
        Some(&constants[id])
    } else {
        None
    }
}

// fn compile_source(src: &str) -> anyhow::Result<Vec<Chunk>> {

// }
