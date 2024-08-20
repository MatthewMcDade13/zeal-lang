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
    ($stack: ident, $op: tt) => {

        {
            let right = $stack.expect_pop().expect_float64();
            let left = $stack.expect_pop().expect_float64();

            let res = ZValue::Bool(ZBool::new(left $op right));
            $stack.push(res);



        }

    };
}

macro_rules! num_binary_op {
    ($stack: ident, $op: tt) => {

        {


            let right = $stack.expect_pop().expect_float64();
            let left = $stack.expect_pop().expect_float64();

            let res = ZValue::Number(left $op right);
            $stack.push(res);



        }

    };
}

pub const STACK_MAX: usize = 0x20;
#[derive(Debug, Clone)]
pub struct VM {
    stack: super::vm::stack::Stack<STACK_MAX>,
    chunks: Vec<Chunk>,
    /// index of current running chunk.
    depth: usize,
    /// program counter for current running chunk. (chunk counter)
    cc: usize,
    globals: HashMap<ZIdent, ZValue>, // runes: RuneTable,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            chunks: Vec::with_capacity(8),
            depth: 0,
            cc: 0,
            globals: HashMap::new(),
            // runes: RuneTable::empty(),
        }
    }

    pub fn dump_stack(&self) -> String {
        self.stack.to_string()
    }

    pub fn dump_chunks(&self) -> String {
        self.chunks
            .iter()
            .map(|c| c.to_string())
            .collect::<String>()
    }

    pub fn dump(&self) -> String {
        let mut dump = String::from("\n ----- DUMP -----\n");
        let stack = self.stack.to_string();
        let bcode = self.dump_chunks();
        dump.push_str(&stack);
        dump.push_str(&bcode);
        dump.push_str(&format!("pc: {}, current depth: {}\n", self.cc, self.depth));
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
        self.chunks.push(new);
        let depth = self.chunks.len() - 1;
        Ok(depth)
    }

    #[inline]
    pub fn chunk(&self) -> &Chunk {
        assert!(self.depth < self.chunks.len());
        &self.chunks[self.depth]
    }

    pub fn opcode_at(&self, index: usize, depth: usize) -> Option<Opcode> {
        let c = self.chunks.get(depth)?;
        c.opcode_at(index)
    }

    pub fn expect_opcode_at(&self, index: usize, depth: usize) -> Opcode {
        let c = &self.chunks[depth];
        c.opcode_at(index).unwrap()
    }

    pub fn exec_all(&mut self) -> anyhow::Result<()> {
        for i in 0..self.chunks.len() {
            self.depth = i;
            self.cc = 0;
            let _ = self.exec_chunk(i)?;
        }
        Ok(())
    }

    pub fn exec_chunk(&mut self, depth: usize) -> anyhow::Result<ZValue> {
        self.cc = 0;
        if let Some(chunk) = self.chunks.get(depth) {
            let stack = &mut self.stack;
            for opcode in chunk.iter() {
                match opcode.op {
                    Op::Return => {}
                    Op::Println => {
                        let v = stack.peek_top().expect("cannot peek an empty stack!");
                        println!("{}", v.to_string())
                    }

                    Op::Print => {
                        let v = stack.peek_top().expect("cannot peek an empty stack!");
                        print!("{}", v.to_string())
                    }

                    Op::Pop => {
                        let _ = stack.expect_pop();
                    }
                    Op::PopN => {
                        let param = opcode.param.unwrap();
                        let n = param.to_u32() as usize;
                        for _ in 0..n {
                            let _ = stack.expect_pop();
                        }
                    }
                    Op::Add => {
                        num_binary_op!(stack, +);
                        // let right = self.stack.pop().expect_float64();
                        // let left = self.stack.pop().expect_float64();
                        // let res = ZValue::Number(left + right);
                        // self.stack.push(res);
                    }
                    Op::Sub => {
                        num_binary_op!(stack, -);
                    }
                    Op::Div => num_binary_op!(stack, /),
                    Op::Mul => num_binary_op!(stack, *),
                    Op::Neg => {
                        let val = stack.expect_pop().expect_float64();
                        stack.push(ZValue::Number(-val));
                    }
                    Op::Not => {
                        let val = stack.expect_pop();
                        let res = if val.is_truthy() { false } else { true };
                        stack.push(ZValue::bool(res));
                    }
                    Op::Nil => {
                        let _ = stack.push(ZValue::Nil);
                    }
                    Op::True => {
                        let _ = stack.push(ZValue::bool(true));
                    }
                    Op::False => {
                        let _ = stack.push(ZValue::bool(false));
                    }
                    Op::Concat => todo!(),
                    Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                        // let param = opcode.param.unwrap();
                        // let id = param.to_u32() as usize;
                        // let v = constants[id].clone();
                        // let v = self.chunk().try_read_const(opcode).unwrap();
                        let v = read_constant(&opcode, &chunk.constants)
                            .expect("Failed to read Constant!");

                        stack.push(v.clone());
                    }
                    Op::Unknown => todo!(),

                    // NOTE: ----- DEFINE GLOBAL -----
                    Op::DefineGlobal8 | Op::DefineGlobal16 | Op::DefineGlobal32 => {
                        let name = read_constant(&opcode, &chunk.constants)
                            .expect("Failed to read constant global!!!");
                        if let ZValue::Ident(s) = name {
                            let val = stack.expect_pop();
                            self.globals.insert(s.clone(), val);
                        }
                    }
                    Op::GetGlobal8 | Op::GetGlobal16 | Op::GetGlobal32 => {
                        if let Some(ZValue::Ident(name)) = read_constant(&opcode, &chunk.constants)
                        {
                            if let Some(val) = self.globals.get(name) {
                                stack.push(val.clone());
                            } else {
                                bail!(
                                    "{}",
                                    RuntimeError::VMUnknownIdentifier {
                                        name: name.clone(),
                                        opcode: opcode,
                                        constants: chunk.constants.to_owned(),
                                        globals: self.globals.clone()
                                    }
                                )
                            }
                        } else {
                            panic!("Attempted to get Global, but its value in VM constants is not ZValue::Ident.")
                        }
                    }

                    Op::SetGlobal8 | Op::SetGlobal16 | Op::SetGlobal32 => {
                        if let Some(ZValue::Ident(name)) = read_constant(&opcode, &chunk.constants)
                        {
                            let top = stack.peek_top().expect("Nothing in stack to peek!!!");

                            if self.globals.contains_key(name) {
                                self.globals.insert(name.clone(), top.clone());
                            } else {
                                bail!(
                                    "{}",
                                    RuntimeError::VMUnknownIdentifier {
                                        name: name.clone(),
                                        opcode: opcode,
                                        constants: chunk.constants.clone(),
                                        globals: self.globals.clone(),
                                    }
                                )
                            }
                        } else {
                        }
                    }
                    // NOTE: ----- END DEFINE GLOBAL -----
                    Op::Eq => logical_binary_op!(stack, ==),
                    Op::Gt => logical_binary_op!(stack, >),
                    Op::Lt => logical_binary_op!(stack, <),
                    Op::Ge => logical_binary_op!(stack, >),
                    Op::Le => logical_binary_op!(stack, <),
                    Op::NotEq => logical_binary_op!(stack, !=),

                    // NOTE: ----- DEFINE LOCAL -----
                    Op::DefineLocal8 | Op::DefineLocal16 | Op::DefineLocal32 => todo!(),
                    // NOTE: ----- END DEFINE LOCAL -----

                    // NOTE: ----- GET LOCAL -----
                    Op::GetLocal8 | Op::GetLocal16 | Op::GetLocal32 => todo!(),
                    // NOTE: ----- END GET LOCAL -----

                    // NOTE: ----- SET LOCAL -----
                    Op::SetLocal8 | Op::SetLocal16 | Op::SetLocal32 => todo!(),
                    // NOTE: ----- END SET LOCAL -----
                };

                // Ok(opcode.offset())
                // vm_exec_opcode(&mut self.stack, &chunk.constants, opcode)?;
            }

            let top = if let Some(t) = self.stack.peek_top() {
                t.clone()
            } else {
                ZValue::Nil
            };
            Ok(top)
        } else {
            bail!(
                "Depth out of range! current_max_depth: {}, given_depth: {}",
                self.chunks.len(),
                depth
            );
        }
    }

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
        let depth = self.compile_source(src)?;
        self.exec_chunk(depth)
    }

    pub fn bytecode(&self) -> &Bytecode {
        self.chunk().code()
    }
}

impl Display for VM {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let d = self.dump();
        write!(f, "{d}")
    }
}

fn read_constant<'a>(opcode: &Opcode, constants: &'a [ZValue]) -> Option<&'a ZValue> {
    if let Some(param) = opcode.param {
        let id = param.to_u32() as usize;
        Some(&constants[id])
    } else {
        println!("{:?}", opcode);
        None
    }
}

// fn compile_source(src: &str) -> anyhow::Result<Vec<Chunk>> {

// }
