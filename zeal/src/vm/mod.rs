use std::{fmt::Display, ops::Index};

use anyhow::bail;
use stack::Stack;

use crate::{
    ast::Ast,
    compiler::{
        chunk::Chunk,
        opcode::{Bytecode, Op, Opcode},
        Archon,
    },
    core_types::val::ZValue,
};

pub mod stack;

macro_rules! binary_op {
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
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            chunks: Vec::with_capacity(8),
            depth: 0,
            cc: 0,
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
            for opcode in chunk.iter() {
                let c = &self.chunks[self.depth];
                let _ = vm_exec_opcode(&mut self.stack, &c.constants, opcode)?;
            }

            // while self.cc < code.len() {
            //     let oc = self.expect_opcode_at(self.cc, depth);
            //
            //     let c = &self.chunks[self.depth];
            //     let offset = vm_exec_opcode(&mut self.stack, &c.constants, oc)?;
            //     self.cc += offset;
            // }
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

    pub fn exec_source(&mut self, src: &str) -> anyhow::Result<ZValue> {
        let depth = self.compile_source(src)?;
        println!("COMPILED SOURCE");
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

/// Executes given opcode and returns the offset index to the next opcode
fn vm_exec_opcode<const S: usize>(
    stack: &mut Stack<S>,
    constants: &[ZValue],
    opcode: Opcode,
) -> anyhow::Result<usize> {
    match opcode.op {
        Op::Return => {}
        Op::Print => {
            let v = stack.expect_pop();
            println!("{}", v.to_string())
        }
        Op::Pop => {
            let _ = stack.expect_pop();
        }
        Op::Add => {
            binary_op!(stack, +);
            // let right = self.stack.pop().expect_float64();
            // let left = self.stack.pop().expect_float64();
            // let res = ZValue::Number(left + right);
            // self.stack.push(res);
        }
        Op::Sub => {
            binary_op!(stack, -);
        }
        Op::Div => binary_op!(stack, /),
        Op::Mul => binary_op!(stack, *),
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
            let param = opcode.param.unwrap();
            let id = param.to_u32() as usize;
            let v = constants[id].clone();
            // let v = self.chunk().try_read_const(opcode).unwrap();
            stack.push(v);
        }
        Op::Unknown => todo!(),
    };

    Ok(opcode.offset())
}

// fn compile_source(src: &str) -> anyhow::Result<Vec<Chunk>> {

// }
