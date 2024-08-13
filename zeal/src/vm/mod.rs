use std::fmt::Display;

use anyhow::bail;

use crate::{
    ast::Ast,
    compiler::{
        chunk::Chunk,
        opcode::{Bytecode, Op, Opcode},
        Archon,
    },
    core_types::val::ZValue,
};

macro_rules! binary_op {
    ($stack: ident, $op: tt) => {

        {

            let right = $stack.pop().expect_float64();
            let left = $stack.pop().expect_float64();
            let res = ZValue::Number(left $op right);
            $stack.push(res);
        }

    };
}

pub const STACK_MAX: usize = 0x20;
#[derive(Debug, Clone)]
pub struct VM {
    stack: Stack<STACK_MAX>,
    chunks: Vec<Chunk>,
    /// index of current running chunk.
    depth: usize,
    /// program counter for current running chunk.
    pc: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            chunks: Vec::with_capacity(8),
            depth: 0,
            pc: 0,
        }
    }

    #[inline]
    /// Compiles string of zeal source code into a single chunk and returns the depth (index) of the newly pushed chunk.
    pub fn compile_source(&mut self, src: &str) -> anyhow::Result<usize> {
        let ast = Ast::from_str(src)?;
        // println!("{ast}");
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
        self.pc = 0;
        for i in 0..self.chunks.len() {
            self.depth = i;
            let _ = self.exec_chunk(i)?;
        }
        Ok(())
    }

    pub fn exec_chunk(&mut self, depth: usize) -> anyhow::Result<ZValue> {
        if let Some(chunk) = self.chunks.get(depth) {
            let code = chunk.code();

            while self.pc < code.len() {
                let oc = self.expect_opcode_at(self.pc, depth);

                let c = &self.chunks[self.depth];
                let offset = vm_exec_opcode(&mut self.stack, &c.constants, oc)?;
                self.pc += offset;
            }
            // println!("{:?}", self.stack);
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
        println!("{:?}", self);
        self.exec_chunk(depth)
    }

    pub fn bytecode(&self) -> &Bytecode {
        self.chunk().code()
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
            let v = stack.pop();
            println!("{}", v.to_string())
        }
        Op::Pop => {
            let _ = stack.pop();
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
            let val = stack.pop().expect_float64();
            stack.push(ZValue::Number(-val));
        }
        Op::Not => {
            let val = stack.pop();
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

#[derive(Debug, Clone)]
struct Stack<const S: usize> {
    top: isize,
    buf: [ZValue; S],
}

impl<const S: usize> Stack<S> {
    pub fn new() -> Self {
        let buf: [ZValue; S] = (0..S)
            .map(|_| ZValue::Nil)
            .collect::<Vec<ZValue>>()
            .try_into()
            .unwrap();
        Self { top: 0, buf }
    }

    pub const fn peekn(&self, n: isize) -> Option<&ZValue> {
        let i = self.top() as isize + n;
        if i < 0 || i >= S as isize {
            None
        } else {
            let i = i as usize;
            Some(&self.buf[i])
        }
    }

    pub fn peekn_mut(&mut self, n: isize) -> Option<&mut ZValue> {
        let i = self.top() as isize + n;
        if i < 0 || i >= S as isize {
            None
        } else {
            let i = i as usize;
            Some(&mut self.buf[i])
        }
    }

    pub const fn peek_top(&self) -> Option<&ZValue> {
        self.peekn(self.top() as isize)
    }

    pub fn peek(&self) -> &ZValue {
        let i = self.top();
        self.peekn(i as isize)
            .expect("Stack peek index out of range! top: {i} | STACK_MAX: {STACK_MAX}")
    }

    pub fn peek_mut(&mut self) -> &mut ZValue {
        let i = self.top();
        self.peekn_mut(i as isize)
            .expect("Stack peek index out of range! top: {i} | STACK_MAX: {STACK_MAX}")
    }

    pub const fn top(&self) -> usize {
        if self.top - 1 < 0 {
            0
        } else {
            self.top as usize - 1
        }
    }

    pub const fn len(&self) -> usize {
        self.top() + 1
    }

    pub const fn max(&self) -> usize {
        S
    }

    /// Pushes value onto top of stack. Returns index of item that was pushed.
    pub fn push(&mut self, val: ZValue) -> usize {
        let i = self.top();
        self.buf[i] = val;
        self.top += 1;
        i
    }

    pub fn pop(&mut self) -> ZValue {
        let i = self.top();
        let v = self.buf[i].clone();
        self.top -= 1;
        v
    }
}
