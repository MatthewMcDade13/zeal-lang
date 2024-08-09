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
    ($self: ident, $op: tt) => {

        {

            let right = $self.stack.pop().expect_float64();
            let left = $self.stack.pop().expect_float64();
            let res = ZValue::Number(left $op right);
            $self.stack.push(res);
        }

    };
}

pub const STACK_MAX: usize = 0xFFFF;
pub struct VM {
    stack: Stack<STACK_MAX>,
    code: Chunk,
    pc: usize,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Stack::new(),
            code: Chunk::default(),
            pc: 0,
        }
    }
    pub fn exec(&mut self, ch: &Chunk) -> anyhow::Result<()> {
        let code = ch.code();
        let consts = ch.constants();
        let mut i = 0;
        while i < code.len() {
            let oc = code.opcode_at(i);
        }
        todo!()
    }

    pub fn exec_chunks(&mut self, chs: &[Chunk]) -> anyhow::Result<()> {
        for c in chs {
            self.exec(c)?;
        }
        Ok(())
    }

    pub fn exec_source(&mut self, src: &str) -> anyhow::Result<()> {
        let cs = compile_source(src)?;
        self.exec_chunks(&cs)?;
        Ok(())
    }

    /// Executes given opcode and returns the offset index to the next opcode
    fn exec_opcode(&mut self, opcode: Opcode) -> anyhow::Result<usize> {
        match opcode.op {
            Op::Return => {}
            Op::Print => {
                let v = self.stack.pop();
                println!("{}", v.to_string())
            }
            Op::Pop => {
                let _ = self.stack.pop();
            }
            Op::Add => {
                binary_op!(self, +);
                // let right = self.stack.pop().expect_float64();
                // let left = self.stack.pop().expect_float64();
                // let res = ZValue::Number(left + right);
                // self.stack.push(res);
            }
            Op::Sub => {
                binary_op!(self, -);
            }
            Op::Div => binary_op!(self, /),
            Op::Mul => binary_op!(self, *),
            Op::Neg => todo!(),
            Op::Not => todo!(),
            Op::Nil => todo!(),
            Op::True => todo!(),
            Op::False => todo!(),
            Op::Concat => todo!(),
            Op::Const8 => todo!(),
            Op::Const16 => todo!(),
            Op::Const24 => todo!(),
            Op::Const32 => todo!(),
            Op::Const64 => todo!(),
            Op::Unknown => todo!(),
        };

        Ok(opcode.offset())
    }
}

fn compile_source(src: &str) -> anyhow::Result<Vec<Chunk>> {
    let ast = Ast::from_str(src)?;
    Archon::compile(&ast)
}

struct Stack<const S: usize> {
    top: usize,
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
        self.top
    }

    pub const fn len(&self) -> usize {
        self.top + 1
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
