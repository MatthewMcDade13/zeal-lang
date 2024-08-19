use std::fmt::Display;

use crate::{
    compiler::opcode::{OpParam, OP16, OP24, OP32, OP64, OP8},
    core_types::{
        str::{ZIdent, ZString},
        val::ZValue,
    },
    sys::copy_slice_into,
    vm::{self, VM},
};

use super::{
    opcode::{read_raw_slice_as_bytecode, Bytecode, Op, Opcode, VarOp},
    state::ScopeState,
};

#[derive(Debug, Clone, Copy)]
pub struct ChunkIter {
    begin: *const u8,
    i: usize,
    len: usize,
}

impl Iterator for ChunkIter {
    type Item = Opcode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 || self.i >= self.len {
            None
        } else {
            let opcode = unsafe { read_raw_slice_as_bytecode(self.begin, self.i, self.len) }
                .expect(&format!("cant get opcode from chunk! {:?}", self));
            let offset = opcode.offset();
            self.i += offset;
            Some(opcode)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub buf: Bytecode,
    pub constants: Vec<ZValue>,
    pub scope: ScopeState,
}

impl IntoIterator for Chunk {
    type Item = Opcode;

    type IntoIter = ChunkIter;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Default for Chunk {
    fn default() -> Self {
        Self::with_capacity(Self::DEFAULT_CAPACITY, Self::DEFAULT_CAPACITY)
    }
}

impl Chunk {
    pub const DEFAULT_CAPACITY: usize = 255;
    pub fn with_capacity(buf_cap: usize, const_cap: usize) -> Self {
        Self {
            buf: Bytecode::from(Vec::with_capacity(buf_cap)),
            constants: Vec::with_capacity(const_cap),
            scope: ScopeState::with_capacity(Self::DEFAULT_CAPACITY),
        }
    }

    pub fn iter(&self) -> ChunkIter {
        let begin = self.buf.slice().as_ptr();
        ChunkIter {
            begin,
            i: 0,
            len: self.buf.len(),
        }
    }

    #[inline]
    pub fn opcode_at(&self, index: usize) -> Option<Opcode> {
        self.buf.opcode_at(index)
    }

    #[inline]
    pub fn expect_opcode_at(&self, index: usize) -> Opcode {
        self.buf.expect_opcode_at(index)
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.buf.len()
    }

    #[inline]
    pub fn const_len(&self) -> usize {
        self.constants.len()
    }

    pub fn append_chunk(&mut self, other: &Chunk) {
        self.buf.append_bytes(other.buf.slice());
        self.constants.extend_from_slice(&other.constants);
    }

    /// Gets bygtecode as a raw &[u8]
    #[inline]
    pub fn bytes(&self) -> &[u8] {
        self.buf.slice()
    }

    pub const fn code(&self) -> &Bytecode {
        &self.buf
    }

    #[inline]
    pub fn constants(&self) -> &[ZValue] {
        self.constants.as_ref()
    }

    pub fn try_read_const(&self, opcode: Opcode) -> Option<&ZValue> {
        match opcode.op {
            Op::Const8 | Op::Const16 | Op::Const24 | Op::Const32 | Op::Const64 => {
                let param = opcode.param.unwrap();
                let id = param.to_u32() as usize;
                Some(&self.constants[id])
            }
            _ => None,
        }
    }

    #[inline]
    pub fn push_string(&mut self, string: &str) -> usize {
        self.push_constant(ZValue::string(ZString::from(string)))
    }

    #[inline]
    pub fn push_ident(&mut self, ident: ZIdent) -> usize {
        self.push_constant(ZValue::ident(ident))
    }

    #[inline]
    pub fn push_ident_str(&mut self, string: &str) -> usize {
        self.push_constant(ZValue::ident_from_str(string))
    }

    pub fn push_variable(&mut self, name: ZIdent, var_op: VarOp) -> usize {
        let v = ZValue::Ident(name);

        let id = self.constants.len();
        self.constants.push(v);

        let param = OpParam::pack(id as u64);
        let is_global = self.scope.depth() == 0;
        let op = match var_op {
            VarOp::Define => {
                if is_global {
                    Op::DefineGlobal8
                } else {
                    Op::DefineLocal8
                }
            }
            VarOp::Get => {
                if is_global {
                    Op::GetGlobal8
                } else {
                    Op::GetLocal8
                }
            }
            VarOp::Set => {
                if is_global {
                    Op::SetGlobal8
                } else {
                    Op::SetLocal8
                }
            }
        };

        let op = Opcode {
            op,
            param: Some(param),
        };
        self.push_opcode(op);
        param.to_u32() as usize
    }
    /// adds v to constant table, returns id of constant
    pub fn push_constant(&mut self, v: ZValue) -> usize {
        let id = self.constants.len();
        self.constants.push(v);
        let param = OpParam::pack(id as u64);
        let op = param.as_const_op();

        let op = Opcode {
            op,
            param: Some(param),
        };
        self.push_opcode(op);
        param.to_u32() as usize
    }

    pub fn push_number(&mut self, n: f64) -> usize {
        self.push_constant(ZValue::number(n))
    }

    pub fn push_opcode<T>(&mut self, opcode: T)
    where
        T: Into<Opcode>,
    {
        let opcode = opcode.into();
        self.buf.push(opcode.op_byte());
        if let Some(p) = opcode.param {
            match p {
                OpParam::Byte(b) => {
                    self.buf.push(b);
                }
                OpParam::Byte16(src) => {
                    self.buf.extend_from_slice(&src);
                }
                OpParam::Byte24(src) => {
                    self.buf.extend_from_slice(&src);
                }
                OpParam::Byte32(src) => {
                    self.buf.extend_from_slice(&src);
                }
                OpParam::Byte64(src) => {
                    self.buf.extend_from_slice(&src);
                }
            }
        }
    }

    pub fn debug_dissassembly(&self) -> String {
        let mut s = String::new();
        let mut i = 0;
        while i < self.buf.len() {
            let opcode = self.buf.opcode_at(i).unwrap();
            let op = match opcode.op {
                Op::Return => "RETURN",
                Op::Println => "PRINTLN",
                Op::Print => "PRINT",
                Op::Pop => "POP",
                Op::PopN => {
                    i += 1;
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("POPN => {}, actual: {}", index, self.constants[index])
                }
                Op::Add => "ADD",
                Op::Sub => "SUB",
                Op::Div => "DIV",
                Op::Mul => "MUL",
                Op::Neg => "NEGATE",
                Op::Not => "NOT",
                Op::Nil => "NIL",
                Op::True => "TRUE",
                Op::False => "FALSE",
                Op::Concat => "CONCAT",
                Op::Const8 => {
                    i += opcode.op.offset();
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST8 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const16 => {
                    i += opcode.op.offset();
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST16 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const24 => {
                    i += opcode.op.offset();
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST24 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const32 => {
                    i += opcode.op.offset();
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST32 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const64 => {
                    i += opcode.op.offset();
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST64 => {}, actual: {}", index, self.constants[index])
                }
                Op::Unknown => "UNKNOWN_OP",
                Op::DefineGlobal8 => todo!(),
                Op::GetGlobal8 => todo!(),
                Op::SetGlobal8 => todo!(),

                Op::DefineLocal8 => todo!(),
                Op::GetLocal8 => todo!(),
                Op::SetLocal8 => todo!(),
                Op::Eq => "EQ",
                Op::Gt => "GT",
                Op::Lt => "LT",
                Op::Ge => "GE",
                Op::Le => "LE",
                Op::NotEq => "NEQ",
                Op::DefineGlobal16 => todo!(),
                Op::GetGlobal16 => todo!(),
                Op::SetGlobal16 => todo!(),
                Op::DefineLocal16 => todo!(),
                Op::GetLocal16 => todo!(),
                Op::SetLocal16 => todo!(),
                Op::DefineGlobal32 => todo!(),
                Op::GetGlobal32 => todo!(),
                Op::SetGlobal32 => todo!(),
                Op::DefineLocal32 => todo!(),
                Op::GetLocal32 => todo!(),
                Op::SetLocal32 => todo!(),
            };

            // println!("{i}");
            s.push_str(&format!("{op}\n"));
            i += 1;
        }
        s
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.debug_dissassembly();
        write!(f, "{s}")
    }
}
