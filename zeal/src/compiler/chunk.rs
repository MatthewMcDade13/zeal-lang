use std::fmt::Display;

use crate::{
    compiler::opcode::{OpParam, OP16, OP24, OP32, OP64, OP8},
    core_types::val::ZValue,
    sys::copy_slice_into,
};

use super::opcode::{read_ptr_as_bytecode, Bytecode, Op, Opcode};

#[derive(Debug, Clone, Copy)]
pub struct ChunkIter {
    begin: *const u8,
    i: usize,
    len: usize,
}

impl Iterator for ChunkIter {
    type Item = Opcode;

    fn next(&mut self) -> Option<Self::Item> {
        if self.i > self.len {
            None
        } else {
            let opcode = unsafe { read_ptr_as_bytecode(self.begin, self.i, self.len) }
                .expect("cant get opcode from chunk!");
            self.i += opcode.offset();
            Some(opcode)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub buf: Bytecode,
    pub constants: Vec<ZValue>,
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
    pub fn bytes(&self) -> &[u8] {
        self.buf.slice()
    }

    pub const fn code(&self) -> &Bytecode {
        &self.buf
    }

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

    pub fn push_number(&mut self, n: f64) {
        let id = self.constants.len();
        let v = ZValue::number(n);
        self.constants.push(v);

        let (param, op) = match id {
            0..0xFF => (OpParam::Byte(id as u8), Op::Const8),
            0xFF..0xFFFF => {
                let b = id as u16;
                (OpParam::Byte16(b.to_le_bytes()), Op::Const16)
            }
            0xFFFF..0xFFFFFF => {
                let b = id as u32;
                let b = b.to_le_bytes();
                let mut dst = [0u8; 3];
                dst.copy_from_slice(&b[..OP24]);
                // for i in 0..3 {
                // dst[i] = b[i];
                // }
                (OpParam::Byte24(dst), Op::Const24)
            }
            _ => panic!("Constant array longer than {}", 0xFFFFFF),
        };

        let op = Opcode {
            op,
            param: Some(param),
        };
        self.push_opcode(op);
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
                Op::Print => "PRINT",
                Op::Pop => "POP",
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
                    i += 1;
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST8 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const16 => {
                    i += 1;
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST16 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const24 => {
                    i += 1;
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST24 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const32 => {
                    i += 1;
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST32 => {}, actual: {}", index, self.constants[index])
                }
                Op::Const64 => {
                    i += 1;
                    let index = opcode.param.unwrap().to_u32() as usize;
                    &format!("CONST64 => {}, actual: {}", index, self.constants[index])
                }
                Op::Unknown => "UNKNOWN_OP",
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
