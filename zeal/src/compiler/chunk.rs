use std::fmt::Display;

use crate::{
    compiler::opcode::{OpParam, OP16, OP24, OP32, OP64, OP8},
    core_types::val::ZValue,
    sys::copy_slice_into,
};

use super::opcode::{Bytecode, Op, Opcode};

pub struct Chunk {
    buf: Bytecode,
    constants: Vec<ZValue>,
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
}

// impl IntoIterator for Chunk {
//     type Item = Opcode;
//
//     type IntoIter = std::vec::IntoIter<>//ChunkIter<_>;
//
//     fn into_iter(self) -> Self::IntoIter {
//         todo!()
//     }
// }
// // impl Display for Chunk {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         let mut s = String::new();
//
//         let mut biter = self.buf.iter();
//         while biter.next() {
//
//         }
//         for i in 0..self.buf.len() {}
//
//         write!(f, "{}", s)
//     }
// }
