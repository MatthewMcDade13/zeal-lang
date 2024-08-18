use std::{
    ops::{Deref, DerefMut, Index, IndexMut},
    slice,
};

use anyhow::bail;

use crate::{
    core_types::val::ZValue,
    sys::{array_from_raw, array_from_slice},
};

#[derive(Debug, Clone, Copy)]
pub enum VarOp {
    Define,
    Get,
    Set,
}

//

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op {
    Return,
    Println,
    Print,
    Pop,
    // Pops n times. parameter: u8
    PopN,
    Add,
    Sub,
    Div,
    Mul,
    Eq,
    Gt,
    Lt,
    Ge,
    Le,
    NotEq,
    Neg,
    Not,
    Nil,
    True,
    False,
    Concat,
    Const8,
    Const16,
    Const24,
    Const32,
    Const64,
    DefineGlobal,
    GetGlobal,
    SetGlobal,

    DefineLocal,
    GetLocal,
    SetLocal,
    Unknown,
}

impl Op {
    pub const fn stride(&self) -> usize {
        match self {
            Op::Const8 => OP8,
            Op::Const16 => OP16,
            Op::Const24 => OP24,
            Op::Const32 => OP32,
            Op::Const64 => OP64,
            Op::PopN => OP8,
            _ => 0,
        }
    }

    pub const fn offset(self) -> usize {
        self.stride() + 1
    }
}

impl From<Op> for u8 {
    fn from(value: Op) -> Self {
        value as u8
    }
}

impl From<u8> for Op {
    fn from(value: u8) -> Self {
        if value >= Op::Unknown as u8 {
            Self::Unknown
        } else {
            let op: Op = unsafe { std::mem::transmute(value) };
            op
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]

pub struct Byte24(u8, u8, u8);
impl Byte24 {
    pub const fn to_slice(self) -> [u8; 3] {
        [self.0, self.1, self.2]
    }
}

pub const OP8: usize = std::mem::size_of::<u8>();
pub const OP16: usize = std::mem::size_of::<u16>();
pub const OP24: usize = std::mem::size_of::<Byte24>();
pub const OP32: usize = std::mem::size_of::<u32>();
pub const OP64: usize = std::mem::size_of::<u64>();

// #[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum OpParam {
    // Op(Op),
    Byte(u8),
    Byte16([u8; 2]),
    Byte24([u8; 3]),
    Byte32([u8; 4]),
    Byte64([u8; 8]),
}

impl OpParam {
    pub const fn nbytes(&self) -> usize {
        match self {
            OpParam::Byte(_) => OP8,
            OpParam::Byte16(_) => OP16,
            OpParam::Byte24(_) => OP24,
            OpParam::Byte32(_) => OP32,
            OpParam::Byte64(_) => OP64,
        }
    }

    pub const fn as_const_op(&self) -> Op {
        match self {
            OpParam::Byte(_) => Op::Const8,
            OpParam::Byte16(_) => Op::Const16,
            OpParam::Byte24(_) => Op::Const24,
            OpParam::Byte32(_) => Op::Const32,
            OpParam::Byte64(_) => Op::Const64,
        }
    }

    pub const fn offset(&self) -> usize {
        self.nbytes() + 1
    }

    /// Tries to get smallest param for any positive 64bit number (eg: Byte for numbers 0-255, ect.)
    pub const fn pack(v: u64) -> Self {
        match v {
            0..0xFF => Self::Byte(v as u8),
            0xFF..0xFFFF => {
                let b = v as u16;
                Self::Byte16(b.to_le_bytes())
            }
            0xFFFF..0xFFFFFFFF => {
                let b = v as u32;
                // for i in 0..3 {
                // dst[i] = b[i];
                // }
                Self::Byte32(b.to_le_bytes())
            }
            _ => Self::Byte64(v.to_le_bytes()),
        }
    }

    pub const fn byte(v: u8) -> Self {
        Self::Byte(v)
    }

    pub const fn byte16(v: u16) -> Self {
        Self::Byte16(v.to_le_bytes())
    }

    pub fn byte24(v: u32) -> Self {
        let bs = &v.to_le_bytes()[..3];
        let mut dst = [0; 3];
        dst.copy_from_slice(bs);
        Self::Byte24(dst)
    }

    pub const fn byte32(v: u32) -> Self {
        Self::Byte32(v.to_le_bytes())
    }

    pub const fn byte64(v: u64) -> Self {
        Self::Byte64(v.to_le_bytes())
    }

    pub const fn to_u8(self) -> u8 {
        match self {
            OpParam::Byte(b) => b,
            OpParam::Byte16(b) => b[0],
            OpParam::Byte24(b) => b[0],
            OpParam::Byte32(b) => b[0],
            OpParam::Byte64(b) => b[0],
        }
    }

    pub fn to_f64(self) -> f64 {
        match self {
            OpParam::Byte(b) => b as f64,
            OpParam::Byte16(b) => f64::from_le_bytes([b[0], b[1], 0, 0, 0, 0, 0, 0]),
            OpParam::Byte24(b) => f64::from_le_bytes([b[0], b[1], b[2], 0, 0, 0, 0, 0]),
            OpParam::Byte32(b) => f64::from_le_bytes([b[0], b[1], b[2], b[3], 0, 0, 0, 0]),
            OpParam::Byte64(b) => f64::from_le_bytes(b),
        }
    }

    pub const fn to_u32(self) -> u32 {
        match self {
            OpParam::Byte(b) => b as u32,
            OpParam::Byte16(b) => u32::from_le_bytes([b[0], b[1], 0, 0]),
            OpParam::Byte24(b) => u32::from_le_bytes([b[0], b[1], b[2], 0]),
            OpParam::Byte32(b) => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
            OpParam::Byte64(b) => u64::from_le_bytes(b) as u32,
        }
    }

    pub const fn to_i32(self) -> i32 {
        match self {
            OpParam::Byte(b) => b as i32,
            OpParam::Byte16(b) => i32::from_le_bytes([b[0], b[1], 0, 0]),
            OpParam::Byte24(b) => i32::from_le_bytes([b[0], b[1], b[2], 0]),
            OpParam::Byte32(b) => i32::from_le_bytes([b[0], b[1], b[2], b[3]]),
            OpParam::Byte64(b) => i64::from_le_bytes(b) as i32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Opcode {
    pub op: Op,
    pub param: Option<OpParam>,
}

impl Opcode {
    pub const fn offset(&self) -> usize {
        if let Some(param) = self.param {
            param.offset()
        } else {
            1
        }
    }
    pub fn op_byte(&self) -> u8 {
        self.op.into()
    }

    pub const fn const_word(v: u64) -> Self {
        match v {
            0..0xFF => Self::const8(v as u8),
            0xFF..0xFFFF => Self::const16(v as u16),
            0xFFFF..0xFFFFFFFF => Self::const32(v as u32),
            _ => Self::const64(v),
        }
    }

    pub fn const_long24(v: u32) -> Self {
        if v <= 0xFFFFFF {
            Self::const24(v)
        } else {
            Self::const32(v)
        }
    }

    pub const fn const8(v: u8) -> Self {
        Self {
            op: Op::Const8,
            param: Some(OpParam::Byte(v)),
        }
    }

    pub const fn const16(v: u16) -> Self {
        Self {
            op: Op::Const16,
            param: Some(OpParam::Byte16(v.to_le_bytes())),
        }
    }

    pub fn const24(v: u32) -> Self {
        let bs = &v.to_le_bytes()[..3];
        let mut dst = [0; 3];
        dst.copy_from_slice(bs);
        Self {
            op: Op::Const24,
            param: Some(OpParam::Byte24(dst)),
        }
    }

    pub const fn const32(v: u32) -> Self {
        Self {
            op: Op::Const32,
            param: Some(OpParam::Byte32(v.to_le_bytes())),
        }
    }

    pub const fn const64(v: u64) -> Self {
        Self {
            op: Op::Const64,
            param: Some(OpParam::Byte64(v.to_le_bytes())),
        }
    }

    pub const fn new(op: Op) -> Self {
        Self { op, param: None }
    }

    pub const fn with_param(op: Op, param: OpParam) -> Self {
        let param = Some(param);
        Self { op, param }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct Bytecode {
    buf: Vec<u8>,
}

impl Deref for Bytecode {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        self.buf.as_ref()
    }
}
impl DerefMut for Bytecode {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.buf.as_mut()
    }
}

impl From<Vec<u8>> for Bytecode {
    fn from(value: Vec<u8>) -> Self {
        Self { buf: value }
    }
}

impl From<&[u8]> for Bytecode {
    fn from(value: &[u8]) -> Self {
        Self {
            buf: value.to_vec(),
        }
    }
}

impl Bytecode {
    pub fn slice(&self) -> &[u8] {
        &self.buf
    }

    pub fn slice_mut(&mut self) -> &mut [u8] {
        &mut self.buf
    }
    pub fn zeroed(len: usize) -> Self {
        Self { buf: vec![0; len] }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn opcode_at(&self, index: usize) -> Option<Opcode> {
        read_slice_as_bytecode(self.buf.as_ref(), index)
    }

    pub fn expect_opcode_at(&self, index: usize) -> Opcode {
        self.opcode_at(index).expect(&format!(
            "Opcode index out of range! given: {index}, expected <: {}",
            self.buf.len()
        ))
    }

    pub fn append_bytes(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes)
    }
}

pub fn read_slice_as_bytecode(ops: &[u8], index: usize) -> Option<Opcode> {
    if index >= ops.len() {
        return None;
    }

    let op = Op::from(ops[index]);
    let stride = op.stride();
    let param = if stride > 0 {
        let iparam = index + stride;
        assert!(iparam < ops.len());
        let param_slice = &ops[index..iparam];
        match stride {
            OP8 => Some(OpParam::Byte(ops[iparam])),
            OP16 => Some(OpParam::Byte16(array_from_slice(param_slice))),
            OP24 => Some(OpParam::Byte24(array_from_slice(param_slice))),
            OP32 => Some(OpParam::Byte32(array_from_slice(param_slice))),
            OP64 => Some(OpParam::Byte64(array_from_slice(param_slice))),
            _ => unreachable!("invalid bytecode parameter length: {}", stride),
        }
    } else {
        None
    };
    Some(Opcode { op, param })
}

pub unsafe fn read_raw_slice_as_bytecode(
    ops: *const u8,
    index: usize,
    len: usize,
) -> Option<Opcode> {
    if len == 0 || index >= len {
        return None;
    }
    let op = *ops.add(index);
    let op = Op::from(op);

    let stride = op.stride();
    let param = if stride > 0 {
        let iparam = index + stride;
        assert!(iparam < len);
        // let param_slice = &ops[index..iparam];

        match stride {
            OP8 => Some(OpParam::Byte(*ops.add(iparam))),
            OP16 => Some(OpParam::Byte16(array_from_raw(ops, len))),
            OP24 => Some(OpParam::Byte24(array_from_raw(ops, len))),
            OP32 => Some(OpParam::Byte32(array_from_raw(ops, len))),
            OP64 => Some(OpParam::Byte64(array_from_raw(ops, len))),
            _ => unreachable!("invalid bytecode parameter length: {}", stride),
        }
    } else {
        None
    };
    Some(Opcode { op, param })
}

//
// impl ChunkIter {
//     pub const fn full_slice(&self) -> &[u8] {
//         unsafe { std::slice::from_raw_parts(self.beg, self.len) }
//     }
//
//     pub const fn len(&self) -> usize {
//         self.len
//     }
//
//     pub const fn end(&self) -> *const u8 {
//         unsafe { self.beg.add(self.len()) }
//     }
// }
//
// impl Iterator for ChunkIter {
//     type Item = Opcode;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         let i = self.curr;
//         let sl = self.full_slice();
//         if i >= self.len() {
//             None
//         } else {
//             let op = read_opcode(sl, i);
//             let offset = if let Some(param) = op.param {
//                 param.nbytes() + 1
//             } else {
//                 1
//             };
//             self.curr += offset;
//             Some(op)
//         }
//     }
// }
// unsafe fn read_opcode_raw(buf: *const u8, index: usize) -> Opcode {
//     let curr = *buf.add(index);
//     let op = Op::from(curr);
//     let stride = op.stride();
//     let param = if stride > 0 {
//         let iparam = buf.add(index + stride);
//         let param_slice = std::slice::from_ptr_range(range)&ops[index..iparam];
//         match stride {
//             OP8 => Some(OpParam::Byte(ops[iparam])),
//             OP16 => Some(OpParam::Byte16(array_from_slice(param_slice))),
//             OP24 => Some(OpParam::Byte24(array_from_slice(param_slice))),
//             OP32 => Some(OpParam::Byte32(array_from_slice(param_slice))),
//             OP64 => Some(OpParam::Byte64(array_from_slice(param_slice))),
//             _ => unreachable!("invalid bytecode parameter length: {}", stride),
//         }
//     } else {
//         None
//     };
//     Opcode { op, param }
// }

fn read_opcode(buf: &[u8], index: usize) -> Option<Opcode> {
    let ops: &[u8] = buf;
    if index >= ops.len() {
        return None;
    }

    let op = Op::from(ops[index]);
    let stride = op.stride();
    let param = if stride > 0 {
        let iparam = index + stride;
        assert!(iparam < ops.len());
        let param_slice = &ops[index..iparam];
        match stride {
            OP8 => Some(OpParam::Byte(ops[iparam])),
            OP16 => Some(OpParam::Byte16(array_from_slice(param_slice))),
            OP24 => Some(OpParam::Byte24(array_from_slice(param_slice))),
            OP32 => Some(OpParam::Byte32(array_from_slice(param_slice))),
            OP64 => Some(OpParam::Byte64(array_from_slice(param_slice))),
            _ => unreachable!("invalid bytecode parameter length: {}", stride),
        }
    } else {
        None
    };
    Some(Opcode { op, param })
}
