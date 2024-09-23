use std::{
    fmt::Display, num::ParseIntError, ops::{Add, Deref, DerefMut, Div, Index, IndexMut, Mul, Rem, Sub}, slice
};


use crate::{
    ast::{BinaryOpType, UnaryOpType}, sys,
};


#[derive(Debug, Clone, Copy)]
pub enum VarOp {
    Declare,
    Get,
    Set,
}

impl Display for VarOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            VarOp::Declare => "decl",
            VarOp::Get => "getvar",
            VarOp::Set => "setvar,",
        };
        write!(f, "{s}")
    }
}

//

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Op {
    Return,
    Call,
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
    DeclareGlobal8,
    GetGlobal8,
    SetGlobal8,

    GetLocal8,
    SetLocal8,

    DeclareGlobal16,
    GetGlobal16,
    SetGlobal16,

    GetLocal16,
    SetLocal16,

    DeclareGlobal32,
    GetGlobal32,
    SetGlobal32,

    GetLocal32,
    SetLocal32,

    /// u16 param
    JumpTrue,
    /// u32 param
    LongJumpTrue,

    /// u16 param
    JumpFalse,
    /// u32 param
    LongJumpFalse,

    /// u16 param
    Jump,
    /// u32 param
    LongJump,

    NoOp,
    Unknown,
}


#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum OpParamSize {
    Byte = OP8 as u8,
    Byte16 = OP16 as u8,
    Byte24 = OP24 as u8,
    Byte32 = OP32 as u8,
    Byte64 = OP64 as u8,
}

impl OpParamSize {
    pub const fn from_param(param: &OpParam) -> Self {
        match param {
            OpParam::Byte(_) => Self::Byte,
            OpParam::Byte16(_) => Self::Byte16,
            OpParam::Byte24(_) => Self::Byte24,
            OpParam::Byte32(_) => Self::Byte32,
            OpParam::Byte64(_) => Self::Byte64,
        }
    }

    pub const fn squash_size(n: u64) -> Self {
        let p = OpParam::squash(n);
        match p {
            OpParam::Byte(_) => Self::Byte,
            OpParam::Byte16(_) => Self::Byte16,
            OpParam::Byte24(_) => Self::Byte24,
            OpParam::Byte32(_) => Self::Byte32,
            OpParam::Byte64(_) => Self::Byte64,
        }
    }

    pub const fn cast(self) -> u8 {
        self as u8
    }

    // TODO/NOTE: Why are these 2 fns here and not in OpParam?!??!?!
    pub const fn global_op(self, varop: VarOp) -> Op {
        Op::global(self, varop)
    }

    pub const fn local_op(self, varop: VarOp) -> Op {
        Op::local(self, varop)
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}



impl Op {
    pub const fn is_valid(&self) -> bool {
        !matches!(self, Op::Unknown)
    }

    pub const fn set_local(size: OpParamSize) -> Self {
        match size {
            OpParamSize::Byte => Op::SetLocal8,
            OpParamSize::Byte16 => Op::SetLocal16,
            OpParamSize::Byte24 => todo!(),
            OpParamSize::Byte32 => Op::SetLocal32,
            OpParamSize::Byte64 => todo!(),
        }
    }

    pub const fn get_local(size: OpParamSize) -> Self {
        match size {
            OpParamSize::Byte => Op::GetLocal8,
            OpParamSize::Byte16 => Op::GetLocal16,
            OpParamSize::Byte24 => todo!(),
            OpParamSize::Byte32 => Op::GetGlobal32,
            OpParamSize::Byte64 => todo!(),
        }
    }

    pub const fn set_global(size: OpParamSize) -> Self {
        match size {
            OpParamSize::Byte => Op::SetGlobal8,
            OpParamSize::Byte16 => Op::SetGlobal8,
            OpParamSize::Byte24 => todo!(),
            OpParamSize::Byte32 => Op::SetGlobal8,
            OpParamSize::Byte64 => todo!(),
        }
    }

    pub const fn get_global(size: OpParamSize) -> Self {
        match size {
            OpParamSize::Byte => Op::GetGlobal8,
            OpParamSize::Byte16 => Op::GetGlobal16,
            OpParamSize::Byte24 => todo!(),
            OpParamSize::Byte32 => Op::GetGlobal32,
            OpParamSize::Byte64 => todo!(),
        }
    }

    pub const fn declare_global(size: OpParamSize) -> Self {
        match size {
            OpParamSize::Byte => Op::DeclareGlobal8,
            OpParamSize::Byte16 => Op::DeclareGlobal16,
            OpParamSize::Byte24 => todo!(),
            OpParamSize::Byte32 => Op::DeclareGlobal32,
            OpParamSize::Byte64 => todo!(),
        }
    }

    pub const fn global(size: OpParamSize, varop: VarOp) -> Self {
        match varop {
            VarOp::Declare => Self::declare_global(size),
            VarOp::Get => Self::get_global(size),
            VarOp::Set => Self::set_global(size),
        }
    }

    pub const fn local(size: OpParamSize, varop: VarOp) -> Self {
        match varop {
            VarOp::Declare => panic!("No opcode exists for declaring a local binding!!!"),
            VarOp::Get => Self::get_local(size),
            VarOp::Set => Self::set_local(size),
        }
    }

    pub const fn stride(self) -> usize {
        if let Some(s) = self.param_size() {
            s.cast() as usize
        } else {
            0
        }
    }

    pub const fn param_size(&self) -> Option<OpParamSize> {
        let s = match self {
            Op::Const8 => OpParamSize::Byte,
            Op::Const16 => OpParamSize::Byte16,
            Op::Const24 => OpParamSize::Byte24,
            Op::Const32 => OpParamSize::Byte32,
            Op::Const64 => OpParamSize::Byte64,
            Op::DeclareGlobal8
            | Op::GetGlobal8
            | Op::SetGlobal8
            | Op::GetLocal8
            | Op::SetLocal8 => OpParamSize::Byte,

            Op::DeclareGlobal16
            | Op::JumpFalse
            | Op::Jump
            | Op::JumpTrue            
            | Op::GetGlobal16
            | Op::SetGlobal16
            | Op::GetLocal16
            | Op::SetLocal16 => OpParamSize::Byte16,

            Op::DeclareGlobal32
            | Op::LongJumpFalse
            | Op::LongJump
            | Op::LongJumpTrue
            | Op::GetGlobal32
            | Op::SetGlobal32
            | Op::GetLocal32
            | Op::SetLocal32 => OpParamSize::Byte32,

            Op::PopN => OpParamSize::Byte,
            _ => return None,
        };
        Some(s)
    }

    // pub const fn offset(self) -> usize {
    //     self.stride() + 1
    // }
}

impl From<Op> for u8 {
    fn from(value: Op) -> Self {
        value as u8
    }
}

impl From<BinaryOpType> for Op {
    fn from(value: BinaryOpType) -> Self {
        match value {
            BinaryOpType::Gt => Op::Gt,
            BinaryOpType::Lt => Op::Lt,
            BinaryOpType::Ge => Op::Ge,
            BinaryOpType::Le => Op::Le,
            BinaryOpType::And => panic!("No Bytecode op for And"),
            BinaryOpType::Or => panic!("No Bytecode op for Or"),
            BinaryOpType::Equals => Op::Eq,
            BinaryOpType::NotEquals => Op::NotEq,
            BinaryOpType::BitAnd => todo!(),
            BinaryOpType::BitOr => todo!(),
            BinaryOpType::Xor => todo!(),
            BinaryOpType::Concat => Op::Concat,
            BinaryOpType::Add => Op::Add,
            BinaryOpType::Sub => Op::Sub,
            BinaryOpType::Mul => Op::Mul,
            BinaryOpType::Div => Op::Div,
        }
    }
}

impl From<UnaryOpType> for Op {
    fn from(value: UnaryOpType) -> Self {
        match value {
            UnaryOpType::Negate => Op::Neg,
            UnaryOpType::Not => Op::Not,
        }
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

pub const OP8: usize = std::mem::size_of::<u8>();
pub const OP16: usize = std::mem::size_of::<u16>();
pub const OP24: usize = std::mem::size_of::<[u8; 3]>();
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


    pub fn from_slice(size: OpParamSize, src: &[u8]) -> Self {
        Self::try_from_slice(size, src).unwrap()
    }

    pub fn try_from_slice(size: OpParamSize, src: &[u8]) -> anyhow::Result<Self> {
        
        let s = match size {
            OpParamSize::Byte => Self::Byte(src[0]),  
            OpParamSize::Byte16 => Self::Byte16(src.try_into()?), 
            OpParamSize::Byte24 => Self::Byte24(src.try_into()?), 
            OpParamSize::Byte32 => Self::Byte32(src.try_into()?),
            OpParamSize::Byte64 => Self::Byte64(src.try_into()?),
        };
        Ok(s)
    }

    pub const fn zeroed(size: OpParamSize) -> Self {
        match size {
            OpParamSize::Byte => Self::Byte(0), 
            OpParamSize::Byte16 => Self::Byte16([0; 2]), 
            OpParamSize::Byte24 => Self::Byte24([0; 3]), 
            OpParamSize::Byte32 => Self::Byte32([0; 4]), 
            OpParamSize::Byte64 => Self::Byte64([0; 8]), 
        }
    }

    pub const fn len(&self) -> usize {
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

    pub const fn to_bytes(self) -> [u8; 8] {
        self.to_usize().to_le_bytes()
    }

    pub fn write_slice(&mut self, sl: &[u8]) {
        match self {
            OpParam::Byte(b) => *b = sl[0],  
            OpParam::Byte16(bs) => sys::copy_slice_into(bs, sl),  
            OpParam::Byte24(bs) => sys::copy_slice_into(bs, sl), 
            OpParam::Byte32(bs) => sys::copy_slice_into(bs, sl),
            OpParam::Byte64(bs) => sys::copy_slice_into(bs, sl),
        }
    }


    pub fn write(&mut self, n: u64) {
        match self {
            OpParam::Byte(b) => *b = n as u8, 
            OpParam::Byte16(bs) => bs.copy_from_slice((n as u16).to_le_bytes().as_ref()), 
            OpParam::Byte24(bs) => bs.copy_from_slice(&(n as u32).to_le_bytes()[..3]), 
            OpParam::Byte32(bs) => bs.copy_from_slice((n as u32).to_le_bytes().as_ref()), 
            OpParam::Byte64(bs) => bs.copy_from_slice((n as u64).to_le_bytes().as_ref()),
        }
        
    }

    /// Tries to get smallest param for any positive 64bit number (eg: Byte for numbers 0-255, ect.)
    pub const fn squash(v: u64) -> Self { 
        match v {
            0..0xFF => Self::Byte(v as u8), 
            0xFF..0xFFFF => {
                let b = v as u16;
                Self::byte16(b)
            }
            0xFFFF..0xFFFFFF => Self::byte24(v as u32),
            0xFFFFFF..0xFFFFFFFF => Self::byte32(v as u32), 
            _ => Self::byte64(v as u64) 
        }
    }



    pub const fn byte(v: u8) -> Self {
        Self::Byte(v)
    }

    pub const fn byte16(v: u16) -> Self {
        Self::Byte16(v.to_le_bytes())
    }

    pub const fn byte24(n: u32) -> Self {
        let [a, b, c, _] = n.to_le_bytes();
        Self::Byte24([a, b, c])
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
    
    pub const fn to_u64(self) -> u64 {
        match self {
            OpParam::Byte(b) => b as u64,
            OpParam::Byte16(b) => u64::from_le_bytes([b[0], b[1], 0, 0, 0, 0, 0, 0]),
            OpParam::Byte24(b) => u64::from_le_bytes([b[0], b[1], b[2], 0, 0, 0, 0, 0]),
            OpParam::Byte32(b) => u64::from_le_bytes([b[0], b[1], b[2], b[3], 0, 0, 0, 0]),
            OpParam::Byte64(b) => u64::from_le_bytes(b),
        }
    }


    pub const fn to_usize(self) -> usize {
        match self {
            OpParam::Byte(b) => b as usize,
            OpParam::Byte16(b) => usize::from_le_bytes([b[0], b[1], 0, 0, 0, 0, 0, 0]),
            OpParam::Byte24(b) => usize::from_le_bytes([b[0], b[1], b[2], 0, 0, 0, 0, 0]),
            OpParam::Byte32(b) => usize::from_le_bytes([b[0], b[1], b[2], b[3], 0, 0, 0, 0]),
            OpParam::Byte64(b) => usize::from_le_bytes(b),
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
pub enum Opcode {
    Byte(Op),
    WithParam { op: Op, param: OpParam },
}

impl Opcode {


    pub const fn no_op() -> Self {
        Self::Byte(Op::NoOp)
    }

    pub const fn try_param(&self) -> Option<&OpParam> {
        match self {
            Opcode::Byte(_) => None, 
            Opcode::WithParam { param, .. } => Some(param), 
        }
    }
    /// Offset from Self::op_byte() to the next valid op_byte()
    pub const fn offset(&self) -> usize {
        match self {
            Opcode::Byte(_) => 1,
            Opcode::WithParam { param, .. } => param.len() + 1,
        }
    }

    pub const fn op(self) -> Op {
        match self {
            Opcode::Byte(op) => op,
            Opcode::WithParam { op, .. } => op,
        }
    }

    pub fn op_byte(&self) -> u8 {
        self.op() as u8
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
        Self::WithParam {
            op: Op::Const8,
            param: OpParam::Byte(v),
        }
    }

    pub const fn const16(v: u16) -> Self {
        Self::WithParam {
            op: Op::Const16,
            param: OpParam::Byte16(v.to_le_bytes()),
        }
    }

    pub fn const24(v: u32) -> Self {
        let bs = &v.to_le_bytes()[..3];
        let mut dst = [0; 3];
        dst.copy_from_slice(bs);
        Self::WithParam {
            op: Op::Const24,
            param: OpParam::Byte24(dst),
        }
    }

    pub const fn const32(v: u32) -> Self {
        Self::WithParam {
            op: Op::Const32,
            param: OpParam::Byte32(v.to_le_bytes()),
        }
    }

    pub const fn const64(v: u64) -> Self {
        Self::WithParam {
            op: Op::Const64,
            param: OpParam::Byte64(v.to_le_bytes()),
        }
    }

    pub const fn new(op: Op) -> Self {
        Self::Byte(op)
    }

    pub const fn with_param(op: Op, param: OpParam) -> Self {
        Self::WithParam { op, param }
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

impl Index<usize> for Bytecode {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buf[index]
    }
}

impl IndexMut<usize> for Bytecode {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.buf[index]
    }
}

impl Bytecode {
    pub fn slice(&self) -> &[u8] {
        &self.buf
    }

    pub fn slice_mut(&mut self) -> &mut [u8] {
        &mut self.buf
    }
    pub const fn zeroed() -> Self {
        Self { buf: Vec::new() }
    }

    pub fn len(&self) -> usize {
        self.buf.len()
    }

    pub fn opcode_slice(&self, index: usize) -> Option<&[u8]> {
        if self.buf.len() == 0 || index >= self.buf.len() {
            return None;
        }
        
        let op = self.buf[index];
        let op = Op::from(op);
        if !op.is_valid() {
            return None;
        }


        
        let stride = op.stride();
        if stride == 0 {
            let end = index + 1;
            Some(&self.buf[index..end])
        } else {
            let beg = index;
            let end = index + stride; 
        
            Some(&self.buf[beg..=end])

        }

    }

    pub fn opcode_slice_mut(&mut self, index: usize) -> Option<&mut [u8]> {
        // assert!(self.buf.len() > 0 && index < self.buf.len());

        if self.buf.len() == 0 || index >= self.buf.len() {
            return None;
        }
        
        let op = self.buf[index];
        let op = Op::from(op);
        if !op.is_valid() {
            return None;
        }


        
        let stride = op.stride();
        if stride == 0 {
            let end = index + 1;
            Some(&mut self.buf[index..end])
        } else {
            let beg = index;
            let end = index + stride; 
        
            Some(&mut self.buf[beg..=end])

        }
            // same size, this is single element 
    }

    pub fn opcode_at(&self, index: usize) -> Option<Opcode> {
        let sl = self.opcode_slice(index)?;
        let (op, p) = match *sl {
            [op] => (op, None),
            [op, a] => (op, Some(OpParam::Byte(a))),
            [op, a, b] => (op, Some(OpParam::Byte16([a, b]))),
            [op, a, b, c] => (op, Some(OpParam::Byte24([a, b, c]))),
            [op, a, b, c, d] => (op, Some(OpParam::Byte32([a, b, c, d]))),
            [op, a, b, c, d, e, f, g, h] => (op, Some(OpParam::Byte64([a, b, c, d, e, f, g, h]))),
            _ => return None
            
        };
        let op = Op::from(op);
        

        let opcode = if let Some(param) = p {
            Opcode::WithParam { op, param}
        } else {
            Opcode::Byte(op)
        };

        Some(opcode)
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

    pub fn write_param_at(&mut self, index: usize, param: OpParam) {
        assert!(
            index < self.buf.len(),
            "Bytecode::write_param_at=> attempt to write to index that is out of range!!! (i: {index}, len: {}, param: {})",
            self.buf.len(),
            param.to_f64(),
        );
        if let Some(opcode) = self.opcode_at(index) {
            match opcode {
                Opcode::Byte(_) =>  panic!("Tried to write to an opcode param, but the opcode does not support parameters!!! Got: {opcode:?}"),
                Opcode::WithParam { op, param: p } => {
                    let op_byte = op as u8; 
                    assert!(op.is_valid(), "Bytecode::write_param_at => indexed bytecode at an index that is an invalid Op byte value. Check index offsets!!! Max valid Op byte: {}, Got: {}", Op::Unknown as u8, op_byte);
                    let beg = index + 1;
                    let end = beg + param.len();
                    let pslice = match &param {
                        OpParam::Byte(b) => &[*b] as &[u8],
                        OpParam::Byte16(bs) => bs,
                        OpParam::Byte24(bs) => bs,
                        OpParam::Byte32(bs) => bs,
                        OpParam::Byte64(bs) => bs,
                    };

                    let dst = &mut self.buf[beg..end];

                    assert!(
                        dst.len() == pslice.len(),
                        "dst.len() {:?} == pslice.len() {:?} :: Opcode: {:?}",
                        dst,
                        pslice,
                        opcode,
                    );
                    dst.copy_from_slice(pslice);

                } 
            }
        } else {
            panic!("Tried to index an opcode that is out of range!!");
        }
    }
}


