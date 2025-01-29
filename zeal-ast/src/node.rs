use zeal_core::mem::str::{PoolStringAddr, StringPool};

#[derive(Debug, Clone)]
pub struct AstBuffer {
    pub strings: StringPool,
    pub tree: Vec<AstNode>,
}

impl AstBuffer {
    pub const fn new() -> Self {
        Self {
            strings: StringPool::new(),
            tree: Vec::new(),
        }
    }
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            strings: StringPool::with_capacity(cap),
            tree: Vec::with_capacity(cap),
        }
    }
}

// NOTE: Structs and arrays are technically the same thing as
// putting all values of struct/array onto buffer, so i suppose we can just
// directly reference the index of the value of the struct/array as if it were
// a regular constant value.

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct AstNode {
    header: NodeHeader,
    value_bytes: [u8; std::mem::size_of::<u64>()],
}

impl AstNode {
    pub const fn bytes32(&self) -> [u8; 4] {
        let bs = &self.value_bytes;
        [bs[0], bs[1], bs[2], bs[3]]
    }
    pub const fn bytes64(&self) -> [u8; 8] {
        let bs = &self.value_bytes;
        [bs[0], bs[1], bs[2], bs[3], bs[4], bs[5], bs[6], bs[7]]
    }

    pub const fn bytes16(&self) -> [u8; 2] {
        let bs = &self.value_bytes;
        [bs[0], bs[1]]
    }

    pub const fn byte(&self) -> u8 {
        self.value_bytes[0]
    }

    pub const fn bool(&self) -> bool {
        if self.byte() == 0 {
            false
        } else {
            true
        }
    }

    pub const fn as_constant(&self) -> AstConstant {
        if matches!(self.header.node_type(), NodeType::Constant) {
            match self.header.constant_type() {
                AstConstantType::None => AstConstant::Zero,
                AstConstantType::Bool => AstConstant::Bool(self.bool()),
                AstConstantType::Num32 => AstConstant::Num32(i32::from_le_bytes(self.bytes32())),
                AstConstantType::Num64 => AstConstant::Num64(i64::from_le_bytes(self.bytes64())),
                AstConstantType::Float32 => {
                    AstConstant::Float32(f32::from_le_bytes(self.bytes32()))
                }
                AstConstantType::Float64 => {
                    AstConstant::Float64(f64::from_le_bytes(self.bytes64()))
                }
                AstConstantType::Word => AstConstant::Word(isize::from_le_bytes(self.bytes64())),
                AstConstantType::String => {
                    AstConstant::String(PoolStringAddr::new(usize::from_le_bytes(self.bytes64())))
                }
            }
        } else {
            AstConstant::Zero
        }
    }

    pub const fn into_constant(self) -> AstConstant {
        self.as_constant()
    }
}

#[derive(Debug, Default, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct NodeTypeValue(u8);

impl NodeTypeValue {
    pub const fn into_type(self) -> NodeType {
        NodeType::from_num(self.0 as u16)
    }

    pub const fn from_type(ty: NodeType) -> Self {
        Self(ty as u8)
    }

    pub const fn new(val: u16) -> Self {
        let nt = NodeType::from_num(val);
        Self::from_type(nt)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AstConstant {
    Zero,
    Bool(bool),
    Num32(i32),
    Num64(i64),
    Float32(f32),
    Float64(f64),
    Word(isize),
    String(PoolStringAddr),
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum AstConstantType {
    None = 0,
    Bool,
    Num32,
    Num64,
    Float32,
    Float64,
    Word,
    String,
}

#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
struct AstConstantTypeValue(u8);

impl AstConstantTypeValue {
    pub const fn from_byte(byte: u8) -> Self {
        let at = Self(byte).into_type();
        Self(at as u8)
    }

    pub const fn from_type(ty: AstConstantType) -> Self {
        Self(ty as u8)
    }

    pub const fn into_type(self) -> AstConstantType {
        match self.0 {
            x if x == AstConstantType::Bool as u8 => AstConstantType::Bool,
            x if x == AstConstantType::Num32 as u8 => AstConstantType::Num32,
            x if x == AstConstantType::Float32 as u8 => AstConstantType::Float32,
            x if x == AstConstantType::Float64 as u8 => AstConstantType::Float64,
            x if x == AstConstantType::Word as u8 => AstConstantType::Word,
            x if x == AstConstantType::String as u8 => AstConstantType::String,
            _ => AstConstantType::None,
        }
    }
}

#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct NodeHeader {
    node_ty: NodeTypeValue,
    constant_ty: AstConstantTypeValue,
}

impl NodeHeader {
    pub const fn node_type(&self) -> NodeType {
        self.node_ty.into_type()
    }

    pub const fn constant_type(&self) -> AstConstantType {
        self.constant_ty.into_type()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum NodeType {
    None = 0,
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
    Negate,
    Not,
    Constant,
    Boolean,
    Call,
    List,
    String,
    Struct,
    Count,
    Invalid,
}

impl NodeType {
    pub const fn max_value() -> usize {
        Self::Count as usize
    }

    pub const fn from_num(val: u16) -> Self {
        if val >= Self::max_value() as u16 {
            Self::Invalid
        } else {
            match val {
                x if x == NodeType::None as u16 => Self::None,
                x if x == NodeType::Add as u16 => Self::Add,
                x if x == NodeType::Sub as u16 => Self::Sub,
                x if x == NodeType::Div as u16 => Self::Div,
                x if x == NodeType::Mul as u16 => Self::Mul,
                x if x == NodeType::Eq as u16 => Self::Eq,
                x if x == NodeType::Gt as u16 => Self::Gt,
                x if x == NodeType::Lt as u16 => Self::Lt,
                x if x == NodeType::Ge as u16 => Self::Ge,
                x if x == NodeType::Le as u16 => Self::Le,
                x if x == NodeType::NotEq as u16 => Self::NotEq,
                x if x == NodeType::Negate as u16 => Self::Negate,
                x if x == NodeType::Not as u16 => Self::Not,
                x if x == NodeType::Constant as u16 => Self::Constant,
                x if x == NodeType::Boolean as u16 => Self::Boolean,
                x if x == NodeType::Call as u16 => Self::Call,
                x if x == NodeType::List as u16 => Self::List,
                x if x == NodeType::Struct as u16 => Self::Struct,
                x if x == NodeType::String as u16 => Self::String,
                _ => Self::Invalid,
            }
        }
    }

    pub const fn to_value(self) -> NodeTypeValue {
        NodeTypeValue(self as u8)
    }
}
