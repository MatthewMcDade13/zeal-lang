#[derive(Debug, Clone, Copy, Default, bytemuck::Zeroable, bytemuck::Pod)]
#[repr(C)]
pub struct AstNode {
    // meta: NodeInfo,
    val: NodeValue,
}

#[derive(Debug, Clone, Copy, Default, bytemuck::Zeroable, bytemuck::Pod)]
#[repr(transparent)]
pub struct NodeId(u32);

#[derive(Debug, Clone, Copy, Default, bytemuck::Zeroable, bytemuck::Pod)]
#[repr(transparent)]
pub struct NodeValue(u64);

#[derive(Debug, Clone, Copy, Default, bytemuck::Zeroable, bytemuck::Pod)]
#[repr(C)]
pub struct NodeInfoFlags {
    /// Represents [NodeType]
    ty: u8,

    /// Rerved, Will probably eventualy use the other 7 bytes for some other
    /// kind of enum, bitflag or other kind of node meta data
    _reserved: [u8; 7],
}

#[derive(Debug, Clone, Copy, Default, bytemuck::Zeroable, bytemuck::Pod)]
#[repr(transparent)]
pub struct StructOffset(u32);

#[derive(Debug, Default, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct NodeInfo {
    id: NodeId,
    flags: NodeInfoFlags,
}

impl NodeInfo {
    pub const fn zeroed() -> Self {
        bytemuck::zeroed::<Self>()
    }
    pub const fn as_type(self) -> NodeType {
        NodeType::from_num(self.flags.ty as u16)
    }

    pub const fn from_type(ty: NodeType) -> Self {
        Self {
            flags: NodeInfoFlags {
                ty: ty as u8,
                ..bytemuck::zeroed::<NodeInfoFlags>()
            },
            ..Self::zeroed()
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, bytemuck::Zeroable)]
#[repr(u8)]
pub enum NodeType {
    #[default]
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
    /// u8
    Byte,
    /// i32
    Int,
    /// i64
    Int64,

    /// f32
    Float,
    /// f64
    Float64,
    /// usize
    Address,

    Boolean,
    Call,
    List,
    String,
    Rune,
    Symbol,
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
                x if x == NodeType::Boolean as u16 => Self::Boolean,
                x if x == NodeType::Byte as u16 => Self::Byte,
                x if x == NodeType::Int as u16 => Self::Int,
                x if x == NodeType::Int64 as u16 => Self::Int64,
                x if x == NodeType::Float as u16 => Self::Float,
                x if x == NodeType::Float64 as u16 => Self::Float64,
                x if x == NodeType::Address as u16 => Self::Address,
                x if x == NodeType::Call as u16 => Self::Call,
                x if x == NodeType::List as u16 => Self::List,
                x if x == NodeType::Struct as u16 => Self::Struct,
                x if x == NodeType::String as u16 => Self::String,
                _ => Self::Invalid,
            }
        }
    }
}
