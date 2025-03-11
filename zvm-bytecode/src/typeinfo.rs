use crate::constval::StringRange;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, zerocopy_derive::KnownLayout)]
#[repr(u32)]
pub enum TypeKind {
    Unit = 0,
    TypeVar,
    Bool,
    Float,
    Float64,
    Int,
    Uint,
    Int64,
    Uint64,
    // isize / i32
    Offset,
    // usize / u32
    IndexAddr,
    Bytes,
    String,
    Symbol,
    Array,
    Struct,
    Count,
}

impl TypeKind {
    const COUNT: u32 = TypeKind::Count as u32;
    pub const fn try_typeid(self) -> Option<TypeId> {
        let n = self as u32;
        if n < Self::COUNT {
            Some(TypeId::new(n))
        } else {
            None
        }
    }

    pub const fn is_native(self) -> bool {
        let n = self as u32;
        n < Self::COUNT
    }

    pub const fn is_composite(self) -> bool {
        !self.is_native()
    }
    pub const fn from_typeid(id: TypeId) -> Self {
        match id.0 {
            x if x == Self::Unit as u32 => Self::Unit,
            x if x == Self::TypeVar as u32 => Self::TypeVar,
            x if x == Self::Bool as u32 => Self::Bool,
            x if x == Self::Float as u32 => Self::Float,
            x if x == Self::Float64 as u32 => Self::Float64,
            x if x == Self::Int as u32 => Self::Int,
            x if x == Self::Int64 as u32 => Self::Int64,
            x if x == Self::Uint as u32 => Self::Uint,
            x if x == Self::Uint64 as u32 => Self::Uint64,
            x if x == Self::Offset as u32 => Self::Offset,
            x if x == Self::IndexAddr as u32 => Self::IndexAddr,
            x if x == Self::Bytes as u32 => Self::Bytes,
            x if x == Self::String as u32 => Self::String,
            x if x == Self::Symbol as u32 => Self::Symbol,
            x if x == Self::Array as u32 => Self::Array,
            _ => Self::Struct,
        }
    }
}

/// Wraps an index id into the type table
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    zerocopy_derive::FromBytes,
    zerocopy_derive::IntoBytes,
)]
#[repr(C)]
pub struct TypeId(u32);

pub struct TypeTable {
    types: Vec<TypeInfo>,
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes, zerocopy_derive::IntoBytes)]
pub struct TypeField {
    /// name of type field, in this modules string/symbol table
    name: StringRange,
    /// byte offset of field value location in struct. From start of the struct
    offset: u32,
    /// type of the field
    typeid: TypeId,
}

pub struct TypeLayout {}

impl TypeId {
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    pub const fn into_kind(self) -> TypeKind {
        TypeKind::from_typeid(self)
    }
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes, zerocopy_derive::IntoBytes)]
#[repr(C)]
pub struct TypeInfo {
    ty: u32,
    // if self.ty is < TypeKind::Count, then this is the same value as self.ty, othersie,
    // it is the index of the type in the type table
    id: TypeId,
}

impl TypeInfo {
    pub const fn try_from_kind(ty: TypeKind) -> Option<Self> {
        if let Some(id) = ty.try_typeid() {
            Some(Self { ty: ty as u32, id })
        } else {
            None
        }
    }

    pub const fn new(ty: TypeKind, id: TypeId) -> Self {
        Self { ty: ty as u32, id }
    }

    pub const fn composite(id: TypeId) -> Self {
        Self {
            ty: TypeKind::Struct as u32,
            id,
        }
    }
}
