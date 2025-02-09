use std::ops::{Deref, Sub};

use zeal_core::mem::{str::PoolStringAddr, MemCell};

/// Used to wrap an index into typeinfo buffer
#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct StructDefId(u32);

#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct StructDef {
    id: StructDefId,
    name: PoolStringAddr,
    fields: MemCell,
    fields_len: u32,
}

/// Holds an unsigned integer, representing the offset distance from an index.
/// This offset is subtracted from the index its offset from
/// Essentially wraps a signed integer for subtracting from unsigned integers and prevents
/// unsigned int underflows when casting from inner unsigned int to signed int
#[derive(
    Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable, PartialEq, Eq, PartialOrd, Ord,
)]
#[repr(transparent)]
pub struct Offset(u32);

impl Offset {
    pub const fn new(delta: usize) -> Self {
        Self(delta as u32)
    }

    pub const fn is_none(self) -> bool {
        self.0 == 0
    }
}

impl Sub<Offset> for usize {
    type Output = usize;

    fn sub(self, rhs: Offset) -> Self::Output {
        let left = self as isize;
        let right = rhs.0 as isize;
        std::cmp::max(0, left - right) as usize
    }
}

impl Sub<usize> for Offset {
    type Output = Self;

    fn sub(self, rhs: usize) -> Self::Output {
        let left = self.0 as isize;
        let right = rhs as isize;
        let res = std::cmp::max(0, left - right);
        Self(res as u32)
    }
}

#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct StructField {
    owner: StructDefId,
    name: PoolStringAddr,
    typeinfo: TypeInfo,
    /// Forward offset from start of struct to the location of this field in owning struct
    /// (owning struct start index + offset)
    offset: Offset,
}

#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct TypeInfo {
    /// Convertable to [CoreType]
    core_ty: u16,
    /// This is not a full u32/u64, as if this core_ty is Struct or List, then
    /// we get the length from type table, not here, thus the max this field
    /// can ever be is 8
    size_bytes: u16,
    /// May or may not be used, depending on the value of core_ty
    /// only really used to determine the size of this particular type if its
    /// not a core primitive
    info: StructDefId,
}

impl TypeInfo {
    pub const fn primitive(ty: CoreType) -> Option<Self> {
        if ty.is_integer() {
            let size_bytes = ty.sizeof_int() as u16;
            let s = Self {
                core_ty: ty as u16,
                size_bytes,
                ..bytemuck::zeroed::<Self>()
            };
            Some(s)
        } else {
            None
        }
    }

    pub const fn structdef(info: StructDefId) -> Self {
        let core_ty = CoreType::Struct as u16;
        let size_bytes = 0;
        Self {
            core_ty,
            size_bytes,
            info,
        }
    }

    pub const fn is_primitive(&self) -> bool {
        CoreType::try_from_byte(self.core_ty as u8).is_some()
    }

    pub const fn is_struct(&self) -> bool {
        let Some(ct) = CoreType::try_from_byte(self.core_ty as u8) else {
            return false;
        };
        ct.is_struct()
    }
}

#[derive(Debug, Clone, Copy, Default)]
#[repr(u8)]
pub enum CoreType {
    #[default]
    None = 0,
    /// u8
    Byte,
    /// i8
    Char,
    /// u16
    UnShort,
    /// i16
    Short,
    /// i32
    Int,
    /// u32
    UnInt,

    /// u64
    UnInt64,
    /// i64
    Int64,

    /// f32
    Float,
    /// f64
    Float64,

    /// usize
    Address,

    /// isize
    PtrSize,

    Boolean,
    /// Contiguous array of elements of same size/type
    Vec,
    String,
    Rune,
    Symbol,

    Struct,

    Count,
    Invalid = 0xFF,
}

impl CoreType {
    pub const fn from_byte(byte: u8) -> Self {
        Self::try_from_byte(byte).expect("Cannot convert from u8 to enum CoreType! Given byte value was greater than the count of variants in CoreType!!")
    }

    pub const fn try_from_byte(byte: u8) -> Option<Self> {
        let s = match byte {
            x if x >= Self::Count as u8 => Self::Invalid,
            x if x == Self::None as u8 => Self::None,
            x if x == Self::Byte as u8 => Self::Byte,
            x if x == Self::Char as u8 => Self::Char,
            x if x == Self::Short as u8 => Self::Short,
            x if x == Self::UnShort as u8 => Self::UnShort,
            x if x == Self::Int as u8 => Self::Int,
            x if x == Self::UnInt as u8 => Self::UnInt,
            x if x == Self::Int64 as u8 => Self::Int64,
            x if x == Self::Float as u8 => Self::Float,
            x if x == Self::Float64 as u8 => Self::Float64,
            x if x == Self::Address as u8 => Self::Address,
            x if x == Self::Vec as u8 => Self::Vec,
            x if x == Self::PtrSize as u8 => Self::PtrSize,
            x if x == Self::Boolean as u8 => Self::Boolean,
            x if x == Self::String as u8 => Self::String,
            x if x == Self::Rune as u8 => Self::Rune,
            x if x == Self::Symbol as u8 => Self::Symbol,
            x if x == Self::Struct as u8 => Self::Struct,
            _ => return None,
        };
        Some(s)
    }
    /// Gets size in bytes of this CoreType. Returns 0 if
    /// type is not an integer type
    pub const fn sizeof_int(&self) -> usize {
        match self {
            CoreType::Boolean | CoreType::Byte | CoreType::Char => 1,
            CoreType::UnShort | CoreType::Short => 2,
            CoreType::Float | CoreType::Int | CoreType::UnInt => 4,
            CoreType::PtrSize
            | CoreType::Address
            | CoreType::Float64
            | CoreType::UnInt64
            | CoreType::Int64 => 8,

            CoreType::Vec
            | CoreType::String
            | CoreType::Rune
            | CoreType::Symbol
            | CoreType::Struct
            | CoreType::None
            | CoreType::Count
            | CoreType::Invalid => 0,
        }
    }

    pub const fn is_integer(&self) -> bool {
        self.sizeof_int() != 0
    }

    pub const fn is_struct(&self) -> bool {
        matches!(self, Self::Struct)
    }
}

/// Associates a symbol or rune with a Type
#[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct Binding {
    identifier: PoolStringAddr,
    typeinfo: TypeInfo,
}
