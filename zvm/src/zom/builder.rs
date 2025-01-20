use bytes::BytesMut;
use zeal_core::string::{ShortBuf, StrBuf};

use crate::code::CodeBlockBuilder;

#[derive(Debug, Clone)]
pub struct ModuleBuilder {
    name: String,
    structdefs: Vec<TypeDef>,
    funcdefs: Vec<TypeDef>,
    strings: Vec<StrBuf>,

    bytecode: CodeBlockBuilder,
}

#[derive(Debug, Clone)]
pub struct FuncBuilder {
    proto: FuncProto,
    buf: BytesMut,
}

#[derive(Debug, Clone)]
pub struct StructField {
    name: StrBuf,
    ty: TypeDef,
    byte_offset: usize,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    name: StrBuf,
    fields: Vec<StructField>,
    implements: Vec<TraitDef>,
}

impl StructDef {
    pub fn size_bytes(&self) -> usize {
        self.fields
            .iter()
            .fold(0, |acc, field| acc + field.ty.size_bytes)
    }
}

#[derive(Debug, Clone)]
pub struct TraitDef {
    name: StrBuf,
    methods: Vec<FuncProto>,
}

#[derive(Debug, Clone)]
pub struct FuncProto {
    name: StrBuf,
    params_ty: Vec<TypeDef>,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    Unit,
    Byte,
    Char,
    U16,
    I16,
    U32,
    I32,
    U64,
    I64,
    Float,
    /// Platform pointer size, ie: usize/isize
    Word,
    /// Similar to C/C++'s void* or golang's interface{}
    Any,
    Pointer,
}

/// Type Definitions for all possible types in Zeal.
/// Tentative Source of truth for Primitive types and User or StdLib defined types
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub ty: Type,
    pub size_bytes: usize,
}

impl TypeDef {
    pub const UNIT_SIZE: usize = 0;
    pub const BYTE_SIZE: usize = std::mem::size_of::<u8>();
    pub const CHAR_SIZE: usize = std::mem::size_of::<i8>();
    pub const U16_SIZE: usize = std::mem::size_of::<u16>();
    pub const I16_SIZE: usize = std::mem::size_of::<i16>();
    pub const U32_SIZE: usize = std::mem::size_of::<u32>();
    pub const I32_SIZE: usize = std::mem::size_of::<i32>();
    pub const U64_SIZE: usize = std::mem::size_of::<u64>();
    pub const I64_SIZE: usize = std::mem::size_of::<i64>();
    pub const FLOAT_SIZE: usize = std::mem::size_of::<f64>();
    pub const STRING_SIZE: usize = std::mem::size_of::<StrBuf>();
    pub const RUNE_SIZE: usize = Self::STRING_SIZE;
    pub const POINTER_SIZE: usize = std::mem::size_of::<usize>();

    pub const fn primitive(prim: PrimitiveType) -> Self {
        let size_bytes = match prim {
            PrimitiveType::Unit => Self::UNIT_SIZE,
            PrimitiveType::Byte => Self::BYTE_SIZE,
            PrimitiveType::Char => Self::CHAR_SIZE,
            PrimitiveType::U16 => Self::U16_SIZE,
            PrimitiveType::I16 => Self::I16_SIZE,
            PrimitiveType::U32 => Self::U32_SIZE,
            PrimitiveType::I32 => Self::I32_SIZE,
            PrimitiveType::U64 => Self::U64_SIZE,
            PrimitiveType::I64 => Self::I64_SIZE,
            PrimitiveType::Float => Self::FLOAT_SIZE,
            PrimitiveType::Word | PrimitiveType::Any | PrimitiveType::Pointer => Self::POINTER_SIZE,
        };
        Self {
            ty: Type::Primitive(prim),
            size_bytes,
        }
    }

    pub const fn string() -> Self {
        Self {
            ty: Type::String,
            size_bytes: std::mem::size_of::<StrBuf>(),
        }
    }

    pub const fn rune() -> Self {
        Self {
            ty: Type::Rune,
            size_bytes: std::mem::size_of::<StrBuf>(),
        }
    }

    pub const fn map() -> Self {
        Self {
            ty: Type::Map,
            size_bytes: Self::POINTER_SIZE,
        }
    }

    pub const fn set() -> Self {
        Self {
            ty: Type::Set,
            size_bytes: Self::POINTER_SIZE,
        }
    }

    pub const fn type_var() -> Self {
        Self {
            ty: Type::TypeVariable,
            size_bytes: 0,
        }
    }

    pub const fn dynamic_any() -> Self {
        Self {
            ty: Type::DynamicAny,
            size_bytes: 0,
        }
    }

    pub fn structdef(val: StructDef) -> Self {
        let size_bytes = val.size_bytes();
        Self {
            ty: Type::Struct(val),
            size_bytes,
        }
    }

    pub const fn traitdef(val: TraitDef) -> Self {
        Self {
            ty: Type::Trait(val),
            size_bytes: 0,
        }
    }

    pub fn funcdef(val: FuncProto) -> Self {
        Self {
            ty: Type::Func(val),
            size_bytes: Self::POINTER_SIZE,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    proto: FuncProto,
    bytecode: BytesMut,
}

#[derive(Debug, Clone)]
pub struct ModuleDef {
    name: ShortBuf,
    funcs: Vec<TypeDef>,
    submodules: Vec<TypeDef>,
    typedefs: Vec<TypeDef>,
}

#[derive(Debug, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Map,
    Set,

    String,
    Rune,
    Func(FuncProto),
    /// Used for generics and maybe type-checking
    TypeVariable,
    DynamicAny,
    Imported,
    Struct(StructDef),

    Trait(TraitDef),
    Module(ModuleDef),
}

impl Type {
    pub const fn unit() -> Self {
        Self::Primitive(PrimitiveType::Unit)
    }

    pub const fn byte() -> Self {
        Self::Primitive(PrimitiveType::Byte)
    }

    pub const fn signed_byte() -> Self {
        Self::Primitive(PrimitiveType::Char)
    }

    pub const fn u16() -> Self {
        Self::Primitive(PrimitiveType::U16)
    }

    pub const fn i16() -> Self {
        Self::Primitive(PrimitiveType::I16)
    }

    pub const fn u32() -> Self {
        Self::Primitive(PrimitiveType::U32)
    }

    pub const fn i32() -> Self {
        Self::Primitive(PrimitiveType::I32)
    }

    pub const fn u64() -> Self {
        Self::Primitive(PrimitiveType::U64)
    }

    pub const fn i64() -> Self {
        Self::Primitive(PrimitiveType::I64)
    }

    pub const fn float() -> Self {
        Self::Primitive(PrimitiveType::Float)
    }
}
