use std::collections::HashMap;

use types::{Prototype, Type, TypeInfo, TypeName, TypedIdent};

use crate::{
    compiler::unit::{CompUnit, FuncChunk},
    core_types::str::{ZIdent, ZString},
};

pub mod sym;
pub mod types;

#[derive(Debug, Clone)]
pub enum SymPath {
    Absolute {
        path: ZString,
        walk_dirs: Vec<ZString>,
        symbols: Vec<ZIdent>,
    },
    Relative {
        root: ZString,
        walk_dirs: Vec<ZString>,
        symbols: Vec<ZIdent>,
    },
}

#[derive(Debug, Clone)]
pub struct Meta {
    pub type_info: types::TypeInfo,
    pub name: ZIdent,
    pub memloc: MemLocation,
    pub vis: Visibility,
    pub scope_depth: usize,
}

impl Default for Meta {
    fn default() -> Self {
        Self {
            type_info: types::TypeInfo::default(),
            name: ZIdent::from(""),
            memloc: MemLocation::Stack,
            vis: Visibility::Pub,
            scope_depth: 0,
        }
    }
}

impl Meta {
    pub const fn function(name: ZIdent, proto: Prototype) -> Self {
        let sym = TypedIdent::Function(proto);
        let type_info = TypeInfo {
            size_bytes: std::mem::size_of::<usize>(),
            sym,
        };
        Self {
            type_info,
            name,
            memloc: MemLocation::Stack,
            vis: Visibility::Priv,
            scope_depth: 0,
        }
    }

    pub fn variable(name: ZIdent, ty: Type) -> Self {
        let vname = name.clone();
        let sym = TypedIdent::Variable(TypeName { name, ty });
        let type_info = TypeInfo {
            size_bytes: ty.min_size_bytes(),
            sym,
        };
        Self {
            type_info,
            name: vname,
            memloc: MemLocation::Stack,
            vis: Visibility::Priv,
            scope_depth: 0,
        }
    }
    pub fn any(name: ZIdent) -> Self {
        Self {
            type_info: TypeInfo::default(),
            name,
            memloc: MemLocation::Undefined,
            vis: Visibility::Priv,
            scope_depth: 0,
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub enum Visibility {
    Pub,
    #[default]
    Priv,
    Sealed,
    // Protected
}

#[derive(Default, Debug, Clone, Copy)]
pub enum MemLocation {
    Stack,
    Heap,
    /// Might be Stack or Heap, or moves between both.
    Transient,
    /// Mem location doesnt exist.
    /// var is compile optomized out or is a non-runtime type.
    Elide,

    /// We dont know yet, will throw error if
    /// can't be resolved by runtime
    #[default]
    Undefined,
}

impl From<Type> for MemLocation {
    fn from(val: Type) -> Self {
        match val {
            Type::Any => Self::Heap,
            Type::Byte => Self::Stack,
            Type::Binary => Self::Heap,
            Type::Num | Type::UNum | Type::Float => Self::Stack,
            Type::String
            | Type::Rune
            | Type::RuneIdent
            | Type::Vector
            | Type::List
            | Type::Map
            | Type::Set
            | Type::Ref => Self::Heap,
            Type::Struct => Self::Transient,
            Type::Trait => Self::Elide,
            Type::Unit => Self::Elide,
            Type::Option => Self::Transient,
            Type::Nil => Self::Elide,
            Type::Bool => Self::Stack,
            Type::Function => Self::Stack,
            Type::Closure => Self::Heap,
            Type::Var => Self::Transient,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ChunkMeta {
    pub code: FuncChunk,
    pub meta: Meta,
}

#[derive(Debug, Clone)]
pub struct CompModule {
    pub name: ZIdent,
    pub code: HashMap<ZIdent, ChunkMeta>,
}
