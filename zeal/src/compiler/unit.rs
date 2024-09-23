use anyhow::bail;

use crate::{ast::expr::FuncExpr, core_types::str::ZIdent};

use super::chunk::Chunk;

/// Compile time representation of a function (Unit of compilation)
#[derive(Debug, Clone)]
pub struct FuncChunk {
    pub arity: u8,
    pub chunk: Chunk,
    pub name: ZIdent,
}

#[derive(Debug, Clone)]
pub enum CompUnit {
    Function(FuncChunk),
    // Struct(StructChunk),
    // Trait(TraitChunk)
}

impl FuncChunk {
    pub const MAX_ARITY: u8 = u8::MAX;

    pub fn empty() -> Self {
        Self {
            arity: 0,
            name: ZIdent::empty(),
            chunk: Chunk::zeroed(),
        }
    }

    pub const fn empty_with_sig(name: ZIdent, arity: u8) -> Self {
        Self {
            arity,
            name,
            chunk: Chunk::zeroed(),
        }
    }

    pub fn script(chunk: Chunk) -> Self {
        let s = String::from("<<Script_Module>>");
        let name = ZIdent::from(s);
        Self {
            name,
            chunk,
            arity: 0,
        }
    }
    //
    pub const fn new(name: ZIdent, arity: u8, chunk: Chunk) -> Self {
        Self { arity, chunk, name }
    }
}

// pub fn try_into_module(self) -> anyhow::Result<Self> {
//     match self {
//         x @ CompUnit::Module { .. } => Ok(x),
//         CompUnit::Fn { chunk, name, .. } => Ok(Self::),
//     }
// }

// pub fn try_into_fn(self, name: &str) -> anyhow::Result<Self> {
//     match self {
//         CompUnit::Module { chunk } => Ok(Self::Fn {
//             arity: 0,
//             chunk,
//             name: ZIdent::from(name),
//         }),
//         x @ CompUnit::Fn { .. } => Ok(x),
//     }
// }
