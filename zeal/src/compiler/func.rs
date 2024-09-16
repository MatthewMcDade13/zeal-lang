use anyhow::bail;

use crate::{ast::expr::FuncForm, core_types::str::ZIdent};

use super::chunk::Chunk;

/// Compile time representation of a function (Unit of compilation)
#[derive(Debug, Default, Clone)]
pub enum FuncChunk {
    #[default]
    NoOp,
    Script {
        chunk: Chunk,
    },
    Fn {
        arity: u8,
        chunk: Chunk,
        name: ZIdent,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum CompType {
    Script,
    Function,
    Module,
}

impl FuncChunk {
    pub const MAX_ARITY: u8 = u8::MAX;

    pub const fn script(chunk: Chunk) -> Self {
        Self::Script { chunk }
    }
    pub const fn function(name: ZIdent, arity: u8, chunk: Chunk) -> Self {
        Self::Fn { arity, chunk, name }
    }

    pub const fn no_op() -> Self {
        Self::NoOp
    }

    pub fn try_into_script(self) -> anyhow::Result<Self> {
        match self {
            FuncChunk::NoOp => bail!("Cannot cast Func::NoOp into Func::Script!!!"),
            x @ FuncChunk::Script { .. } => Ok(x),
            FuncChunk::Fn { chunk, .. } => Ok(Self::Script { chunk }),
        }
    }

    pub fn try_into_fn(self, name: &str) -> anyhow::Result<Self> {
        match self {
            FuncChunk::NoOp => bail!("Cannot cast Func::NoOp into Func::Script!!!"),
            FuncChunk::Script { chunk } => Ok(Self::Fn {
                arity: 0,
                chunk,
                name: ZIdent::from(name),
            }),
            x @ FuncChunk::Fn { .. } => Ok(x),
        }
    }

    pub fn chunk_mut(&mut self) -> &mut Chunk {
        if let Some(c) = self.try_chunk_mut() {
            c
        } else {
            panic!("Cannot get chunk from a Func::NoOp!!!")
        }
    }

    pub const fn chunk(&self) -> &Chunk {
        if let Some(c) = self.try_chunk() {
            c
        } else {
            panic!("Cannot get chunk from a Func::NoOp!!!")
        }
    }

    pub const fn try_chunk(&self) -> Option<&Chunk> {
        match self {
            FuncChunk::NoOp => None,
            FuncChunk::Script { chunk, .. } => Some(chunk),
            FuncChunk::Fn { chunk, .. } => Some(chunk),
        }
    }

    pub fn try_chunk_mut(&mut self) -> Option<&mut Chunk> {
        match self {
            FuncChunk::NoOp => None,
            FuncChunk::Script { chunk, .. } => Some(chunk),
            FuncChunk::Fn { chunk, .. } => Some(chunk),
        }
    }

    pub const fn arity(&self) -> usize {
        match self {
            FuncChunk::NoOp => 0,
            FuncChunk::Script { .. } => 0xDEADBEEFusize,
            FuncChunk::Fn { arity, .. } => *arity as usize,
        }
    }

    #[inline]
    pub fn name(&self) -> &str {
        match self {
            FuncChunk::NoOp => "NoOp",
            FuncChunk::Script { .. } => "Func::Script",
            FuncChunk::Fn { name, .. } => name.string(),
        }
    }
}
