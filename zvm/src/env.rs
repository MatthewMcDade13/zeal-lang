use std::ops::{Deref, DerefMut};

use crate::{chunk::ChunkBuilder, val::Val};

pub struct CompileEnv {
    pub parent: Option<Box<Self>>,

    pub state: ChunkBuilder,
}

impl CompileEnv {
    pub fn child(parent: Box<Self>) -> Self {
        Self {
            parent: Some(parent),
            state: ChunkBuilder::default(),
        }
    }

    pub fn root() -> Self {
        Self {
            parent: None,
            state: ChunkBuilder::default(),
        }
    }
}

impl Deref for CompileEnv {
    type Target = ChunkBuilder;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl DerefMut for CompileEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}
