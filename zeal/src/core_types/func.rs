use crate::compiler::func::FuncChunk;

/// Run-time representation of a function
#[derive(Debug, Clone)]
pub struct Func {
    binary: FuncChunk,
}
