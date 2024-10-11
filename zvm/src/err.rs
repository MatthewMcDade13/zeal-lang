use std::collections::HashMap;

use zeal_core::{str::ZIdent, val::ZValue};

use crate::{
    opcode::{Op, Opcode, VarOp},
    state::Scope,
};

#[derive(Debug, thiserror::Error, Clone)]
pub enum CompileError {
    #[error("Unresolved local identifier: {name}. Compiling Op: {op}, locals: {scope_state}")]
    UnresolvedLocal {
        name: ZIdent,
        op: Op,
        scope_state: Scope,
    },
    #[error("Invalid Assigment on Identifier: {name}, var_op: {op_ty} ")]
    InvalidAssignment { op_ty: VarOp, name: ZIdent },
}

#[derive(Debug, thiserror::Error, Clone)]
pub enum RuntimeError {
    #[error("Invalid type: expected: {expected_type}; got: {actual_type}. Inside function: {in_fn:?}. Message: {message:?}" )]
    InvalidType {
        expected_type: &'static str,
        actual_type: &'static str,
        in_fn: Option<&'static str>,
        message: Option<String>,
    },
    #[error(
            "Unknown Identifier: {name}, encountered from opcode: {opcode:?}, vm constants: {constants:?}, globals: {globals:?}"
        )]
    VMUnknownIdentifier {
        name: ZIdent,
        opcode: Opcode,
        constants: Vec<ZValue>,
        globals: HashMap<ZIdent, ZValue>,
    },
}
