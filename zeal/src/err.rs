#[derive(Debug, Clone, Copy)]
pub struct ErrorLine {
    line: usize,
    col: usize,
}

#[derive(Debug, Clone)]
pub struct ZealErr<T> {
    pub error: T,
    pub line: ErrorLine,
    pub message: Option<String>,
}

pub mod parse {
    use crate::ast::{
        lex::{LexTok, Tok, TokType},
        Expr,
    };

    use super::{lex::LexError, ZealErr};
    pub type ParseErrInfo = ZealErr<ParseError>;

    #[derive(thiserror::Error, Default, Debug, Clone)]
    pub enum ParseError {
        #[error("PARSE_ERR => Unknown Token: {0}")]
        InvalidToken(String),

        #[error("")]
        UnmatchedEndTag,

        #[error("PARSE_ERR => Unexpected Token!! Expected: {expected:?}, Got: {got:?}")]
        UnexpectedToken { expected: TokType, got: TokType },

        #[error("PARSE_ERR => Expected newline or ';' after {expr_ty} \n\t=> Expression: {expr}\n\t=> Token: {tok}")]
        MissingTerminal {
            expr_ty: String,
            expr: Expr,
            tok: Tok,
        },

        #[error("Expected primary (parse-atom) in expression: {expr}, found: {tok}")]
        ExpectedPrimary { expr: Expr, tok: Tok },

        #[error("PARSE_ERR => Invalid assignment. only identifiers are allowed on left hand side of assignmnent exprs")]
        InvalidAssignment,

        #[default]
        #[error("Unknown parse error occured.")]
        Unknown,
    }
}

pub mod lex {
    use std::num::ParseIntError;

    #[derive(thiserror::Error, Default, Debug, Clone, PartialEq)]
    pub enum LexError {
        #[error("Invalid or Unknown Symbol: {0}")]
        InvalidSymbol(String),
        #[default]
        #[error("Illegal Character encountered")]
        IllegalCharacter,
        #[error("Error Parsing Invalid Number: {0}")]
        InvalidNumber(String),
    }

    impl From<std::num::ParseFloatError> for LexError {
        fn from(err: std::num::ParseFloatError) -> Self {
            LexError::InvalidNumber(err.to_string())
        }
    }

    impl From<ParseIntError> for LexError {
        fn from(err: ParseIntError) -> Self {
            use std::num::IntErrorKind::*;
            match err.kind() {
                PosOverflow | NegOverflow => LexError::InvalidNumber("overflow error".to_owned()),
                _ => LexError::InvalidNumber("other error".to_owned()),
            }
        }
    }
}

pub mod core {
    use std::collections::HashMap;

    use crate::{
        ast::VarType,
        compiler::{
            opcode::{Op, Opcode, VarOp},
            state::Scope,
        },
        core_types::{str::ZIdent, val::ZValue},
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
}
