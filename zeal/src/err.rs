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
    use std::fmt::Display;

    use crate::ast::{
        lex::{LexTok, LineInfo, Tok, TokType},
        Expr,
    };

    #[derive(Default, Debug, Clone)]
    pub struct ExprInfo {
        pub ty: String,
        pub string: ExprString,
    }

    impl From<&Expr> for ExprInfo {
        fn from(value: &Expr) -> Self {
            let ty = value.type_str().to_owned();
            let string = value.to_string();
            Self { ty, string }
        }
    }

    #[derive(Debug, Clone)]
    pub struct ParseErrInfo {
        pub expr: Option<ExprInfo>,
        pub prev: Tok,
        pub curr: Tok,
        pub next: Option<Tok>,
    }

    impl Display for ParseErrInfo {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let LineInfo { line, col } = self.curr.info;

            let (expr, expr_ty) = if let Some(ex) = &self.expr {
                (ex.string.as_str(), ex.ty.as_str())
            } else {
                ("n/a".into(), "n/a".into())
            };

            let prev = &self.prev;
            let next = if let Some(n) = &self.next {
                n.to_string()
            } else {
                "None".into()
            };

            write!(f, "At => (L:{line}|C:{col})\n\t-> While parsing expression: {expr}\n\t -> of type: {expr_ty}.\n\t-> Prev: {prev}\n\t -> Next: {next}")
        }
    }

    pub type ExprString = String;

    #[derive(thiserror::Error, Default, Debug, Clone)]
    pub enum ParseError {
        #[error("PARSE_ERR => Unknown Token: {0}")]
        InvalidToken(String),

        #[error("")]
        UnmatchedEndTag,

        #[error("PARSE_ERR => Unexpected Token!! {0}")]
        UnexpectedToken(ParseErrInfo), //{ expected: TokType, got: TokType },
        //
        #[error("PARSE_ERR => Expected Block expression. :: {0}")]
        ExpectedBlock(ParseErrInfo),

        // #[error("PARSE_ERR => Expected newline or ';' after {expr_ty} \n\t=> Expression: {expr_str}\n\t=> Token: {tok}")]
        // MissingTerminal(ParseErrInfo), //
        // expr_ty: String,
        // expr_str: ExprString,
        // tok: Tok,
        // },
        #[error("PARSE_ERR => Binding name must be a valid identifer. Got: {got} :: {info}")]
        UnexpectedBindName { got: String, info: ParseErrInfo },
        // tok: Tok,

        // }
        #[error("Expected primary. Got: {got}. :: {info} ")]
        ExpectedPrimary { got: String, info: ParseErrInfo },

        #[error("PARSE_ERR => Invalid assignment. only identifiers are allowed on left hand side of assignmnent exprs")]
        InvalidAssignment,

        #[error("End of File")]
        Eof,

        #[default]
        #[error("Unknown parse error occured.")]
        Unknown,
    }
}

pub mod lex {
    use std::num::ParseIntError;

    #[derive(thiserror::Error, Debug, Default, Clone, PartialEq)]
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
