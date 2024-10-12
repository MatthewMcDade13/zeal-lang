use std::{fmt::Display, num::ParseIntError};

use crate::{
    expr::Expr,
    lex::{LineInfo, Tok},
    parse::Parser,
};

#[derive(Default, Debug, Clone)]
pub struct ExprInfo {
    pub ty: String,
    pub string: String,
}

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
            ("n/a", "n/a")
        };

        let prev = &self.prev;
        let curr = &self.curr;
        let next = if let Some(n) = &self.next {
            n.to_string()
        } else {
            "None".into()
        };

        write!(f, "At => (L:{line}|C:{col})\n\t-> While parsing expression: {expr}\n\t -> of type: {expr_ty}.\n\t-> Current: {curr}\n\t-> Prev: {prev}\n\t-> Next: {next}")
    }
}

impl ParseErrInfo {
    pub fn from(p: &Parser, expr: Option<&Expr>) -> Self {
        let expr = expr.map(|e| ExprInfo {
            ty: e.type_str().to_owned(),
            string: todo!(), //e.to_string(),
        });
        Self {
            expr,
            prev: p.peek_prev().clone(),
            curr: p.peek().clone(),
            next: None,
        }
    }
}

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
