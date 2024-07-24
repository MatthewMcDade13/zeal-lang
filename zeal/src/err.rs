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
    use super::{lex::LexError, ZealErr};
    pub type ParseErrInfo = ZealErr<ParseError>;

    #[derive(thiserror::Error, Default, Debug, Clone, PartialEq)]
    pub enum ParseError {
        #[error("Unknown Token: {0}")]
        InvalidToken(String),

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
