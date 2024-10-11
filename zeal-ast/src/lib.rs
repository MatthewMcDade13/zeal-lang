pub mod err;
pub mod expr;
pub mod lex;
pub mod parse;

use expr::{AstList, ExprStmt};
use lex::{Tok, TokBuffer};
use parse::Parser;

use std::fmt::Display;
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

pub trait AstWalker<T, R> {
    fn visit(&mut self, node: &T) -> anyhow::Result<R>;
}

pub trait AstNode<W: AstWalker<Self, R>, R>
where
    Self: Sized,
{
    fn walk(&self, walker: &mut W) -> anyhow::Result<R>;
}

#[derive(Debug, Clone)]
pub struct Ast {
    pub tree: AstList<ExprStmt>,
}

impl Ast {
    pub fn from_toks(toks: &[Tok]) -> anyhow::Result<Self> {
        let ast = Parser::parse_ast(toks)?;
        // let symbols = Self::build_symbol_table(&tree);
        Ok(ast)
        // Ok(Self(ast))
    }

    pub fn from_file(path: &str) -> anyhow::Result<Self> {
        let buf = TokBuffer::read_file(path)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
}

impl std::str::FromStr for Ast {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buf = TokBuffer::read_string(s)?;
        let s = Self::from_toks(buf.slice())?;
        Ok(s)
    }
}

impl Display for Ast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut res = format!("----- AST ----- \n");
        // res.push_str(&self.tree.to_string());
        // if let Expr::List(ExprList::Block(l)) | Expr::List(ExprList::Tuple(l)) = &self.tree {
        //     for i in 0..l.len() {
        //         let expr = l.index(i);
        //         res.push_str(&expr.to_string());
        //     }
        // } else {
        //     let s = &format!("{}", self.tree.to_string());
        //     res.push_str(s);
        // }
        res.push_str("----- END AST -----\n");
        write!(f, "{}", res)
    }
}
#[derive(Debug, Clone, Copy)]
pub enum BinaryOpType {
    Gt,
    Lt,
    Ge,
    Le,
    And,
    Or,
    Equals,
    NotEquals,
    BitAnd,
    BitOr,
    Xor,
    Concat,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = todo!();
        // let s = match self {
        //     BinaryOpType::Gt => idents::GT,
        //     BinaryOpType::Lt => idents::LT,
        //     BinaryOpType::Ge => idents::GE,
        //     BinaryOpType::Le => idents::LE,
        //     BinaryOpType::And => idents::AND,
        //     BinaryOpType::Or => idents::OR,
        //     BinaryOpType::Equals => idents::EQUAL,
        //     BinaryOpType::NotEquals => idents::NOT_EQUAL,
        //     BinaryOpType::BitAnd => todo!(),
        //     BinaryOpType::BitOr => todo!(),
        //     BinaryOpType::Xor => todo!(),
        //     BinaryOpType::Concat => idents::CONCAT,
        //     BinaryOpType::Add => idents::ADD,
        //     BinaryOpType::Sub => idents::SUB,
        //     BinaryOpType::Mul => idents::MUL,
        //     BinaryOpType::Div => idents::DIV,
        // };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOpType {
    Call,
    Negate,
    Not,
}

impl Display for UnaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = todo!();
        // let s = match self {
        //     UnaryOpType::Call => "(...)",
        //     UnaryOpType::Negate => idents::SUB,
        //     UnaryOpType::Not => idents::NOT,
        // };
        write!(f, "{s}")
    }
}
