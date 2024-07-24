use std::{
    cell::RefCell, collections::HashMap, fmt::Write, hash::Hash, num::ParseIntError, rc::Rc,
};

// TODO :: MAKE THE FUCKING GC FIRST
//
use logos::{Lexer, Logos, Skip};

use crate::{
    core_types::{
        num::ZFloat64,
        str::{ZString, ZSymbol},
        ZValue,
    },
    err::lex::LexError,
};

#[derive(Debug, Clone)]
pub struct TokBuffer(Box<[Tok]>);

impl TokBuffer {
    pub fn read_file(path: &str) -> anyhow::Result<Self> {
        let s = std::fs::read_to_string(path)?;
        Self::read_string(&s)
    }

    pub fn read_string(src: &str) -> anyhow::Result<Self> {
        let mut lex = LexTok::lexer(src);
        let mut buf = Vec::with_capacity(32);
        while let Some(t) = lex.next() {
            let t = t?;
            let line = LineInfo {
                line: lex.extras.0,
                col: lex.extras.1,
            };
            let tok = Tok::from_tok(t, lex.slice(), line);
            buf.push(tok);
        }
        let s = Self(buf.into_boxed_slice());
        Ok(s)
    }

    #[inline]
    pub fn slice(&self) -> &[Tok] {
        self.0.as_ref()
    }

    #[inline]
    pub fn slice_mut(&mut self) -> &mut [Tok] {
        self.0.as_mut()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LineInfo {
    pub line: usize,
    pub col: usize,
}

// type LexResult<T> = anyhow::Result<T, LexError>;

#[derive(Debug, Clone)]
pub struct Tok {
    pub tok: LexTok,
    pub lexeme: String,
    pub ty: TokType,
    pub info: LineInfo,
}

impl Tok {
    fn from_tok(tok: LexTok, lexeme: &str, info: LineInfo) -> Self {
        let ty = TokType::from(&tok);
        let lexeme = lexeme.to_owned();
        Self {
            tok,
            lexeme,
            ty,
            info,
        }
    }

    pub const fn is_expr_terminal(&self) -> bool {
        match self.ty {
            TokType::Semicolon | TokType::NewLine => true,
            _ => false,
        }
    }

    pub fn into_sym(self) -> ZSymbol {
        ZSymbol::from(self.lexeme)
    }
}

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tok = &self.tok;
        let lexeme = &self.lexeme;
        let s = format!("Lexeme: {lexeme} => {tok:?}");
        f.write_str(&s)
    }
}
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokType {
    Begin,
    FatArrow,
    Dot,
    Semicolon,
    Bang,
    BangEq,
    EqEq,
    Eof,
    Comment,

    Struct,
    Trait,
    Impl,
    If,
    Else,
    Fn,

    And,

    Or,
    Return,

    Super,

    ThisSelf,

    Let,

    Const,

    Loop,

    For,

    While,

    Break,

    Match,

    Continue,

    Print,

    Plus,

    Minus,

    ForwardSlash,

    Star,

    NewLine,
    True,
    False,

    // Bool,
    Do,

    Pub,

    Ref,

    End,

    BraceOpen,

    BraceClosed,

    BracketOpen,
    BracketClose,

    Colon,

    Comma,

    Nil,

    Number,

    Str,
    #[default]
    Ident,

    Gt,
    Ge,
    Eq,
    NotEq,
    Lt,
    Le,
    DoubleColon,

    OpenParen,
    CloseParen,
}

// impl From<LexTok> for TokType {
//     fn from(value: LexTok) -> Self {
//        let s = Self::from(&value);
//     }
// }
//
impl From<&LexTok> for TokType {
    fn from(value: &LexTok) -> Self {
        match value {
            LexTok::Begin => TokType::Begin,
            LexTok::Struct => TokType::Struct,
            LexTok::Trait => TokType::Trait,
            LexTok::Impl => TokType::Impl,
            LexTok::If => TokType::If,
            LexTok::Else => TokType::Else,
            LexTok::Fn => TokType::Fn,
            LexTok::And => TokType::And,
            LexTok::Or => TokType::Or,
            LexTok::Return => TokType::Return,
            LexTok::Super => TokType::Super,
            LexTok::ThisSelf => TokType::ThisSelf,
            LexTok::Let => TokType::Let,
            LexTok::Const => TokType::Const,
            LexTok::Loop => TokType::Loop,
            LexTok::For => TokType::For,
            LexTok::While => TokType::While,
            LexTok::Break => TokType::Break,
            LexTok::Match => TokType::Match,
            LexTok::Continue => TokType::Continue,
            LexTok::Print => TokType::Print,
            LexTok::Plus => TokType::Plus,
            LexTok::Minus => TokType::Minus,
            LexTok::ForwardSlash => TokType::ForwardSlash,
            LexTok::Star => TokType::Star,
            LexTok::NewLine => TokType::NewLine,
            LexTok::Symbol => TokType::Ident,
            LexTok::Bool(true) => TokType::True,
            LexTok::Bool(false) => TokType::False,
            LexTok::Do => TokType::Do,
            LexTok::Pub => TokType::Pub,
            LexTok::Ref => TokType::Ref,
            LexTok::End => TokType::End,
            LexTok::BraceOpen => TokType::BraceOpen,
            LexTok::BraceClosed => TokType::BraceClosed,
            LexTok::BracketOpen => TokType::BracketOpen,
            LexTok::BracketClose => TokType::BraceClosed,
            LexTok::Colon => TokType::Colon,
            LexTok::Comma => TokType::Comma,
            LexTok::Nil => TokType::Nil,
            LexTok::Number(_) => TokType::Number,
            LexTok::String(_) => TokType::Str,
            LexTok::Semicolon => TokType::Semicolon,
            LexTok::Dot => TokType::Dot,
            LexTok::Bang => TokType::Bang,
            LexTok::Equal => TokType::Eq,
            LexTok::EqualEqual => TokType::EqEq,
            LexTok::BangEqual => TokType::BangEq,
            LexTok::OpenParen => TokType::OpenParen,
            LexTok::CloseParen => TokType::CloseParen,
            LexTok::DoubleColon => TokType::DoubleColon,
            LexTok::Comment => TokType::Comment,
        }
    }
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(extras = (usize, usize))]
#[logos(error = LexError)]
pub enum LexTok {
    #[token("begin")]
    Begin,

    #[token(";")]
    Semicolon,
    #[token("struct")]
    Struct,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("fn")]
    Fn,

    #[token("and")]
    And,

    #[token("or")]
    Or,
    #[token("return")]
    Return,

    #[token("super")]
    Super,

    #[token("self")]
    ThisSelf,

    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("loop")]
    Loop,

    #[token("for")]
    For,

    #[token("while")]
    While,

    #[token("break")]
    Break,

    #[token("match")]
    Match,

    #[token("continue")]
    Continue,

    #[token("print")]
    Print,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("/")]
    ForwardSlash,

    #[token("*")]
    Star,

    #[regex(r"\n", newline_cb)]
    NewLine,

    #[regex(r"\w+")]
    Symbol,

    #[token("false", |_| false)]
    #[token("true", |_| true)]
    Bool(bool),

    #[token("do")]
    Do,

    #[token("pub")]
    Pub,

    #[token("ref")]
    Ref,

    #[token("end")]
    End,

    #[token(".")]
    Dot,

    #[token("!")]
    Bang,

    #[token("=")]
    Equal,

    #[token("==")]
    EqualEqual,

    #[token("!=")]
    BangEqual,

    #[token("{")]
    BraceOpen,

    #[token("}")]
    BraceClosed,

    #[token("[")]
    BracketOpen,
    #[token("]")]
    BracketClose,

    #[token("(")]
    OpenParen,

    #[token(")")]
    CloseParen,

    #[token(":")]
    Colon,

    #[token("::")]
    DoubleColon,

    #[token(",")]
    Comma,

    #[token("nil")]
    Nil,

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap(), priority = 50)]
    Number(f64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    String(String),
    #[token("//")]
    Comment,
}

fn newline_cb(lex: &mut Lexer<LexTok>) {
    lex.extras.0 += 1;
    lex.extras.1 = lex.span().end;
}
