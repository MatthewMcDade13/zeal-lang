use std::{
    fmt::{Display, Write},
    rc::Rc,
    usize,
};

use logos::{Lexer, Logos, Skip};

use crate::{err::LexError, expr::AstRune};

/// A line of Toks that (hopefully) represent a statement/expression boundary.
/// Splits on Newline/Semicolon/Terminal Tok
// pub struct BufferLine {
//     head: Tok,
//     body:
//     tail: Tok,
//
// }
//

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
            let col = lex.span().start as isize;
            let col = col - lex.extras.curr_tok as isize;
            let col = if col < 0 { 0usize } else { col as usize };
            let line = LineInfo {
                line: lex.extras.line,
                col,
            };
            let tok = Tok::from_tok(t, lex.slice(), line);
            buf.push(tok);
        }
        buf.push(Tok::eof());
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

impl Display for LineInfo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line = self.line;
        let col = self.col;
        let s = format!("(L:{line},C:{col})");
        write!(f, "{s}")
    }
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
    pub const EOF_STR: &'static str = "__EOF__";

    pub fn eof() -> Self {
        Self {
            tok: LexTok::String(Self::EOF_STR.into()),
            lexeme: Self::EOF_STR.into(),
            ty: TokType::Eof,
            info: LineInfo {
                line: usize::MAX,
                col: usize::MAX,
            },
        }
    }

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

    pub fn into_ast_rune(self) -> AstRune {
        Rc::from(self.lexeme)
    }

    pub const fn is_begin_expr(&self) -> bool {
        matches!(
            self.ty,
            TokType::Begin
                | TokType::Var
                | TokType::Struct
                | TokType::Trait
                | TokType::Impl
                | TokType::If
                | TokType::Func
                | TokType::Return
                | TokType::Let
                | TokType::Const
                | TokType::Loop
                | TokType::For
                | TokType::While
                | TokType::Match
                | TokType::Print
                | TokType::Println
                | TokType::Do
                | TokType::Pub
                | TokType::Ref
                | TokType::BraceOpen
                | TokType::OpenParen
        )
    }

    pub const fn is_expr_terminal(&self) -> bool {
        matches!(self.ty, TokType::Semicolon | TokType::NewLine)
    }
}

impl std::fmt::Display for Tok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tok = &self.tok;
        let lexeme = &self.lexeme;
        let info = self.info;
        let s = format!("Token [ {lexeme} => {tok:?}, Lineno => {info} ]");
        f.write_str(&s)
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum TokType {
    SinglePipe,
    DoublePipe,

    Rune,
    When,
    Each,
    ElseIf,
    Then,
    Begin,
    FatArrow,
    ArrowLeft,
    ArrowRight,
    Dot,
    Semicolon,
    Bang,
    BangEq,
    EqEq,
    Eof,
    Comment,
    Var,

    Struct,
    Trait,
    Impl,
    If,
    Else,
    Func,

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
    Println,

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

    Int,
    Float,

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

impl From<&LexTok> for TokType {
    fn from(value: &LexTok) -> Self {
        match value {
            LexTok::Begin => TokType::Begin,
            LexTok::Struct => TokType::Struct,
            LexTok::Trait => TokType::Trait,
            LexTok::Impl => TokType::Impl,
            LexTok::If => TokType::If,
            LexTok::Else => TokType::Else,
            LexTok::Func => TokType::Func,
            LexTok::And => TokType::And,
            LexTok::Or => TokType::Or,
            LexTok::Return => TokType::Return,
            LexTok::Super => TokType::Super,
            LexTok::ThisSelf => TokType::ThisSelf,
            LexTok::Let => TokType::Let,
            LexTok::Var => TokType::Var,
            LexTok::Const => TokType::Const,
            LexTok::Loop => TokType::Loop,
            LexTok::For => TokType::For,
            LexTok::While => TokType::While,
            LexTok::Break => TokType::Break,
            LexTok::Match => TokType::Match,
            LexTok::Continue => TokType::Continue,
            LexTok::Println => TokType::Println,
            LexTok::Print => TokType::Print,
            LexTok::Plus => TokType::Plus,
            LexTok::Minus => TokType::Minus,
            LexTok::ForwardSlash => TokType::ForwardSlash,
            LexTok::Star => TokType::Star,
            LexTok::NewLine => TokType::NewLine,
            LexTok::Ident => TokType::Ident,
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
            LexTok::Float(_) => TokType::Float,
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
            LexTok::Then => TokType::Then,
            LexTok::ElseIf => TokType::ElseIf,
            LexTok::Each => TokType::Each,
            LexTok::Lt => TokType::Lt,
            LexTok::Le => TokType::Le,
            LexTok::Gt => TokType::Gt,
            LexTok::Ge => TokType::Ge,
            LexTok::When => TokType::When,
            LexTok::FatArrow => TokType::FatArrow,
            LexTok::ArrowRight => TokType::ArrowRight,
            LexTok::ArrowLeft => TokType::ArrowLeft,
            LexTok::Rune => TokType::Rune,
            LexTok::Int(_) => TokType::Int,
            LexTok::SinglePipe => TokType::SinglePipe,
            LexTok::DoublePipe => TokType::DoublePipe,
        }
    }
}

#[derive(Default, Debug, Clone, Copy)]
pub struct LexState {
    pub line: usize,
    pub col: usize,
    pub curr_tok: usize,
}

#[derive(Debug, Clone, Logos)]
#[logos(skip r"[ \t\r\f]+")]
#[logos(extras = LexState)]
#[logos(error = LexError)]
pub enum LexTok {
    #[token(r"^\s:([a-z][a-zA-Z0-9_]\??)$")]
    Rune,
    #[token("when")]
    When,
    #[token("each")]
    Each,
    #[token("elseif")]
    ElseIf,
    #[token("then")]
    Then,
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
    #[token("func")]
    Func,

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

    #[token("var")]
    Var,

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

    #[token("println")]
    Println,

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

    #[regex(r"[_a-zA-Z]\w*\??")]
    Ident,

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

    #[token("<")]
    Lt,

    #[token("<=")]
    Le,

    #[token(">")]
    Gt,

    #[token(">=")]
    Ge,

    #[token("->")]
    ArrowRight,

    #[token("<-")]
    ArrowLeft,

    #[token("=>")]
    FatArrow,

    #[regex(r"-?\d+", |lex| lex.slice().parse::<isize>().unwrap())]
    Int(isize),

    #[regex(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>().unwrap(), priority = 50)]
    Float(f64),

    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| lex.slice().to_owned())]
    String(String),
    #[token("//")]
    Comment,

    #[token("|")]
    SinglePipe,

    #[token("||")]
    DoublePipe,
}

fn newline_cb(lex: &mut Lexer<LexTok>) -> Skip {
    lex.extras.line += 1;
    lex.extras.curr_tok = lex.span().end;

    Skip
    // lex.extras.col = lex.span().start - lex.extras.curr_tok;
}
