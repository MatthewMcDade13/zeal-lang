pub mod bytes;
pub mod func;
pub mod htable;
pub mod num;
pub mod str;
pub mod val;
pub mod vec;

use std::mem::MaybeUninit;

use phf::phf_map;
use str::ZIdent;
use val::ZValue;

use crate::ast::{lex::LexTok, VarType};

pub mod idents {
    use phf::phf_set;
    use val::ZValue;

    use super::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub enum CoreIdent {
        Not,
        Plus,
        Minus,
        Mul,
        Div,
        Gt,
        Lt,
        Ge,
        Le,
        Eq,
        NotEq,
        Print,
        Println,
        Concat,
        Struct,
        Trait,
        Impl,
        If,
        Then,
        Elseif,
        Begin,
        Else,
        True,
        False,
        Fn,
        Nil,
        And,
        Or,
        Return,
        Super,
        ThisSelf,
        Let,
        Var,
        Const,
        Loop,
        For,
        While,
        Break,
        Continue,
        Do,
        End,
        Use,
        Call,
    }

    pub const IDENTS: phf::Set<&str> = phf_set! {
        "=",
        "==",
        "<",
        "<=",
        ">",
        ">=",
    "+",
        "-",
        "/",
        "*",
         "++",
        "struct",
        "trait",
        "impl",
        "if",
        "then",
         "elseif",
         "begin",
        "else",
        "true",
        "false",
        "fn",
        "nil",
        "and",
        "or",
        "return",
        "super",
        "self",
        "let",
        "var",
        "const",
        "loop",
        "for",
        "while",
        "break",
        "match",
        "continue",
        "println",
        "print",
        "do",
        "end",
        "use",
        "call"
    };

    // TODO: Change these into a const fn that matches on CoreIdent enum.
    pub const LE: &str = "<=";
    pub const LT: &str = "<";
    pub const GE: &str = ">=";
    pub const GT: &str = ">";
    pub const NOT: &str = "!";
    pub const NOT_EQUAL: &str = "!=";

    pub const AST_SETVAR: &str = "setvar";
    pub const VAR: &str = "var";

    pub const EQUAL: &str = "==";
    pub const ASSIGN: &str = "=";
    pub const ADD: &str = "+";
    pub const SUB: &str = "-";
    pub const DIV: &str = "/";
    pub const MUL: &str = "*";
    pub const CONCAT: &str = "++";

    pub const STRUCT: &str = "struct";
    pub const TRAIT: &str = "trait";
    pub const IMPL: &str = "impl";
    pub const IF: &str = "if";
    pub const THEN: &str = "then";
    pub const ELSEIF: &str = "elseif";
    pub const BEGIN: &str = "begin";
    pub const ELSE: &str = "else";
    pub const TRUE: &str = "true";
    pub const FALSE: &str = "false";

    pub const FN: &str = "fn";
    pub const NIL: &str = "nil";
    pub const AND: &str = "and";
    pub const OR: &str = "or";
    pub const RETURN: &str = "return";

    pub const SUPER: &str = "super";
    pub const SELF: &str = "self";
    pub const LET: &str = "let";
    pub const CONST: &str = "const";
    pub const LOOP: &str = "loop";
    pub const FOR: &str = "for";
    pub const WHILE: &str = "while";

    pub const BREAK: &str = "break";
    pub const MATCH: &str = "match";
    pub const CONTINUE: &str = "continue";
    pub const PRINTLN: &str = "println";
    pub const PRINT: &str = "print";
    pub const DO: &str = "do";
    pub const END: &str = "end";
    pub const USE: &str = "use";

    /// allows 'lisp style' function application. ex: call add 1 2
    pub const CALL: &str = "call";
}
