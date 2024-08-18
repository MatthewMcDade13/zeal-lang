pub mod bytes;
pub mod htable;
pub mod num;
pub mod str;
pub mod val;
pub mod vec;

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

    pub const IDENTS: phf::Set<&'static str> = phf_set! {
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
    pub const LE: &'static str = "<=";
    pub const LT: &'static str = "<";
    pub const GE: &'static str = ">=";
    pub const GT: &'static str = ">";
    pub const NOT: &'static str = "!";
    pub const NOT_EQUAL: &'static str = "!=";

    pub const AST_SETVAR: &'static str = "setvar";
    pub const VAR: &'static str = "var";

    pub const EQUAL: &'static str = "==";
    pub const ASSIGN: &'static str = "=";
    pub const ADD: &'static str = "+";
    pub const SUB: &'static str = "-";
    pub const DIV: &'static str = "/";
    pub const MUL: &'static str = "*";
    pub const CONCAT: &'static str = "++";

    pub const STRUCT: &'static str = "struct";
    pub const TRAIT: &'static str = "trait";
    pub const IMPL: &'static str = "impl";
    pub const IF: &'static str = "if";
    pub const THEN: &'static str = "then";
    pub const ELSEIF: &'static str = "elseif";
    pub const BEGIN: &'static str = "begin";
    pub const ELSE: &'static str = "else";
    pub const TRUE: &'static str = "true";
    pub const FALSE: &'static str = "false";

    pub const FN: &'static str = "fn";
    pub const NIL: &'static str = "nil";
    pub const AND: &'static str = "and";
    pub const OR: &'static str = "or";
    pub const RETURN: &'static str = "return";

    pub const SUPER: &'static str = "super";
    pub const SELF: &'static str = "self";
    pub const LET: &'static str = "let";
    pub const CONST: &'static str = "const";
    pub const LOOP: &'static str = "loop";
    pub const FOR: &'static str = "for";
    pub const WHILE: &'static str = "while";

    pub const BREAK: &'static str = "break";
    pub const MATCH: &'static str = "match";
    pub const CONTINUE: &'static str = "continue";
    pub const PRINTLN: &'static str = "println";
    pub const PRINT: &'static str = "print";
    pub const DO: &'static str = "do";
    pub const END: &'static str = "end";
    pub const USE: &'static str = "use";

    /// allows 'lisp style' function application. ex: call add 1 2
    pub const CALL: &'static str = "call";
}

#[derive(Debug, Clone)]
pub struct TypeInfo {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Meta {
    pub var_type: Option<VarType>,
    pub type_info: TypeInfo,
    pub val: ZIdent,
    pub binding_for: Option<ZValue>,
}
impl Meta {
    pub fn new() -> Self {
        Self {
            var_type: None,
            type_info: TypeInfo {
                name: String::new(),
            },
            val: ZIdent::new(""),
            binding_for: None,
        }
    }
}
