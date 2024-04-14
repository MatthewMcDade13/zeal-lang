pub static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "struct" => TokenType::Struct,
    "trait" => TokenType::Trait,
    "impl" => TokenType::Impl,
    "if" => TokenType::If,
    "else" => TokenType::Else,
    "true" => TokenType::True,
    "false" => TokenType::False,
    "fn" => TokenType::Fn,
    "nil" => TokenType::Nil,
    "and" => TokenType::And,
    "or" => TokenType::Or,
    "return" => TokenType::Return,
    "super" => TokenType::Super,
    "self" => TokenType::ThisSelf,
    "let" => TokenType::Let,
    "const" => TokenType::Const,
    "loop" => TokenType::Loop,
    "for" => TokenType::For,
    "while" => TokenType::While,
    "break" => TokenType::Break,
    "switch" => TokenType::Switch,
    "continue" => TokenType::Continue,
    "print" => TokenType::Print,
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    Print,
    FatArrow,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    ForwardSlash,
    Star,
    Bang,
    Equal,
    BangEqual,
    EqualEqual,
    Gt,
    Ge,
    Lt,
    Le,
    Ident,
    String,
    Number,
    And,
    Struct,
    Trait,
    Impl,
    Else,
    False,
    True,
    Fn,
    If,

    Nil,
    Or,
    Return,
    Super,
    ThisSelf,
    Let,
    Const,
    Eof,
    Loop,
    For,
    While,
    Break,
    Switch,
    Continue,
    Comment,
    Unknown,
    Colon,
    DoubleColon,
}

use anyhow::*;
use phf::phf_map;
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone)]
pub struct Token {
    pub ty: TokenType,
    pub literal: Value,
    pub line: u32,
    pub lexeme: String,
}

impl Token {
    pub const fn empty() -> Self {
        Self {
            ty: TokenType::Unknown,
            literal: Value::Nil,
            line: 0,
            lexeme: String::new(),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            ty,
            literal,
            lexeme,
            line,
        } = self;
        write!(f, "LineNo:{line} {ty:?} :: {lexeme} :: {literal:?}")
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
    // String(String),
    Boolean(bool),
    Nil,
    Obj(HashMap<String, Value>),
    // No-op / whitespace placeholder (for parsing)
    Void,
}

impl Value {
    pub const fn is_falsey(&self) -> bool {
        match self {
            Value::Boolean(false) => true,
            Value::Nil => true,
            _ => false,
        }
    }

    pub const fn is_obj(&self) -> bool {
        match self {
            Value::Obj(_) => true,
            _ => false,
        }
    }

    // pub fn as_obj(&self) -> anyhow::Result<Object> {
    //     if let Self::Obj(obj) = self {
    //         Ok(obj.clone())
    //     } else {
    //         bail!("Expected Object, got: {}", self.type_string())
    //     }
    // }
    //
    // pub fn as_number(&self) -> anyhow::Result<f64> {
    //     let value = self.clone();
    //     if let Self::Number(n) = value {
    //         Ok(n)
    //     } else {
    //         let type_str = value.type_string();
    //         bail!(
    //             "{}",
    //             AstWalkError::TypeError {
    //                 value,
    //                 message: format!("Expected Number, got: {}", type_str)
    //             }
    //         )
    //     }
    // }
    //
    // pub fn as_bool(&self) -> anyhow::Result<bool> {
    //     let value = self.clone();
    //     if let Self::Boolean(b) = value {
    //         Ok(b)
    //     } else {
    //         let type_str = value.type_string();
    //         bail!(
    //             "{}",
    //             AstWalkError::TypeError {
    //                 value,
    //                 message: format!("Expected Boolean, got: {}", type_str)
    //             }
    //         )
    //     }
    // }
    //
    // pub fn as_string(&self) -> anyhow::Result<String> {
    //     let value = self.clone();
    //     if let Self::Obj(Object::String(string)) = value {
    //         Ok(string)
    //     } else {
    //         let type_str = value.type_string();
    //         bail!("Expected String, got: {}", type_str);
    //     }
    // }
    //
    // pub fn type_string(&self) -> String {
    //     match self {
    //         Value::Number(_) => "Number".into(),
    //         Value::Boolean(_) => "Boolean".into(),
    //         Value::Obj(obj) => match obj {
    //             Object::String(_) => "String".into(),
    //         },
    //         Value::Nil => "Unit".into(),
    //     }
    // }
    //
    pub const fn is_bool(&self) -> bool {
        if let Self::Boolean(_) = self {
            true
        } else {
            false
        }
    }

    pub const fn is_nil(&self) -> bool {
        if let Self::Nil = self {
            true
        } else {
            false
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Value::Number(left) => {
                if let Value::Number(right) = other {
                    left == right
                } else {
                    false
                }
            }
            Value::Boolean(left) => {
                if let Value::Boolean(right) = other {
                    left == right
                } else {
                    false
                }
            }
            Value::Obj(obj) => {
                false
                //     Object::String(left) => {
                //         if let Value::Obj(Object::String(right)) = other {
                //             left == right
                //         } else {
                //             false
                //         }
                //     }
            }
            Value::Nil => {
                if let Value::Nil = other {
                    true
                } else {
                    false
                }
            }
            Value::Void => todo!(),
        }
        // Value::Nil => todo!(),
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Value::Number(n) => n.to_string(),
            Value::Boolean(b) => b.to_string(),
            Value::Obj(obj) => {
                "".into()
                // Object::String(string) => string.to_owned(),
            }
            Value::Nil => String::from("nil"),
            Value::Void => todo!(),
        };
        write!(f, "{}", str)
    }
}
