use std::{fmt::Display, rc::Rc};

use crate::chunk::FuncChunk;

#[derive(Debug, Default, Clone)]
pub enum Val {
    Byte(u8),
    Bool(bool),
    UNum(usize),
    Num(isize),
    Float(f64),
    Rune(Rc<str>),
    String(Rc<str>),
    Ptr(Box<Self>),
    Func(Rc<FuncChunk>),
    #[default]
    Unit,
}

impl Val {
    #[inline]
    pub fn string(s: &str) -> Self {
        Self::String(Rc::from(s))
    }

    pub const fn float(n: f64) -> Self {
        Self::Float(n)
    }

    #[inline]
    pub fn rune(name: &str) -> Self {
        Self::Rune(Rc::from(name))
    }

    #[inline]
    pub fn expect_float64(&self) -> f64 {
        if let Self::Float(n) = self {
            *n
        } else {
            panic!("Expected float64, got: {self}")
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Val::Byte(n) => *n != 0,
            Val::Bool(true) => true,
            Val::Bool(false) => false,
            Val::UNum(n) => *n != 0,
            Val::Num(n) => *n != 0,
            Val::Float(f) => *f != 0.0,
            Val::Rune(rc) => rc.len() != 0,
            Val::String(rc) => rc.len() != 0,
            Val::Ptr(val) => val.is_truthy(),
            Val::Func(rc) => rc.chunk.len() != 0,
            Val::Unit => false,
        }
    }

    #[inline]
    pub fn is_falsey(&self) -> bool {
        !self.is_truthy()
    }
}

impl From<bool> for Val {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<f64> for Val {
    fn from(value: f64) -> Self {
        Self::Float(value)
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Val::Byte(b) => b.to_string(),
            Val::Bool(false) => String::from("false"),
            Val::Bool(true) => String::from("true"),
            Val::UNum(n) => n.to_string(),
            Val::Num(n) => n.to_string(),
            Val::Float(n) => n.to_string(),
            Val::Rune(rc) => rc.as_ref().to_string(),
            Val::String(rc) => rc.as_ref().to_string(),
            Val::Ptr(val) => val.to_string(),
            Val::Unit => String::from("()"),
            Val::Func(rc) => {
                let name = rc.name();
                let arity = rc.arity;
                let chunk = &rc.chunk;

                format!("{name}/{arity} ->\n\t{chunk}")
            }
        };
        write!(f, "{s}")
    }
}