use std::{fmt::Display, rc::Rc};

use crate::chunk::FuncChunk;

// pub struct NativeFunc<T: Fn(&[Val]) -> anyhow::Result<Val>>(T);

#[derive(Debug, Clone)]
pub struct NativeFunc {
    pub func: fn(&[Val]) -> anyhow::Result<Val>,
    pub name: Rc<str>,
    pub arity: usize,
}

#[derive(Debug, Default, Clone)]
pub enum Val {
    Byte(u8),
    SByte(i8),
    Bool(bool),
    UNum(usize),
    Num(isize),
    Float(f64),
    Rune(Rc<str>),
    String(Rc<str>),
    Ptr(Box<Self>),
    Func(Rc<FuncChunk>),
    NativeFunc(NativeFunc),
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

    pub fn as_float64(&self) -> Option<f64> {
        if let Self::Float(n) = self {
            Some(*n)
        } else {
            None
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
            Val::SByte(b) => *b != 0,
            Val::NativeFunc(_) => true,
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

                format!("{name}/{arity} ->\n\t{chunk:?}")
            }
            Val::SByte(sb) => sb.to_string(),
            Val::NativeFunc(NativeFunc { name, .. }) => format!("__native__/{name}"),
        };
        write!(f, "{s}")
    }
}
