use std::{fmt::Display, ops::Deref, rc::Rc};

use zeal_core::string::StrBuf;

use crate::code::FuncBlock;

// pub struct NativeFunc<T: Fn(&[Val]) -> anyhow::Result<Val>>(T);

#[derive(Debug, Clone)]
pub struct NativeFunc {
    pub func: fn(&[Val]) -> anyhow::Result<Val>,
    pub name: Rc<str>,
    pub arity: usize,
}

#[derive(Debug, Clone, Hash)]
pub struct SymbolName(Rc<str>);

impl SymbolName {
    pub fn new(string: &str) -> Self {
        Self(Rc::from(string))
    }

    pub fn unwrap(self) -> Rc<str> {
        Rc::clone(&self.0)
    }

    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

impl PartialEq<str> for SymbolName {
    fn eq(&self, other: &str) -> bool {
        self.0.as_ref() == other
    }
}

impl PartialEq for SymbolName {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ref() == other.0.as_ref()
    }
}

impl Eq for SymbolName {}

impl PartialOrd for SymbolName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        let a = self.0.as_ref();
        let b = other.as_str();
        a.partial_cmp(b)
    }
}

impl PartialOrd<str> for SymbolName {
    fn partial_cmp(&self, other: &str) -> Option<std::cmp::Ordering> {
        let s = self.0.as_ref();
        s.partial_cmp(other)
    }
}

impl Ord for SymbolName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let a = self.0.as_ref();
        let b = other.as_str();
        a.cmp(b)
    }
}

impl From<String> for SymbolName {
    fn from(value: String) -> Self {
        Self(Rc::from(value))
    }
}

impl From<&String> for SymbolName {
    fn from(value: &String) -> Self {
        Self::new(value.as_ref())
    }
}

impl From<Rc<str>> for SymbolName {
    fn from(value: Rc<str>) -> Self {
        Self(Rc::clone(&value))
    }
}

impl Display for SymbolName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.as_ref().to_string();
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone)]
pub enum ValLiteral {
    Byte(u8),
    SByte(i8),
    Bool(bool),
    UNum(usize),
    Num(isize),
    Float(f64),
    String(StrBuf),
}

#[derive(Debug, Clone)]
pub enum FuncVal {
    Native(NativeFunc),
    Zeal(FuncBlock),
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
    Func(Rc<FuncBlock>),
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
            Val::Func(rc) => rc.code.len() != 0,
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
                let chunk = &rc.code;

                format!("{name}/{arity} ->\n\t{chunk}")
            }
            Val::SByte(sb) => sb.to_string(),
            Val::NativeFunc(NativeFunc { name, .. }) => format!("__native__/{name}"),
        };
        write!(f, "{s}")
    }
}
