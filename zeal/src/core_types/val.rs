use std::{cell::RefCell, fmt::Display, ops::Add, rc::Rc};

use super::{
    bytes::{ZBuffer, ZByte},
    htable::ZHashTable,
    num::{ZBool, ZFloat64},
    str::{ZIdent, ZRune, ZString},
    vec::ZVec,
};

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZMutRef(Rc<RefCell<ZValue>>);

#[derive(Default, Debug, Clone)]
pub enum ZValue {
    #[default]
    Nil,
    Bool(ZBool),
    Number(ZFloat64),
    Byte(ZByte),
    Buffer(ZBuffer),
    Str(ZString),
    Vec(ZVec),
    Obj(ZHashTable),
    MutRef(ZMutRef),
    Rune(ZRune),
    Sym(ZIdent),

    // No-op / whitespace placeholder (for parsing)
    Unit,
}

impl ZValue {
    pub const fn is_sym(&self) -> bool {
        if let Self::Sym(_) = self {
            true
        } else {
            false
        }
    }

    pub const fn type_string(&self) -> &'static str {
        match self {
            ZValue::Nil => "Nil",
            ZValue::Bool(_) => "Bool",
            ZValue::Number(_) => "Number",
            ZValue::Byte(_) => "Byte",
            ZValue::Buffer(_) => "Buffer",
            ZValue::Str(_) => "String",
            ZValue::Vec(_) => "Vector",
            ZValue::Obj(_) => "Object",
            ZValue::MutRef(_) => "Ref",
            ZValue::Rune(_) => "Rune",
            ZValue::Sym(_) => "Symbol",
            ZValue::Unit => "()",
        }
    }

    #[inline]
    pub fn is_truthy(&self) -> bool {
        !self.is_falsey()
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            ZValue::Nil => true,
            ZValue::Bool(b) => b.0,
            ZValue::Number(n) => n.unwrap() == 0.0,
            ZValue::Byte(b) => *b == ZByte::new(0),
            ZValue::Buffer(buf) => buf.len() == 0,
            ZValue::Str(s) => s.len() == 0,
            ZValue::Vec(v) => v.len() == 0,
            ZValue::Obj(_) => false,
            ZValue::MutRef(_) => false,
            ZValue::Rune(r) => false,
            ZValue::Sym(sym) => false,
            ZValue::Unit => true,
        }
    }

    pub fn expect_float64(&self) -> ZFloat64 {
        if let Self::Number(n) = self {
            *n
        } else {
            panic!(
                "CORE PANIC! :: Value: {} cannot be converted to a Number! ",
                self
            )
        }
    }

    pub const fn try_float64(&self) -> Option<ZFloat64> {
        if let Self::Number(n) = self {
            Some(*n)
        } else {
            None
        }
    }

    pub fn expect_string(&self) -> ZString {
        if let Self::Str(s) = self {
            s.clone()
        } else {
            panic!("CORE PANIC! :: Value cannot be converted to a String!");
        }
    }

    pub fn try_string(&self) -> Option<ZString> {
        if let Self::Str(s) = self {
            Some(s.clone())
        } else {
            None
        }
    }

    pub const fn nil() -> Self {
        Self::Nil
    }

    pub const fn bool(v: bool) -> Self {
        Self::Bool(ZBool::new(v))
    }

    pub const fn number(v: f64) -> Self {
        Self::Number(ZFloat64::new(v))
    }

    pub fn symbol(sym: &ZIdent) -> Self {
        Self::Sym(sym.clone())
    }

    pub fn string(s: ZString) -> Self {
        Self::Str(s.clone())
    }
}

// impl From<&str> for ZValue {
//     fn from(value: &str) -> Self {
//         todo!()
//     }
// }

impl Display for ZValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s: String = match self {
            ZValue::Nil => "nil".into(),
            ZValue::Bool(ZBool(true)) => "true".into(),
            ZValue::Bool(ZBool(false)) => "false".into(),
            ZValue::Number(n) => n.to_string(),
            ZValue::Byte(b) => b.to_string(),
            ZValue::Buffer(_buf) => todo!(),
            ZValue::Str(s) => s.to_string(),
            ZValue::Vec(_) => todo!(),
            ZValue::Obj(_) => todo!(),
            ZValue::MutRef(_) => todo!(),
            ZValue::Rune(ri) => ri.to_string(),
            ZValue::Sym(s) => s.to_string(),
            ZValue::Unit => "unit()".into(),
        };
        write!(f, "{}", s)
    }
}
