use std::{cell::RefCell, fmt::Display, hash::Hash, ops::Add, rc::Rc};

use super::{
    bytes::ZByte,
    num::{ZBool, ZFloat64},
    str::{ZIdent, ZRune, ZString},
};

#[derive(Debug, Clone)]
pub enum Val {
    Byte(u8),
    Bool(bool),
    UNum(usize),
    Num(isize),
    Float(f64),
    Rune(Rc<str>),
    String(Rc<str>),
    Ptr(Box<Self>),
}

// NOTE: Use A Dynamic Slab Allocator for the VM Stack
// so we can put primiatiives (eh: numbers, small strings, bytes, ect) as well as
// Immutable User defined structs on the VM Stack. Also implement a dynamic byte vec(iah) for user
// defined structs, so member access can be a quick and dead simple index/pointer offset (+ transmute)
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
    // Buffer(ZBuffer),
    Str(ZString),
    // Vec(ZVec),

    // Obj(ZHashTable),
    // MutRef(ZMutRef),
    Rune(ZRune),
    Ident(ZIdent),
    // List(Rc<[Self]>),

    // No-op / whitespace placeholder (for parsing)
    // Unit,
}

impl Hash for ZValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl Eq for ZValue {}

impl PartialEq for ZValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ZValue::Nil => matches!(other, ZValue::Nil),
            ZValue::Bool(sb) => {
                if let Self::Bool(other_bool) = other {
                    sb == other_bool
                } else {
                    false
                }
            }
            ZValue::Number(n) => {
                if let Self::Number(other_n) = other {
                    n == other_n
                } else {
                    false
                }
            }
            ZValue::Byte(_) => todo!(),
            // ZValue::Buffer(_) => todo!(),
            ZValue::Str(s) => {
                if let Self::Str(other_s) = other {
                    s == other_s
                } else {
                    false
                }
            }
            // ZValue::Vec(_) => todo!(),
            // ZValue::Obj(_) => todo!(),
            // ZValue::MutRef(_) => todo!(),
            ZValue::Rune(_) => todo!(),
            ZValue::Ident(ident) => {
                if let Self::Ident(other_ident) = other {
                    ident == other_ident
                } else {
                    false
                }
            } //     if let Self::List(other_li) = other {
              //         li == other_li
              //     } else {
              //         false
              //     }
              // }
        }
    }
}

// impl Hash for ZValue {
//     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
//         match self {
//             ZValue::Nil => (0xDEADBEEFu64).hash(state),
//             ZValue::Bool(b) => b.hash(state),
//             ZValue::Number(n) => n.u64().hash(state),
//             ZValue::Byte(_) => todo!(),
//             ZValue::Buffer(_) => todo!(),
//             ZValue::Str(st) => st.hash(state),
//             ZValue::Vec(v) => v.hash(state),
//             ZValue::Obj(_) => todo!(),
//             ZValue::MutRef(_) => todo!(),
//             ZValue::Rune(_) => todo!(),
//             ZValue::Ident(ident) => ident.name().hash(state),
//             ZValue::Unit => 0xACAB.hash(state),
//             // ZValue::List(li) => li.hash(state),
//         }
//     }
// }

impl From<&ZIdent> for ZValue {
    fn from(value: &ZIdent) -> Self {
        Self::Ident(value.clone())
    }
}

impl From<ZIdent> for ZValue {
    fn from(value: ZIdent) -> Self {
        Self::Ident(value)
    }
}

impl ZValue {
    pub fn expect_ident(&self) -> ZIdent {
        if let Self::Ident(ident) = self {
            ident.clone()
        } else {
            panic!(
                "Failed to unwrap ZValue as ZIdent!!! Found; {}",
                self.type_string()
            );
        }
    }

    // pub fn binary_operator(&self) -> Option<Op> {
    //     if let Self::Ident(ident) = self {
    //         ident.binary_operator()
    //     } else {
    //         None
    //     }
    // }
    //
    // pub fn unary_operator(&self) -> Option<Op> {
    //     if let ZValue::Ident(ident) = self {
    //         ident.unary_operator()
    //     } else {
    //         None
    //     }
    // }
    //
    // pub fn empty_vec() -> Self {
    //     Self::Vec(ZVec::new())
    // }
    pub const fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(_))
    }

    pub const fn type_string(&self) -> &'static str {
        match self {
            ZValue::Nil => "Nil",
            ZValue::Bool(_) => "Bool",
            ZValue::Number(_) => "Number",
            ZValue::Byte(_) => "Byte",
            // ZValue::Buffer(_) => "Buffer",
            ZValue::Str(_) => "String",
            // ZValue::Vec(_) => "Vector",
            // ZValue::Obj(_) => "Object",
            // ZValue::MutRef(_) => "Ref",
            ZValue::Rune(_) => "Rune",
            ZValue::Ident(_) => "Symbol",
            // ZValue::List(_) => "AstList",
        }
    }

    #[inline]
    pub fn is_truthy(&self) -> bool {
        !self.is_falsey()
    }

    pub fn is_falsey(&self) -> bool {
        match self {
            ZValue::Nil => true,
            ZValue::Bool(b) => !b.0,
            ZValue::Number(n) => n.unwrap() == 0.0,
            ZValue::Byte(b) => *b == ZByte::new(0),
            // ZValue::Buffer(buf) => buf.len() == 0,
            ZValue::Str(s) => s.len() == 0,
            // ZValue::Vec(v) => v.len() == 0,
            // ZValue::Obj(_) => false,
            // ZValue::MutRef(_) => false,
            ZValue::Rune(r) => false,
            ZValue::Ident(sym) => false,
            // ZValue::List(li) => li.len() == 0,
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

    pub fn ident_from_str(string: &str) -> Self {
        Self::Ident(ZIdent::new(string))
    }

    pub fn ident(ident: ZIdent) -> Self {
        Self::Ident(ident)
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
            // ZValue::Buffer(_buf) => todo!(),
            ZValue::Str(s) => format!("\"{}\"", s.to_string()),
            // ZValue::Vec(v) => v.iter().map(|v| v.to_string()).collect::<String>(),
            // ZValue::Obj(_) => todo!(),
            // ZValue::MutRef(_) => todo!(),
            ZValue::Rune(ri) => ri.to_string(),
            ZValue::Ident(s) => format!("#{}", s.to_string()),
            // ZValue::List(_) => todo!(),
        };
        write!(f, "{}", s)
    }
}
