pub mod bytes;
pub mod htable;
pub mod num;
pub mod str;
pub mod vec;

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Display,
    rc::Rc,
};

use bytes::{ZBuffer, ZByte};
use htable::ZHashTable;
use num::{ZBool, ZFloat64};
use str::{ZRune, ZString, ZSymbol};
use vec::ZVec;

// #[derive(Debug, Clone)]
// pub enum ZealVal {
//     Const(Rc<Value>),
//     Mut(Rc<RefCell<Value>>),
// }
//
// impl ZealVal {
//     pub fn new(v: Value) -> Self {
//         Self::Const(Rc::new(v))
//     }
//
//     pub fn new_mut(v: Value) -> Self {
//         Self::Mut(Rc::new(RefCell::new(v)))
//     }
//
//     fn f(x: &mut i32) {
//         *x = 50
//     }
//
//     pub fn bool(b: bool) -> Self {
//         Self::Const(Value::Bool(b))
//     }
// }
//

pub type ZList = Rc<Vec<ZValue>>;
// pub type ZVec = Rc<[Value]>;

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZMutRef(Rc<RefCell<ZValue>>);

#[derive(Debug, Clone)]
pub enum ZValue {
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
    Sym(ZSymbol),

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
    pub const fn nil() -> Self {
        Self::Nil
    }

    pub const fn bool(v: bool) -> Self {
        Self::Bool(ZBool::new(v))
    }
}

impl Display for ZValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

// pub enum ZValue {}
