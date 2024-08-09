pub mod bytes;
pub mod htable;
pub mod num;
pub mod str;
pub mod val;
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

pub mod keywords {
    pub const ADD: &'static str = "+";
    pub const SUB: &'static str = "-";
    pub const DIV: &'static str = "/";
    pub const MUL: &'static str = "*";
    pub const CONCAT: &'static str = "++";
}

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
