use std::{cell::RefCell, collections::HashMap, rc::Rc};

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

pub type ZList = Rc<RefCell<Vec<Value>>>;
pub type ZVec = Rc<[Value]>;
pub type ZHashmap = Rc<HashMap<String, Value>>;
pub type ZString = Rc<str>;

#[derive(Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Str(ZString),
    Vec(ZVec),
    List(ZList),
    Obj(ZHashmap),

    // No-op / whitespace placeholder (for parsing)
    Unit,
}
