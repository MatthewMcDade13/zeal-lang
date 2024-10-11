use std::rc::Rc;

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
