use std::rc::Rc;

/// Small string Optomized String, if longer than 23 bytes,
/// falls back to Rc<str>
pub enum RuneStr {
    Small([u8; 23]),
    Heap(Rc<str>),
}
