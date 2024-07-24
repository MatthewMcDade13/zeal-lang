use std::{cell::Cell, rc::Rc};

#[repr(transparent)]
#[derive(Debug, Default, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ZByte(u8);

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZBuffer(Rc<[Cell<ZByte>]>);
