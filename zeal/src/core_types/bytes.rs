use std::{
    cell::Cell,
    ops::{Deref, DerefMut},
    rc::Rc,
};

#[repr(transparent)]
#[derive(Debug, Default, Copy, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct ZByte(u8);

impl ZByte {
    pub const fn new(b: u8) -> Self {
        Self(b)
    }
}

impl Deref for ZByte {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ZByte {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZBuffer(Rc<[Cell<ZByte>]>);

impl Deref for ZBuffer {
    type Target = [Cell<ZByte>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
