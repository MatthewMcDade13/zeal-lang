use std::rc::Rc;

use super::ZValue;

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZVec(Rc<[ZValue]>);

impl From<&[ZValue]> for ZVec {
    fn from(value: &[ZValue]) -> Self {
        Self(Rc::from(value.to_vec()))
    }
}

impl ZVec {
    pub fn new() -> Self {
        Self(Rc::new([]))
    }

    pub fn slice(&self) -> &[ZValue] {
        self.0.as_ref()
    }
}
