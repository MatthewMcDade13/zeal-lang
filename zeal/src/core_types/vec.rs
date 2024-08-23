use std::{fmt::Display, ops::Deref, rc::Rc};

use super::val::ZValue;

#[repr(transparent)]
#[derive(Debug, Clone, Hash)]
pub struct ZVec(Rc<[ZValue]>);

impl Deref for ZVec {
    type Target = [ZValue];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Display for ZVec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::from("Vec[");
        for v in self.0.iter() {
            let vs = v.to_string();
            let vs = format!(" {vs} ");
            s.push_str(&vs);
        }
        s.push_str("]");
        write!(f, "{}", s)
    }
}

impl From<&[ZValue]> for ZVec {
    fn from(value: &[ZValue]) -> Self {
        Self(Rc::from(value.to_vec()))
    }
}
impl From<Vec<ZValue>> for ZVec {
    fn from(value: Vec<ZValue>) -> Self {
        Self(Rc::from(value))
    }
}

impl ZVec {
    pub fn new() -> Self {
        Self(Rc::new([]))
    }

    pub fn slice(&self) -> &[ZValue] {
        self.0.as_ref()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}
