use std::{collections::HashMap, rc::Rc};

use super::val::ZValue;

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZHashTable(Rc<HashMap<String, ZValue>>);
