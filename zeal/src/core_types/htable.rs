use std::{collections::HashMap, rc::Rc};

use super::ZValue;

#[repr(transparent)]
#[derive(Debug, Clone)]
pub struct ZHashTable(Rc<HashMap<String, ZValue>>);
