use std::collections::HashMap;

use crate::val::Value;

#[derive(Debug, Default, Clone)]
pub struct Env {
    outer: Option<Box<Self>>,
    data: HashMap<String, Value>,
}

impl Env {
    pub fn core() -> Self {}
}
