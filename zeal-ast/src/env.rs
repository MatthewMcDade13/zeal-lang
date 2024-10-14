use std::collections::HashMap;

pub struct Env {
    pub parent: Option<HashMap<String, Self>>,
    pub scope: HashMap<String, Self>,
}

impl Env {
    pub const fn is_root(&self) -> bool {
        self.parent.is_none()
    }
}
