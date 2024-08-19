use crate::core_types::str::ZString;

#[derive(Debug, Clone)]
pub struct ScopeState {
    pub locals: Vec<Local>,
    depth: usize,
}

impl ScopeState {
    pub const fn new() -> Self {
        Self {
            locals: Vec::new(),
            depth: 0,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            locals: Vec::with_capacity(capacity),
            depth: 0,
        }
    }

    pub fn add_local(&mut self, name: ZString) {
        let local = Local {
            name,
            depth: self.depth(),
        };
        self.locals.push(local);
    }

    pub const fn depth(&self) -> usize {
        self.depth
    }

    pub fn start_scope(&mut self) {
        self.depth += 1;
    }

    pub fn end_scope(&mut self) {
        if self.depth != 0 {
            self.depth -= 1;
        } else {
            panic!("mismatched end_scope!!!. (Attempt to decrement depth when depth == 0)")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub name: ZString,
    pub depth: usize,
}
