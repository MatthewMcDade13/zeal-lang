
use std::fmt::Display;

use crate::core_types::str::{ZIdent, ZString};

#[derive(Debug, Clone)]
pub struct Scope {
    pub locals: Vec<Local>,
    depth: BindScope,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindScope {
    #[default]
    Global,
    Local {
        depth: usize,
    },
}

impl Display for BindScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Global => "Global_Scope",
            Self::Local { depth } => &format!("Scope_Depth => {depth}"),
        }; 
        write!(f, "{s}")
    }
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::from("\n----- LOCALS -----\n");
        for (i, l) in self.locals.iter().enumerate() {
            let n = i + 1;
            let line = format!("{n} {l}\n");
            s.push_str(&line);
        }
        s.push_str("----- END LOCALS ------\n");
        write!(f, "{s}")
    }
}

impl BindScope {

    pub const fn unwrap(self) -> usize {
        match self {
            BindScope::Global => panic!("Attempt to unwrap BindScope failed! Expeceted: BindScope::Local(n), Got: BindScope::Global"), 
            BindScope::Local { depth } => {depth} 
        }
    }

    pub const fn is_local(self) -> bool {
        match self {
            BindScope::Global => false,
            BindScope::Local { .. } => true,
        }
    }

    pub const fn is_global(self) -> bool {
        match self {
            BindScope::Global => true,
            BindScope::Local { .. } => false,
        }
    }

    pub const fn decn(self, n: usize) -> Self {
        match self {
            BindScope::Global => panic!("Cannot decrement depth for a Global Scope!!!"), 
            BindScope::Local { depth } => {
                let d = depth as isize;
                let n = n as isize;
                let res = d - n;
                if res >= 0 {
                    Self::Local { depth: res as usize }
                } else {
                    Self::Global
                }
            } 
        }
    }
    
    pub const fn incn(self, n: usize) -> Self {
        if n == 0 {
            self
        } else {
        match self {
            BindScope::Global => Self::Local { depth: n - 1 }, 
            BindScope::Local { depth } => {
                Self::Local { depth: depth + n }
            } 
        }

        }
    }

    #[inline]
    pub fn inc_mut(&mut self) -> Self {
        *self = self.incn(1);
        *self
}

    #[inline]
    pub fn dec_mut(&mut self) -> Self {
        *self = self.decn(1);
        *self
    }
}

impl Scope {
    pub const fn new() -> Self {
        Self {
            locals: Vec::new(),
            depth: BindScope::Global,
        }
    }

    #[inline]
    pub fn peek_top(&self) -> Option<&Local> {
        if self.locals.len() >= 1 {

        let back = self.locals.len() - 1;
            Some(&self.locals[back])
        } else {
            None
        }

    }

    /// Looks for local and returns its index in locals vec
    pub fn resolve_local(&self, ident: &ZIdent) -> Option<usize> {
        for (i, l) in self.locals.iter().rev().enumerate() {
            if l.ident.name() == ident.name() {
                return Some(i);
            }
        }
        None
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            locals: Vec::with_capacity(capacity),
            depth: BindScope::Global,
        }
    }

    pub fn add_local(&mut self, name: ZIdent) {
        if let BindScope::Local {depth }= self.depth() {

            let local = Local { ident: name, depth: BindScope::Local { depth } };
            self.locals.push(local);
        } else {
            panic!("Tried to add local when current scope is Global!!!");
        }


    }

    pub const fn depth(&self) -> BindScope {
        self.depth
    }

    pub fn start_scope(&mut self) {
        self.depth.inc_mut();
    }

    pub fn end_scope(&mut self) -> usize {
        match self.depth {
            BindScope::Global => {
                panic!("mismatched end_scope!!!. (Attempt to decrement depth when depth == 0)");
            }
            _ => { 
                let new_depth = self.depth.dec_mut();

                let mut i = 0;

                while let Some(local) = self.peek_top() {
                    if local.depth > new_depth {
                        i += 1;
                        let _ = self.locals.pop();
                    }
                }

                i
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Local {
    pub ident: ZIdent,
    pub depth: BindScope,
}

impl Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}
