
use std::{cmp::Ordering, fmt::Display};

use zeal_core::str::ZIdent;


#[derive(Debug, Clone)]
pub struct Scope {
    pub locals: Vec<Local>,
    depth: BindScope,
}

#[derive(Debug, Default, Clone, Copy, Eq)]
pub enum BindScope {
    #[default]
    Global,
    Local {
        depth: usize,
    },
}


impl PartialOrd for BindScope {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
         match self {
            BindScope::Global => match other {
                BindScope::Global => Some(Ordering::Equal), 
                BindScope::Local { .. } =>  {
                    Some(Ordering::Less)
                }, 
            }, 
            BindScope::Local { depth: sdepth } => match other {
                BindScope::Global => Some(Ordering::Greater), 
                BindScope::Local { depth: other_depth } => sdepth.partial_cmp(other_depth) 
            } 
            BindScope::Local {depth: 0 } => match other {
                BindScope::Global => Some(Ordering::Greater), 
                BindScope::Local { depth } => 0.partial_cmp(depth), 
            }
        }

    }
}

impl PartialEq for BindScope {
    fn eq(&self, other: &Self) -> bool {
        match self {
            BindScope::Global => match other {
                BindScope::Global => true, 
                BindScope::Local { depth } => false, 
            }, 
            BindScope::Local { depth: sdepth } => match other {
                BindScope::Global => false, 
                BindScope::Local { depth: other_depth } => sdepth == other_depth 
            } 
        }
    }
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

    pub fn decn(self, n: usize) -> Self {
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

        let l = self.locals.last()?;
        Some(l)
        // if self.locals.len() >= 1 {
        //
        //     let back = self.locals.len() - 1;
        //     Some(&self.locals[back])
        // } else {
        //     None
        // }

    }

    /// Looks for local and returns its index in locals vec
    pub fn resolve_local(&self, ident: &ZIdent) -> Option<usize> {
        // let mut i= self.locals.len() as isize;

        // while i >= 0 {
        //     i -= 1;
        //     let l = &self.locals[i as usize ];
        //     println!("{i}: {} {} {}", l.ident.name(), ident.name(), l.ident.name() == ident.name());
        //     if l.ident.name() == ident.name() {
        //         return Some(i as usize);
        //     }
        //
        //
        // }
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
                match new_depth {
                    BindScope::Global => {

                        let n = self.locals.len();
                        self.locals.clear();
                        n
                    }
                    BindScope::Local { depth } => {
                        let mut i = 0;

                        while let Some(local) = self.peek_top() {
                            if local.depth.unwrap() >  depth {
                                self.locals.pop();
                                i += 1;
                            } else {
                                break;
                            }
                        }

                        i 
                    }
                }
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
