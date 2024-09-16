
use std::{cmp::Ordering, fmt::Display};

use crate::core_types::{func::Func, str::{ZIdent, ZString}};

use super::func::FuncChunk;


#[derive(Debug, Clone)]
pub struct CompState {
    pub target: FuncChunk, 
    pub parent_scope: Option<Box<Self>>, 
}
    


impl CompState {
    pub const fn new_root(target: FuncChunk) -> Self {
        Self { target, parent_scope: None }
    }

    pub fn new(target: FuncChunk, parent_scope: Self) -> Self {
        let parent_scope = Some(Box::new(parent_scope));
        Self { target, parent_scope }
    }

    pub const fn is_root(&self) -> bool {
        self.parent_scope.is_none()
        // matches!(self, Self::Root { .. })
    }

    // pub const fn target(&self) -> &FuncChunk { 
    //     match self { 
    //         Self::Root { target } => target, 
    //         Self::Node { target, .. } => target 
    //     } 
    // } 
    //
    // pub fn target_mut(&mut self) -> &mut FuncChunk { 
    //     match self { 
    //         Self::Root { target } => target, 
    //         Self::Node { target, .. } => target 
    //     } 
    // } 

}


#[derive(Debug, Clone)]
pub struct Scope {
    pub locals: Vec<Local>,
    depth: ScopeDepth,
}

#[derive(Debug, Default, Clone, Copy, Eq)]
pub enum ScopeDepth {
    #[default]
    Global,
    Local {
        depth: usize,
    },
}


impl PartialOrd for ScopeDepth {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
         match self {
            ScopeDepth::Global => match other {
                ScopeDepth::Global => Some(Ordering::Equal), 
                ScopeDepth::Local { .. } =>  {
                    Some(Ordering::Less)
                }, 
            }, 
            ScopeDepth::Local { depth: sdepth } => match other {
                ScopeDepth::Global => Some(Ordering::Greater), 
                ScopeDepth::Local { depth: other_depth } => sdepth.partial_cmp(other_depth) 
            } 
            ScopeDepth::Local {depth: 0 } => match other {
                ScopeDepth::Global => Some(Ordering::Greater), 
                ScopeDepth::Local { depth } => 0.partial_cmp(depth), 
            }
        }

    }
}

impl PartialEq for ScopeDepth {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ScopeDepth::Global => match other {
                ScopeDepth::Global => true, 
                ScopeDepth::Local { depth } => false, 
            }, 
            ScopeDepth::Local { depth: sdepth } => match other {
                ScopeDepth::Global => false, 
                ScopeDepth::Local { depth: other_depth } => sdepth == other_depth 
            } 
        }
    }
}

impl Display for ScopeDepth {
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

impl ScopeDepth {

    pub const fn unwrap(self) -> usize {
        match self {
            ScopeDepth::Global => panic!("Attempt to unwrap BindScope failed! Expeceted: BindScope::Local(n), Got: BindScope::Global"), 
            ScopeDepth::Local { depth } => {depth} 
        }
    }

    pub const fn is_local(self) -> bool {
        match self {
            ScopeDepth::Global => false,
            ScopeDepth::Local { .. } => true,
        }
    }

    pub const fn is_global(self) -> bool {
        match self {
            ScopeDepth::Global => true,
            ScopeDepth::Local { .. } => false,
        }
    }

    pub fn decn(self, n: usize) -> Self {
        match self {
            ScopeDepth::Global => panic!("Cannot decrement depth for a Global Scope!!!"), 
            ScopeDepth::Local { depth } => {
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
            ScopeDepth::Global => Self::Local { depth: n - 1 }, 
            ScopeDepth::Local { depth } => {
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
            depth: ScopeDepth::Global,
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
            if l.ident.string() == ident.string() {
                return Some(i);
            }
        }
        None
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            locals: Vec::with_capacity(capacity),
            depth: ScopeDepth::Global,
        }
    }

    pub fn add_local(&mut self, name: ZIdent) {
        if let ScopeDepth::Local {depth }= self.depth() {

            let local = Local { ident: name, depth: ScopeDepth::Local { depth } };
            self.locals.push(local);
        } else {
            panic!("Tried to add local when current scope is Global!!!");
        }


    }

    pub const fn depth(&self) -> ScopeDepth {
        self.depth
    }

    pub fn start_scope(&mut self) {
        self.depth.inc_mut();
    }

    pub fn end_scope(&mut self) -> usize {
        match self.depth {
            ScopeDepth::Global => {
                panic!("mismatched end_scope!!!. (Attempt to decrement depth when depth == 0)");
            }
            _ => { 
                let new_depth = self.depth.dec_mut();
                match new_depth {
                    ScopeDepth::Global => {

                        let n = self.locals.len();
                        self.locals.clear();
                        n
                    }
                    ScopeDepth::Local { depth } => {
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
    pub depth: ScopeDepth,
}

impl Display for Local {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}
