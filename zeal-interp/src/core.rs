use std::{collections::HashMap, rc::Rc};

use zvm::{
    native::{zvm_print, zvm_println},
    val::{NativeFunc, Val},
};

pub type EnvTable = HashMap<String, Val>;

#[derive(Default, Debug, Clone)]
pub struct Env {
    parent: Option<Box<Self>>,
    table: EnvTable,
}

impl Env {
    pub fn new_core_root() -> Self {
        let table = core_root_table();
        Self {
            parent: None,
            table,
        }
    }

    #[inline]
    pub fn new_root() -> Self {
        Self::default()
    }

    pub fn new(parent: Self) -> Self {
        let parent = Some(Box::new(parent));
        Self {
            parent,
            table: HashMap::new(),
        }
    }

    pub fn lookup(&self, name: &str) -> Option<&Val> {
        self.table.get(name)
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Val> {
        self.table.get_mut(name)
    }

    pub fn resolve(&self, name: &str) -> Option<&Val> {
        if let Some(v) = self.lookup(name) {
            Some(v)
        } else if let Some(p) = self.parent.as_ref() {
            p.resolve(name)
        } else {
            None
        }
    }

    pub fn resolve_mut(&mut self, name: &str) -> Option<&mut Val> {
        if let Some(v) = self.lookup_mut(name) {
            Some(v)
        } else {
            match &mut self.parent {
                Some(p) => p.resolve_mut(name),
                _ => None,
            }
        };
        None
    }
}

fn core_root_table() -> EnvTable {
    let mut table = EnvTable::new();
    table.insert(
        "print".into(),
        Val::NativeFunc(NativeFunc {
            func: zvm_print,
            name: Rc::from("print"),
            arity: 1,
        }),
    );
    table.insert(
        "println".into(),
        Val::NativeFunc(NativeFunc {
            func: zvm_println,
            name: Rc::from("println"),
            arity: 1,
        }),
    );
    table
}
