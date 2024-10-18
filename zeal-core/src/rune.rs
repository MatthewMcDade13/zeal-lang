use std::{
    collections::HashMap,
    ops::{Deref, DerefMut},
};

use crate::buf::ShortStr;

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Rune(ShortStr);

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RuneId(pub(crate) usize);

impl RuneId {
    pub const fn get(&self) -> usize {
        self.0
    }
}

impl Deref for RuneId {
    type Target = usize;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Rune {
    pub const fn zeroed() -> Self {
        let sb = ShortStr::zeroed();
        Self(sb)
    }
    fn new(name: &str) -> Self {
        let s = ShortStr::new(name);
        Self(s)
    }

    pub fn as_str(&self) -> &str {
        match &self.0 {
            crate::buf::ShortBuffer::Short(short_vec) => std::str::from_utf8(short_vec.as_ref())
                .expect("Error converting u8 slice to str!!")
                .trim_end(),
            crate::buf::ShortBuffer::Tall(tv) => tv.as_ref(),
        }
    }
}

impl Deref for Rune {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
#[derive(Debug, Clone)]
pub struct RuneTableBuilder {
    table: HashMap<String, RuneId>,
}

impl RuneTableBuilder {
    pub fn new() -> Self {
        Self {
            table: HashMap::with_capacity(16),
        }
    }

    pub fn add_rune(&mut self, rname: &str) {
        println!("{rname}");
        if !self.table.contains_key(rname) {
            let id = RuneId(self.table.len());
            self.table.insert(rname.to_string(), id);
        }
    }

    pub fn build(mut self) -> RuneTable {
        let mut r = vec![Rune::zeroed(); self.table.len()];

        for (k, v) in self.table.drain() {
            r[v.0] = Rune::new(k.as_str())
        }

        RuneTable { buf: r }
    }
}

impl Default for RuneTableBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct RuneTable {
    buf: Vec<Rune>,
}

impl RuneTable {
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buf: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn lookup_id(&self, rid: RuneId) -> Option<&Rune> {
        self.buf.get(rid.0)
    }

    #[inline]
    pub fn lookup_name(&self, name: &str) -> Option<&Rune> {
        self.buf.iter().find(|r| r.as_str() == name)
    }

    #[inline]
    pub fn get_id(&self, name: &str) -> Option<RuneId> {
        for (i, r) in self.buf.iter().enumerate() {
            if r.as_str() == name {
                return Some(RuneId(i));
            }
        }
        None
    }

    // pub fn add(&mut self, rname: &str) -> Self {}

    // pub fn has(&self, rname: &str) -> bool {
    //     self.buf.iter().any(|r| r.as_str() == rname)
    // }
}

impl Deref for RuneTable {
    type Target = [Rune];

    fn deref(&self) -> &Self::Target {
        self.buf.as_slice()
    }
}

impl DerefMut for RuneTable {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.buf.as_mut_slice()
    }
}
