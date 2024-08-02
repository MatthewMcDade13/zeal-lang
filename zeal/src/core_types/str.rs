use std::{
    fmt::Display,
    hash::Hash,
    ops::{Deref, Index},
    rc::Rc,
};

use anyhow::bail;

use crate::sys;

#[repr(transparent)]
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct ZString(Rc<str>);

impl Deref for ZString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl Hash for ZString {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl From<String> for ZString {
    fn from(value: String) -> Self {
        Self(Rc::from(value))
    }
}

impl From<&str> for ZString {
    fn from(value: &str) -> Self {
        Self(Rc::from(value))
    }
}
impl ZString {
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }
}

pub const MAX_RUNE_LEN: usize = 64;
pub const RUNE_BUF_EMPTY: [u8; MAX_RUNE_LEN] = [0u8; MAX_RUNE_LEN];

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuneName([u8; MAX_RUNE_LEN]);
impl RuneName {
    pub const fn empty() -> Self {
        Self(RUNE_BUF_EMPTY)
    }

    pub fn new(name: &str) -> anyhow::Result<Self> {
        if name.len() <= MAX_RUNE_LEN {
            let buf = Self::new_unchecked(name);
            Ok(buf)
        } else {
            bail!("Rune length is too long! Max characters is {MAX_RUNE_LEN}")
        }
    }

    pub fn new_unchecked(name: &str) -> Self {
        let mut buf = [0u8; MAX_RUNE_LEN];
        let name_bytes = name.as_bytes();
        sys::copy_slice_into(&mut buf, name_bytes);
        Self(buf)
    }

    pub fn str(&self) -> &str {
        std::str::from_utf8(self.0.as_ref()).expect("Error Reading byte slice as utf8 &str")
    }
}

impl Default for RuneName {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Debug, Clone)]
pub struct RuneTable {
    runes: Vec<RuneName>,
    next: usize,
}

impl RuneTable {
    pub fn empty() -> Self {
        Self {
            runes: Vec::with_capacity(16),
            next: 0,
        }
    }

    pub(crate) fn add_unchecked(&mut self, name: &str) -> ZRune {
        let id = self.next;
        self.next += 1;
        let name = RuneName::new_unchecked(name);
        self.runes.push(name);
        ZRune(id)
    }

    pub fn add(&mut self, name: &str) -> anyhow::Result<ZRune> {
        if let Some((_, i)) = self.lookup_name(name) {
            Ok(ZRune(i))
        } else {
            let id = self.next;
            self.next += 1;
            let name = RuneName::new(name)?;
            self.runes.push(name);
            let r = ZRune(id);
            Ok(r)
        }
    }

    pub fn get(&self, name: &str) -> Option<ZRune> {
        if let Some((_, i)) = self.lookup_name(name) {
            Some(ZRune(i))
        } else {
            None
        }
    }

    pub fn lookup(&self, rune: ZRune) -> Option<&str> {
        let id = rune.id();
        if id < self.runes.len() {
            Some(self.runes[id].str())
        } else {
            None
        }
    }

    /// Looks for rune by given name
    fn lookup_name(&self, name: &str) -> Option<(&RuneName, usize)> {
        for (i, rn) in self.runes.iter().enumerate() {
            if name == rn.str() {
                return Some((rn, i));
            }
        }
        None
    }
}

impl Index<ZRune> for RuneTable {
    type Output = str;

    fn index(&self, index: ZRune) -> &Self::Output {
        self.lookup(index).expect("Index out of range!!!")
    }
}

/// Wrapper for a unique id that can be used to look up name in global Symbol (rune) table
#[derive(Debug, Clone, Copy)]
pub struct ZRune(usize);

impl ZRune {
    pub const fn id(&self) -> usize {
        self.0
    }
}

impl Display for ZRune {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct ZSymbol(ZString);

impl ZSymbol {
    pub fn name(&self) -> &str {
        self.0.as_ref()
    }

    #[inline]
    pub fn has_rune_prefix(&self) -> bool {
        self.0.starts_with('#')
    }

    // #[inline]
    // pub fn needs_promote_rune(&self) -> bool {
    //     self.has_rune_prefix() && self.0.len() <= MAX_RUNE_LEN
    // }

    pub fn into_rune(self, rt: &mut RuneTable) -> anyhow::Result<ZRune> {
        rt.add(&self.0)
    }
}

impl Display for ZSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.to_string())
    }
}

use crate::ast::lex::Tok;
impl From<Tok> for ZSymbol {
    fn from(value: Tok) -> Self {
        value.into_sym()
    }
}

impl From<&str> for ZSymbol {
    fn from(value: &str) -> Self {
        Self(ZString::from(value))
    }
}

impl From<String> for ZSymbol {
    fn from(value: String) -> Self {
        Self(ZString::from(value))
    }
}

impl From<Rc<str>> for ZSymbol {
    fn from(value: Rc<str>) -> Self {
        Self(ZString(Rc::clone(&value)))
    }
}