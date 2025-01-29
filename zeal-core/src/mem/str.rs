use std::{
    collections::{HashMap, HashSet},
    hash::{self, Hash, Hasher},
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::{append_byte_slice, copy_slice_into};

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PoolStr(String);

impl PoolStr {
    pub fn is_prefixed_str(string: &str) -> bool {
        let len = string.len();
        if len > 1 {
            let p = string.as_bytes()[0];
            StringPrefix::is_valid_byte(p)
        } else {
            false
        }
    }

    // Creates a new PoolStr. Looks at the fist byte of
    // string argument to see if it is a valid string prefix.
    // if not it will create a PoolStr of given type, otherwise just creates a new
    // PoolStr with given string
    pub fn from_string(prefix: StringPrefix, string: &str) -> Self {
        if string.len() <= 1 {
            let s = format!("[[[ EMPTY_STRING: len({}) ]]]", string.len());
            Self(s)
        } else {
            if Self::is_prefixed_str(string) {
                Self(string.into())
            } else {
                let mut s = String::with_capacity(string.len() + 1);
                s.push(prefix.as_char());
                s.push_str(string);
                Self(s)
            }
        }
    }

    // Creates a new PoolStr from a valid prefixed string.
    // Returns None if string had a invalid prefix
    pub fn new_prefixed(string: &str) -> Option<Self> {
        if string.len() <= 1 {
            None
        } else {
            if Self::is_prefixed_str(string) {
                Some(Self(string.to_string()))
            } else {
                None
            }
        }
    }

    // Creates a new PoolStr from a string-type prefix and an unprefixed string.
    pub fn new(ty: StringPrefix, unprefixed_str: &str) -> Self {
        let len = unprefixed_str.len() + 1;
        let mut s = String::with_capacity(len);

        s.push(ty.as_char());
        s.push_str(unprefixed_str);

        Self(s)
    }

    pub fn literal(literal: &str) -> Self {
        Self::new(StringPrefix::Literal, literal)
    }

    pub fn prefixed_literal(literal: &str) -> Option<Self> {
        if literal.len() <= 1 {
            None
        } else {
            if Self::is_prefixed_str(literal) {
                Self::new_prefixed(literal)
            } else {
                None
            }
        }
    }

    pub fn rune(unprefixed_rune: &str) -> Self {
        Self::new(StringPrefix::Rune, unprefixed_rune)
    }

    pub fn prefixed_rune(rune: &str) -> Option<Self> {
        if rune.len() <= 1 {
            None
        } else {
            if Self::is_prefixed_str(rune) {
                Self::new_prefixed(rune)
            } else {
                None
            }
        }
    }
    pub fn symbol(unprefixed_symbol: &str) -> Self {
        Self::new(StringPrefix::Symbol, unprefixed_symbol)
    }

    pub fn prefixed_symbol(symbol: &str) -> Option<Self> {
        if symbol.len() <= 1 {
            None
        } else {
            if Self::is_prefixed_str(symbol) {
                Self::new_prefixed(symbol)
            } else {
                None
            }
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    #[inline]
    pub fn as_unprefixed(&self) -> &str {
        &self.0[1..]
    }

    // Gets length of string without its prefix
    #[inline]
    pub fn unprefixed_len(&self) -> usize {
        self.len() - 1
    }

    // Gets length of string with its prefix
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn prefix(&self) -> StringPrefix {
        let byte = self.0.as_bytes()[0];
        StringPrefix::new(byte).expect(&format!("Invalid StringPool string prefix! Got: {byte}, Valid Values are: [Literal({}), Rune({}), Symbol({})]", StringPrefix::Literal.as_byte(), StringPrefix::Rune.as_byte(), StringPrefix::Symbol.as_byte()))
    }
}

impl Deref for PoolStr {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Byte-Index / Address of the start of a String in the StringPool
/// index points to where the 0xFF delimiter is, along-side its string type, then the string
/// in reference.
#[derive(
    Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable, PartialEq, Eq, PartialOrd, Ord,
)]
#[repr(C)]
pub struct PoolStringAddr {
    pub begin: u32,
    pub len: u32,
}

impl PoolStringAddr {
    pub const fn end(&self) -> usize {
        (self.begin + self.len) as usize
    }
}

#[derive(Debug, Clone, Default)]
pub struct StringPoolBuilder {
    strings: StringPool,
    seen: HashMap<String, PoolStringAddr>, // buildval: StringPool,
}

impl Deref for StringPoolBuilder {
    type Target = StringPool;

    fn deref(&self) -> &Self::Target {
        &self.strings
    }
}

impl DerefMut for StringPoolBuilder {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.strings
    }
}

impl StringPoolBuilder {
    pub fn new() -> Self {
        let mut strings = StringPool::new();
        // prime pool. makes this easy to iterate
        strings.pool.push(' ');
        Self {
            strings: StringPool::new(),
            seen: HashMap::new(),
        }
    }

    pub fn add_string(&mut self, string: PoolStr) -> PoolStringAddr {
        let len = string.len();
        assert!(
            len <= u16::MAX as usize,
            "Cannot add a string of length longer than: {} to Ast StringPool!!!",
            u16::MAX
        );
        let i = self.strings.pool.len() as u32;
        self.strings.pool.push_str(string.as_str());
        self.strings.pool.push(' ');

        PoolStringAddr {
            begin: i,
            len: len as u32,
        }
    }

    pub fn add_literal(&mut self, literal: &str) -> PoolStringAddr {
        let ps = PoolStr::from_string(StringPrefix::Literal, literal);
        self.add_string(ps)
    }

    pub fn add_rune(&mut self, rune: &str) -> PoolStringAddr {
        let ps = PoolStr::from_string(StringPrefix::Rune, rune);
        self.add_string(ps)
    }

    pub fn add_symbol(&mut self, symbol: &str) -> PoolStringAddr {
        let ps = PoolStr::from_string(StringPrefix::Symbol, symbol);
        self.add_string(ps)
    }

    #[inline]
    pub fn entries(&self) -> Vec<&str> {
        self.pool.split(' ').collect::<Vec<_>>()
    }

    #[inline]
    pub fn entries_as_poolstr(&self) -> Vec<PoolStr> {
        self.pool
            .split(' ')
            .map(|s| PoolStr::new_prefixed(s).expect("Error Converting &str to PoolStr!"))
            .collect::<Vec<_>>()
    }

    pub fn entries_addr(&self) -> Vec<PoolStringAddr> {
        const DELIM: u8 = ' ' as u8;
        let mut len = 0;

        let mut buffer = Vec::with_capacity(self.pool.len() / 8);
        let pool_bytes = self.pool.as_bytes();
        for (i, b) in pool_bytes.iter().enumerate() {
            if *b == DELIM && i + 1 < pool_bytes.len() {
                let length = len;
                len = 0;
                buffer.push(PoolStringAddr {
                    begin: i as u32 + 1,
                    len: length as u32,
                });
            } else {
                len += 1;
            }
        }
        buffer
    }

    pub fn has_string(&self, string: &str) -> bool {
        self.entries().contains(&string)
    }

    fn push_prefix(&mut self, prefix: StringPrefix) {
        let (delim, ty, length) = prefix.into_char_tup();
        // NOTE: ORDER OF THIS IS IMPORTANT FOR SEAIALIZEATION AND DESEERIALIZEATION!!!!!
        self.pool.push(ty);
        self.pool.push(length);
    }

    fn add_entry(&mut self, prefix: StringPrefix, entry: &str) -> StringRef {
        let i = self.pool.len();
        self.push_prefix(prefix);
        self.pool.push_str(entry);
        StringRef {
            prefix,
            address: PoolStringAddr::from_usize(i),
            string: &self.pool[i..],
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct StringPool {
    pool: String,
}

impl StringPool {
    pub const fn empty() -> Self {
        Self {
            pool: String::new(),
        }
    }

    pub const fn new() -> Self {
        Self::empty()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            pool: String::with_capacity(cap),
        }
    }
    #[inline]
    pub fn as_bytes(&self) -> &[u8] {
        self.pool.as_bytes()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, bytemuck::Zeroable, Hash)]
#[repr(u8)]
pub enum StringPrefix {
    Invalid = 0,
    Literal = '"' as u8,
    Rune = ':' as u8,
    Symbol = '\\' as u8,
}

impl StringPrefix {
    pub const fn new(byte: u8) -> Option<Self> {
        match byte {
            x if x == Self::Literal as u8 => Some(Self::Literal),
            x if x == Self::Rune as u8 => Some(Self::Rune),
            x if x == Self::Symbol as u8 => Some(Self::Symbol),
            _ => None,
        }
    }

    pub const fn is_valid_byte(byte: u8) -> bool {
        Self::new(byte).is_some()
    }

    pub const fn as_char(self) -> char {
        // SAFETY: This is okay, since the only way to create this type is to return use new,
        // which would return None if this byte is invalid.
        unsafe { char::from_u32_unchecked(self as u32) }
    }

    pub const fn as_byte(self) -> u8 {
        self as u8
    }
}

// #[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
// #[repr(C)]
// pub struct StringPrefix {
//     /// Max length is u16::MAX, i figure this is fine, as no symbol in the language, or any rune or
//     /// string literal will ever be bigger than u16::MAX, and i think its reasonable to enfore this
//     /// limit
//     length: u16,
// }
//
// impl StringPrefix {
//     const DELIM: u8 = ' ' as u8;
//
//     pub const fn literal(length: u16) -> Self {
//         Self {
//             delim: Self::delim(),
//             ty: StringType::Literal as u8,
//             length,
//         }
//     }
//
//     pub const fn rune(length: u16) -> Self {
//         Self {
//             delim: Self::delim(),
//             ty: StringType::Rune as u8,
//             length,
//         }
//     }
//
//     pub const fn symbol(length: u16) -> Self {
//         Self {
//             delim: Self::delim(),
//             ty: StringType::Symbol as u8,
//             length,
//         }
//     }
//
//     pub const fn delim() -> u8 {
//         Self::DELIM
//     }
//
//     pub const fn new(ty: StringType, length: u16) -> Option<Self> {
//         if matches!(ty, StringType::Invalid) {
//             None
//         } else {
//             let ty = ty as u8;
//             let s = Self {
//                 delim: Self::delim(),
//                 ty,
//                 length,
//             };
//             Some(s)
//         }
//     }
//
//     pub const fn into_char_tup(self) -> (char, char, char) {
//         unsafe {
//             let a = char::from_u32_unchecked(self.delim as u32);
//             let b = char::from_u32_unchecked(self.ty as u32);
//             let c = char::from_u32_unchecked(self.length as u32);
//             (a, b, c)
//         }
//     }
//
//     pub const fn from_char(c: char, length: u16) -> Option<Self> {
//         if let Some(s) = Self::from_byte(c as u8, length) {
//             Some(s)
//         } else {
//             None
//         }
//     }
//
//     pub const fn from_byte(byte: u8, length: u16) -> Option<Self> {
//         if let Some(ty) = StringType::new(byte) {
//             let ty = ty as u8;
//             let s = Self {
//                 delim: Self::delim(),
//                 ty,
//                 length,
//             };
//             Some(s)
//         } else {
//             None
//         }
//     }
// }
