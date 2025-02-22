use std::{
    collections::{HashMap, HashSet},
    hash::{self, Hash, Hasher},
    marker::{PhantomData, PhantomPinned},
    ops::{Deref, DerefMut, Index, Range},
    pin::Pin,
    ptr::NonNull,
    rc::Rc,
};

use rkyv::{Archive, Deserialize, Serialize};

use crate::{append_byte_slice, copy_slice_into};

#[derive(Debug, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct PoolString(String);

impl PoolString {
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
        } else if Self::is_prefixed_str(string) {
            Self(string.into())
        } else {
            let mut s = String::with_capacity(string.len() + 1);
            s.push(prefix.as_char());
            s.push_str(string);
            Self(s)
        }
    }

    // Creates a new PoolStr from a valid prefixed string.
    // Returns None if string had a invalid prefix
    pub fn new_prefixed(string: &str) -> Option<Self> {
        if string.len() <= 1 {
            None
        } else if Self::is_prefixed_str(string) {
            Some(Self(string.to_string()))
        } else {
            None
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
        } else if Self::is_prefixed_str(literal) {
            Self::new_prefixed(literal)
        } else {
            None
        }
    }

    pub fn rune(unprefixed_rune: &str) -> Self {
        Self::new(StringPrefix::Rune, unprefixed_rune)
    }

    pub fn prefixed_rune(rune: &str) -> Option<Self> {
        if rune.len() <= 1 {
            None
        } else if Self::is_prefixed_str(rune) {
            Self::new_prefixed(rune)
        } else {
            None
        }
    }
    pub fn symbol(unprefixed_symbol: &str) -> Self {
        Self::new(StringPrefix::Symbol, unprefixed_symbol)
    }

    pub fn prefixed_symbol(symbol: &str) -> Option<Self> {
        if symbol.len() <= 1 {
            None
        } else {
            match Self::is_prefixed_str(symbol) {
                true => Self::new_prefixed(symbol),
                false => None,
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

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn prefix(&self) -> StringPrefix {
        let byte = self.0.as_bytes()[0];
        StringPrefix::new(byte).unwrap_or_else(|| panic!("Invalid StringPool string prefix! Got: {byte}, Valid Values are: [Literal({}), Rune({}), Symbol({})]", StringPrefix::Literal.as_byte(), StringPrefix::Rune.as_byte(), StringPrefix::Symbol.as_byte()))
    }
}
impl From<&str> for PoolString {
    fn from(value: &str) -> Self {
        if Self::is_prefixed_str(value) {
            if let Some(ps) = Self::new_prefixed(value) {
                ps
            } else {
                Self::new(StringPrefix::Literal, value)
            }
        } else {
            Self::new(StringPrefix::Literal, value)
        }
    }
}

impl From<String> for PoolString {
    fn from(value: String) -> Self {
        Self::from(value.as_str())
    }
}

impl Deref for PoolString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PoolStr<'pool>(Pin<&'pool str>);

impl PoolStr<'_> {
    #[inline]
    pub fn as_str(&self) -> &str {
        self.deref()
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

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn prefix(&self) -> StringPrefix {
        let byte = self.0.as_bytes()[0];
        StringPrefix::new(byte).unwrap_or_else(|| panic!("Invalid StringPool string prefix! Got: {byte}, Valid Values are: [Literal({}), Rune({}), Symbol({})]", StringPrefix::Literal.as_byte(), StringPrefix::Rune.as_byte(), StringPrefix::Symbol.as_byte()))
    }
}

impl Deref for PoolStr<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.get_ref()
    }
}

/// Byte-Index / Address of the start of a String in the StringPool
/// index points to the first character of the string
/// in reference.
#[derive(Debug, Clone, Copy, Default, PartialEq, PartialOrd, Archive, Serialize, Deserialize)]
#[rkyv(derive(Debug, Clone, Copy, Default), compare(PartialEq, PartialOrd))]
#[repr(C)]
pub struct PoolStringAddr {
    pub address: u32,
    pub len: u32,
}

impl PoolStringAddr {
    pub const fn new(pool_index: usize, len: usize) -> Self {
        Self {
            address: pool_index as u32,
            len: len as u32,
        }
    }

    pub const fn end(&self) -> usize {
        (self.address + self.len) as usize
    }

    pub const fn range(&self) -> Range<usize> {
        let begin = self.address as usize;
        let end = begin + self.len as usize;
        begin..end
    }
}

#[derive(Debug, Clone, Default)]
pub struct StringPoolBuilder {
    strings: StringPool,
    seen: HashMap<PoolString, PoolStringAddr>, // buildval: StringPool,
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
        Self {
            strings: StringPool::new(),
            seen: HashMap::new(),
        }
    }

    pub fn build(self) -> StringPool {
        let Self { strings, .. } = self;
        strings
    }

    pub fn add_string(&mut self, string: PoolString) -> PoolStringAddr {
        if let Some(addr) = self.seen.get(&string) {
            *addr
        } else {
            let i = self.pool.len();
            self.pool.push(' ');
            self.pool.push_str(string.as_str());
            let addr = PoolStringAddr::new(i, string.len());
            self.seen.insert(string, addr);
            addr
        }
    }

    pub fn add_literal(&mut self, literal: &str) -> PoolStringAddr {
        let ps = PoolString::from_string(StringPrefix::Literal, literal);
        self.add_string(ps)
    }

    pub fn add_rune(&mut self, rune: &str) -> PoolStringAddr {
        let ps = PoolString::from_string(StringPrefix::Rune, rune);
        self.add_string(ps)
    }

    pub fn add_symbol(&mut self, symbol: &str) -> PoolStringAddr {
        let ps = PoolString::from_string(StringPrefix::Symbol, symbol);
        self.add_string(ps)
    }

    #[inline]
    pub fn entries(&self) -> Vec<&str> {
        self.pool.split(' ').collect::<Vec<_>>()
    }

    #[inline]
    pub fn entries_as_poolstr(&self) -> Vec<PoolString> {
        self.pool
            .split(' ')
            .map(|s| PoolString::new_prefixed(s).expect("Error Converting &str to PoolStr!"))
            .collect::<Vec<_>>()
    }

    pub fn entries_addr(&self) -> Vec<PoolStringAddr> {
        let mut len = 0;

        let mut buffer = Vec::with_capacity(self.pool.len() / 8);
        let pool_bytes = self.pool.as_bytes();
        for (i, b) in pool_bytes.iter().enumerate() {
            if *b == StringPool::delim_byte() && i + 1 < pool_bytes.len() {
                let length = len;
                len = 0;
                buffer.push(PoolStringAddr {
                    address: i as u32 + 1,
                    len: length as u32,
                });
            } else {
                len += 1;
            }
        }
        buffer
    }

    pub fn has_string(&self, prefix: StringPrefix, string: &str) -> bool {
        self.seen.contains_key(&PoolString::new(prefix, string))
    }
}

#[derive(Debug, Clone, Default)]
pub struct StringPool {
    pool: String,
    _pin: PhantomPinned,
}

impl StringPool {
    const DELIM: char = ' ';
    const DELIM_BYTE: u8 = b' ';

    pub fn builder() -> StringPoolBuilder {
        StringPoolBuilder::new()
    }

    pub const fn delim() -> char {
        Self::DELIM
    }

    pub const fn delim_byte() -> u8 {
        Self::DELIM_BYTE
    }

    pub const fn empty() -> Self {
        Self {
            pool: String::new(),
            _pin: PhantomPinned,
        }
    }

    pub const fn from_string(string: String) -> Self {
        Self {
            pool: string,
            _pin: PhantomPinned,
        }
    }

    pub const fn new() -> Self {
        Self::empty()
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            pool: String::with_capacity(cap),
            ..Default::default()
        }
    }
    pub fn entries(&self) -> Vec<&str> {
        self.pool.split(' ').collect::<Vec<_>>()
    }

    #[inline]
    pub fn entries_as_poolstr(self: Pin<&Self>) -> Vec<PoolString> {
        self.pool
            .split(' ')
            .map(|s| PoolString::new_prefixed(s).expect("Error Converting &str to PoolStr!"))
            .collect::<Vec<_>>()
    }

    pub fn entries_addr(self: Pin<&Self>) -> Vec<PoolStringAddr> {
        let mut len = 0;

        let mut buffer = Vec::with_capacity(self.pool.len() / 8);
        let pool_bytes = self.pool.as_bytes();
        for (i, b) in pool_bytes.iter().enumerate() {
            if *b == StringPool::delim_byte() && i + 1 < pool_bytes.len() {
                let length = len;
                len = 0;
                buffer.push(PoolStringAddr {
                    address: i as u32 + 1,
                    len: length as u32,
                });
            } else {
                len += 1;
            }
        }
        buffer
    }

    #[inline]
    pub fn as_bytes<'a>(self: &'a Pin<&'a Self>) -> &'a [u8] {
        self.pool.as_bytes()
    }

    pub fn address_of(self: Pin<&Self>, string: &str) -> Option<PoolStringAddr> {
        for (i, s) in self.entries().iter().enumerate() {
            if *s == string {
                return Some(PoolStringAddr::new(i, s.len()));
            }
        }

        None
    }

    pub fn get_ref<'a>(self: &'a Pin<&'a Self>, addr: PoolStringAddr) -> Option<PoolStr<'a>> {
        if addr.end() >= self.pool.len() {
            None
        } else {
            let string = &self[addr];
            let pstr = PoolStr(Pin::new(string));
            Some(pstr)
        }
    }
}

impl Index<PoolStringAddr> for StringPool {
    type Output = str;

    fn index(&self, index: PoolStringAddr) -> &Self::Output {
        &self.pool[index.range()]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, bytemuck::Zeroable, Hash)]
#[repr(u8)]
pub enum StringPrefix {
    Invalid = 0,
    Literal = b'"',
    Rune = b':',
    Symbol = b'\\',
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
