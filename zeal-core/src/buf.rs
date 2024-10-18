use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
    rc::Rc,
};

use crate::copy_slice_into;

pub const DEFAULT_SMALL_BUF_SIZE: usize = 23;

#[derive(Debug, Clone)]
pub enum ShortBuffer<T, const SHORT_SIZE: usize = DEFAULT_SMALL_BUF_SIZE, Fallback = Vec<T>> {
    Short(ShortVec<T, SHORT_SIZE>),
    Tall(Fallback),
}

impl ShortStr {
    pub const fn zeroed() -> Self {
        Self::Short(ShortVec::zeroed())
    }
}

impl Default for ShortStr {
    fn default() -> Self {
        Self::zeroed()
    }
}

#[derive(Debug, Clone)]
pub struct ShortVec<T, const SIZE: usize = DEFAULT_SMALL_BUF_SIZE> {
    buf: [T; SIZE],
}

impl<const S: usize> ShortVec<u8, S> {
    pub const fn zeroed() -> Self {
        Self { buf: [0u8; S] }
    }
}

impl<T, const S: usize> ShortVec<T, S>
where
    T: Default + Copy,
{
    pub fn new() -> Self {
        Self {
            buf: [T::default(); S],
        }
    }
}

impl<T, const S: usize> Default for ShortVec<T, S>
where
    T: Default + Copy,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<const S: usize> ShortVec<u8, S> {
    pub fn from_buf(buf: [u8; S]) -> Option<Self> {
        if buf.len() > S {
            None
        } else {
            Some(Self { buf })
        }
    }

    pub const fn new_bytes() -> Self {
        Self { buf: [0u8; S] }
    }
}

impl ShortStr {
    pub const SIZE: usize = DEFAULT_SMALL_BUF_SIZE;

    pub fn new(string: &str) -> Self {
        if string.len() > Self::SIZE {
            Self::Tall(Rc::from(string))
        } else {
            let mut buf = [0u8; Self::SIZE];
            copy_slice_into(&mut buf, string.as_bytes());
            Self::Short(ShortVec::from_buf(buf).unwrap())
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            ShortBuffer::Short(short_vec) => {
                std::str::from_utf8(short_vec.as_ref()).expect("Unable to convert u8 slice to str")
            }
            ShortBuffer::Tall(tv) => tv.as_ref(),
        }
    }

    pub fn len(&self) -> usize {
        match self {
            ShortBuffer::Short(short_vec) => {
                for (i, c) in short_vec.iter().rev().enumerate() {
                    if *c != 0 {
                        return i + 1;
                    }
                }
                short_vec.len()
            }
            ShortBuffer::Tall(tv) => tv.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            ShortBuffer::Short(short_vec) => short_vec[0] == 0,
            ShortBuffer::Tall(tv) => tv.len() == 0,
        }
    }

    pub fn is_full(&self) -> bool {
        match self {
            ShortBuffer::Short(short_vec) => *short_vec.last().unwrap() != 0,
            ShortBuffer::Tall(_) => false,
        }
    }

    pub const fn capacity(&self) -> usize {
        DEFAULT_SMALL_BUF_SIZE
    }
}

impl<T> AsRef<[T]> for ShortVec<T> {
    fn as_ref(&self) -> &[T] {
        &self.buf
    }
}

impl<T> AsMut<[T]> for ShortVec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        &mut self.buf
    }
}

impl<T> Deref for ShortVec<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        &self.buf
    }
}

impl<T> DerefMut for ShortVec<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buf
    }
}
//
// impl<T> ShortBuffer<T> {
//     pub fn push(&mut self, val: T)
//     where
//         T: Clone,
//     {
//         match self {
//             ShortBuffer::Short(sh) => {
//                 if let Some(v) = sh.try_push(val) {
//                     let mut buf = Vec::with_capacity(sh.buf.len() * 2);
//                     crate::clone_slice_into(buf.as_mut(), &sh.buf);
//                     *self = Self::Tall(buf);
//                     self.push(v)
//                 }
//             }
//             ShortBuffer::Tall(v) => {
//                 v.push(val);
//             }
//         }
//     }
//
//     /// Tries to put val into self if self is Tall. returns None if successfully pushed,
//     /// otherwise self is Short and we retrun  the given val back to caller
//     pub fn try_push_tall(&mut self, val: T) -> Option<T> {
//         if let Self::Tall(buf) = self {
//             buf.push(val);
//             None
//         } else {
//             Some(val)
//         }
//     }
// }

pub type ShortBytes = ShortBuffer<u8>;
pub type ShortStr = ShortBuffer<u8, DEFAULT_SMALL_BUF_SIZE, Rc<str>>;

impl PartialEq for ShortStr {
    fn eq(&self, other: &Self) -> bool {
        self.as_str().eq(other.as_str())
    }
}

impl Eq for ShortStr {}

impl PartialOrd for ShortStr {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Ord for ShortStr {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Hash for ShortStr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
    }
}

impl<T> Deref for ShortBuffer<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        match self {
            ShortBuffer::Short(s) => s.as_ref(),
            ShortBuffer::Tall(t) => t.as_ref(),
        }
    }
}

impl<T> DerefMut for ShortBuffer<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            ShortBuffer::Short(s) => s.as_mut(),
            ShortBuffer::Tall(t) => t.as_mut(),
        }
    }
}
