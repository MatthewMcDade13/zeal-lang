use std::ops::{Deref, DerefMut};

pub const DEFAULT_SMALL_BUF_SIZE: usize = 24;

#[derive(Debug, Clone)]
pub enum ShortBuffer<T, const SHORT_SIZE: usize = DEFAULT_SMALL_BUF_SIZE, Fallback = Vec<T>> {
    Short(ShortVec<T, SHORT_SIZE>),
    Tall(Fallback),
}

#[derive(Debug, Clone)]
pub struct ShortVec<T, const SIZE: usize = DEFAULT_SMALL_BUF_SIZE> {
    buf: [T; SIZE],
    top: usize,
}

impl<T, const S: usize> ShortVec<T, S>
where
    T: Default + Copy,
{
    pub fn new() -> Self {
        Self {
            buf: [T::default(); S],
            top: 0,
        }
    }
}

impl<const S: usize> ShortVec<u8, S> {
    pub const fn new_bytes() -> Self {
        Self {
            buf: [0u8; S],
            top: 0,
        }
    }
}

impl<T, const S: usize> ShortVec<T, S> {
    /// Tries to put val into self. returns None if successfully pushed,
    /// otherwise self is full and we retrun  the given val back to caller
    pub fn try_push(&mut self, val: T) -> Option<T> {
        if self.top >= S {
            Some(val)
        } else {
            let i = self.top;
            self.buf[i] = val;
            self.top += 1;
            None
        }
    }

    pub fn pop(&mut self) -> Option<T>
    where
        T: Clone,
    {
        if self.top > 0 {
            let i = self.top;
            self.top -= 1;
            Some(self.buf[i].clone())
        } else {
            None
        }
    }

    pub const fn len(&self) -> usize {
        self.top
    }

    pub const fn is_empty(&self) -> bool {
        self.top == 0
    }

    pub const fn is_full(&self) -> bool {
        self.top >= S
    }

    pub const fn capacity(&self) -> usize {
        S
    }

    /// Pops and discards poped value. returns number of sucessful pops
    /// (ie: 0 if vec is empty)
    pub fn pop_forget(&mut self, npops: usize) -> usize {
        let pops = crate::clamp(0, npops, self.top);
        self.top -= pops;
        pops
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

impl<T> ShortBuffer<T> {
    pub fn push(&mut self, val: T)
    where
        T: Clone,
    {
        match self {
            ShortBuffer::Short(sh) => {
                if let Some(v) = sh.try_push(val) {
                    let mut buf = Vec::with_capacity(sh.buf.len() * 2);
                    crate::clone_slice_into(buf.as_mut(), &sh.buf);
                    *self = Self::Tall(buf);
                    self.push(v)
                }
            }
            ShortBuffer::Tall(v) => {
                v.push(val);
            }
        }
    }

    /// Tries to put val into self if self is Tall. returns None if successfully pushed,
    /// otherwise self is Short and we retrun  the given val back to caller
    pub fn try_push_tall(&mut self, val: T) -> Option<T> {
        if let Self::Tall(buf) = self {
            buf.push(val);
            None
        } else {
            Some(val)
        }
    }
}

pub type ShortBytes = ShortBuffer<u8>;

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
