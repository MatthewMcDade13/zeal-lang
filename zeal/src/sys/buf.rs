use std::ops::{Deref, DerefMut};

pub const DEFAULT_SMALL_BUF_SIZE: usize = 24;

#[derive(Debug, Clone)]
pub enum ShortBuffer<T, const SHORT_SIZE: usize = DEFAULT_SMALL_BUF_SIZE, Fallback = Vec<T>> {
    Short([T; SHORT_SIZE]),
    Tall(Fallback),
}

impl<T> ShortBuffer<T, DEFAULT_SMALL_BUF_SIZE, Vec<T>> {}

pub type ShortBytes = ShortBuffer<u8, DEFAULT_SMALL_BUF_SIZE>;
pub type ShortBuf<T> = ShortBuffer<T, DEFAULT_SMALL_BUF_SIZE>;

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
