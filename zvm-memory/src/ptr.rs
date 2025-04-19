use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use anyhow::Context;

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct RawSlice<T> {
    begin: NonNull<T>,
    len: u32,
}

impl<T> RawSlice<T> {
    #[inline]
    pub fn new(begin: NonNull<T>, len: u32) -> anyhow::Result<Self> {
        Self::try_new(begin, len).context("Cannot create RawSlice of len 0!!!")
    }

    pub const fn expect_new(begin: NonNull<T>, len: u32) -> Self {
        if len == 0 {
            panic!("Cannot create RawSlice of len 0!!");
        } else {
            Self { begin, len }
        }
    }

    pub const fn try_new(begin: NonNull<T>, len: u32) -> Option<Self> {
        if len == 0 {
            None
        } else {
            Some(Self { begin, len })
        }
    }

    pub const fn root_ptr(&self) -> NonNull<T> {
        self.begin
    }

    pub const fn into_raw_parts(self) -> (NonNull<T>, usize) {
        let Self { begin, len } = self;
        (begin, len as usize)
    }

    pub const fn as_bytes(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.begin.as_ptr(), self.len as usize) }
    }

    pub const fn as_bytes_mut(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.begin.as_ptr(), self.len as usize) }
    }

    pub const fn len(&self) -> usize {
        self.len as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> Deref for RawSlice<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_bytes()
    }
}

impl<T> DerefMut for RawSlice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_bytes_mut()
    }
}

pub type RawBytes = RawSlice<u8>;
