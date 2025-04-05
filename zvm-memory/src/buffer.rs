use core::ptr::NonNull;

use crate::util_lite::{self, clone_slice_into};

const NONE: u8 = 0xFF;
const NONE_PTR: NonNull<u8> = NonNull::new(&NONE as *const u8 as *mut _).unwrap();

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Bytes {
    buf: NonNull<u8>,
    len: usize,
}

impl Bytes {
    pub const fn from_parts(buf: NonNull<u8>, len: usize) -> Self {
        Self { buf, len }
    }

    pub const fn slice(&self) -> &[u8] {
        unsafe { core::slice::from_raw_parts(self.buf.as_ptr(), self.len) }
    }

    pub const fn slice_mut(&self) -> &mut [u8] {
        unsafe { core::slice::from_raw_parts_mut(self.buf.as_ptr(), self.len) }
    }

    pub const fn as_ptr(&self) -> NonNull<u8> {
        self.buf
    }

    pub fn write(&self, other: &Self) {
        self.write_bytes(other.slice());
    }

    pub fn write_bytes(&self, other: &[u8]) {
        let dst = self.slice_mut();
        clone_slice_into(dst, other);
    }

    pub const fn len(&self) -> usize {
        self.len
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}
