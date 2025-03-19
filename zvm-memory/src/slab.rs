use core::{cell::Cell, ptr::NonNull};

use crate::block::MemBlock;

pub struct SlabLayer<const SIZE: usize> {
    len: usize,
    mem: NonNull<ByteBlock<SIZE>>,
}

#[repr(C)]
struct SlabMemory {}

#[derive(Debug)]
#[repr(C)]
pub struct ByteBlock<const SIZE: usize> {
    next: Cell<u32>,
    rc: Cell<u32>,
    bytes: MemBlock<SIZE>,
}

impl<const S: usize> ByteBlock<S> {
    #[inline]
    pub(crate) fn next(&self) -> usize {
        self.next.get() as usize
    }

    #[inline]
    pub(crate) fn ref_count(&self) -> usize {
        self.rc.get() as usize
    }

    #[inline]
    pub fn is_alive(&self) -> bool {
        self.ref_count() > 0
    }
}
