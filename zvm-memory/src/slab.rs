use core::{alloc::Layout, cell::Cell, marker::PhantomData, ptr::NonNull};

use alloc::alloc::alloc_zeroed;

use crate::ptr::{Mem, MemSize, Memory, RcMeta, RefCount};

#[derive(Debug, Clone)]
#[repr(C)]
pub struct SlabMeta {
    rc: RefCount,
    size_bytes: MemSize,
    next: usize,
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct SlimMeta {
    rc: RefCount,
    next: usize,
}

#[derive(Debug)]
#[repr(C)]
pub struct SizedSlab<const SIZE: usize> {
    mem: NonNull<u8>,
    len: usize,
    // We use a slimmed meta struct for this, since SIZE is static
    _phantom: PhantomData<[Memory<[u8; SIZE], SlimMeta>]>,
}

impl<const S: usize> SizedSlab<S> {
    pub fn new(len: usize) -> Self {
        todo!()
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct Slab {
    mem: NonNull<u8>,
    len: usize,
    _phantom: PhantomData<[Mem<[u8], SlabMeta>]>,
}
