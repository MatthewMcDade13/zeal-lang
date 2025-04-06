use core::{alloc::Layout, cell::Cell, marker::PhantomData, ptr::NonNull};

use alloc::alloc::alloc_zeroed;

use crate::Byteable;

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct SlabMeta {
    rc: u32,
    next: u32,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct SlabMem<T: Byteable + ?Sized> {
    meta: SlabMeta,
    cell: T,
}

pub type SlabBytes<const SIZE: usize> = SlabMem<[u8; SIZE]>;
pub type AnyCell = SlabMem<[u8]>;

pub struct HeapMeta {
    meta: SlabMeta,
    size_bytes: u32,
}

pub struct HeapCell {
    inner: NonNull<u8>,
    _phantom: PhantomData<SlabMem<[u8]>>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub enum SlabCell {
    L8(SlabBytes<8>),
    L16(SlabBytes<16>),
    L24(SlabBytes<24>),
    L32(SlabBytes<32>),
    L64(SlabBytes<64>),
    L128(SlabBytes<128>),
    L256(SlabBytes<256>),
    L512(SlabBytes<512>),
    L1024(SlabBytes<1024>),
    L2048(SlabBytes<2048>),
}

#[derive(Debug)]
#[repr(C)]
pub struct SizedSlab<const SIZE: usize> {
    mem: NonNull<u8>,
    len: usize,
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
    // _phantom: PhantomData<[Mem<[u8], SlabMeta>]>,
}
