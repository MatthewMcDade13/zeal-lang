use core::{
    alloc::{GlobalAlloc, Layout},
    cell::Cell,
    marker::PhantomData,
    ptr::NonNull,
};

use alloc::alloc::alloc_zeroed;

use crate::{Byteable, mem::WideMem, ty::Zallocator};

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

pub type SlabBytes<const SIZE: usize> = WideMem<[u8; SIZE], SlabMeta>;
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

unsafe impl Zallocator for Slab {
    type Meta = SlabMeta;

    fn free(&self, ptr: crate::ptr::Any) {
        todo!()
    }

    fn zalloc<T>(&self) -> crate::ptr::Zptr<T, Self::Meta, Self>
    where
        T: Byteable,
    {
        todo!()
    }

    fn zalloc_bytes(&self, size: usize) -> crate::ptr::Zptr<u8, Self::Meta, Self> {
        todo!()
    }
}

unsafe impl GlobalAlloc for Slab {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        todo!()
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        todo!()
    }
}
