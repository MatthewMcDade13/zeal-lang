extern crate alloc;

pub mod block;
pub mod buff;
pub mod heap;
pub mod mem;
pub mod ptr;
pub mod slab;
pub mod util_lite;
pub mod zalloc;

pub unsafe trait ZealMemoryLayout {}

pub trait Byteable {
    fn as_bytes(&self) -> &[u8];
    fn as_bytes_mut(&mut self) -> &mut [u8];

    fn ref_from_bytes(bytes: &[u8]) -> &Self;
    fn mut_from_bytes(bytes: &mut [u8]) -> &mut Self;
}

impl<T> Byteable for T
where
    T: bytemuck::NoUninit + bytemuck::AnyBitPattern,
{
    fn as_bytes(&self) -> &[u8] {
        bytemuck::bytes_of(self)
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        bytemuck::bytes_of_mut(self)
    }

    fn ref_from_bytes(bytes: &[u8]) -> &Self {
        bytemuck::from_bytes(bytes)
    }

    fn mut_from_bytes(bytes: &mut [u8]) -> &mut Self {
        bytemuck::from_bytes_mut(bytes)
    }
}

pub type Nothing = ();
pub type VoidT = libc::c_void;

// pub const DYN_GLOBAL l
pub mod ty {
    use bytemuck::{ByteEq, ByteHash};

    use crate::ptr::{Any, Thin, Zptr};

    use super::*;
    use core::{marker::PhantomData, ptr::NonNull};

    #[derive(
        Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, bytemuck::Zeroable, bytemuck::Contiguous,
    )]
    #[repr(u8)]
    pub enum AllocTag {
        Slab = 0,
        Heap,
        System,
        Count,
    }

    pub unsafe trait Zallocator: Sized {
        type Meta;

        fn free(&self, ptr: Any);
        fn zalloc<T>(&self) -> Zptr<T, Self::Meta, Self>
        where
            T: Byteable;

        fn zalloc_bytes(&self, size: usize) -> Zptr<u8, Self::Meta, Self>;
    }

    pub struct MemClass<Allocator: Zallocator> {
        meta: Allocator::Meta,
        alloc: Allocator,
    }

    pub type ClassTag<T> = PhantomData<MemClass<T>>;
}
