#![no_std]

extern crate alloc;

pub mod block;
pub mod buffer;
pub mod ptr;
pub mod slab;
pub mod util_lite;
pub mod zalloc;

pub trait Byteable {
    fn as_bytes(&self) -> &[u8];
    fn as_bytes_mut(&mut self) -> &mut [u8];

    fn ref_from_bytes(bytes: &[u8]) -> &Self;
    fn mut_from_bytes(bytes: &mut [u8]) -> &mut Self;
}

impl<T> Byteable for T
where
    T: bytemuck::Pod + bytemuck::Zeroable,
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
