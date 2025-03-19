use core::ops::{Deref, DerefMut};

use zerocopy_derive::{FromBytes, Immutable, IntoBytes, KnownLayout};

use crate::util_lite::copy_slice_into;

pub trait MemCell<T = Self>
where
    Self: Sized,
{
    fn cast_bytes(&self) -> &[u8];
    fn cast_bytes_mut(&mut self) -> &mut [u8];

    fn size_bytes() -> usize {
        core::mem::size_of::<T>()
    }

    fn write_bytes(&mut self, bytes: &[u8]) {
        let dst = self.cast_bytes_mut();
        copy_slice_into(dst, bytes);
    }

    fn read_bytes(bytes: &[u8]) -> &Self;
}

impl<T> MemCell<T> for T
where
    T: zerocopy::FromBytes
        + zerocopy::IntoBytes
        + zerocopy::Immutable
        + zerocopy::KnownLayout
        + Copy,
{
    // fn from_bytes(bytes: &[u8]) -> T {
    // *Self::read_bytes(bytes)
    // }

    fn read_bytes(bytes: &[u8]) -> &T {
        T::ref_from_bytes(bytes).expect("Failed to cast bytes to type!")
    }

    fn cast_bytes(&self) -> &[u8] {
        T::as_bytes(self)
    }

    fn cast_bytes_mut(&mut self) -> &mut [u8] {
        T::as_mut_bytes(self)
    }
}

#[derive(Debug, Copy, Clone, FromBytes, IntoBytes, Immutable, KnownLayout)]
#[repr(transparent)]
pub struct MemBlock<const SIZE: usize>([u8; SIZE]);

impl<const S: usize> Deref for MemBlock<S> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<const S: usize> DerefMut for MemBlock<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<const S: usize> MemBlock<S> {
    pub const fn zeroed() -> Self {
        Self([0u8; S])
    }

    pub fn from_bytes(bytes: &[u8]) -> Self {
        let mut s = Self::zeroed();
        copy_slice_into(&mut s.0, bytes);
        s
    }
}

impl<const S: usize> Default for MemBlock<S> {
    fn default() -> Self {
        Self::zeroed()
    }
}
