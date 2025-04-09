use core::{
    alloc::{Layout, LayoutError},
    any::Any,
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use alloc::borrow::ToOwned;
use bytemuck::NoUninit;

use crate::{abi::Byteable, ptr::Ptr};

/// Growable, mutable Rc Buffer.
pub struct SlabBuffer {}

#[derive(Debug, Clone)]
#[repr(C)]
struct RefCount {
    pub rc: Cell<usize>,
    pub wc: Cell<usize>,
}

#[derive(Debug)]
#[repr(C)]
pub struct Mem<T: ?Sized> {
    refc: RefCount,
    mem: T,
}

impl<T> Mem<T>
where
    T: Byteable,
{
    pub const fn layout(size_bytes: usize) -> Result<Layout, LayoutError> {
        let result = Layout::new::<RefCount>();
        let mem = match Layout::array::<u8>(size_bytes) {
            Ok(m) => m,
            Err(e) => return Result::Err(e),
        };
        let result = match result.extend(mem) {
            Ok((r, _)) => r,
            Err(e) => return Result::Err(e),
        };
        Ok(result)
    }

    pub const fn layout_array(size_bytes: usize, len: usize) -> Result<Layout, LayoutError> {
        let l = match Self::layout(size_bytes) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };
        Layout::array::<u8>(l.size() * len)
    }

    #[inline]
    pub fn ref_count(&self) -> usize {
        self.refc.rc.get()
    }

    #[inline]
    pub fn weak_count(&self) -> usize {
        self.refc.wc.get()
    }

    // pub fn write_bytes(&mut self, bytes: &[u8]) {
    //     let n = core::cmp::min(self.mem.len(), bytes.len());
    //     self.mem[..n].copy_from_slice(&bytes[..n])
    // }

    // pub fn write<T>(&mut self, val: T)
    // where
    //     T: Byteable,
    // {
    //     let bytes = val.as_bytes();
    //     self.write_bytes(bytes);
    // }
}

pub struct Buf(Ptr<Mem<[u8]>>);
