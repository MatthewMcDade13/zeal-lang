use std::{
    alloc::Layout,
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use anyhow::Context;

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct SlabCell<const SIZE: usize>([u8; SIZE]);

impl<const S: usize> SlabCell<S> {
    pub const fn zeroed() -> Self {
        bytemuck::zeroed::<Self>()
    }
}

impl<const S: usize> Default for SlabCell<S> {
    fn default() -> Self {
        Self::zeroed()
    }
}

impl<const S: usize> Deref for SlabCell<S> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl<const S: usize> DerefMut for SlabCell<S> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0.as_mut()
    }
}

#[derive(Debug)]
pub struct Slab<const CELL_SIZE: usize> {
    mem: UnsafeCell<NonNull<SlabCell<CELL_SIZE>>>,
    len: Cell<usize>,
    cap: Cell<usize>,
    _phantom: PhantomData<[SlabCell<CELL_SIZE>]>,
}

impl<const S: usize> Slab<S> {
    pub const EMPTY: SlabCell<S> = SlabCell::zeroed();
    pub const NONE_PTR: NonNull<SlabCell<S>> = Self::none_ptr();

    pub const fn none_ptr() -> NonNull<SlabCell<S>> {
        let p = &Self::EMPTY as *const _;
        unsafe { NonNull::new_unchecked(p as *mut _) }
    }

    pub const fn empty() -> Self {
        Self {
            mem: UnsafeCell::new(Self::NONE_PTR),
            len: Cell::new(0),
            cap: Cell::new(0),
            _phantom: PhantomData,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len.get()
    }

    #[inline]
    pub fn cap(&self) -> usize {
        self.cap.get()
    }

    #[inline]
    pub fn memory(&self) -> NonNull<SlabCell<S>> {
        unsafe { *self.mem.get() }
    }

    /// @param grow_count number of cells to grow by. no-op if grow_count == 0
    pub fn grow(&self, grow_count: usize) {
        if grow_count == 0 {
            return;
        }
        let new_len = grow_count + self.len.get();
        if new_len >= self.cap() {}
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.memory() == Self::NONE_PTR
    }
}

// impl<const S: usize> Clone for Slab<S> {
//     #[inline]
//     fn clone(&self) -> Self {
//         let other = todo!();
//         Self {
//             mem: self.mem.clone(),
//             len: self.len.clone(),
//             cap: self.cap.clone(),
//             _phantom: self._phantom.clone(),
//         }
//     }
// }

impl<const S: usize> Drop for Slab<S> {
    fn drop(&mut self) {
        todo!()
    }
}
