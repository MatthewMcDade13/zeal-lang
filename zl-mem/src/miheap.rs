use core::{
    alloc::Layout,
    ptr::{NonNull, null_mut},
};

use crate::alloc::alloc::Allocator;

use anyhow::Context;
use libmimalloc_sys::{self as mi, mi_heap_realloc_aligned};

// TODO: Implement allocator flags
bitflags::bitflags! {

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
    pub struct HeapOpts : u8 {
        const None = 0;
        /// Allow indvidual free (mi_free) on blocks allocated by heap
        const IndividualFree = 0b00000001;
        /// Don't call m_heap_destroy and call mi_heap_delete on Arena dropping
        const NoDestroy = 0b00000010;

        /// Im going to add more flags, but for now we allow all variants
        const _ = !0;
    }
}

/// Arena Memory Allocator
/// uses mimalloc heap internally
#[derive(Debug)]
pub struct Heap {
    ptr: NonNull<mi::mi_heap_t>,
    flags: HeapOpts,
}

impl Heap {
    #[inline(always)]
    pub fn new() -> Option<Self> {
        let ptr = unsafe { mi::mi_heap_new() };
        NonNull::new(ptr).map(|ptr| Self {
            ptr,
            flags: HeapOpts::None,
        })
    }

    /// Same as new, but panics if mi_heap_new returns a null pointer
    #[inline(always)]
    pub fn new_expect() -> Self {
        Self::new().expect("Could not create a new mimalloc heap!")
    }

    /// Calls mi_heap_destroy
    /// force frees all memory owned by this heap block.
    /// use @see [Self::delete] if you want to have mimalloc move still active memory
    /// to a new heap
    #[inline(always)]
    pub fn destroy(&self) {
        unsafe {
            mi::mi_heap_destroy(self.ptr.as_ptr());
        }
    }

    #[inline(always)]
    pub fn delete(&self) {
        unsafe {
            mi::mi_heap_delete(self.ptr.as_ptr());
        }
    }

    #[inline]
    pub fn malloc<T>(&self) -> anyhow::Result<NonNull<T>> {
        let size = core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        let p = self.malloc_aligned_bytes(size, align)?;
        Ok(p.cast::<T>())
    }

    #[inline]
    pub fn zalloc<T>(&self) -> anyhow::Result<NonNull<T>> {
        let size = core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        let p = self.zalloc_aligned_bytes(size, align)?;
        Ok(p.cast::<T>())
    }

    #[inline]
    pub fn calloc<T>(&self, count: usize) -> anyhow::Result<NonNull<T>> {
        let size = core::mem::size_of::<T>();
        let align = core::mem::align_of::<T>();
        let p = self.calloc_aligned_bytes(count, size, align)?;
        Ok(p.cast::<T>())
    }

    #[inline]
    pub fn malloc_bytes(&self, size_bytes: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = if size_bytes >= mi::MI_SMALL_SIZE_MAX {
            unsafe { mi::mi_heap_malloc(self.ptr(), size_bytes) }
        } else {
            unsafe { mi::mi_heap_malloc_small(self.ptr(), size_bytes) }
        };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn malloc_aligned_bytes(
        &self,
        size_bytes: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_heap_malloc_aligned(self.ptr(), size_bytes, align) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn zalloc_bytes(&self, size_bytes: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_heap_zalloc(self.ptr(), size_bytes) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn zalloc_aligned_bytes(
        &self,
        size_bytes: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_heap_zalloc_aligned(self.ptr(), size_bytes, align) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn calloc_bytes(&self, count: usize, size_bytes: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_heap_calloc(self.ptr(), count, size_bytes) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn calloc_aligned_bytes(
        &self,
        count: usize,
        size_bytes: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_heap_calloc_aligned(self.ptr(), count, size_bytes, align) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }

    pub fn realloc_aligned_bytes(
        &self,
        ptr: NonNull<u8>,
        newsize: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe {
            mi::mi_heap_realloc_aligned(
                self.ptr(),
                ptr.cast::<core::ffi::c_void>().as_ptr(),
                newsize,
                align,
            )
        };
        let p = NonNull::new(p).context("Failed to reallocate memory!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), newsize);
        Ok(res)
    }

    pub fn realloc_bytes(&self, ptr: NonNull<u8>, newsize: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe {
            mi::mi_heap_realloc(
                self.ptr(),
                ptr.cast::<core::ffi::c_void>().as_ptr(),
                newsize,
            )
        };
        let p = NonNull::new(p).context("Failed to reallocate memory!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), newsize);
        Ok(res)
    }

    pub fn recalloc<T>(&self, ptr: NonNull<T>, newcount: usize) -> anyhow::Result<NonNull<[T]>> {
        self.recalloc_aligned_bytes(
            ptr.cast::<u8>(),
            newcount,
            core::mem::size_of::<T>(),
            core::mem::align_of::<T>(),
        )
        .map(|p| NonNull::slice_from_raw_parts(p.cast::<T>(), newcount * core::mem::size_of::<T>()))
    }

    pub fn recalloc_aligned_bytes(
        &self,
        ptr: NonNull<u8>,
        newcount: usize,
        size: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = ptr.cast::<core::ffi::c_void>();
        let p =
            unsafe { mi::mi_heap_recalloc_aligned(self.ptr(), p.as_ptr(), newcount, size, align) };
        let p = NonNull::new(p).context("Failed to reallocate memory!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), newcount * size);
        Ok(res)
    }

    #[inline(always)]
    pub fn print_stats(&self) {
        unsafe { mi::mi_stats_print(null_mut()) };
    }

    pub const fn ptr(&self) -> *mut mi::mi_heap_t {
        self.ptr.as_ptr()
    }
}

impl Drop for Heap {
    fn drop(&mut self) {
        self.destroy();
    }
}

impl Default for Heap {
    fn default() -> Self {
        Self::new_expect()
    }
}

unsafe impl Allocator for Heap {
    fn allocate(
        &self,
        layout: Layout,
    ) -> Result<core::ptr::NonNull<[u8]>, ::alloc::alloc::AllocError> {
        let ptr = if layout.size() >= mi::MI_SMALL_SIZE_MAX {
            unsafe { mi::mi_heap_malloc_aligned(self.ptr(), layout.size(), layout.align()) }
        } else {
            unsafe { mi::mi_heap_malloc_small(self.ptr(), layout.size()) }
        };
        if let Some(ptr) = NonNull::new(ptr.cast::<u8>()) {
            Ok(core::ptr::NonNull::slice_from_raw_parts(ptr, layout.size()))
        } else {
            Err(::alloc::alloc::AllocError)
        }
    }

    unsafe fn deallocate(&self, _: core::ptr::NonNull<u8>, _: Layout) {}

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, alloc::alloc::AllocError> {
        let ptr = if layout.size() >= mi::MI_SMALL_SIZE_MAX {
            unsafe { mi::mi_heap_zalloc_aligned(self.ptr(), layout.size(), layout.align()) }
        } else {
            unsafe {
                let p = mi::mi_heap_malloc_small(self.ptr(), layout.size());
                p.write_bytes(0, layout.size());
                p
            }
        };
        if let Some(ptr) = NonNull::new(ptr.cast::<u8>()) {
            Ok(core::ptr::NonNull::slice_from_raw_parts(ptr, layout.size()))
        } else {
            Err(::alloc::alloc::AllocError)
        }
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, alloc::alloc::AllocError> {
        core::debug_assert!(
            new_layout.size() >= old_layout.size(),
            "`new_layout.size()` must be greater than or equal to `old_layout.size()`"
        );

        let new_ptr = unsafe {
            mi::mi_heap_realloc_aligned(
                self.ptr(),
                ptr.cast::<core::ffi::c_void>().as_ptr(),
                new_layout.size(),
                new_layout.align(),
            )
        };
        if let Some(new_ptr) = NonNull::new(new_ptr) {
            Ok(NonNull::slice_from_raw_parts(
                new_ptr.cast::<u8>(),
                new_layout.size(),
            ))
        } else {
            unsafe { mi::mi_free(ptr.cast::<core::ffi::c_void>().as_ptr()) }
            Err(::alloc::alloc::AllocError)
        }
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, alloc::alloc::AllocError> {
        core::debug_assert!(
            new_layout.size() >= old_layout.size(),
            "`new_layout.size()` must be greater than or equal to `old_layout.size()`"
        );

        let new_ptr = unsafe {
            mi::mi_heap_rezalloc_aligned(
                self.ptr(),
                ptr.cast::<core::ffi::c_void>().as_ptr(),
                new_layout.size(),
                new_layout.align(),
            )
        };
        if let Some(new_ptr) = NonNull::new(new_ptr) {
            Ok(NonNull::slice_from_raw_parts(
                new_ptr.cast::<u8>(),
                new_layout.size(),
            ))
        } else {
            unsafe { mi::mi_free(ptr.cast::<core::ffi::c_void>().as_ptr()) }
            Err(::alloc::alloc::AllocError)
        }
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, alloc::alloc::AllocError> {
        core::debug_assert!(
            new_layout.size() <= old_layout.size(),
            "`new_layout.size()` must be smaller than or equal to `old_layout.size()`"
        );
        let new_ptr = unsafe {
            mi::mi_heap_realloc_aligned(
                self.ptr(),
                ptr.cast::<core::ffi::c_void>().as_ptr(),
                new_layout.size(),
                new_layout.align(),
            )
        };
        if let Some(new_ptr) = NonNull::new(new_ptr) {
            Ok(NonNull::slice_from_raw_parts(
                new_ptr.cast::<u8>(),
                new_layout.size(),
            ))
        } else {
            unsafe { mi::mi_free(ptr.cast::<core::ffi::c_void>().as_ptr()) }
            Err(::alloc::alloc::AllocError)
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use alloc::{rc::Rc, string::String, vec::Vec};

    #[test]
    fn basic() -> anyhow::Result<()> {
        let alloc = Heap::new_expect();
        let mut xs = Vec::new_in(alloc);
        xs.resize(64, 0);
        for (i, x) in xs.iter_mut().enumerate() {
            *x = i * i;
        }
        Ok(())
    }
}

/// Heap is the same thing as an arena, as far as i know lol
pub type Arena = Heap;
