use core::{
    alloc::{GlobalAlloc, Layout},
    ptr::{NonNull, null_mut},
};

use crate::alloc::alloc::Allocator;

use anyhow::Context;
use libmimalloc_sys as mi;

/// Arena Memory Allocator
/// uses mimalloc heap internally
pub struct GenAllocator;

impl GenAllocator {
    #[inline(always)]
    pub fn malloc<T>(size_bytes: usize) -> anyhow::Result<NonNull<T>> {
        let p = Self::malloc_bytes(size_bytes)?;
        Ok(p.cast::<T>())
    }

    #[inline]
    pub fn malloc_aligned<T>(size_bytes: usize, align: usize) -> anyhow::Result<NonNull<T>> {
        let p = Self::malloc_aligned_bytes(size_bytes, align)?;
        Ok(p.cast::<T>())
    }

    #[inline]
    pub fn zalloc<T>(size_bytes: usize) -> anyhow::Result<NonNull<T>> {
        let p = Self::malloc_bytes(size_bytes)?;
        Ok(p.cast::<T>())
    }
    #[inline]
    pub fn zalloc_aligned<T>(size_bytes: usize, align: usize) -> anyhow::Result<NonNull<T>> {
        let p = Self::malloc_aligned_bytes(size_bytes, align)?;
        Ok(p.cast::<T>())
    }
    #[inline]
    pub fn calloc<T>(size_bytes: usize) -> anyhow::Result<NonNull<T>> {
        let p = Self::malloc_bytes(size_bytes)?;
        Ok(p.cast::<T>())
    }
    #[inline]
    pub fn calloc_aligned<T>(size_bytes: usize, align: usize) -> anyhow::Result<NonNull<T>> {
        let p = Self::malloc_aligned_bytes(size_bytes, align)?;
        Ok(p.cast::<T>())
    }
    #[inline]
    pub fn malloc_bytes(size_bytes: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = if size_bytes >= mi::MI_SMALL_SIZE_MAX {
            unsafe { mi::mi_malloc(size_bytes) }
        } else {
            unsafe { mi::mi_malloc_small(size_bytes) }
        };
        let p = NonNull::new(p).context("Failed to allocate memory from mi_malloc!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn malloc_aligned_bytes(size_bytes: usize, align: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_malloc_aligned(size_bytes, align) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn zalloc_bytes(size_bytes: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_zalloc(size_bytes) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn zalloc_aligned_bytes(size_bytes: usize, align: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_zalloc_aligned(size_bytes, align) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn calloc_bytes(count: usize, size_bytes: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_calloc(count, size_bytes) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }
    #[inline]
    pub fn calloc_aligned_bytes(
        count: usize,
        size_bytes: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_calloc_aligned(count, size_bytes, align) };
        let p = NonNull::new(p).context("Failed to allocate memory from heap!")?;
        let res = NonNull::slice_from_raw_parts(p.cast::<u8>(), size_bytes);
        Ok(res)
    }

    pub fn realloc(ptr: NonNull<u8>, newsize: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_realloc(ptr.cast::<core::ffi::c_void>().as_ptr(), newsize) };
        let p = NonNull::new(p).context("Failed to realloc memory!")?;
        Ok(NonNull::slice_from_raw_parts(p.cast::<u8>(), newsize))
    }

    pub fn rezalloc(ptr: NonNull<u8>, newsize: usize) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe { mi::mi_rezalloc(ptr.cast::<core::ffi::c_void>().as_ptr(), newsize) };
        let p = NonNull::new(p).context("Failed to realloc memory!")?;
        Ok(NonNull::slice_from_raw_parts(p.cast::<u8>(), newsize))
    }

    pub fn recalloc(
        ptr: NonNull<u8>,
        newcount: usize,
        size: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p =
            unsafe { mi::mi_recalloc(ptr.cast::<core::ffi::c_void>().as_ptr(), newcount, size) };
        let p = NonNull::new(p).context("Failed to realloc memory!")?;
        Ok(NonNull::slice_from_raw_parts(
            p.cast::<u8>(),
            newcount * size,
        ))
    }

    pub fn realloc_aligned(
        ptr: NonNull<u8>,
        newsize: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe {
            mi::mi_realloc_aligned(ptr.cast::<core::ffi::c_void>().as_ptr(), newsize, align)
        };
        let p = NonNull::new(p).context("Failed to realloc memory!")?;
        Ok(NonNull::slice_from_raw_parts(p.cast::<u8>(), newsize))
    }

    pub fn rezalloc_aligned(
        ptr: NonNull<u8>,
        newsize: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe {
            mi::mi_rezalloc_aligned(ptr.cast::<core::ffi::c_void>().as_ptr(), newsize, align)
        };
        let p = NonNull::new(p).context("Failed to realloc memory!")?;
        Ok(NonNull::slice_from_raw_parts(p.cast::<u8>(), newsize))
    }

    pub fn recalloc_aligned(
        ptr: NonNull<u8>,
        newcount: usize,
        size: usize,
        align: usize,
    ) -> anyhow::Result<NonNull<[u8]>> {
        let p = unsafe {
            mi::mi_recalloc_aligned(
                ptr.cast::<core::ffi::c_void>().as_ptr(),
                newcount,
                size,
                align,
            )
        };
        let p = NonNull::new(p).context("Failed to realloc memory!")?;
        Ok(NonNull::slice_from_raw_parts(
            p.cast::<u8>(),
            newcount * size,
        ))
    }

    #[inline(always)]
    pub fn print_stats(&self) {
        unsafe { mi::mi_stats_print(null_mut()) };
    }

    #[inline(always)]
    pub fn free_bytes(ptr: NonNull<u8>) {
        unsafe { mi::mi_free(ptr.cast::<core::ffi::c_void>().as_ptr()) }
    }

    #[inline(always)]
    pub fn free<T>(ptr: NonNull<T>) {
        Self::free_bytes(ptr.cast::<u8>())
    }
}

unsafe impl Allocator for GenAllocator {
    fn allocate(
        &self,
        layout: Layout,
    ) -> Result<core::ptr::NonNull<[u8]>, ::alloc::alloc::AllocError> {
        Self::malloc_aligned_bytes(layout.size(), layout.align())
            .map_err(|_| ::alloc::alloc::AllocError)
    }

    unsafe fn deallocate(&self, ptr: core::ptr::NonNull<u8>, _: Layout) {
        Self::free_bytes(ptr);
    }

    fn allocate_zeroed(&self, layout: Layout) -> Result<NonNull<[u8]>, alloc::alloc::AllocError> {
        Self::zalloc_aligned_bytes(layout.size(), layout.align())
            .map_err(|_| ::alloc::alloc::AllocError)
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

        Self::realloc_aligned(ptr, new_layout.size(), new_layout.align())
            .map_err(|_| ::alloc::alloc::AllocError)
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

        Self::realloc_aligned(ptr, new_layout.size(), new_layout.align())
            .map_err(|_| ::alloc::alloc::AllocError)
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
        Self::realloc_aligned(ptr, new_layout.size(), new_layout.align())
            .map_err(|_| ::alloc::alloc::AllocError)
    }
}

unsafe impl GlobalAlloc for GenAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        if let Ok(p) = Self::malloc_aligned_bytes(layout.size(), layout.align()) {
            p.cast::<u8>().as_ptr()
        } else {
            null_mut()
        }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, _: Layout) {
        if let Some(p) = NonNull::new(ptr) {
            Self::free(p);
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use alloc::{rc::Rc, string::String, vec::Vec};

    #[test]
    fn basic() -> anyhow::Result<()> {
        let mut xs = Vec::new_in(GenAllocator);
        xs.resize(64, 0);
        for (i, x) in xs.iter_mut().enumerate() {
            *x = i * i;
        }
        Ok(())
    }
}
