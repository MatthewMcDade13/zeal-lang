use core::{
    alloc::{GlobalAlloc, Layout},
    cell::Cell,
    mem::MaybeUninit,
    ptr::{NonNull, null_mut},
};

use crate::alloc::alloc::Allocator;

use libmimalloc_sys as mi;

bitflags::bitflags! {

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Eq, Ord, Hash)]
    pub struct ArenaOpts : u8 {
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
pub struct Arena {
    ptr: NonNull<mi::mi_heap_t>,
    flags: ArenaOpts,
}

impl Arena {
    #[inline(always)]
    pub fn new() -> Option<Self> {
        let ptr = unsafe { mi::mi_heap_new() };
        NonNull::new(ptr).map(|ptr| Self {
            ptr,
            flags: ArenaOpts::None,
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

    #[inline(always)]
    pub fn print_stats(&self) {
        unsafe { mi::mi_stats_print(null_mut()) };
    }

    pub const fn ptr(&self) -> *mut mi::mi_heap_t {
        self.ptr.as_ptr()
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        self.destroy();
    }
}

impl Default for Arena {
    fn default() -> Self {
        Self::new_expect()
    }
}

unsafe impl Allocator for Arena {
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
        let alloc = Arena::new_expect();
        let mut xs = Vec::new_in(alloc);
        xs.resize(64, 0);
        for (i, x) in xs.iter_mut().enumerate() {
            *x = i * i;
        }
        Ok(())
    }
}
