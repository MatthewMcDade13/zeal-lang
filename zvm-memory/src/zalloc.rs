use core::ptr::NonNull;

use crate::{
    Byteable,
    ptr::{Mem, MemMeta, MemSize, MemoryCell, cast_cell},
};

pub fn zalloc_bytes(size: usize) -> crate::ptr::Any {
    unsafe {
        let mem = libc::calloc(size, core::mem::size_of::<u8>());
        let mem = NonNull::new(mem).expect("Failed to allocate byte block!");
        mem.cast::<u8>()
    }
}

pub fn zalloc_array<T>(len: usize) -> crate::ptr::Any
where
    T: Byteable,
{
    unsafe {
        let mem = libc::calloc(len, size_of::<T>());
        let mem = NonNull::new(mem).expect("Failed to allocate new memory!");
        mem.cast::<u8>()
    }
}

#[inline]
pub fn zalloc<T>() -> crate::ptr::Any
where
    T: Byteable,
{
    zalloc_array::<T>(1)
}

pub fn realloc(ptr: crate::ptr::Any, new_size: usize) -> crate::ptr::Any {
    unsafe {
        let mem = libc::realloc(ptr.as_ptr() as _, new_size);
        let mem = NonNull::new(mem as _).expect("Could not reallocate memory!!");

        mem.cast::<u8>()
    }
}

pub fn realloc_bytes(ptr: crate::ptr::Any, new_len: usize, elem_size: usize) -> crate::ptr::Any {
    unsafe {
        let mem = libc::reallocarray(ptr.as_ptr() as _, new_len, elem_size);
        let mem = NonNull::new(mem as _).expect("Could not reallocate! memory");
        mem.cast::<u8>()
    }
}

#[inline]
pub fn realloc_array<T>(ptr: crate::ptr::Any, new_len: usize) -> crate::ptr::Any {
    realloc_bytes(ptr, new_len, size_of::<T>())
}

pub fn zalloc_cell<T>(meta: T::Meta) -> Mem<T::Cell, T::Meta>
where
    T: MemoryCell,
    T::Cell: Byteable,
    T::Meta: MemMeta,
{
    let mem = zalloc_bytes(T::size_bytes());
    cast_cell::<T>(mem)
}
#[macro_export]
macro_rules! free {
    ($ptr:expr) => {
        unsafe {
            if !$ptr.is_null() {
                libc::free($ptr as *mut _);
                $ptr = core::ptr::null_mut();
            }
        }
    };
}

#[inline]
pub fn free(ptr: crate::ptr::Any) {
    unsafe { libc::free(ptr.as_ptr() as _) };
}
