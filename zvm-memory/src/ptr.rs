use core::{
    marker::{PhantomData, PhantomPinned},
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use anyhow::Context;

/// non-owning slice, pointer + length
// NOTE: May want to change this from a pointer to a handle... hmmm
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Slice<T> {
    begin: NonNull<T>,
    len: u32,
}

impl<T> Slice<T> {
    #[inline]
    pub fn new(begin: NonNull<T>, len: u32) -> anyhow::Result<Self> {
        Self::try_new(begin, len).context("Cannot create RawSlice of len 0!!!")
    }

    pub const fn expect_new(begin: NonNull<T>, len: u32) -> Self {
        if len == 0 {
            panic!("Cannot create RawSlice of len 0!!");
        } else {
            Self { begin, len }
        }
    }

    pub const fn try_new(begin: NonNull<T>, len: u32) -> Option<Self> {
        if len == 0 {
            None
        } else {
            Some(Self { begin, len })
        }
    }

    pub const fn root_ptr(&self) -> NonNull<T> {
        self.begin
    }

    pub const fn into_raw_parts(self) -> (NonNull<T>, usize) {
        let Self { begin, len } = self;
        (begin, len as usize)
    }

    pub const fn as_bytes(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.begin.as_ptr(), self.len as usize) }
    }

    pub const fn as_bytes_mut(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.begin.as_ptr(), self.len as usize) }
    }

    pub const fn len(&self) -> usize {
        self.len as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<T> Deref for Slice<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_bytes()
    }
}

impl<T> DerefMut for Slice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_bytes_mut()
    }
}

pub type RawBytes = Slice<u8>;

#[repr(transparent)]
pub struct Own<T: ?Sized> {
    ptr: NonNull<T>,
    _pd: PhantomData<T>,
}

impl<T> Clone for Own<T> {
    #[inline]
    fn clone(&self) -> Self {
        unsafe { Self::new(self.ptr.read()) }
    }
}

impl<T> Deref for Own<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref() }
    }
}

impl<T> DerefMut for Own<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut() }
    }
}

impl<T> Own<T> {
    pub fn new(val: T) -> Self {
        let ptr = unsafe {
            let p = libc::calloc(1, size_of::<T>()).cast::<T>();
            let p = NonNull::new(p).expect("Out of memory!!");
            p.write(val);
            p
        };
        Self {
            ptr,
            _pd: PhantomData,
        }
    }
}

impl<T> Drop for Own<T>
where
    T: ?Sized,
{
    fn drop(&mut self) {
        unsafe { libc::free(self.ptr.as_ptr().cast::<libc::c_void>()) }
    }
}
