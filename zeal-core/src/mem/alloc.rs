use std::{
    alloc::Layout,
    cell::{Cell, UnsafeCell},
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
};

use super::marker::ZealMemMut;

pub mod ptr {

    pub const NIL_BYTES: [u8; 4] = [0xFF; 4];
    pub const NIL_VALUE: usize = u32::from_le_bytes(NIL_BYTES) as usize;
    pub const NIL: *const u8 = &NIL_BYTES[0] as *const _;
}

#[derive(Debug)]
#[repr(C)]
pub struct RcCell<T: ZealMemMut> {
    strong_count: Cell<usize>,
    weak_count: Cell<usize>,
    val: UnsafeCell<T>,
}

#[repr(C)]
pub struct SharedPtr<T: ZealMemMut, A: Zallocator> {
    inner: Ptr<RcCell<T>>,
    _phantom: PhantomData<A>,
}

#[repr(C)]
pub struct OwnedPtr<T: ZealMemMut, A: Zallocator> {
    inner: Ptr<T>,
    _phantom: PhantomData<A>,
}

impl<T, A> Drop for OwnedPtr<T, A>
where
    T: ZealMemMut,
    A: Zallocator,
{
    fn drop(&mut self) {}
}

/// Allocator Trait for Static "global" allocators
/// For an allocator with a finite lifetime, see [ScopedZalloc]
pub trait Zallocator: 'static + Sized {
    type PtrType;
    fn alloc<T>(&self, val: T) -> Self::PtrType;
    fn free<T>(&self, ptr: Self::PtrType);
    fn instance() -> &'static Self;
}

/// A Scoped Lifetime Allocator. Use for any allocator doesnt
/// have a 'static lifetime. .
/// For static lifetime allocators, see [Zallocator]
pub trait ScopedZalloc: Sized {
    type PtrType;
    fn alloc<T>(&self, val: T) -> Self::PtrType;
    fn free<T>(&self, ptr: Self::PtrType);
}

/// Indexs into memory that behave like addresses
/// This differs from raw pointers in that addresses hold unsigned indexes into
/// some contiguous buffer somewhere in memory
/// This is an address to an untyped / runtime dynamic type
/// TODO: Implement this...
pub struct AnyAddr {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, bytemuck::Zeroable)]
pub struct SizedPtr<T> {
    begin: Ptr<T>,
    size_bytes: usize,
}

impl<T> SizedPtr<T> where T: ZealMemMut {}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct Ptr<T>(*mut T);

impl<T> Ptr<T>
where
    T: ZealMemMut,
{
    pub const fn layout() -> Layout {
        Layout::new::<T>()
    }

    pub const fn nil() -> Self {
        Self(ptr::NIL as *mut _)
    }
    pub const fn from_raw(ptr: *mut T) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(Self(ptr))
        }
    }

    pub const fn new(ptr: NonNull<u8>) -> Self {
        Self(ptr.as_ptr() as *mut _)
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        self.0 as *const _ == ptr::NIL
    }

    pub const fn as_ptr(&self) -> *const T {
        self.0 as *const _
    }

    pub const fn as_mut_ptr(&mut self) -> *mut T {
        self.0 as *mut _
    }

    pub const fn as_ref(&self) -> &T {
        unsafe { self.0.as_ref().expect("Null pointer dereference!") }
    }

    pub const fn as_mut(&mut self) -> &mut T {
        unsafe { self.0.as_mut().expect("Null pointer dereference!") }
    }

    pub const fn read(&self) -> T {
        unsafe { std::ptr::read(self.0) }
    }

    pub const fn write(&mut self, val: T)
    where
        T: Copy,
    {
        unsafe { std::ptr::write(self.0, val) }
    }

    pub fn emplace(&mut self, val: T)
    where
        T: Drop,
    {
        unsafe { *self.0 = val }
    }
}
impl<T> Default for Ptr<T>
where
    T: bytemuck::Pod,
{
    fn default() -> Self {
        Self::nil()
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
pub struct AnyPtr(*mut u8);

impl Default for AnyPtr {
    fn default() -> Self {
        Self::nil()
    }
}

impl AnyPtr {
    pub const fn from_raw(ptr: *mut u8) -> Option<Self> {
        if ptr.is_null() {
            None
        } else {
            Some(Self(ptr))
        }
    }

    pub const fn new(ptr: NonNull<u8>) -> Self {
        Self(ptr.as_ptr())
    }

    pub const fn nil() -> Self {
        Self(ptr::NIL as *mut _)
    }

    #[inline]
    pub fn is_nil(&self) -> bool {
        self.0 as *const _ == ptr::NIL
    }

    pub const fn ptr(&self) -> *const u8 {
        self.0 as *const _
    }

    pub const fn ptr_mut(&mut self) -> *mut u8 {
        self.0
    }
}

pub mod global {
    use std::{alloc::Layout, ptr::NonNull};

    #[inline]
    pub unsafe fn zalloc_buffer_zeroed(size_bytes: usize) -> NonNull<u8> {
        zalloc_array_zeroed::<u8>(size_bytes)
    }

    pub unsafe fn zalloc_buffer<const SIZE: usize>(src: [u8; SIZE]) -> NonNull<u8> {
        let buf = zalloc_buffer_zeroed(src.len());
        let psrc = NonNull::new(src.as_ref().as_ptr() as *mut _)
            .expect("Given source buffer is null or invalid!!");
        std::ptr::copy(psrc.as_ptr(), buf.as_ptr(), src.len());
        buf
    }

    pub unsafe fn zalloc_zeroed<T>() -> NonNull<T>
    where
        T: bytemuck::Zeroable,
    {
        let layout = Layout::new::<T>();
        let v = std::alloc::alloc_zeroed(layout) as *mut _;
        NonNull::new(v).expect("Global Allocator out of memory!!!")
    }

    pub unsafe fn zalloc<T>(val: T) -> NonNull<T> {
        let layout = Layout::new::<T>();
        let v = std::alloc::alloc_zeroed(layout) as *mut _;
        let vptr = NonNull::new(v).expect("Global Allocator out of memory!!!");
        NonNull::write(vptr, val);
        vptr
    }

    pub unsafe fn zalloc_array_zeroed<T>(array_len: usize) -> NonNull<T>
    where
        T: bytemuck::Zeroable,
    {
        let layout = Layout::array::<T>(array_len).expect("Array length size too large!!!");
        let arr = std::alloc::alloc_zeroed(layout);
        todo!()
    }
}
