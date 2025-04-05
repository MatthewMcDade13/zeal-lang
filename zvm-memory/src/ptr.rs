use core::{
    alloc::GlobalAlloc,
    cell::Cell,
    marker::PhantomData,
    mem::transmute,
    ops::Deref,
    ptr::NonNull,
    sync::atomic::{AtomicBool, AtomicU32, AtomicUsize, Ordering},
};

use anyhow::Context;

use crate::{Byteable, util_lite};

#[repr(C)]
pub struct Memory<T: Byteable + ?Sized, Meta> {
    meta: Meta,
    val: T,
}

impl Byteable for [u8] {
    fn as_bytes(&self) -> &[u8] {
        self
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        self
    }

    fn ref_from_bytes(bytes: &[u8]) -> &Self {
        bytes
    }

    fn mut_from_bytes(bytes: &mut [u8]) -> &mut Self {
        bytes
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Mem<T: Byteable + ?Sized, Meta: MemMeta = MemSize> {
    ptr: NonNull<u8>,
    _phantom: PhantomData<Memory<T, Meta>>,
}

// pub struct MemBytes {
//     ptr: NonNull<u8>,
//     _phantom: PhantomData<Memory<[u8], MemSize>>,
// }
pub type MemBytes = Mem<[u8], MemSize>;

impl MemBytes {
    // pub fn alloc<Alloc>(len: usize) -> Self {
    //     unsafe {
    //         let ptr = libc::calloc(size_of::<u8>(), len + size_of::<MemSize>()) as *mut u8;
    //         let ptr = NonNull::new(ptr).expect("Failed to allocate!");
    //         let mptr = ptr.cast::<MemSize>();

    //         let meta = MemSize::new(len);
    //         NonNull::write(mptr, meta);
    //         Self {
    //             ptr,
    //             _phantom: PhantomData,
    //         }
    //     }
    // }
}

impl<T, M> Deref for Mem<T, M>
where
    T: Byteable,
    M: MemMeta,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

// impl<T, M> Deref for Mem<T, M>
// where
//     T: Byteable + ?Sized,
// {
//     type Target = ;

//     fn deref(&self) -> &Self::Target {
//         &self.ptr
//     }
// }

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct MemSize(usize);

impl MemSize {
    pub const fn new(size: usize) -> Self {
        Self(size)
    }

    pub const fn get(self) -> usize {
        self.0
    }
}

impl<T, M> Mem<T, M>
where
    T: Byteable + ?Sized,
    M: MemMeta,
{
    pub(crate) const fn from_ptr(ptr: NonNull<u8>) -> Self {
        Self {
            ptr,
            _phantom: PhantomData,
        }
    }
    pub const fn read_meta(&self) -> M {
        unsafe {
            let ptr = self.ptr.cast::<M>();
            NonNull::read(ptr)
        }
    }

    pub const fn meta_ref(&self) -> &M {
        unsafe { self.ptr.cast::<M>().as_ref() }
    }
}

#[repr(C)]
pub struct RcMeta {
    rc: RefCount,
    size_bytes: MemSize,
}

pub type RcMem<T> = Mem<T, RcMeta>;

pub trait MemMeta {
    fn size_bytes(&self) -> usize;
}

impl<T, M> Mem<T, M>
where
    T: Byteable,
    M: MemMeta,
{
    pub fn write(&self, val: T) {
        unsafe {
            let ptr = self.ptr.as_ptr().cast::<T>();
            if core::mem::needs_drop::<T>() {
                *ptr = val;
            } else {
                core::ptr::write(ptr, val);
            }
        }
    }

    pub fn mem_begin_aligned(&self) -> NonNull<u8> {
        unsafe {
            let ptr = self.ptr.cast::<M>();
            let ptr = ptr.add(1).cast::<u8>();
            let offset = ptr.align_offset(core::mem::align_of::<T>());
            ptr.add(offset)
        }
    }

    #[inline]
    pub fn inner_bytes(&self) -> &[u8] {
        unsafe {
            let meta = self.read_meta();
            let begin = self.mem_begin_aligned();

            core::slice::from_raw_parts(begin.as_ptr(), meta.size_bytes())
        }
    }

    #[inline]
    pub fn inner_bytes_mut(&self) -> &mut [u8] {
        unsafe {
            let meta = self.read_meta();
            let begin = self.mem_begin_aligned();

            core::slice::from_raw_parts_mut(begin.as_ptr(), meta.size_bytes())
        }
    }

    #[inline]
    pub fn inner(&self) -> &T {
        let bytes = self.inner_bytes();
        T::ref_from_bytes(bytes)
    }

    #[inline]
    pub fn inner_mut(&self) -> &mut T {
        let bytes = self.inner_bytes_mut();
        T::mut_from_bytes(bytes)
    }

    pub fn inner_size_bytes(&self) -> usize {
        self.read_meta().size_bytes()
    }
}

impl<T> RcMem<T>
where
    T: Byteable,
{
    pub const fn ref_count(&self) -> &RefCount {
        &self.meta_ref().rc
    }

    pub const fn size_bytes(&self) -> usize {
        self.read_meta().size_bytes.get()
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct RefCount {
    rc: AtomicUsize,
    wc: AtomicUsize,
}

impl RefCount {
    /// Increments storng reference count by 1.
    /// returns new reference count as as usize
    pub fn inc_strong(&self) -> usize {
        let count = self.rc.load(Ordering::SeqCst);
        let count = count.wrapping_add(1);
        self.store_strong(count);
        count
    }

    /// Decrements storng reference count by 1.
    /// if count is 0, then nothing is done and 0 is returned
    /// returns new reference count as as usize
    pub fn dec_strong(&self) -> usize {
        let count = self.strong();
        let count = count.wrapping_sub(1);
        self.store_strong(count);
        count
    }

    #[inline]
    pub fn strong(&self) -> usize {
        self.rc.load(Ordering::SeqCst)
    }

    #[inline]
    pub fn weak(&self) -> usize {
        self.wc.load(Ordering::SeqCst)
    }

    #[inline]
    fn store_strong(&self, count: usize) {
        self.rc.store(count, Ordering::SeqCst)
    }
}

impl Clone for RefCount {
    fn clone(&self) -> Self {
        Self {
            rc: AtomicUsize::from(self.rc.load(core::sync::atomic::Ordering::Relaxed)),
            wc: AtomicUsize::from(self.wc.load(core::sync::atomic::Ordering::Relaxed)),
        }
    }
}

impl MemMeta for RcMeta {
    fn size_bytes(&self) -> usize {
        self.size_bytes.get()
    }
}

impl MemMeta for MemSize {
    fn size_bytes(&self) -> usize {
        self.get()
    }
}

pub type Any = NonNull<u8>;
pub type Void = NonNull<libc::c_void>;

pub trait MemoryCell {
    type Meta;
    type Cell;

    fn meta(&self) -> &Self::Meta;
    fn cell(&self) -> &Self::Cell;

    fn write_meta(&self, meta: Self::Meta);
    fn write_cell(&self, cell: Self::Cell);

    fn size_bytes() -> usize {
        Self::meta_size() + Self::cell_size()
    }

    fn meta_size() -> usize {
        core::mem::size_of::<Self::Meta>()
    }

    fn cell_size() -> usize {
        core::mem::size_of::<Self::Cell>()
    }

    // fn meta(&self) -> Self::Meta;

    // fn cell(&self) -> &Self::Cell;
    // fn cell_bytes(&self) -> &[u8];
}

impl<T, M> MemoryCell for Mem<T, M>
where
    T: Byteable,
    M: MemMeta,
{
    type Meta = M;

    type Cell = T;

    fn write_meta(&self, meta: M) {
        let mptr = self.ptr.cast::<M>();
        unsafe { NonNull::write(mptr, meta) };
    }

    fn meta(&self) -> &Self::Meta {
        self.meta_ref()
    }

    fn cell(&self) -> &Self::Cell {
        self.inner()
    }

    fn write_cell(&self, mut cell: Self::Cell) {
        let bytes = T::as_bytes_mut(&mut cell);
        util_lite::copy_slice_into(self.inner_bytes_mut(), bytes);
    }
}

pub fn cast_cell<T>(mem: crate::ptr::Any) -> Mem<T::Cell, T::Meta>
where
    T: MemoryCell,
    T::Cell: Byteable,
    T::Meta: MemMeta,
{
    Mem::from_ptr(mem)
}
