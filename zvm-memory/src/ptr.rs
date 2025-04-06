use core::{
    marker::PhantomData,
    ops::Deref,
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::Byteable;

pub trait Cast<To: ?Sized, From: ?Sized = To> {
    fn cast(&self) -> &To;
    fn cast_from(other: &From) -> &Self;
}

impl<T> Cast<[u8]> for T
where
    T: Byteable,
{
    fn cast(&self) -> &[u8] {
        self.as_bytes()
    }

    fn cast_from(other: &[u8]) -> &Self {
        Self::ref_from_bytes(other)
    }
}

pub trait CastMut<To: ?Sized, From: ?Sized = To> {
    fn cast_mut(&mut self) -> &mut To;
    fn cast_from_mut(other: &mut From) -> &mut Self;
}

impl<T> CastMut<[u8]> for T
where
    T: Byteable,
{
    fn cast_mut(&mut self) -> &mut [u8] {
        self.as_bytes_mut()
    }

    fn cast_from_mut(other: &mut [u8]) -> &mut Self {
        Self::mut_from_bytes(other)
    }
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

#[derive(Debug)]
#[repr(C)]
pub struct RefCount {
    rc: AtomicUsize,
    wc: AtomicUsize,
}

impl RefCount {
    pub const fn zeroed() -> Self {
        Self {
            rc: AtomicUsize::new(0),
            wc: AtomicUsize::new(0),
        }
    }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub struct Slice<T = u8> {
    ptr: NonNull<T>,
    len: u32,
}

impl<T> Slice<T> {
    pub const fn as_ref(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.ptr.as_ptr() as _, self.len as usize) }
    }

    pub const fn as_ptr(&self) -> NonNull<T> {
        self.ptr
    }

    pub const fn len(&self) -> usize {
        self.len as usize
    }

    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

pub type Any = NonNull<libc::c_void>;
pub type Bytes = Slice<u8>;

#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    num_derive::Num,
    num_derive::NumOps,
    num_derive::Zero,
    num_derive::One,
    Hash,
    bytemuck::Zeroable,
    bytemuck::Pod,
)]
#[repr(transparent)]
pub struct Offset(pub i32);

impl Offset {
    pub const fn new(val: i32) -> Self {
        Self(val)
    }

    pub const fn from_isize(val: isize) -> Self {
        Self(val as i32)
    }

    pub const fn usize(self) -> usize {
        self.get() as usize
    }

    pub const fn get(self) -> i32 {
        self.0
    }

    pub const fn sized<T>() -> Self {
        Self(size_of::<T>() as i32)
    }

    pub const unsafe fn add_ptr(self, ptr: Any) -> Any {
        ptr.add(self.usize())
    }

    pub unsafe fn add_aligned_to<T>(self, ptr: Any) -> Any {
        self.add_aligned(ptr, align_of::<T>())
    }
    pub unsafe fn add_aligned(self, ptr: Any, align: usize) -> Any {
        let ptr = ptr.add(self.usize());
        let offset = ptr.align_offset(align);
        ptr.add(offset)
    }
}

pub mod cast {
    use core::ptr::NonNull;

    use super::Any;

    pub const fn to_any<T: ?Sized>(ptr: NonNull<T>) -> Any {
        ptr.cast::<libc::c_void>()
    }
    pub const fn from_any<T>(ptr: Any) -> NonNull<T> {
        ptr.cast::<T>()
    }
}

/// A pointer to any memory allcoated by Zeal runtime
#[derive(Debug)]
#[repr(transparent)]
pub struct ZPtr<T: Byteable + ?Sized, Meta = ()> {
    inner: Any,
    _pd: PhantomData<(*mut T, Meta)>,
}

impl<T, M> ZPtr<T, M> where T: Byteable {}

#[derive(Debug)]
#[repr(transparent)]
pub struct Thin<T: Byteable + ?Sized> {
    inner: Any,
    _pd: PhantomData<T>,
}

impl<T> AnyPointer for Thin<T>
where
    T: Byteable,
{
    type Meta = Anchor;
    type Pointee = T;

    fn root_ptr(&self) -> Any {
        self.inner
    }
}

impl<T, M> AnyPointer for ZPtr<T, M>
where
    T: Byteable,
{
    type Meta = M;

    type Pointee = T;

    fn root_ptr(&self) -> Any {
        self.inner
    }
}

impl<T> Deref for ZPtr<T>
where
    T: Byteable,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

pub trait ZPointer<Meta, Pointee>: AnyPointer
where
    Pointee: Byteable,
{
}

pub trait Pointerz<T>: AnyPointer
where
    T: Byteable,
{
}

impl<T, M, P> ZPointer<M, P> for T
where
    T: AnyPointer,
    P: Byteable,
{
}

impl<T, P> Pointerz<P> for T
where
    T: AnyPointer,
    P: Byteable,
{
}

/// Memory Layout of allocated data
/// [Anchor 8 bytes][Metadata 4 + bytes][BlockT Allocated Block]
///
/// Min Size: Anchor + sizeof(pointer) ~ 12 bytes
pub trait AnyPointer {
    type Meta;
    type Pointee: Byteable;

    // pub trait ZallocMem<T, Meta> {
    /// At least an i32
    const MIN_META_SIZE: usize = core::mem::size_of::<i32>();
    const MIN_MEMORY_SIZE: usize = size_of::<Anchor>() + size_of::<isize>();

    /// Byte offset from root poitner to begin of Metadata
    const OFFSET_META: usize = size_of::<Anchor>();
    const OFFSET_POINTEE: usize = Self::OFFSET_META + size_of::<Self::Meta>();

    // fn meta_begin()

    #[inline]
    fn meta_begin(&self) -> NonNull<Self::Meta> {
        unsafe {
            let ptr = self.root_ptr();
            let ptr = ptr.add(Self::OFFSET_META);
            let offset = ptr.align_offset(align_of::<Self::Meta>());
            ptr.add(offset).cast::<Self::Meta>()
        }
    }

    fn anchor(&self) -> Anchor {
        unsafe { self.root_ptr().cast::<Anchor>().read() }
    }

    #[inline]
    fn meta(&self) -> &Self::Meta {
        unsafe { self.meta_mut().as_ref() }
    }

    #[inline]
    fn meta_mut(&self) -> NonNull<Self::Meta> {
        self.meta_begin()
    }

    fn data(&self) -> Any {
        self.root_ptr()
    }

    fn root_ptr(&self) -> Any;

    #[inline]
    fn is_valid(&self) -> bool {
        let base = self.root_ptr();
        let anchor = self.as_anchor();
        base.addr() == anchor.addr()
    }

    #[inline]
    fn expect_valid(s: &Self) {
        if !s.is_valid() {
            panic!(
                "Zalloc Memory pointer is not valid!!!. is_valid() returned false! Ensure return values of base_ptr and meta are the same address in memory!!!"
            );
        }
    }

    #[inline]
    fn static_size_bytes() -> usize {
        size_of::<Self::Meta>() + size_of::<Self::Pointee>()
    }

    #[inline]
    fn size_bytes(&self) -> usize {
        Self::meta_size() + self.inner_size()
    }

    #[inline]
    fn inner(&self) -> &Self::Pointee {
        unsafe { self.inner_begin().as_ref() }
    }

    #[inline]
    fn inner_begin(&self) -> NonNull<Self::Pointee> {
        let anch = self.anchor();
        let ptr = unsafe { anch.jump_aligned::<Self::Pointee>(self.root_ptr()) };
        ptr.cast::<Self::Pointee>()
    }

    #[inline]
    fn inner_end(&self) -> Any {
        unsafe { crate::ptr::cast::to_any(self.inner_begin().add(1)) }
    }

    #[inline]
    fn inner_size(&self) -> usize {
        self.anchor().inner_size as usize
    }

    #[inline]
    fn meta_size() -> usize {
        core::cmp::max(Self::MIN_META_SIZE, size_of::<Self::Meta>())
    }

    #[inline]
    fn as_anchor(&self) -> NonNull<Anchor> {
        self.root_ptr().cast::<Anchor>()
    }
}

/// @brief First byte of any memory allocated by zallocator/zvm.
/// @details works similarly to flatbuffers, where the first 8  btyes of every pointed to memory contains a
/// i32 offset (always positive, but use negative value later on as a flag to mean something else, maybe useful for pointers to pointers/marker types ect...)
/// followed by a u32 containing the allocations specific inner size in bytes, not including the size of header
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, bytemuck::Pod, bytemuck::Zeroable,
)]
#[repr(C)]
pub struct Anchor {
    offset: Offset,
    inner_size: u32,
}

impl Default for Anchor {
    fn default() -> Self {
        Self::new_unsized()
    }
}

pub struct ThinMem<T: Byteable + ?Sized> {
    anchor: Anchor,
    data: T,
}

impl Anchor {
    pub const SIZE: usize = size_of::<Self>();
    pub const OFFSET_SIZE: Offset = Offset::sized::<Self>();

    pub const fn with_meta<T, M>() -> Self {
        Self {
            offset: Offset(size_of::<M>() as i32 + Self::SIZE as i32),
            inner_size: size_of::<T>() as u32,
        }
    }

    pub const fn with_meta_unsized<M>() -> Self {
        Self {
            offset: Offset(size_of::<M>() as i32 + Self::SIZE as i32),
            inner_size: 0,
        }
    }

    pub const fn new<T>() -> Self {
        Self {
            offset: Offset::new(size_of::<Self>() as i32),
            inner_size: size_of::<T>() as u32,
        }
    }

    pub const fn new_unsized() -> Self {
        Self {
            offset: Offset::new(size_of::<Self>() as i32),
            inner_size: 0,
        }
    }

    pub unsafe fn jump_aligned<T>(&self, ptr: Any) -> Any {
        self.offset.add_aligned_to::<T>(ptr)
    }
}

#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, bytemuck::Pod, bytemuck::Zeroable,
)]
#[repr(C)]
pub struct SizedAnchor {
    base: Anchor,
    size_bytes: u32,
}
