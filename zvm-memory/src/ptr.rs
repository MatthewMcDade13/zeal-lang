use core::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use crate::{
    Byteable,
    mem::{Anchor, ThinMem, WideMem},
    ty::{ClassTag, Zallocator},
};

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
pub struct RawSlice<T = u8> {
    ptr: *mut T,
    len: u32,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, bytemuck::Zeroable)]
#[repr(C, u32)]
pub enum Slice<T> {
    #[default]
    Empty = 0,
    Data(RawSlice<T>),
}

impl<T> Slice<T> {
    pub const fn empty() -> Self {
        Self::Empty
    }

    pub const fn from_raw(rslice: RawSlice<T>) -> Self {
        Self::Data(rslice)
    }

    pub const fn from_raw_parts(root: *mut T, len: usize) -> Self {
        let sl = RawSlice::<T>::from_raw_parts(root, len);
        match sl {
            Some(raw) => Self::Data(raw),
            None => Self::Empty,
        }
    }

    pub const fn into_raw(self) -> Option<RawSlice<T>> {
        match self {
            Slice::Empty => None,
            Slice::Data(raw_slice) => Some(raw_slice),
        }
    }

    pub const fn into_raw_parts(self) -> Option<(NonNull<T>, usize)> {
        if let Self::Data(raw) = self {
            if let Some(p) = NonNull::new(raw.ptr) {
                Some((p, raw.len()))
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl<T> RawSlice<T> {
    pub const fn from_raw_parts(ptr: *mut T, len: usize) -> Option<Self> {
        if len == 0 || ptr.is_null() {
            None
        } else {
            Some(Self {
                ptr,
                len: len as u32,
            })
        }
    }

    pub const fn into_raw_parts(self) -> Option<(NonNull<T>, usize)> {
        if self.len == 0 {
            None
        } else {
            match NonNull::new(self.ptr) {
                Some(ptr) => Some((ptr, self.len())),
                None => None,
            }
        }
    }

    pub const fn size_bytes(&self) -> usize {
        self.len() * size_of::<T>()
    }

    /// We cant construct a new instance of RawSlice that is empty, so this is safe.
    /// @see [Slice] for a datatype that can be empty
    pub const fn as_ref(&self) -> &[T] {
        unsafe { core::slice::from_raw_parts(self.ptr as _, self.len()) }
    }

    /// We cant construct a new instance of RawSlice that is empty, so this is safe.
    /// @see [Slice] for a datatype that can be empty
    pub const fn as_mut(&mut self) -> &mut [T] {
        unsafe { core::slice::from_raw_parts_mut(self.ptr as *mut _, self.len()) }
    }

    /// Unsafe if len == 0, as ptr will be null.
    /// use try_ptr for safe version
    pub const unsafe fn as_ptr(&self) -> *mut T {
        self.ptr
    }

    pub const fn try_ptr(&self) -> Option<NonNull<T>> {
        if self.is_empty() {
            None
        } else {
            match NonNull::new(self.ptr) {
                Some(ptr) => Some(ptr),
                None => None,
            }
        }
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
        match self {
            Slice::Empty => &[],
            Slice::Data(raw_slice) => raw_slice.as_ref(),
        }
    }
}

impl<T> DerefMut for Slice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Slice::Empty => &mut [],
            Slice::Data(raw_slice) => raw_slice.as_mut(),
        }
    }
}

impl<T> Deref for RawSlice<T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> DerefMut for RawSlice<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

pub type Any = NonNull<libc::c_void>;
pub type Bytes = RawSlice<u8>;

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
        if self.get() < 0 {
            0
        } else {
            self.get() as usize
        }
    }

    pub const fn is_neg(self) -> bool {
        self.0 < 0
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

/// A pointer to memory allocated by Zeal runtime
/// with no metadata (only anchor and data) @see [ThinMem]
/// because we have no metadata, this pointer must be treated like a raw pointer. the alternative
/// being that Thin uniquely owns its data it points to, and therefore would only be used with move semantics
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Thin<T: Byteable + ?Sized, Alloc: Zallocator> {
    pub(crate) inner: NonNull<ThinMem<T>>,
    _pd: ClassTag<Alloc>,
}

/// A pointer to memory allocated by Zeal runtime
/// with metadata. @see [WideMem]
#[derive(Debug)]
#[repr(transparent)]
pub struct Zptr<T: Byteable + ?Sized, Meta, Alloc: Zallocator> {
    pub(crate) inner: NonNull<WideMem<T, Meta>>,
    _pd: ClassTag<Alloc>,
}

impl<T, M, A> Zptr<T, M, A>
where
    T: Byteable,
    A: Zallocator,
{
    pub const fn thin(self) -> Thin<T, A> {
        Thin {
            inner: self.inner.cast::<ThinMem<T>>(),
            _pd: PhantomData,
        }
    }
}

impl<T, M, A> From<Thin<T, A>> for Zptr<T, M, A>
where
    T: Byteable,
    A: Zallocator,
{
    fn from(value: Thin<T, A>) -> Self {
        Self {
            inner: value.inner.cast::<WideMem<T, M>>(),
            _pd: PhantomData,
        }
    }
}

impl<T, M, A> From<Zptr<T, M, A>> for Thin<T, A>
where
    T: Byteable,
    A: Zallocator,
{
    fn from(value: Zptr<T, M, A>) -> Self {
        Self {
            inner: value.inner.cast(),
            _pd: PhantomData,
        }
    }
}

impl<T, A> Thin<T, A>
where
    T: Byteable,
    A: Zallocator,
{
    pub const MIN_META_SIZE: usize = core::mem::size_of::<i32>();
    pub const MIN_MEMORY_SIZE: usize = size_of::<Anchor>() + size_of::<isize>();

    /// Byte offset from root poitner to begin of Metadata or pointee data
    pub const OFFSET_ANCHOR: usize = size_of::<Anchor>();

    // fn meta_begin()

    // #[inline]
    // fn meta_begin(&self) -> NonNull<Self::Meta> {
    //     unsafe {
    //         let ptr = self.root_ptr();
    //         let ptr = ptr.add(Self::OFFSET_META);
    //         let offset = ptr.align_offset(align_of::<Self::Meta>());
    //         ptr.add(offset).cast::<Self::Meta>()
    //     }
    // }

    pub const fn anchor(&self) -> Anchor {
        unsafe { self.root().cast::<Anchor>().read() }
    }

    pub fn new(alloc: &A) -> Self {
        alloc.zalloc().thin()
    }

    pub fn init(alloc: &A, val: T) -> Self {
        let s = Self::new(alloc);
        s.write(val);
        s
    }

    pub fn write(&self, val: T) {
        let p = self.inner_begin().as_ptr();
        if core::mem::needs_drop::<T>() {
            unsafe { *p = val };
        } else {
            unsafe { core::ptr::write(p, val) };
        }
    }

    #[inline]
    pub unsafe fn read(self) -> T {
        self.inner_begin().read()
    }

    #[inline]
    pub fn into_inner(self) -> T {
        unsafe { self.read() }
    }

    // #[inline]
    // fn meta(&self) -> &Self::Meta {
    //     unsafe { self.meta_mut().as_ref() }
    // }

    // #[inline]
    // fn meta_mut(&self) -> NonNull<Self::Meta> {
    //     self.meta_begin()
    // }

    pub const fn data(&self) -> Any {
        self.root()
    }

    pub const fn root(&self) -> Any {
        self.inner.cast()
    }

    #[inline]
    pub fn is_valid(&self) -> bool {
        let base = self.root();
        let anchor = self.as_anchor();
        base.addr() == anchor.addr()
    }

    #[inline]
    pub fn expect_valid(s: &Self) {
        if !s.is_valid() {
            panic!(
                "Zalloc Memory pointer is not valid!!!. is_valid() returned false! Ensure return values of base_ptr and meta are the same address in memory!!!"
            );
        }
    }

    #[inline]
    pub fn inner(&self) -> &T {
        unsafe { self.inner_begin().as_ref() }
    }

    #[inline]
    pub fn inner_begin(&self) -> NonNull<T> {
        let anch = self.anchor();
        let ptr = unsafe { anch.jump_aligned::<T>(self.root()) };
        ptr.cast::<T>()
    }

    #[inline]
    pub fn inner_end(&self) -> Any {
        unsafe { crate::ptr::cast::to_any(self.inner_begin().add(1)) }
    }

    pub const fn inner_size(&self) -> usize {
        self.anchor().elem_len as usize
    }

    // pub const fn meta_size() -> usize {
    //     core::cmp::max(Self::MIN_META_SIZE, size_of::<Self::Meta>())
    // }

    pub const fn as_anchor(&self) -> NonNull<Anchor> {
        self.root().cast::<Anchor>()
    }

    pub const fn wide(self) -> Zptr<T, (), A> {
        self.wide_meta::<()>()
    }

    pub const fn wide_meta<Meta>(self) -> Zptr<T, Meta, A> {
        Zptr {
            inner: self.inner.cast(),
            _pd: PhantomData,
        }
    }
}

impl<T, A> Deref for Thin<T, A>
where
    T: Byteable,
    A: Zallocator,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        todo!()
    }
}
