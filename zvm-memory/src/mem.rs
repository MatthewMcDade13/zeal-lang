use core::{marker::PhantomData, ptr::NonNull};

use crate::{
    Byteable,
    ptr::{Any, Offset, RawSlice, Slice},
    ty::{ClassTag, Zallocator},
};

/// @brief First byte of any memory allocated by zallocator/zvm.
/// @details works similarly to flatbuffers, where the first 8  btyes of every pointed to memory contains a
/// i32 offset (always positive, but use negative value later on as a flag to mean something else, maybe useful for pointers to pointers/marker types ect...)
/// followed by a u32 containing the allocations specific inner size in bytes, not including the size of header
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, bytemuck::Pod, bytemuck::Zeroable,
)]
#[repr(C)]
pub struct Anchor {
    pub offset: Offset,
    pub inner_size: u32,
}

impl Default for Anchor {
    fn default() -> Self {
        Self::new_unsized()
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct ThinMem<T: ?Sized> {
    pub anchor: Anchor,
    pub data: T,
}

#[derive(Debug)]
#[repr(C)]
pub struct WideMem<T: ?Sized, Meta> {
    pub anchor: Anchor,
    pub meta: Meta,
    pub data: T,
}

impl<T, M> Clone for WideMem<T, M>
where
    T: Clone,
    M: Clone,
{
    fn clone(&self) -> Self {
        Self {
            anchor: self.anchor,
            meta: self.meta.clone(),
            data: self.data.clone(),
        }
    }
}

impl<T, M> Copy for WideMem<T, M>
where
    T: Copy + bytemuck::Pod,
    M: Copy,
{
}

// TODO: Verify this works... WRITE TESTS
impl<T, M> Byteable for WideMem<T, M>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    M: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    fn as_bytes(&self) -> &[u8] {
        let s = self as *const Self;
        let ptr = s.cast::<u8>();
        unsafe { core::slice::from_raw_parts(ptr, size_of::<Self>()) }
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        let s = self as *mut Self;
        let ptr = s.cast::<u8>();
        unsafe { core::slice::from_raw_parts_mut(ptr, size_of::<Self>()) }
    }

    fn ref_from_bytes(bytes: &[u8]) -> &Self {
        todo!()
    }

    fn mut_from_bytes(bytes: &mut [u8]) -> &mut Self {
        todo!()
    }
}

impl<T, M> WideMem<T, M>
where
    T: bytemuck::Zeroable,
{
    pub unsafe fn zalloc(meta: M) -> NonNull<Self> {
        Self::zalloc_with(bytemuck::zeroed::<T>(), meta)
    }

    pub unsafe fn zalloc_with(val: T, meta: M) -> NonNull<Self> {
        let ptr = libc::calloc(1, size_of::<Self>());
        let mut ptr = NonNull::new(ptr)
            .expect("Failed to Allocate more memory! Out of Memory!!")
            .cast::<Self>();
        ptr.write(Self {
            anchor: Anchor::with_meta::<T, M>(),
            meta,
            data: val,
        });
        ptr
    }

    pub unsafe fn zalloc_array(len: usize) -> Slice<Self>
    where
        M: Default,
    {
        let ptr = libc::calloc(len, size_of::<Self>());
        let mut ptr = NonNull::new(ptr)
            .expect("Failed to Allocate more memory! Out of Memory!!!")
            .cast::<Self>();
        Slice::from_raw_parts(ptr.as_ptr(), len)
    }

    #[inline]
    pub unsafe fn free(s: NonNull<Self>) {
        let ptr = s.cast::<libc::c_void>();
        libc::free(ptr.as_ptr());
    }

    #[inline]
    pub fn inner_offset() -> usize
    where
        Self: Default,
    {
        bytemuck::offset_of!(Self, data)
    }

    #[inline]
    pub fn meta_offset() -> usize
    where
        Self: Default,
    {
        bytemuck::offset_of!(Self, meta)
    }
}

impl<T> ThinMem<T>
where
    T: bytemuck::Zeroable,
{
    pub unsafe fn zalloc() -> NonNull<Self> {
        Self::zalloc_with(bytemuck::zeroed::<T>())
    }

    pub unsafe fn zalloc_with(val: T) -> NonNull<Self> {
        let mem = Self::zalloc();

        let ptr = libc::calloc(1, size_of::<Self>());
        let mut ptr = NonNull::new(ptr)
            .expect("Failed to Allocate more memory! Out of Memory!!")
            .cast::<Self>();
        ptr.write(Self {
            anchor: Anchor::no_meta::<T>(),
            data: val,
        });
        ptr
    }

    pub unsafe fn free(s: NonNull<Self>) {
        let ptr = s.cast::<libc::c_void>();
        libc::free(ptr.as_ptr());
    }

    #[inline]
    pub fn inner_offset() -> usize
    where
        Self: Default,
    {
        bytemuck::offset_of!(Self, data)
    }
}

impl<T> ThinMem<T>
where
    T: bytemuck::Zeroable,
{
    pub const fn zeroed() -> Self {
        Self {
            anchor: Anchor::no_meta::<T>(),
            data: bytemuck::zeroed::<T>(),
        }
    }
}

impl<T, M> WideMem<T, M>
where
    T: bytemuck::Zeroable,
    M: bytemuck::Zeroable,
{
    pub const fn zeroed() -> Self {
        Self {
            anchor: Anchor::with_meta::<T, M>(),
            meta: bytemuck::zeroed::<M>(),
            data: bytemuck::zeroed::<T>(),
        }
    }
}

impl<T> Default for ThinMem<T>
where
    T: Default,
{
    fn default() -> Self {
        Self {
            anchor: Anchor::no_meta::<T>(),
            data: T::default(),
        }
    }
}

impl<T, M> Default for WideMem<T, M>
where
    T: Default,
    M: Default,
{
    fn default() -> Self {
        Self {
            anchor: Anchor::with_meta::<T, M>(),
            meta: M::default(),
            data: T::default(),
        }
    }
}

// impl<T, M> Default for WideMem<T, M>
// where
//     T: Byteable + Default,
//     M: Default,
// {
//     fn default() -> Self {
//         todo!()
//     }
// }

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

    pub const fn no_meta<T>() -> Self {
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
