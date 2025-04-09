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
///
///
/// @details This is what allows us to cast between @ref [Thin] and @ref [Zptr] pointers.
/// Since allocators implementing @ref [Zallocator] must allocate with
/// @ref [ThinMem]
///     or
/// @ref [WideMem]
/// and both of these memory types have a field of type @ref [Anchor] as its first field
///
/// Because of this, when we deref a @ref [Thin] or @ref [Zptr], the first 4 bytes are always
/// an integer offset (negative is ignored for now but may be used later)
/// that tell us the byte offset to where the inner allocated data resides, regardless if there is metadata or not.
#[derive(
    Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, bytemuck::Pod, bytemuck::Zeroable,
)]
#[repr(C)]
pub struct _Root {
    pub offset: Offset,
    pub inner_size: u32,
}

#[derive(
    Debug,
    Copy,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    num_derive::Unsigned,
    num_derive::Num,
    num_derive::NumOps,
    num_derive::One,
    num_derive::Zero,
)]
#[repr(transparent)]
pub struct RootOffset(usize);

impl RootOffset {
    pub const fn empty() -> Self {
        Self::new(0)
    }

    pub const fn new(extra: usize) -> Self {
        Self(size_of::<Root>() + extra)
    }

    pub const fn with_meta<M>() -> Self {
        Self::new(size_of::<M>())
    }

    pub const fn extend(self, extra: usize) -> Self {
        Self(self.0 + extra)
    }
}

impl Default for RootOffset {
    fn default() -> Self {
        Self::empty()
    }
}

impl From<RootOffset> for usize {
    fn from(value: RootOffset) -> Self {
        value.0
    }
}

impl AsRef<usize> for RootOffset {
    fn as_ref(&self) -> &usize {
        &self.0
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(C)]
pub enum Root {
    /// This will always be the same as
    /// Self::Offset(RootOffset::default()) or Self::Offset(size_of::<Self>())
    Empty,
    Offset(RootOffset),
    Sizeof {
        offset: RootOffset,
        inner_size: usize,
    },
    Count {
        offset: RootOffset,
        elem_len: usize,
    },
    Extended {
        offset: RootOffset,
        user_data: [u8; size_of::<usize>()],
    },
}

impl Root {
    const EMPTY: Self = Self::Offset(RootOffset::empty());

    pub const fn empty() -> Self {
        Self::Empty
    }

    pub const fn empty_val() -> &'static Self {
        &Self::EMPTY
    }

    pub const fn new(val: usize) -> Self {
        Self::Offset(RootOffset(val))
    }

    pub const fn new_with<M>(val: usize) -> Self {
        Self::new(val).extend::<M>()
    }

    pub const fn sizeof<T>() -> Self {
        Self::Sizeof {
            offset: RootOffset::empty(),
            inner_size: size_of::<T>(),
        }
    }

    pub const fn sizeof_with<T, M>() -> Self {
        Self::sizeof::<T>().extend::<M>()
    }

    pub const fn array(len: usize) -> Self {
        Self::Count {
            offset: RootOffset::empty(),
            elem_len: len,
        }
    }

    pub const fn array_with<M>(len: usize) -> Self {
        Self::array(len).extend::<M>()
    }

    pub const fn userdata(data: [u8; size_of::<usize>()]) -> Self {
        Self::Extended {
            offset: RootOffset::empty(),
            user_data: data,
        }
    }

    pub const fn userdata_with<M>(data: [u8; size_of::<usize>()]) -> Self {
        Self::userdata(data).extend::<M>()
    }

    pub const fn extend<M>(self) -> Self {
        let size: usize = const { size_of::<M>() };
        match self {
            Root::Empty => Self::Offset(RootOffset::new(size)),
            Root::Offset(root_offset) => Self::Offset(root_offset.extend(size)),
            Root::Sizeof { offset, inner_size } => Self::Sizeof {
                offset: offset.extend(size),
                inner_size,
            },
            Root::Count { offset, elem_len } => Self::Count {
                offset: offset.extend(size),
                elem_len,
            },
            Root::Extended { offset, user_data } => Self::Extended {
                offset: offset.extend(size),
                user_data,
            },
        }
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::Empty
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct ThinMem<T: ?Sized> {
    pub root: Root,
    pub data: T,
}

#[derive(Debug)]
#[repr(C)]
pub struct WideMem<T: ?Sized, Meta> {
    pub root: Root,
    pub meta: Meta,
    pub data: T,
}

// NOTE: look at Box::into_boxed_slice for converting ThinMem<T> to ThinMem<[T]> (DST!!!)
impl<T, M> Clone for WideMem<T, M>
where
    T: Clone,
    M: Clone,
{
    fn clone(&self) -> Self {
        Self {
            root: self.root,
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
// impl<T, M> Byteable for WideMem<T, M>
// where
//     T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
//     M: bytemuck::AnyBitPattern + bytemuck::NoUninit,
// {
//     fn as_bytes(&self) -> &[u8] {
//         let s = self as *const Self;
//         let ptr = s.cast::<u8>();
//         unsafe { core::slice::from_raw_parts(ptr, size_of::<Self>()) }
//     }

//     fn as_bytes_mut(&mut self) -> &mut [u8] {
//         let s = self as *mut Self;
//         let ptr = s.cast::<u8>();
//         unsafe { core::slice::from_raw_parts_mut(ptr, size_of::<Self>()) }
//     }

//     fn ref_from_bytes(bytes: &[u8]) -> &Self {
//         todo!()
//     }

//     fn mut_from_bytes(bytes: &mut [u8]) -> &mut Self {
//         todo!()
//     }
// }

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
            root: todo!(),
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
            root: Root::no_meta::<T>(),
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
            root: Root::no_meta::<T>(),
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
            root: Root::extend::<T, M>(),
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
            root: Root::no_meta::<T>(),
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
            root: Root::extend::<T, M>(),
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
