use std::{
    marker::PhantomData,
    ops::{Deref, Index, IndexMut},
    ptr::NonNull,
    rc::Rc,
};

pub const KB: usize = 1024;
pub const KB_512: usize = KB * 512;
pub const MB: usize = KB * KB;
pub const GB: usize = KB * KB * KB;
pub const MB1: usize = MB;
pub const MB2: usize = MB * 2;
pub const MB3: usize = MB * 3;
pub const MB4: usize = MB * 4;

// TODO: Implement logic to switch to Vec<u8> in case static stack
// overflows or is more performant
#[derive(Debug, Clone)]
enum SlabBuffer<const SIZE: usize> {
    Static([u8; SIZE]),
    Heap(Vec<u8>),
}

impl<const S: usize> Index<usize> for SlabBuffer<S> {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        match self {
            SlabBuffer::Static(buf) => &buf[index],
            SlabBuffer::Heap(buf) => &buf[index],
        }
    }
}
impl<const S: usize> Index<std::ops::Range<usize>> for SlabBuffer<S> {
    type Output = [u8];

    fn index(&self, r: std::ops::Range<usize>) -> &Self::Output {
        match self {
            SlabBuffer::Static(buf) => &buf[r.start..r.end],
            SlabBuffer::Heap(buf) => &buf[r.start..r.end],
        }
    }
}

impl<const S: usize> IndexMut<usize> for SlabBuffer<S> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        match self {
            SlabBuffer::Static(buf) => &mut buf[index],
            SlabBuffer::Heap(buf) => &mut buf[index],
        }
    }
}
impl<const S: usize> IndexMut<std::ops::Range<usize>> for SlabBuffer<S> {
    fn index_mut(&mut self, r: std::ops::Range<usize>) -> &mut Self::Output {
        match self {
            SlabBuffer::Static(buf) => &mut buf[r.start..r.end],
            SlabBuffer::Heap(buf) => &mut buf[r.start..r.end],
        }
    }
}

pub type VMStack = SlabStack<KB_512>;

/// A 'Slab' style allocator that can allocate for any T: Sized + Copy
/// pops are a simple subtraction. Objects in Stack are expected to be Immutable.
///
///
#[repr(C)]
#[derive(Debug)]
pub struct SlabStack<const SIZE: usize> {
    buf: SlabBuffer<SIZE>,
    top: usize,
}

impl<const S: usize> Default for SlabStack<S> {
    fn default() -> Self {
        const { Self::zeroed() }
    }
}
impl<const S: usize> SlabStack<S> {
    pub const STACK_MAX: usize = KB * 512;
    pub const fn zeroed() -> Self {
        Self {
            buf: SlabBuffer::Static([0; S]),
            top: 0,
        }
    }

    #[inline]
    pub fn buf(&self) -> &[u8] {
        match &self.buf {
            SlabBuffer::Static(buf) => buf,
            SlabBuffer::Heap(buf) => buf,
        }
    }

    #[inline]
    pub fn buf_mut(&mut self) -> &mut [u8] {
        match &mut self.buf {
            SlabBuffer::Static(buf) => buf,
            SlabBuffer::Heap(buf) => buf,
        }
    }

    pub const fn size(&self) -> usize {
        S
    }

    pub fn push_alloc<T>(&mut self, val: T) -> StackPtr<T, S>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let slot = self.push(val);
        StackPtr { slot, parent: self }
    }

    pub fn push<T>(&mut self, val: T) -> StackSlot<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        const { assert!(std::mem::size_of::<T>() < std::mem::size_of::<[u8; S]>()) };
        const { assert!(std::mem::size_of::<T>() <= StackItem::<T>::MAX_SIZE) };

        let offset = std::mem::size_of::<T>() + self.top;
        assert!(offset < self.size());

        let val_bytes = bytemuck::bytes_of(&val);
        let dst = &mut self.buf[self.top..offset];

        assert!(val_bytes.len() == dst.len());

        dst.copy_from_slice(val_bytes);

        self.top += std::mem::size_of::<T>();

        let slot = self.top;
        self.buf[self.top] = std::mem::size_of::<T>() as u8;

        self.top += 1;
        StackSlot::new(slot)
    }

    pub fn peek_top<T>(&self) -> &T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let item = self.item(StackSlot::new(self.top));
        unsafe { item.val.as_ref() }
    }

    /// Decrements top index, does not return value at top of Stack
    pub fn pop_cast<T>(&mut self) -> T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let (top_val, size) = {
            let item = self.item_bytes(self.top);
            let size = item.size as usize;
            assert!(std::mem::size_of::<T>() == size);

            let ival = item.cast_into::<T>();
            let val = unsafe { NonNull::read(ival.val) };
            (val, size)
        };

        let top = self.top as isize;

        let offset = top - size as isize;
        assert!(offset >= 0);
        let offset = offset as usize;
        self.top -= offset;

        top_val
    }

    pub fn lookup<T>(&self, slot: StackSlot<T>) -> &T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let item = self.item(slot);
        unsafe { item.val.as_ref() }
    }

    pub const fn top_handle<T>(&self) -> StackSlot<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        StackSlot::new(self.top)
    }

    pub fn verify_slot_size<T>(&self, slot: StackSlot<T>) -> bool
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let size = self.query_slot_size(slot);
        size == std::mem::size_of::<T>()
    }

    pub fn is_valid_slot<T>(&self, slot: StackSlot<T>) -> bool
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        self.top > slot.i && self.verify_slot_size(slot)
    }

    pub fn query_slot_size<T>(&self, slot: StackSlot<T>) -> usize
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        self.buf[slot.i] as usize
    }

    fn top_item_bytes(&self) -> StackItemBytes {
        self.item_bytes(self.top)
    }

    fn top_item<T>(&self) -> StackItem<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let bytes = self.item_bytes(self.top);
        bytes.cast_into()
    }

    // #[inline]
    // fn item_bytes(&self, slot: Slot) -> StackItemBytes {
    //     slot.into_item_bytes(self)
    // }

    #[inline]
    fn item<T>(&self, slot: StackSlot<T>) -> StackItem<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        self.item_bytes(slot.i).cast_into::<T>()
    }

    fn item_bytes<'a>(&'a self, slot: usize) -> StackItemBytes<'a> {
        let i = slot;
        let size = self.buf[i] as isize;

        let offset = i as isize - size;
        assert!(offset >= 0, ": {offset}");
        let offset = offset as usize;
        let bytes = &self.buf[offset..i];

        StackItemBytes {
            buf: NonNull::new(bytes.as_ptr() as *mut _).expect("Null Pointer!!!"),
            size: size as u8,
            _phantom: PhantomData,
        }
    }
}

impl<const S: usize, T> Index<StackSlot<T>> for SlabStack<S>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    type Output = T;

    fn index(&self, index: StackSlot<T>) -> &Self::Output {
        let item = self.item_bytes(index.i).cast_into::<T>();
        unsafe { item.val.as_ref() }
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackSlot<T: bytemuck::AnyBitPattern + bytemuck::NoUninit> {
    i: usize,
    _phantom: PhantomData<T>,
}

impl<T> From<usize> for StackSlot<T>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    fn from(value: usize) -> Self {
        Self {
            i: value,
            _phantom: PhantomData,
        }
    }
}

// #[repr(transparent)]
// #[derive(Debug, Clone, Copy)]
// pub struct Slot {
//     i: usize,
// }

impl<T> StackSlot<T>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    const fn new(slot: usize) -> Self {
        Self {
            i: slot,
            _phantom: PhantomData,
        }
    }
}

#[repr(transparent)]
#[derive(Debug, Copy)]
struct StackItem<'stack, T> {
    val: NonNull<T>,
    _phantom: PhantomData<&'stack T>,
}

/// 1:1 representation of Item in allocation buffer.
/// each item is [...bytes_of_T, size_byte] where Self::buf points to ...bytes_of_T
/// Cannot be cast directly from Stack buffer, this is a 'view' into the
/// region of stack space that its member occupies
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct StackItemBytes<'stack> {
    buf: NonNull<u8>,
    size: u8,
    _phantom: PhantomData<&'stack [u8]>,
}

impl<'a> StackItemBytes<'a> {
    pub fn slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.buf.as_ptr(), self.size as usize) }
    }

    pub const fn cast_into<T>(self) -> StackItem<'a, T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        StackItem {
            val: self.buf.cast::<T>(),
            _phantom: PhantomData,
        }
    }
}

impl<'a, T> StackItem<'a, T> {
    pub const MAX_SIZE: usize = u8::MAX as usize;

    pub fn as_ref(&self) -> &T {
        unsafe { self.val.as_ref() }
    }
}

impl<'a, T> StackItem<'a, T>
where
    T: Clone,
{
    pub fn clone_inner(&self) -> T {
        let val = unsafe { self.val.as_ref() };
        T::clone(val)
    }
}

impl<'a, T> StackItem<'a, T>
where
    T: Copy,
{
    pub fn copy_inner(&self) -> T {
        let val = self.val.as_ptr();
        unsafe { *val }
    }
}

impl<'a, T> Clone for StackItem<'a, T> {
    fn clone(&self) -> Self {
        let val = NonNull::new(self.val.as_ptr()).expect("Null Pointer!!!!");
        Self {
            val,
            _phantom: PhantomData,
        }
    }
}

/// Pointer to a Slot in parent SlabStack.
/// Not actually a pointer, but an index into a Slab as well as a ref to the slab itself.
#[derive(Debug, Clone, Copy)]
pub struct StackPtr<'stack, T: bytemuck::AnyBitPattern + bytemuck::NoUninit, const S: usize> {
    slot: StackSlot<T>,
    parent: &'stack SlabStack<S>,
}

impl<'a, T, const S: usize> Deref for StackPtr<'a, T, S>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.parent.lookup(self.slot)
    }
}
