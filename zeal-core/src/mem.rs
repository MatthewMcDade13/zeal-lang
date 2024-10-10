use std::{marker::PhantomData, ops::Index, ptr::NonNull};

pub const KB: usize = 1024;
pub const MB: usize = KB * KB;
pub const GB: usize = KB * KB * KB;
pub const MB1: usize = MB * 1;
pub const MB2: usize = MB * 2;
pub const MB3: usize = MB * 3;
pub const MB4: usize = MB * 4;

/// A 'Slab' style allocator that can allocate for any T: Sized + Copy
/// pops are a simple subtraction. Objects in Stack are expected to be Immutable.
///
///
#[repr(C)]
#[derive(Debug)]
pub struct SlabStack<const SIZE: usize> {
    buf: [u8; SIZE],
    top: usize,
}

impl<const S: usize> Default for SlabStack<S> {
    fn default() -> Self {
        const { Self::zeroed() }
    }
}
impl<const S: usize> SlabStack<S> {
    pub const fn zeroed() -> Self {
        Self {
            buf: [0u8; S],
            top: 0,
        }
    }

    pub const fn size(&self) -> usize {
        S
    }

    pub fn push<T>(&mut self, val: T) -> RefSlot<T>
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
        let slot = self.top;
        self.top += offset;

        self.buf[self.top] = std::mem::size_of::<T>() as u8;
        RefSlot::new(slot)
    }

    pub fn peek_top<T>(&self) -> &T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let item = RefSlot::new(self.top).into_item(self);
        unsafe { item.val.as_ref() }
    }

    /// Decrements top index, does not return value at top of Stack
    pub fn pop_ignore(&mut self) {
        let top = self.top_slot();
        let size = self.slot_size(top) as isize;
        let top = self.top as isize;

        let offset = top - size;
        assert!(offset >= 0);

        self.top -= offset as usize;
    }

    pub fn pop<T>(&mut self) -> T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let slot = self.top_rslot::<T>().i;
        self.pop_ignore();
        let item = self.item(RefSlot::new(slot));
        unsafe { NonNull::read(item.val) }
    }

    pub const fn top_slot(&self) -> Slot {
        Slot::new(self.top)
    }

    pub const fn slot_size(&self, slot: Slot) -> usize {
        self.buf[slot.i] as usize
    }

    pub fn item_slot<T>(&self, slot: Slot) -> &T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let sl = slot.cast::<T>();
        let item = sl.into_item(self);
        unsafe { item.val.as_ref() }
    }

    pub const fn top_rslot<T>(&self) -> RefSlot<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        RefSlot::new(self.top)
    }

    pub const fn rslot_size<T>(&self, slot: RefSlot<T>) -> usize
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        self.buf[slot.i] as usize
    }

    pub fn rget<T>(&self, slot: RefSlot<T>) -> &T
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let item = slot.into_item(self);
        unsafe { item.val.as_ref() }
    }

    #[inline]
    fn item_bytes(&self, slot: Slot) -> StackItemBytes {
        slot.into_item_bytes(self)
    }

    #[inline]
    fn item<T>(&self, slot: RefSlot<T>) -> StackItem<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        slot.into_item(self)
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefSlot<'stack, T: bytemuck::AnyBitPattern + bytemuck::NoUninit> {
    i: usize,
    _phantom: PhantomData<&'stack T>,
}

impl<'a, T> RefSlot<'a, T>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    pub const fn to_erased_type(self) -> Slot<'a> {
        Slot::from_refslot(self)
    }
}

pub struct Slot<'stack> {
    i: usize,
    _phantom: PhantomData<&'stack dyn std::any::Any>,
}

impl<'a> Slot<'a> {
    const fn new(slot: usize) -> Self {
        Self {
            i: slot,
            _phantom: PhantomData,
        }
    }

    pub const fn from_refslot<T>(rs: RefSlot<T>) -> Self
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        Self {
            i: rs.i,
            _phantom: PhantomData,
        }
    }

    pub const fn cast<T>(self) -> RefSlot<'a, T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        RefSlot {
            i: self.i,
            _phantom: PhantomData,
        }
    }

    #[inline]
    fn into_item_bytes<const S: usize>(self, stack: &SlabStack<S>) -> StackItemBytes {
        into_item_bytes(self.i, stack)
    }
}

impl<'a, T> RefSlot<'a, T>
where
    T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
{
    pub const fn new(slot: usize) -> Self {
        Self {
            i: slot,
            _phantom: PhantomData,
        }
    }

    #[inline]
    fn into_item_bytes<const S: usize>(self, stack: &SlabStack<S>) -> StackItemBytes {
        into_item_bytes(self.i, stack)
    }

    fn into_item<const S: usize>(self, stack: &SlabStack<S>) -> StackItem<T>
    where
        T: bytemuck::AnyBitPattern + bytemuck::NoUninit,
    {
        let bytes = self.into_item_bytes(stack);
        assert!(bytes.size as usize == std::mem::size_of::<T>());

        let val = bytes.buf.cast::<T>();

        StackItem {
            val,
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

fn into_item_bytes<const S: usize>(i: usize, stack: &SlabStack<S>) -> StackItemBytes {
    let size = stack.buf[i] as isize;

    let offset = i as isize - size;
    assert!(offset >= 0);
    let offset = offset as usize;
    let bytes = &stack.buf[offset..i];

    StackItemBytes {
        buf: NonNull::new(bytes.as_ptr() as *mut _).expect("Null Pointer!!!"),
        size: size as u8,
        _phantom: PhantomData,
    }
}
