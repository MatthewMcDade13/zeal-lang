use std::{
    any::TypeId,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::clamp;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Block {
    /// Id index that maps back to headers buffer
    pub id: BlockSlot,
    /// Actual byte index of data in Slab buffer. should be equal to slot + sizeof(val) * n
    pub slot: SlabAddress,
    /// Size of the whole block in bytes. sizeof(val) * n elemeents if array, otherwise sizeof(val)
    pub size_bytes: usize,
    /// Different from size_bytes when member of a contiguous array.
    pub val_size: usize,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SlabCell<T: bytemuck::Zeroable + bytemuck::Pod> {
    pub id: BlockSlot,
    pub data: NonNull<T>,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SlabWidePtr<T: bytemuck::Zeroable + bytemuck::Pod> {
    id: BlockSlot,
    parent: NonNull<SlabStack>,
    _phantom: PhantomData<T>,
}

impl<T> Deref for SlabWidePtr<T>
where
    T: bytemuck::Zeroable + bytemuck::Pod,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        let p = unsafe { self.parent.as_ref() };
        if let Some(block) = p.get_block(self.id.0) {
            let bytes = p.get_mem(block);
            bytemuck::from_bytes(bytes)
        } else {
            panic!("Tried to dereference SlabWidePtr!!! Tried lookup up blockid: {}, but failed for an unknown reason.", self.id.0);
        }
    }
}

impl<T> DerefMut for SlabWidePtr<T>
where
    T: bytemuck::Zeroable + bytemuck::Pod,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        let p = unsafe { self.parent.as_mut() };
        if let Some(block) = p.get_block(self.id.0) {
            let block = *block;
            let bytes = p.get_mem_mut(&block);
            bytemuck::from_bytes_mut(bytes)
        } else {
            panic!("Tried to dereference SlabWidePtr!!! Tried lookup up blockid: {}, but failed for an unknown reason.", self.id.0);
        }
    }
}

/// Wraps index id for meta Block header buffer.
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct BlockSlot(usize);

/// Wraps index slot for starting byte of an item in
/// SlabStack
#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
pub struct SlabAddress(usize);

impl SlabAddress {
    pub const fn new(addr: usize) -> Self {
        Self(addr)
    }
}

impl BlockSlot {
    pub const fn new(slot: usize) -> Self {
        Self(slot)
    }
    pub const fn get(&self) -> usize {
        self.0
    }
}

impl<T> Deref for SlabCell<T>
where
    T: bytemuck::Pod + bytemuck::Zeroable,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.data.as_ref() }
    }
}

impl<T> DerefMut for SlabCell<T>
where
    T: bytemuck::Pod + bytemuck::Zeroable,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.data.as_mut() }
    }
}

impl<T> SlabCell<T>
where
    T: bytemuck::Pod + bytemuck::Zeroable,
{
    pub const fn size_bytes() -> usize {
        std::mem::size_of::<T>()
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CellRef<'a, T: bytemuck::Zeroable + bytemuck::Pod> {
    pub id: usize,
    pub data: NonNull<T>,
    _phantom: PhantomData<&'a T>,
}

impl<'a, T> Deref for CellRef<'a, T>
where
    T: bytemuck::Zeroable + bytemuck::Pod,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.data.as_ref() }
    }
}

impl<'a, T> DerefMut for CellRef<'a, T>
where
    T: bytemuck::Zeroable + bytemuck::Pod,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.data.as_mut() }
    }
}

impl<'a, T> CellRef<'a, T>
where
    T: bytemuck::Zeroable + bytemuck::Pod,
{
    pub const fn size_bytes() -> usize {
        std::mem::size_of::<T>()
    }
}

#[derive(Debug, Clone)]
pub struct SlabStack {
    meta: Vec<Block>,
    buf: Vec<u8>,
    top: usize,
}

impl SlabStack {
    pub fn new_with_size(size: usize) -> Self {
        let mut buf = Vec::new();
        let meta = Vec::new();
        buf.resize(size, 0);
        Self { buf, meta, top: 0 }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buf: Vec::with_capacity(capacity),
            meta: Vec::new(),
            top: 0,
        }
    }

    pub fn new() -> Self {
        Self {
            buf: Vec::new(),
            meta: Vec::new(),
            top: 0,
        }
    }

    pub fn alloc_block<T>(&mut self, val: T) -> Block
    where
        T: bytemuck::Zeroable + bytemuck::Pod,
    {
        let size = const { std::mem::size_of::<T>() };
        let end = self.top + size;
        let dst = &mut self.buf[self.top..=end];

        let src = bytemuck::bytes_of(&val);
        assert!(dst.len() == src.len());
        dst.copy_from_slice(src);

        let id = BlockSlot(self.meta.len());
        let slot = SlabAddress(self.top);

        let b = Block {
            id,
            slot,
            size_bytes: size,
            val_size: size,
        };
        self.meta.push(b);
        b
    }

    pub fn alloc<T>(&mut self, val: T) -> SlabCell<T>
    where
        T: bytemuck::Zeroable + bytemuck::Pod,
    {
        let size = const { std::mem::size_of::<T>() };
        let bytes = bytemuck::bytes_of(&val);
        let start = self.top;
        let end = start + size;
        let dst = &mut self.buf[start..=end];

        dst.copy_from_slice(bytes);
        let id = BlockSlot(self.meta.len());
        let slot = SlabAddress(start);
        self.meta.push(Block {
            id,
            slot,
            size_bytes: size,
            val_size: size,
        });

        let data = &mut self.buf[start];
        let data = NonNull::new(data).expect("Cannot wrap a null pointer!!!");
        let data = data.cast::<T>();

        self.top = end + 1;

        SlabCell { id, data }
    }

    /// Pops last n items off of SlabStack. decrements top by each
    /// item's size_bytes field in Block meta vector
    /// Returns number of items popped off of buf
    pub fn pop_discard(&mut self, n: usize) -> usize {
        if self.meta.is_empty() {
            return 0;
        }

        let mut top = self.top as isize;
        for x in 0..n {
            if let Some(block) = self.meta.pop() {
                top -= block.size_bytes as isize;
            } else {
                return x;
            }
        }
        self.top = std::cmp::max(0isize, top) as usize;
        n
    }

    pub fn pop_block(&mut self) -> Option<Block> {
        self.meta.pop()
    }

    pub fn pop<T>(&mut self) -> Option<&T>
    where
        T: bytemuck::Zeroable + bytemuck::Pod,
    {
        if let Some(block) = self.meta.pop() {
            self.top -= block.size_bytes;

            let bytes = &mut self.buf[block.slot.0..=block.size_bytes];
            let val = bytemuck::from_bytes::<T>(bytes);
            Some(val)
        } else {
            None
        }
    }

    pub fn get_mem_mut(&mut self, block: &Block) -> &mut [u8] {
        assert!(self.buf.len() > block.slot.0);
        let val_bytes = &mut self.buf[block.slot.0..=block.size_bytes];
        val_bytes
    }

    pub fn get_mem(&self, block: &Block) -> &[u8] {
        assert!(self.buf.len() > block.slot.0);
        let val_bytes = &self.buf[block.slot.0..=block.size_bytes];
        val_bytes
    }

    pub fn lookup<T>(&mut self, blockid: usize) -> Option<&T>
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        if let Some(block) = self.meta.get(blockid) {
            let start = block.slot.0;
            let end = start + block.size_bytes;
            let bytes = &mut self.buf[start..=end];
            let val = bytemuck::from_bytes(bytes);
            Some(val)
        } else {
            None
        }
    }

    pub fn get_block(&self, blockid: usize) -> Option<&Block> {
        self.meta.get(blockid)
    }

    fn read_from_bytes<T>(&self, blockid: usize) -> Option<SlabCell<T>>
    where
        T: Sized + bytemuck::Zeroable + bytemuck::Pod,
    {
        if let Some(block) = self.meta.get(blockid) {
            let val = read_buffer_from::<T>(&self.buf, block.slot.0);
            let data = std::ptr::from_ref(val);
            let data = NonNull::new(data as *mut T).expect("Unable to wrap null pointer!!!");
            let sc = SlabCell {
                id: BlockSlot(blockid),

                data,
            };
            Some(sc)
        } else {
            None
        }
    }
}

pub fn read_buffer_from<T>(buf: &[u8], start: usize) -> &T
where
    T: bytemuck::Zeroable + bytemuck::Pod + Sized,
{
    let size = const { std::mem::size_of::<T>() };
    assert!(start + size < buf.len());

    let span = &buf[start..=size];
    bytemuck::from_bytes::<T>(span)
}
