use std::{
    alloc::Layout,
    any::TypeId,
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
    ptr::NonNull,
};

use crate::clamp;

use super::block::Block;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct SlabPtr<T: bytemuck::Zeroable + bytemuck::Pod> {
    id: BlockSlot,
    parent: NonNull<SlabStack>,
    _phantom: PhantomData<T>,
}

impl<T> SlabPtr<T>
where
    T: bytemuck::Zeroable + bytemuck::Pod,
{
    pub fn as_raw(&mut self) -> Option<NonNull<T>> {
        let p = self.parent.as_ptr();
        let p = unsafe { &mut *p };

        if let Some(val) = p.lookup_mut::<T>(self.id) {
            let ptr = std::ptr::from_ref(val);
            let ptr = NonNull::new(ptr as *mut _).expect("Cannot wrap null pointer");
            Some(ptr)
        } else {
            None
        }
    }

    pub fn lookup_block(&self) -> &Block {
        let slab = unsafe { self.parent.as_ref() };
        &slab.blocks[self.id.0]
    }
}

impl<T> Deref for SlabPtr<T>
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

impl<T> DerefMut for SlabPtr<T>
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

impl Index<BlockSlot> for SlabStack {
    type Output = Block;

    fn index(&self, index: BlockSlot) -> &Self::Output {
        &self.blocks[index.0]
    }
}

impl IndexMut<BlockSlot> for SlabStack {
    fn index_mut(&mut self, index: BlockSlot) -> &mut Self::Output {
        &mut self.blocks[index.0]
    }
}

impl Index<usize> for SlabStack {
    type Output = Block;

    fn index(&self, index: usize) -> &Self::Output {
        let back = self.blocks.len() - 1;
        let slot = clamp(0, index, back);
        &self.blocks[slot]
    }
}

impl IndexMut<usize> for SlabStack {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let back = self.blocks.len() - 1;
        let slot = clamp(0, index, back);
        &mut self.blocks[slot]
    }
}

impl Index<&Block> for SlabStack {
    type Output = [u8];

    fn index(&self, index: &Block) -> &Self::Output {
        self.get_mem(index)
    }
}
impl IndexMut<&Block> for SlabStack {
    fn index_mut(&mut self, index: &Block) -> &mut Self::Output {
        self.get_mem_mut(index)
    }
}

impl Index<Block> for SlabStack {
    type Output = [u8];

    fn index(&self, index: Block) -> &Self::Output {
        &self[&index]
    }
}

impl IndexMut<Block> for SlabStack {
    fn index_mut(&mut self, index: Block) -> &mut Self::Output {
        self.get_mem_mut(&index)
    }
}

/// Wraps index id for meta Block header buffer.
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BlockSlot(pub(crate) usize);

/// Wraps index slot for starting byte of an item in
/// SlabStack
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct SlabAddress(pub(crate) usize);

impl SlabAddress {
    const fn new(addr: usize) -> Self {
        Self(addr)
    }
}

impl BlockSlot {
    const fn new(slot: usize) -> Self {
        Self(slot)
    }
    pub const fn get(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct SlabStack {
    /// each block represents a single item allocated in stack
    blocks: Vec<Block>,
    buf: Vec<u8>,
    /// Used for tracking allocation and pushing items onto stack
    top: usize,
}

impl SlabStack {
    /// Creates a new SlabStack buffer and resizes it to given
    /// size such that buffer.len() == size. Entire buffer is zero filled
    ///
    /// given size is in bytes
    pub fn new_with_size(size: usize) -> Self {
        let mut buf = Vec::new();
        let meta = Vec::with_capacity(size / 8);
        buf.resize(size, 0);
        Self {
            buf,
            blocks: meta,
            top: 0,
        }
    }

    /// Gets the number of blocks that are currently allocated
    /// and accessible
    pub fn len(&self) -> usize {
        self.blocks.len()
    }

    /// Gets the number of bytes currently allocated
    pub fn size_bytes(&self) -> usize {
        self.buf.len()
    }

    /// Initializes a new SlabStack buffer with given capacity in bytes.
    /// block buffer is initialized with given capacity / 8
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buf: Vec::with_capacity(capacity),
            blocks: Vec::with_capacity(capacity / 8),
            top: 0,
        }
    }

    pub fn new() -> Self {
        Self {
            buf: Vec::new(),
            blocks: Vec::new(),
            top: 0,
        }
    }

    /// Pushes parameter val to back of slab-stack.
    /// returns the Block that was allocated.
    ///
    /// This method is similar to Self::push(), except it returns
    /// the block metadata for the given value instead of a pointer to the value.
    /// You can lookup the value that the returned block points to by passing it
    /// to the lookup_ptr method.
    pub fn push_block<T>(&mut self, val: T) -> Block
    where
        T: bytemuck::Zeroable + bytemuck::Pod,
    {
        let size = const { std::mem::size_of::<T>() };
        let end = self.top + size;
        let dst = &mut self.buf[self.top..end];

        let src = bytemuck::bytes_of(&val);
        assert!(dst.len() == src.len());
        dst.copy_from_slice(src);

        let id = BlockSlot(self.blocks.len());
        let slot = SlabAddress(self.top);

        let b = Block {
            id,
            slot,
            size_bytes: size,
            item_size: size,
        };
        self.top = end;
        self.blocks.push(b);
        b
    }

    /// Pushes given value to the back of the slab-stack.
    /// returns a fat pointer to the value that was allocated.
    pub fn push<T>(&mut self, val: T) -> SlabPtr<T>
    where
        T: bytemuck::Zeroable + bytemuck::Pod,
    {
        let size = const { std::mem::size_of::<T>() };
        let bytes = bytemuck::bytes_of(&val);
        let start = self.top;
        let end = start + size;
        let dst = &mut self.buf[start..end];

        dst.copy_from_slice(bytes);
        let id = BlockSlot(self.blocks.len());
        let slot = SlabAddress(start);
        self.blocks.push(Block {
            id,
            slot,
            size_bytes: size,
            item_size: size,
        });

        self.top = end;
        let parent = NonNull::new(self).expect("Attempt to wrap null pointer!!!");
        SlabPtr {
            id,
            parent,
            _phantom: PhantomData,
        }
    }

    /// Pops last n items off of SlabStack.
    /// Grabs the block index of meta.len() - n, clamping to range
    /// of block buffer such that when n >= block.len() then
    ///
    /// entire stack is cleared of all items
    /// Returns number of blocks popped off of buf.
    ///
    pub fn popn(&mut self, n: usize) -> usize {
        if self.blocks.is_empty() {
            return 0;
        }

        let buflen = self.blocks.len() as isize;

        if n as isize >= buflen {
            self.clear();
            return buflen as usize;
        }

        let back = buflen - n as isize;
        let back = clamp(0, back, buflen - 1) as usize;

        if back == 0 {
            self.clear();
            buflen as usize
        } else {
            let block = &self.blocks[back];

            self.top = block.slot.0;
            n
        }
    }

    pub fn is_empty(&self) -> bool {
        self.buf.is_empty() && self.blocks.is_empty()
    }

    pub fn pop_block(&mut self) -> Option<Block> {
        let block = self.blocks.pop()?;
        self.top = block.slot.0;
        Some(block)
    }

    pub fn pop<T>(&mut self) -> Option<T>
    where
        T: bytemuck::Zeroable + bytemuck::Pod,
    {
        if let Some(block) = self.blocks.pop() {
            self.top = block.slot.0;
            let start = block.slot.0;
            let end = start + block.size_bytes;

            let bytes = &mut self.buf[start..end];
            let val = bytemuck::from_bytes::<T>(bytes);
            Some(*val)
        } else {
            None
        }
    }

    pub fn get_mem_mut(&mut self, block: &Block) -> &mut [u8] {
        assert!(self.buf.len() > block.slot.0);
        let start = block.slot.0;
        let end = start + block.size_bytes;

        (&mut self.buf[start..end]) as _
    }

    pub fn get_mem(&self, block: &Block) -> &[u8] {
        assert!(self.buf.len() > block.slot.0);
        let start = block.slot.0;
        let end = start + block.size_bytes;

        (&self.buf[start..end]) as _
    }

    pub fn lookup<T>(&mut self, block_slot: BlockSlot) -> Option<&T>
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let blockid = block_slot.0;
        if let Some(block) = self.blocks.get(blockid) {
            let start = block.slot.0;
            let end = start + block.size_bytes;
            let bytes = &mut self.buf[start..end];
            let val = bytemuck::from_bytes(bytes);
            Some(val)
        } else {
            None
        }
    }

    pub fn lookup_mut<T>(&mut self, block_slot: BlockSlot) -> Option<&mut T>
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let blockid = block_slot.0;
        if let Some(block) = self.blocks.get(blockid) {
            let start = block.slot.0;
            let end = start + block.size_bytes;
            let bytes = &mut self.buf[start..end];
            let val = bytemuck::from_bytes_mut(bytes);
            Some(val)
        } else {
            None
        }
    }

    pub fn lookup_ptr<T>(&mut self, block: &Block) -> Option<SlabPtr<T>>
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let id = block.id.0;
        assert!(id < self.blocks.len(), "Access Violation, tried to read old blockid: {id} that now points to unallocated memory!");

        let parent = NonNull::new(self).expect("Cannot wrap null pointer!");

        let id = BlockSlot(id);
        let sp = SlabPtr {
            id,
            parent,
            _phantom: PhantomData,
        };
        Some(sp)
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
        self.buf.clear();
    }

    pub fn get_block(&self, blockid: usize) -> Option<&Block> {
        self.blocks.get(blockid)
    }
}
mod tests {
    use super::*;

    #[repr(C)]
    #[derive(Debug, Clone, Copy, bytemuck::Zeroable, bytemuck::Pod)]
    struct Point {
        x: usize,
        y: usize,
    }

    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, bytemuck::Zeroable, bytemuck::Pod)]
    struct Wrap(isize);

    #[test]
    fn slabstack_alloc() -> anyhow::Result<()> {
        let mut ss = SlabStack::new_with_size(4096);
        let a = Point { x: 500, y: 1600 };
        let b = Wrap(690000);
        let c: i64 = 123123123123123;
        let ca = ss.push(a);
        let cb = ss.push(b);
        let cc = ss.push(c);

        assert_eq!(ca.x, 500);
        assert_eq!(ca.y, 1600);

        assert_eq!(cb.0, 690000);
        assert_eq!(*cc, 123123123123123);
        Ok(())
    }

    #[test]
    fn slabstack_alloc_block() -> anyhow::Result<()> {
        let mut ss = SlabStack::new_with_size(4096);
        let a = Point { x: 500, y: 1600 };
        let b = Wrap(690000);
        let c: i64 = 123123123123123;
        let ca = ss.push_block(a);
        let cb = ss.push_block(b);
        let cc = ss.push_block(c);

        let ca = ss.lookup_ptr::<Point>(&ca).expect("Did not find pointer");
        let cb = ss.lookup_ptr::<Wrap>(&cb).expect("Did not find pointer");
        let cc = ss.lookup_ptr::<i64>(&cc).expect("Did not find pointer");

        assert_eq!(ca.x, 500);
        assert_eq!(ca.y, 1600);

        assert_eq!(cb.0, 690000);
        assert_eq!(*cc, 123123123123123);
        Ok(())
    }
}
