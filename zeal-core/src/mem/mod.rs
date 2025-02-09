pub mod alloc;
pub mod block;
pub mod marker;
pub mod ptr;
pub mod slab;
pub mod slab_stack;
pub mod str;

/// Trait for Container types that hold a byte buffer, or something that can
/// be referenced with as_bytes and can be indexed with MemCell.
/// Intended for something like a SlabStack, where each element in the container type
/// could be different and be reinterpreted depending on what is accessing it.
pub trait Memory: AsRef<[u8]> {
    fn try_read_ref<T>(&self, cell: MemCell) -> Option<&T>
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let slice = self.as_ref();
        if cell.end() >= slice.len() {
            None
        } else {
            let res = self.read_ref::<T>(cell);
            Some(res)
        }
    }

    fn try_read<T>(&self, cell: MemCell) -> Option<T>
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let slice = self.as_ref();
        if cell.end() >= slice.len() {
            None
        } else {
            let res = self.read::<T>(cell);
            Some(res)
        }
    }

    fn read_ref<T>(&self, cell: MemCell) -> &T
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let slice = self.as_ref();
        let val_bytes = &slice[cell.as_range()];
        bytemuck::from_bytes(val_bytes)
    }

    fn read<T>(&self, cell: MemCell) -> T
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        *self.read_ref(cell)
    }

    fn read_mut<T>(&mut self, cell: MemCell) -> &mut T
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
        Self: MemoryMut,
    {
        let slice = self.as_bytes_mut();
        let val_bytes = &mut slice[cell.as_range()];
        bytemuck::from_bytes_mut(val_bytes)
    }
}

pub trait MemoryConst: Memory {
    fn as_bytes(&self) -> &[u8] {
        self.as_ref()
    }
}

pub trait MemoryMut: Memory + AsMut<[u8]> {
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        self.as_mut()
    }
}

impl<T> MemoryConst for T where T: Memory {}
impl<T> MemoryMut for T where T: Memory + AsMut<[u8]> {}
impl<T> Memory for T where T: AsRef<[u8]> {}

#[derive(
    Debug,
    Clone,
    Copy,
    Default,
    bytemuck::Pod,
    bytemuck::Zeroable,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
)]
#[repr(C)]
pub struct MemCell {
    pub begin: u32,
    pub size_bytes: u32,
}

impl MemCell {
    pub const fn new(index: usize, length: usize) -> Self {
        let begin = index as u32;
        let len = length as u32;
        Self {
            begin,
            size_bytes: len,
        }
    }

    pub const fn end(&self) -> usize {
        (self.begin + self.size_bytes) as usize
    }

    pub const fn as_range(&self) -> std::ops::Range<usize> {
        let begin = self.begin as usize;
        begin..self.end()
    }

    pub fn read_from<T, Out>(self, mem: &T) -> &Out
    where
        T: Memory,
        Out: bytemuck::Pod + bytemuck::Zeroable,
    {
        mem.read_ref::<Out>(self)
    }

    pub fn read_from_mut<T, Out>(self, mem: &mut T) -> &mut Out
    where
        T: MemoryMut,
        Out: bytemuck::Pod + bytemuck::Zeroable,
    {
        mem.read_mut::<Out>(self)
    }

    pub fn read_from_bytes<T>(self, bytes: &[u8]) -> &T
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let slice = &bytes[self.as_range()];
        bytemuck::from_bytes::<T>(slice)
    }
}
