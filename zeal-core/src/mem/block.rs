use std::{alloc::Layout, any::TypeId};

use super::slab_stack::{BlockSlot, SlabAddress};

#[derive(Debug, Clone, Copy)]
pub enum NumberType {
    /// [u8]
    Byte,
    /// [i8]
    Char,
    /// [i16]
    Short,
    /// [u16]
    Word,
    /// [i32]
    Int,
    /// [u32]
    Uint,
    /// [i64]
    Int64,
    /// [u64]
    Uint64,
    /// [f32]
    Float,
    /// [f64]
    Float64,
    /// [usize]
    Slot,
    /// [isize]
    Offset,
}

impl NumberType {
    pub const fn size_bytes(&self) -> usize {
        match self {
            Self::Byte | NumberType::Char => 1,
            Self::Short | NumberType::Word => 2,
            Self::Int | NumberType::Float | NumberType::Uint => 4,
            Self::Float64 | NumberType::Int64 | NumberType::Uint64 => 8,
            Self::Slot => std::mem::size_of::<usize>(),
            Self::Offset => std::mem::size_of::<isize>(),
        }
    }

    pub const fn align(&self) -> usize {
        match self {
            Self::Byte => std::mem::align_of::<u8>(),
            Self::Char => std::mem::align_of::<i8>(),
            Self::Short => std::mem::align_of::<u16>(),
            Self::Word => std::mem::align_of::<i16>(),
            Self::Int => std::mem::align_of::<i32>(),
            Self::Uint => std::mem::align_of::<u32>(),
            Self::Int64 => std::mem::align_of::<i64>(),
            Self::Uint64 => std::mem::align_of::<u64>(),
            Self::Float => std::mem::align_of::<f32>(),
            Self::Float64 => std::mem::align_of::<f64>(),
            Self::Slot => std::mem::align_of::<usize>(),
            Self::Offset => std::mem::align_of::<isize>(),
        }
    }

    pub const fn layout(&self) -> Layout {
        // This is safe as we can't initialize this enum with an invalid alignment
        unsafe { Layout::from_size_align_unchecked(self.size_bytes(), self.align()) }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypeLayout {
    pub ty: TypeId,
    pub layout: Layout,
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    Numeric(NumberType),
}

pub struct StructType {
    typeid: TypeId,
    layout: Layout,
}

#[derive(Debug, Clone, Copy)]
pub enum BlockType {
    /// Type determined at runtime
    Any(Layout),
    /// Primitive Scalar
    /// i.e. f32, f64, u8, ect...
    Scalar(NumberType),
    Struct(TypeLayout),
    Tuple {
        val_tys: [(); Self::TUPLE_MAX_LEN],
        len: u8,
    },
    Array {
        /// Type and layout of this array
        ty: TypeLayout,
        /// Type and layout of this arrays elements
        inner_ty: TypeLayout,
        len: usize,
    },
    /// Buffer of bytes
    Buffer {
        layout: Layout,
    },
}

impl BlockType {
    pub const TUPLE_MAX_LEN: usize = 4;
}

#[derive(Debug, Clone, Copy)]
pub struct Block {
    /// Id index that maps back to headers buffer
    pub id: BlockSlot,
    /// Actual byte index of data in Slab buffer
    pub slot: SlabAddress,
    /// Size of the whole block in bytes. sizeof(val) * n elemeents if array, otherwise sizeof(val)
    pub size_bytes: usize,
    /// Different from size_bytes when block refers to an array. size is 1 when block is string
    pub item_size: usize,
}

impl Block {
    /// Gets the number of elements in vector or string that
    /// this block refers to. 0 if this block is a non array/string value.
    pub const fn array_len(&self) -> usize {
        self.size_bytes / self.item_size
    }

    /// Creates a new block at index = id, pointing to byte index slab address = slot.
    /// infers Block size from generic parameter T.
    pub const fn new<T>(id: usize, slot: usize) -> Self {
        let size_bytes = std::mem::size_of::<T>();

        Self {
            id: BlockSlot(id),
            slot: SlabAddress(slot),
            size_bytes,
            item_size: size_bytes,
        }
    }

    // Creates a new block at index @ id, pointing to byte index slab address @ slot.
    // This block will be pointing to a contiguous sequence of values T.
    /// infers Block size from generic parameter T and size of buffer from size_of::<T>() * length.
    pub const fn new_array<T>(id: usize, slot: usize, length: usize) -> Self {
        let id = BlockSlot(id);
        let slot = SlabAddress(slot);
        let item_size = std::mem::size_of::<T>();
        let size_bytes = item_size * length;
        Self {
            id,
            slot,
            size_bytes,
            item_size,
        }
    }
}
