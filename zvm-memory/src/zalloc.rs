use core::marker::PhantomData;

use alloc::alloc::alloc_zeroed;

use crate::{buffer::Mem, ptr::Ptr};

pub struct Slab {
    block_size: usize,
    len: usize,
    begin: Ptr<Mem>,
    end: Ptr<Mem>,
}

impl Slab {
    pub fn new(block_size: usize, len: usize) -> Self {
        let layout = Mem::layout_array(block_size, len).expect("Error creating BufferMem layout!!");
        let mem = unsafe { alloc_zeroed(layout) };
        let mem = mem.cast::<Mem>();
    }
}
