use std::{alloc::Layout, boxed::ThinBox, ptr::NonNull};

use anyhow::Context;

struct Meta {
    rc: u32,
}

#[derive(Debug)]
#[repr(C)]
pub struct Bytes {
    ptr: ThinBox<[u8]>,
}

impl Bytes {
    pub const fn layout_of(cap: usize) -> Layout {
        match Layout::array::<u8>(cap) {
            Ok(l) => l,
            Err(_) => panic!("cannot create layout!"),
        }
    }
    pub fn new(cap: usize) -> Self {
        let ptr: ThinBox<[u8]> = ThinBox::new_unsize([]);
        Self { ptr }
    }
}
