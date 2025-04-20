use core::cell::UnsafeCell;

#[repr(C)]
pub struct MemCell<T: ?Sized, Meta = ()> {
    pub meta: Meta,
    pub data: UnsafeCell<T>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct StackMeta {
    prev: u32,
}

pub type StackCell<T> = MemCell<StackMeta, T>;
