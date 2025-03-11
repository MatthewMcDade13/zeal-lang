use std::ptr::NonNull;

pub struct Block {}

pub struct Stack {
    mem: NonNull<u8>,
}
