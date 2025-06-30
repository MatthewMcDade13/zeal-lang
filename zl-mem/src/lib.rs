#![no_std]
#![feature(allocator_api)]

pub mod arena;
pub mod gen_alloc;
pub mod miheap;

extern crate alloc;
