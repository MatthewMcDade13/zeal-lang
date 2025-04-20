#![feature(thin_box)]
#![no_std]

use core::{alloc::GlobalAlloc, sync::atomic::AtomicU32};

extern crate alloc;

pub mod buff;
pub mod mem;
pub mod ptr;
pub mod stack;

// pub(crate) struct SystemAlloc;

// #[global_allocator]
// pub(crate) static GLOBAL: SystemAlloc = SystemAlloc;

// unsafe impl GlobalAlloc for SystemAlloc {
//     unsafe fn alloc(&self, layout: core::alloc::Layout) -> *mut u8 {
//         unsafe { libc::malloc(layout.pad_to_align().size()).cast::<u8>() }
//     }

//     unsafe fn dealloc(&self, ptr: *mut u8, _layout: core::alloc::Layout) {
//         unsafe { libc::free(ptr.cast::<libc::c_void>()) }
//     }
// }

// #[repr(transparent)]
// pub struct RefCount(pub AtomicU32);
