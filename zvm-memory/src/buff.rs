use core::ptr::NonNull;
use std::marker::PhantomData;

use crate::mem::{Anchor, ThinMem, WideMem};

#[repr(C)]
// pub struct Buff<T> {
//     ptr: NonNull<ThinMem<[T]>>,
//     len: usize,
//     _pd: PhantomData<ThinMem<[T]>>,
// }

// impl<T> Buff<T>
// where
//     T: bytemuck::Zeroable,
// {
//     pub fn new(len: usize) -> Self {
//         unsafe {
//             let p = libc::calloc(1, size_of::<ThinMem<T>>() + (size_of::<T>() * len))
//                 .cast::<ThinMem<T>>();
//             let p = NonNull::new(p).expect("Out of memory!!");
//             let t: &ThinMem<[T]> =
//                 core::mem::transmute(core::slice::from_raw_parts(p.as_ptr(), len));

//             p.write(ThinMem {
//                 anchor: Anchor::no_meta::<T>(),
//                 data: bytemuck::zeroed::<T>(),
//             });
//         }
//         todo!()
//     }
// }

// impl<T> Clone for Buff<T>
// where
//     T: Clone,
// {
//     fn clone(&self) -> Self {
//         todo!()
//     }
// }

// impl<T> Drop for Buff<T> {
//     fn drop(&mut self) {
//         unsafe { libc::free(self.ptr.cast::<libc::c_void>().as_ptr()) }
//     }
// }

// pub struct SizedBuff<const SIZE: usize, T>(ThinMem<[T; SIZE]>);

