use core::ops::{Deref, DerefMut};

/// Non-Null pointer. API gaurantees this will never be null
/// Similar to core::NonNull, but this is a *mut T instead of a *const T
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Ptr<T: ?Sized>(*mut T);

impl<T: ?Sized> Ptr<T> {
    pub const fn new(val: *mut T) -> Option<Self> {
        if val.is_null() { None } else { Some(Self(val)) }
    }

    pub const fn as_ref(&self) -> &T {
        unsafe { &*self.0 }
    }

    pub const fn as_mut(&self) -> &mut T {
        unsafe { &mut *self.0 }
    }

    pub const fn as_ptr(&self) -> *mut T {
        self.0
    }

    pub const fn as_const(&self) -> *const T {
        self.0 as _
    }
}

// impl<T: ?Sized> Deref for Ptr<T> {
//     type Target = T;

//     fn deref(&self) -> &Self::Target {
//         // SAFETY: We cant make new pointers without checking that they are not null first
//         // Similar to core::NonNull<>
//         unsafe { core::ptr::read(self.0) }
//     }
// }

// impl<T: ?Sized> DerefMut for Ptr<T> {
//     fn deref_mut(&mut self) -> &mut Self::Target {
//         // SAFETY: We cant make new pointers without checking that they are not null first
//         // Similar to core::NonNull<>
//         unsafe { &mut *self.0 }
//     }
// }
