use std::{cell::RefCell, mem::MaybeUninit, rc::Rc};

/// Heap-Allocated, Referenc-Counted, Regrowable and Reassignable,
/// Array. Array Items are immutable.
pub struct ConstArray<T> {
    count: usize,
    buf: Option<Rc<RefCell<[T]>>>,
}

impl<T> ConstArray<T> {
    pub const fn empty() -> Self {
        let count = 0;
        let buf = None;
        Self { count, buf }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        let count = 0;
        let mut buf: Vec<T> = Vec::new();
        let b = buf.into_boxed_slice();
    }
}

impl<T> ConstArray<T>
where
    T: bytemuck::Zeroable,
{
    pub fn zeroed_cap(len: usize) -> Self {
        let mut buf = Vec::with_capacity(len);
        for c in buf.spare_capacity_mut().iter_mut() {
            c.write(T::zeroed());
        }
        unsafe {
            buf.set_len(len);
        }

        let buf = Rc::from(RefCell::new(*buf));
    }
}
