use std::{borrow::BorrowMut, cell::RefCell, mem::MaybeUninit, rc::Rc};

/// Heap-Allocated, Referenc-Counted, Regrowable and Reassignable,
/// Array. Array Items are immutable.
pub struct ConstArray<T> {
    count: usize,
    buf: Option<Rc<[T]>>,
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
        todo!()
    }

    pub fn t(&mut self) {
        if let Some(rc) = self.buf.as_mut() {
            for i in rc.iter_mut() {}
        } else {
            todo!()
        }
    }
}

impl<T> ConstArray<T>
where
    T: bytemuck::Zeroable,
{
    pub fn zeroed_cap(len: usize) -> Self {
        let buf = RefCell::new(Vec::with_capacity(len));
        for c in buf.borrow_mut().spare_capacity_mut().iter_mut() {
            c.write(T::zeroed());
        }
        unsafe {
            buf.borrow_mut().set_len(len);
        }

        todo!()
        // let buf: Rc<RefCell<[T]>> = Rc::from(buf.borrow_mut().into_boxed_slice());
        // Self {
        //     count: 0,
        //     buf: Some(buf.into()),
        // }
    }
}
