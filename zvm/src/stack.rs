use std::{
    fmt::Display,
    ops::{Index, IndexMut},
};

use zeal_core::val::ZValue;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, Default)]
pub struct StackCursor(isize);

impl StackCursor {
    pub const fn zero() -> Self {
        Self(0)
    }

    pub const fn new() -> Self {
        Self::empty()
    }

    pub const fn empty() -> Self {
        Self(-1)
    }

    pub const fn offset(self, offset: usize) -> Self {
        let offset = (offset as isize) * -1;
        self.addn(offset)
    }

    pub const fn inc(self) -> Self {
        self.addn(1)
    }

    pub const fn dec(self) -> Self {
        self.addn(-1)
    }

    const fn addn(self, n: isize) -> Self {
        let i = self.0 as isize + n;
        if i < 0 {
            Self(-1)
        } else {
            Self(i)
        }
    }

    pub const fn len(self) -> usize {
        if self.0 < 0 {
            0
        } else {
            self.0 as usize + 1
        }
    }

    /// if self.0 < 0
    pub const fn is_end(self) -> bool {
        self.len() == 0
    }

    /// Unwraps index if greater than or equal to 0, otherwise returns 0
    pub const fn as_index(self) -> usize {
        if let Some(i) = self.try_as_index() {
            i
        } else {
            0
        }
    }

    /// same as [as_index], but returns None when self.0 < 0
    pub const fn try_as_index(self) -> Option<usize> {
        if self.is_end() {
            None
        } else {
            Some(self.0 as usize)
        }
    }

    // pub fn inner(self) -> isize {
    //     self.0
    // }
}

#[derive(Debug, Clone)]
pub struct Stack<const S: usize, T>
where
    T: Clone,
{
    pub cursor: StackCursor,
    buf: [T; S],
}

impl<const S: usize, T> Display for Stack<S, T>
where
    T: Clone + std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::from("\n----------\nSTACK\n----------\n");

        for (n, v) in self.peek_iter().enumerate() {
            let line = format!("{n} => {v}\n");
            s.push_str(&line);
        }
        s.push_str("\n---------- \nEND STACK\n ----------\n");
        write!(f, "{s}")
    }
}

pub type StackIter<T> = StackPeeker<T>;

/// Iterator for Stack. moves through stack from back to front (top to bottom)
#[derive(Debug, Clone, Copy)]
pub struct StackPeeker<T> {
    buf_begin: *const T,
    i: usize,

    buf_len: usize,
}

impl<T> Iterator for StackPeeker<T>
where
    T: Clone,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.buf_len == 0 || self.i >= self.buf_len {
            None
        } else {
            let i = (self.buf_len - 1) - self.i;
            let zval = unsafe {
                let v = self.buf_begin.add(i);
                v.as_ref().expect("null ptr deref!!!")
            };
            self.i += 1;

            Some(zval.clone())
        }
    }
}

impl<const S: usize> IntoIterator for Stack<S, ZValue> {
    type Item = ZValue;

    type IntoIter = StackPeeker<ZValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.peek_iter()
    }
}

impl<const S: usize, T> Stack<S, T>
where
    T: Clone,
{
    pub const fn len(&self) -> usize {
        if let Some(index) = self.cursor.try_as_index() {
            index + 1
        } else {
            0
        }
    }

    pub fn peek_iter(&self) -> StackPeeker<T> {
        let buf_begin = self.buf.as_ptr();
        StackPeeker {
            buf_begin,
            i: 1,
            buf_len: self.len(),
        }
    }

    /// Peeks stacck value offset n from top of stack
    /// eg n = 5, peeks top - 5.
    pub const fn peekn(&self, n: usize) -> Option<&T> {
        let c = self.cursor.offset(n);
        if let Some(index) = c.try_as_index() {
            Some(&self.buf[index])
        } else {
            None
        }
    }

    pub fn peekn_mut(&mut self, n: isize) -> Option<&mut T> {
        let c = self.cursor.addn(n);
        if let Some(index) = c.try_as_index() {
            Some(&mut self.buf[index])
        } else {
            None
        }
    }

    pub const fn peek_top(&self) -> Option<&T> {
        if self.is_empty() {
            None
        } else {
            self.peekn(0)
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.cursor.is_end()
    }

    pub const fn top_index(&self) -> Option<usize> {
        self.cursor.try_as_index()
    }

    pub const fn max(&self) -> usize {
        S
    }

    /// Pushes value onto top of stack. Returns index of item that was pushed.
    pub fn push(&mut self, val: T) -> usize {
        self.cursor = if self.cursor.0 < 0 {
            StackCursor::zero()
        } else {
            self.cursor.inc()
        };
        let i = self.cursor.0 as usize;

        self.buf[i] = val;
        i
    }
}

impl<const S: usize, T> Stack<S, T>
where
    T: Clone + Default + std::fmt::Display + std::fmt::Debug,
{
    pub fn new() -> Self {
        let buf: [T; S] = (0..S)
            .map(|_| T::default())
            .collect::<Vec<T>>()
            .try_into()
            .unwrap();
        Self {
            cursor: StackCursor::empty(),
            buf,
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        if let Some(i) = self.cursor.try_as_index() {
            let v = self.buf[i].clone();
            self.cursor = self.cursor.dec();

            Some(v)
        } else {
            None
        }
    }

    pub fn expect_pop(&mut self) -> T {
        if let Some(v) = self.pop() {
            v
        } else {
            panic!("Can't pop stack empty stack! {self}")
        }
    }
}

impl<const S: usize, T> Default for Stack<S, T>
where
    T: Clone + Default + std::fmt::Display + std::fmt::Debug,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<const S: usize, T> Index<std::ops::RangeFrom<usize>> for Stack<S, T>
where
    T: Clone,
{
    type Output = [T];

    fn index(&self, r: std::ops::RangeFrom<usize>) -> &Self::Output {
        &self.buf[r.start..]
    }
}

impl<const S: usize, T> Index<usize> for Stack<S, T>
where
    T: Clone,
{
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.buf[index]
    }
}

impl<const S: usize, T> IndexMut<usize> for Stack<S, T>
where
    T: Clone,
{
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.buf[index]
    }
}
