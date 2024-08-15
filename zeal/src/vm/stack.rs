use std::fmt::Display;

use crate::core_types::val::ZValue;

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
pub struct Stack<const S: usize> {
    pub cursor: StackCursor,
    buf: [ZValue; S],
}

impl<const S: usize> Display for Stack<S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::from("\n----------\nSTACK\n----------\n");
        for (n, v) in self.peek_iter().enumerate() {
            let line = format!("{n} => {v}\n");
            s.push_str(&line);
        }
        s.push_str("\n---------- END STACK ----------\n");
        write!(f, "{s}")
    }
}

pub type StackIter = StackPeeker;

#[derive(Debug, Clone, Copy)]
pub struct StackPeeker {
    buf_begin: *const ZValue,
    i: usize,

    buf_len: usize,
}

impl Iterator for StackPeeker {
    type Item = ZValue;

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

impl<const S: usize> IntoIterator for Stack<S> {
    type Item = ZValue;

    type IntoIter = StackPeeker;

    fn into_iter(self) -> Self::IntoIter {
        self.peek_iter()
    }
}

impl<const S: usize> Stack<S> {
    pub fn new() -> Self {
        let buf: [ZValue; S] = (0..S)
            .map(|_| ZValue::Nil)
            .collect::<Vec<ZValue>>()
            .try_into()
            .unwrap();
        Self {
            cursor: StackCursor::empty(),
            buf,
        }
    }

    pub fn peek_iter(&self) -> StackPeeker {
        let buf_begin = self.buf.as_ptr();
        StackPeeker {
            buf_begin,
            i: 1,
            buf_len: self.len(),
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.cursor.is_end()
    }

    /// Peeks stacck value offset n from top of stack
    /// eg n = 5, peeks top - 5.
    pub const fn peekn(&self, n: usize) -> Option<&ZValue> {
        let c = self.cursor.offset(n);
        if let Some(index) = c.try_as_index() {
            Some(&self.buf[index])
        } else {
            None
        }
    }

    pub fn peekn_mut(&mut self, n: isize) -> Option<&mut ZValue> {
        let c = self.cursor.addn(n);
        if let Some(index) = c.try_as_index() {
            Some(&mut self.buf[index])
        } else {
            None
        }
    }

    pub const fn peek_top(&self) -> Option<&ZValue> {
        if self.is_empty() {
            None
        } else {
            self.peekn(0)
        }
    }

    // pub fn peek(&self) -> &ZValue {
    //     let i = self.top();
    //     self.peekn(i as isize)
    //         .expect("Stack peek index out of range! top: {i} | STACK_MAX: {STACK_MAX}")
    // }
    //
    // pub fn peek_mut(&mut self) -> &mut ZValue {
    //     let i = self.top();
    //     self.peekn_mut(i as isize)
    //         .expect("Stack peek index out of range! top: {i} | STACK_MAX: {STACK_MAX}")
    // }

    pub const fn top_index(&self) -> Option<usize> {
        self.cursor.try_as_index()
    }

    pub const fn len(&self) -> usize {
        if let Some(index) = self.cursor.try_as_index() {
            index + 1
        } else {
            0
        }
    }

    pub const fn max(&self) -> usize {
        S
    }

    /// Pushes value onto top of stack. Returns index of item that was pushed.
    pub fn push(&mut self, val: ZValue) -> usize {
        self.cursor = if self.cursor.0 < 0 {
            StackCursor::zero()
        } else {
            self.cursor.inc()
        };
        let i = self.cursor.0 as usize;

        self.buf[i] = val;
        // self.cursor = self.cursor.inc();
        i
    }

    pub fn expect_pop(&mut self) -> ZValue {
        self.pop().expect("Cant pop stack with 0 items!!!")
    }

    pub fn pop(&mut self) -> Option<ZValue> {
        if let Some(i) = self.cursor.try_as_index() {
            let v = self.buf[i].clone();
            self.cursor = self.cursor.dec();

            Some(v)
        } else {
            None
        }
    }
}
