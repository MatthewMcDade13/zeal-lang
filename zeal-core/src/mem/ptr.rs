use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut, Index, IndexMut},
    ptr::NonNull,
};

pub const KB: usize = 1024;
pub const MB: usize = KB * KB;
pub const GB: usize = KB * KB * KB;
pub const MB1: usize = MB * 1;
pub const MB2: usize = MB * 2;
pub const MB3: usize = MB * 3;
pub const MB4: usize = MB * 4;

pub trait BufAlloc: Index<usize> {
    fn alloc<T>(val: T) -> IdPtr<T, Self>
    where
        Self: Sized;
}

pub struct IndexRef<'a, T, Parent: BufAlloc> {
    id: usize,
    alloc: &'a mut Parent,
    _phantom: PhantomData<T>,
}

pub struct IdPtr<T, Alloc: BufAlloc> {
    id: usize,
    alloc: NonNull<Alloc>,
    _phantom: PhantomData<T>,
}

impl<'a, T, P> Deref for IndexRef<'a, T, P>
where
    P: BufAlloc,
{
    type Target = <P as Index<usize>>::Output;

    fn deref(&self) -> &Self::Target {
        &self.alloc[self.id]
    }
}

impl<'a, T, P> DerefMut for IndexRef<'a, T, P>
where
    P: BufAlloc + IndexMut<usize>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.alloc[self.id]
    }
}

impl<T, P> IdPtr<T, P>
where
    P: BufAlloc,
{
    pub fn into_ref<'a>(mut self) -> IndexRef<'a, T, P> {
        IndexRef {
            id: self.id,
            alloc: unsafe { self.alloc.as_mut() },
            _phantom: PhantomData,
        }
    }
}

impl<T, P> Deref for IdPtr<T, P>
where
    P: BufAlloc,
{
    type Target = <P as Index<usize>>::Output;

    fn deref(&self) -> &Self::Target {
        let a = unsafe { self.alloc.as_ref() };
        &a[self.id]
    }
}

impl<T, P> DerefMut for IdPtr<T, P>
where
    P: BufAlloc + IndexMut<usize>,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        let a = unsafe { self.alloc.as_mut() };
        &mut a[self.id]
    }
}
