use std::cell::Cell;

pub mod buf;
pub mod mem;
pub mod rune;
pub mod slab;

#[inline]
pub fn clamp<T>(lower: T, val: T, higher: T) -> T
where
    T: Ord,
{
    std::cmp::max(lower, std::cmp::min(val, higher))
}

pub fn append_byte_slice(dst: &mut Vec<u8>, src: &[u8]) {
    let i = dst.len() - 1;
    let end = i + src.len();
    dst.resize(dst.len() + src.len(), 0);
    assert!(end < dst.len());
    let dst = &mut dst[i..=end];

    copy_slice_into(dst, src);
}

#[inline]
pub fn copy_slice_into<T>(dst: &mut [T], src: &[T])
where
    T: Copy,
{
    let n = std::cmp::min(dst.len(), src.len());
    dst[..n].copy_from_slice(&src[..n])
}

#[inline]
pub fn clone_slice_into<T>(dst: &mut [T], src: &[T])
where
    T: Clone,
{
    let n = std::cmp::min(dst.len(), src.len());
    dst[..n].clone_from_slice(&src[..n])
}

pub unsafe fn array_from_raw<const S: usize, T>(ptr: *const T) -> [T; S]
where
    T: Default + Copy,
{
    let mut i = 0;
    let mut result = [T::default(); S];
    while i < S {
        let elem = ptr.add(i).as_ref().expect("null ptr deref!!!");
        result[i] = *elem;
        i += 1;
    }
    result
}

#[inline]
pub fn array_from_slice<const S: usize, T>(sl: &[T]) -> [T; S]
where
    T: Default + Copy,
{
    array_from_slice_with(sl, T::default())
}

pub fn array_from_slice_with<const S: usize, T>(sl: &[T], default_val: T) -> [T; S]
where
    T: Copy,
{
    let mut arr = [default_val; S];
    copy_slice_into(&mut arr, sl);
    arr
}

/// Unique ID Assignment. Simply increments a u64 number.
/// This means that u64::MAX is the limit to the number of
/// IDs 'generated'
#[derive(Debug, Clone)]
pub struct IDGen(Cell<u64>);

impl IDGen {
    pub const fn new() -> Self {
        Self(Cell::new(0))
    }

    pub fn try_gen(&self) -> Option<u64> {
        let n = self.0.get();
        if n >= u64::MAX - 1 {
            None
        } else {
            let n = n + 1;
            self.0.set(n);
            Some(n)
        }
    }

    pub fn gen_or_reset(&self) -> u64 {
        if let Some(n) = self.try_gen() {
            n
        } else {
            self.reset();
            0
        }
    }

    pub fn reset(&self) {
        self.0.set(0);
    }

    pub unsafe fn gen_unchecked(&self) -> u64 {
        let n = self.0.get() + 1;
        self.0.set(n);
        n
    }
}
