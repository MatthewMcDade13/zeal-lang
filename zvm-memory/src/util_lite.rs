#[inline]
pub fn clamp<T>(lower: T, val: T, higher: T) -> T
where
    T: Ord,
{
    core::cmp::max(lower, core::cmp::min(val, higher))
}

#[inline]
pub fn copy_slice_into<T>(dst: &mut [T], src: &[T])
where
    T: Copy,
{
    let n = core::cmp::min(dst.len(), src.len());
    dst[..n].copy_from_slice(&src[..n])
}

#[inline]
pub fn clone_slice_into<T>(dst: &mut [T], src: &[T])
where
    T: Clone,
{
    let n = core::cmp::min(dst.len(), src.len());
    dst[..n].clone_from_slice(&src[..n])
}
