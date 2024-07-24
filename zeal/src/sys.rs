pub fn copy_slice_into<T>(dst: &mut [T], src: &[T])
where
    T: Copy,
{
    let n = std::cmp::min(dst.len(), src.len());
    dst[..n].copy_from_slice(&src[..n])
}

pub fn clone_slice_into<T>(dst: &mut [T], src: &[T])
where
    T: Clone,
{
    let n = std::cmp::min(dst.len(), src.len());
    dst[..n].clone_from_slice(&src[..n])
}
