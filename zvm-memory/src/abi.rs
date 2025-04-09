pub unsafe trait Byteable {
    fn from_bytes(bytes: &[u8]) -> &Self;
    fn from_bytes_mut(bytes: &mut [u8]) -> &mut Self;
    fn as_bytes(&self) -> &[u8];
    fn as_bytes_mut(&mut self) -> &mut [u8];

    // unsafe fn cast_from_bytes(bytes: &[u8]) -> &Self
    // where
    //     Self: Sized,
    // {
    //     let bptr = bytes.as_ptr();
    //     let s = bptr.cast::<Self>();
    //     unsafe { s.as_ref().expect("Failed to cast from byte slice!") }
    // }
}

unsafe impl<T> Byteable for T
where
    T: bytemuck::Pod + bytemuck::Zeroable,
{
    fn from_bytes(bytes: &[u8]) -> &Self {
        bytemuck::from_bytes(bytes)
    }

    fn from_bytes_mut(bytes: &mut [u8]) -> &mut Self {
        bytemuck::from_bytes_mut(bytes)
    }

    fn as_bytes(&self) -> &[u8] {
        bytemuck::bytes_of(self)
    }

    fn as_bytes_mut(&mut self) -> &mut [u8] {
        bytemuck::bytes_of_mut(self)
    }
}
