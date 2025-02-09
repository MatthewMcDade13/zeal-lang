pub trait ZealMem: Sized {
    fn as_bytes(&self) -> &[u8];
    fn from_bytes(bytes: &[u8]) -> &Self;
}

pub trait ZealMemMut: ZealMem {
    fn as_bytes_mut(&mut self) -> &mut [u8];
    fn from_bytes_mut(bytes: &mut [u8]) -> &Self;
}

impl<T> ZealMem for T
where
    T: bytemuck::Pod + bytemuck::Zeroable,
{
    fn as_bytes(&self) -> &[u8] {
        bytemuck::bytes_of(self)
    }

    fn from_bytes(bytes: &[u8]) -> &Self {
        bytemuck::from_bytes(bytes)
    }
}

impl<T> ZealMemMut for T
where
    T: bytemuck::Pod + bytemuck::Zeroable,
{
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        bytemuck::bytes_of_mut(self)
    }

    fn from_bytes_mut(bytes: &mut [u8]) -> &Self {
        bytemuck::from_bytes_mut(bytes)
    }
}
