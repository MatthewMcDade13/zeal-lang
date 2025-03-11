use zerocopy_derive::{FromBytes, IntoBytes};

#[derive(Debug, Clone, Copy, FromBytes, IntoBytes)]
pub struct StringRange {
    pub slot: usize,
    pub len: usize,
}
