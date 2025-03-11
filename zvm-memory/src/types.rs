use zerocopy_derive::{FromBytes, IntoBytes};

#[derive(Debug, FromBytes, IntoBytes)]
#[repr(C)]
pub struct Struct {}

impl Struct {}

pub struct StructField {}
