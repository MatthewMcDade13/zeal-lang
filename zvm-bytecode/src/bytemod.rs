use core::ops::{Deref, DerefMut};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ZealMagic(u32);

impl Default for ZealMagic {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq<u32> for ZealMagic {
    fn eq(&self, other: &u32) -> bool {
        self.0 == *other
    }
}

impl ZealMagic {
    const MAGIC: u32 = u32::from_be_bytes([b'Z', b'E', b'A', b'L']);
    pub const fn new() -> Self {
        Self(Self::MAGIC)
    }
}

#[repr(C)]
pub struct ModHeader {
    magic: ZealMagic,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u32)]
pub enum SectionType {
    /// Unique string identifiers for Type, function and variable symbols
    Runes = 0,
    /// For now, primitive integers and floats, and strings. Eventually Structs and arrays of T as
    /// well
    Constants,
    /// Type information,
    TypeDefs,
    /// Funcion prototypes/signatures
    FuncProtos,

    /// Bytecode for each function in this module, separated by a soon to be chosen delimiter
    FuncBytecode,
    /// Bytecode used for lazy eval
    InitBytecode,
    /// Extended Sections
    #[default]
    Ext,
}

/// Aboslute index/address, relative to start of main module memory
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct AddrAbsolute(u32);

impl Deref for AddrAbsolute {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for AddrAbsolute {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Index/address that is an offset from some other addr.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct AddrIndirect(u32);

impl Deref for AddrIndirect {
    type Target = u32;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for AddrIndirect {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[repr(C)]
pub struct SectionInfo {
    pub ty: SectionType,
    pub begin: u32,
    pub len: u32,
}

impl SectionInfo {
    pub const fn end(&self) -> usize {
        (self.begin + self.len) as usize
    }
}
