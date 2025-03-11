use std::marker::PhantomData;

use zerocopy::FromBytes;
use zerocopy_derive::{FromBytes, Immutable, KnownLayout};

use crate::{constval::StringRange, typeinfo::TypeInfo};

pub struct ModuleView<'a> {
    header: &'a ModuleHeader,
    imports: &'a Section<Import>,
    exports: &'a Section<Export>,
}

#[derive(Debug, FromBytes, KnownLayout, Immutable)]
#[repr(C)]
pub struct Module {
    pub header: ModuleHeader,
    pub data: [u8],
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes, KnownLayout, Immutable)]
#[repr(transparent)]
pub struct ZealMagic(u32);

impl ZealMagic {
    const MAGIC: u32 = u32::from_le_bytes([b'Z', b'E', b'A', b'L']); //b'Z' + b'E' + b'A' + b'L';

    pub const fn new() -> Self {
        Self(Self::MAGIC)
    }

    pub const fn is_valid(self) -> bool {
        self.0 == Self::MAGIC
    }

    pub const fn as_bytes(self) -> [u8; 4] {
        self.0.to_le_bytes()
    }
}

impl PartialEq<[u8]> for ZealMagic {
    fn eq(&self, other: &[u8]) -> bool {
        self.is_valid()
            && other.len() == 4
            && self.0 == u32::from_le_bytes([other[0], other[1], other[2], other[3]])
    }
}

impl PartialEq<[u8; 4]> for ZealMagic {
    fn eq(&self, other: &[u8; 4]) -> bool {
        let n = u32::from_le_bytes(*other);
        self.is_valid() && n == self.0
    }
}
impl PartialEq<u32> for ZealMagic {
    fn eq(&self, other: &u32) -> bool {
        self.is_valid() && *other == self.0
    }
}

#[derive(Debug, Clone, Copy, FromBytes, KnownLayout, Immutable)]
#[repr(transparent)]
pub struct SectionId(u32);

impl SectionId {
    pub const fn imports() -> Self {
        Self(SectionType::Imports as u32)
    }

    pub const fn exports() -> Self {
        Self(SectionType::Exports as u32)
    }

    pub const fn constant_pool() -> Self {
        Self(SectionType::ConstantPool as u32)
    }

    pub const fn typedefs() -> Self {
        Self(SectionType::TypeDefs as u32)
    }

    pub const fn functionn_defs() -> Self {
        Self(SectionType::FunctionDefs as u32)
    }

    pub const fn symbol_table() -> Self {
        Self(SectionType::SymbolTable as u32)
    }

    pub const fn bytecode() -> Self {
        Self(SectionType::Bytecode as u32)
    }
}

#[derive(Debug, Clone, Copy, zerocopy_derive::IntoBytes)]
#[repr(u32)]
pub enum SectionType {
    ModuleHeader = 0xFF,
    Imports,
    Exports,
    SymbolTable,
    ConstantPool,
    TypeDefs,
    FunctionDefs,
    Bytecode,
}

#[derive(Debug, Clone, Copy, FromBytes, KnownLayout, Immutable)]
#[repr(transparent)]
pub struct ModuleVersion([u8; 4]);

impl ModuleVersion {
    pub const fn new(major: u8, minor: u8, patch: u8, build: u8) -> Self {
        Self([major, minor, patch, build])
    }

    pub const fn from_u32(version: u32) -> Self {
        Self(version.to_le_bytes())
    }

    pub const fn to_u32(self) -> u32 {
        u32::from_le_bytes(self.0)
    }

    pub const fn maojor(&self) -> u8 {
        self.0[0]
    }
    pub const fn minor(&self) -> u8 {
        self.0[1]
    }
    pub const fn patch(&self) -> u8 {
        self.0[2]
    }
    pub const fn build(&self) -> u8 {
        self.0[3]
    }
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes, KnownLayout, Immutable)]
#[repr(C)]
pub struct ModuleHeader {
    magic: ZealMagic,
    id: SectionId,
    version: ModuleVersion,
    imports: SectionLoc,
    exports: SectionLoc,
    symbols: SectionLoc,
    constvals: SectionLoc,
    typedefs: SectionLoc,
    func_defs: SectionLoc,
    bytecode: SectionLoc,
    total_size: u64,
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes)]
#[repr(C)]
pub struct Import {
    name: StringRange,
    path: StringRange,
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes)]
#[repr(C)]
pub struct Export {
    name: StringRange,
    typeinfo: TypeInfo,
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes, KnownLayout, Immutable)]
#[repr(C)]
pub struct SectionLoc {
    pub begin: usize,
    pub end: usize,
}

#[derive(Debug, zerocopy_derive::FromBytes)]
#[repr(C)]
pub struct Section<T: zerocopy::FromBytes> {
    id: SectionId,
    len: usize,
    _phantom: PhantomData<T>,
    data: [u8],
}
