use std::{fmt::Display, marker::PhantomData, path::Path, rc::Rc};

use anyhow::{bail, ensure, Context};
use bytes::Bytes;

/// ZealVM Bytecode Object Module format
/// A.K.A Zeal Object Module (ZOM) :: .zom file extension
/**
*    =====================================
*    === Bytecode format memory layout ===
*    =====================================
*
*    +----------------------------+
*    |       Module Header        |
*    |  (e.g., magic, version)    |
*    +----------------------------+
*    |      String Pool           |
*    |                            |
*    +----------------------------+
*    |      Symbol Table          |
*    |   (import/export info)     |
*    +----------------------------+
*    |     Type/Structure Info    |
*    | (optional custom metadata) |
*    +----------------------------+
*    |      Function Table        |
*    | (function descriptors)     |
*    +----------------------------+
*    |   Bytecode Instructions    |
*    | (function bodies, etc.)    |
*    +----------------------------+
*
*
*
*
* */

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable, PartialEq, Eq)]
#[repr(C)]
pub struct ByteChunk {
    pub begin: u32,
    pub size_bytes: u32,
}

impl ByteChunk {
    const PLACEHOLDER_IMPORT: u32 = u32::MAX - 1;

    pub const fn header() -> Self {
        Self {
            begin: 0,
            size_bytes: ObjHeader::SIZE_BYTES as u32,
        }
    }

    pub const PLACEHOLDER: Self = Self {
        begin: 0,
        size_bytes: Self::PLACEHOLDER_IMPORT,
    };

    pub const fn placeholder() -> Self {
        Self::PLACEHOLDER
    }

    pub const fn new(begin: usize, len_bytes: usize) -> Self {
        let begin = begin as u32;
        let len_bytes = len_bytes as u32;
        Self {
            begin,
            size_bytes: len_bytes,
        }
    }

    pub fn is_import(&self) -> bool {
        self == &Self::PLACEHOLDER
    }

    pub const fn is_emtpy(&self) -> bool {
        self.begin == 0 && self.size_bytes == 0
    }
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct ZealId(u32);

impl ZealId {
    pub const MAGIC: Self = Self(0x7A65616C);

    pub fn verify(n: u32) -> anyhow::Result<()> {
        let _ = Self::new(n)?;
        Ok(())
    }

    pub fn verify_bytes(buf: &[u8]) -> anyhow::Result<()> {
        let _ = Self::from_bytes(buf)?;
        Ok(())
    }

    pub fn new(n: u32) -> anyhow::Result<Self> {
        if n == ZealId::MAGIC.0 {
            Ok(Self::MAGIC)
        } else {
            bail!(
                "Invalid magic number for Zeal Module Object. Expected: 0x7A65616C, got: 0x{n:X}"
            );
        }
    }

    pub fn from_bytes(bytes: &[u8]) -> anyhow::Result<Self> {
        if bytes.len() < 4 {
            bail!(
                "Invalid bytes count for Zeal Module Object Magic Number!. Expected 4 bytes, got: {}",
                bytes.len(),
            );
        } else {
            let bs = [bytes[0], bytes[1], bytes[2], bytes[3]];
            let n = u32::from_le_bytes(bs);
            Self::new(n)
        }
    }
}

#[derive(
    Debug, Clone, Copy, Default, bytemuck::Zeroable, bytemuck::Pod, PartialEq, Eq, PartialOrd, Ord,
)]
#[repr(C)]
pub struct ZOMVersion {
    pub number: u8,
    pub major: u8,
    pub minor: u8,
    pub patch: u8,
}

impl Display for ZOMVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            number,
            major,
            minor,
            patch,
        } = *self;
        write!(f, "v{number}.{major}.{minor}.{patch}")
    }
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct ObjHeader {
    pub magic: ZealId,
    pub version: ZOMVersion,

    pub name_loc: ByteChunk,

    pub strings_loc: ByteChunk,
    pub strings_count: u32,

    pub symbols_loc: ByteChunk,
    pub symbols_count: u32,

    pub typedefs_loc: ByteChunk,
    pub typedefs_count: u32,

    pub type_fields_loc: ByteChunk,
    pub type_fields_count: u32,

    pub functions_loc: ByteChunk,
    pub functions_count: u32,

    pub func_params_loc: ByteChunk,
    pub func_params_count: u32,

    pub bytecode_loc: ByteChunk,
}

impl ObjHeader {
    pub const SIZE_BYTES: usize = std::mem::size_of::<Self>();
    pub const STRINGS_DELIM: char = '|';
    pub const BYTECODE_BLOCK_DELIM_BYTES: [u8; 4] = [0, 0, 0xDE, 0xAD];
    pub const BYTECODE_BLOCK_DELIM: u32 = u32::from_be_bytes(Self::BYTECODE_BLOCK_DELIM_BYTES);

    pub const fn module_size_bytes(&self) -> usize {
        let mut size = std::mem::size_of::<u32>() as u32;
        size += self.strings_loc.size_bytes;
        size += self.symbols_loc.size_bytes;
        size += self.typedefs_loc.size_bytes;
        size += self.type_fields_loc.size_bytes;
        size += self.functions_loc.size_bytes;
        size += self.func_params_loc.size_bytes;
        size += self.bytecode_loc.size_bytes;

        // NOTE: (1/18/2025) :: Each block of bytecode is related to exactly 1 function inside its
        // own object module. At the end of each bytecode block is
        // a byte/number delimeter, to help with separation and
        // maybe even make it harder for a function to fallthrough to the next function.
        size += std::mem::size_of_val(&Self::BYTECODE_BLOCK_DELIM) as u32 * self.functions_count;
        size as usize
    }

    /// Casts a byte slice to an ObjectHeader reference. Does not verify
    /// anything before cast, will possibly panic if there are any
    /// mismatch in size and/or alignment
    pub fn cast_from_bytes(buf: &[u8]) -> &Self {
        let slice = &buf[..Self::SIZE_BYTES];
        let s = bytemuck::from_bytes(slice);
        s
    }

    /// Creates a typed view into the Object Module buffer.
    /// Checks that buffer is proper size before casting into Header
    pub fn view_from_bytes(buf: &[u8]) -> anyhow::Result<&Self> {
        ensure!(
            buf.len() >= Self::SIZE_BYTES,
            "buffer is too small to parse to Bytecode Object Header"
        );
        let s = Self::cast_from_bytes(buf);
        Ok(s)
    }

    #[inline]
    pub fn from_bytes(buf: &[u8]) -> anyhow::Result<Self> {
        let view = Self::view_from_bytes(buf)?;
        Ok(*view)
    }
}

#[derive(Debug, Clone)]
pub struct StringPool(bytes::Bytes);

impl StringPool {
    pub const DELIM: char = '|';
}

#[derive(Debug, Clone)]
pub struct StringPoolView<'a> {
    pub pool: &'a str,
    pub strings: Vec<&'a str>,
}

impl<'a> StringPoolView<'a> {
    pub fn split(&self) -> Vec<&'a str> {
        self.pool.split(StringPool::DELIM).collect::<Vec<&'a str>>()
    }
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct SymbolInfo {
    pub value_ty: ValueType,
    pub location: SymbolLocation,
}

impl SymbolInfo {
    pub const fn is_import(&self) -> bool {
        self.location.is_import()
    }
    pub const fn is_module_local(&self) -> bool {
        !self.is_import()
    }
}

/// Value Type Primitive of symbol
/// NOTE: Was going to have this be a u8, but making it a u32 for
/// bytemuck and alignment reasons.
#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct ValueType(u16);

impl ValueType {
    pub const DYNAMIC: Self = Self(0);
    pub const BOOL: Self = Self(1);
    pub const BYTE: Self = Self(2);
    /// Signed byte
    pub const CHAR: Self = Self(3);
    pub const INT: Self = Self(4);
    pub const UINT: Self = Self(5);
    pub const FLOAT: Self = Self(6);
    pub const STRING: Self = Self(7);
    pub const ARRAY: Self = Self(8);
    pub const STRUCT: Self = Self(9);
    pub const FUNC: Self = Self(10);
    pub const FUNC_NATIVE: Self = Self(11);
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct SymbolLocation(u16);

impl SymbolLocation {
    pub const IMPORT: Self = Self(1);
    pub const LOCAL: Self = Self(2);
    pub const VM_NATIVE: Self = Self(3);
    pub const EXTERN_C: Self = Self(4);
    pub const GLOBAL: Self = Self(5);

    const END_RANGE: Self = Self(6);

    pub const fn new(n: u8) -> Option<Self> {
        let s = match n {
            1 => Self::IMPORT,
            2 => Self::LOCAL,
            3 => Self::VM_NATIVE,
            4 => Self::EXTERN_C,
            5 => Self::GLOBAL,
            _ => return None,
        };
        Some(s)
    }

    pub const fn is_import(&self) -> bool {
        self.0 == Self::IMPORT.0
    }

    pub const fn is_module_local(&self) -> bool {
        !self.is_import()
    }

    pub const fn is_valid(&self) -> bool {
        self.0 > 0 && self.0 < Self::END_RANGE.0
    }

    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct TypeFieldInfo {
    pub parent_entry: u32,
    /// Position of field in struct. i.e. the first field in struct definiton, is position 0
    pub position: u32,
    pub name_loc: ByteChunk,
    pub byte_offset: u32,

    pub value_ty: ValueType,
    pub is_public: u16,

    pub type_symbol_loc: u32,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct TypeEntry {
    pub name_loc: ByteChunk,
    pub symbol_loc: ByteChunk,
    pub size_bytes: u32,

    pub fields_loc: ByteChunk,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct FuncEntry {
    pub name_loc: ByteChunk,
    pub bytecode_loc: ByteChunk,
    pub params_loc: ByteChunk,
    pub params_count: u32,
}
impl FuncEntry {
    /// Gets size of full function definiton in bytes. Includes params and bytecode instructions
    pub const fn size_bytes(&self) -> usize {
        let param_len = self.params_loc.size_bytes as usize;
        let bytecode_len = self.bytecode_loc.size_bytes as usize;
        param_len + bytecode_len
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FuncView<'a> {
    pub header: &'a FuncEntry,
    pub name: &'a str,
    pub params: &'a [FuncParam],
    pub return_ty: &'a TypeEntry,
    pub bytecode: &'a [u8],
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct FuncParam {
    pub parent_entry: u32,
    pub position: u32,
    pub value_ty: SymbolInfo,
    pub type_symbol_loc: u32,
    pub name_loc: ByteChunk,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct SymbolEntry {
    pub name_loc: ByteChunk,
    pub info: SymbolInfo,
    pub value_loc: ByteChunk,
}

impl SymbolEntry {
    pub const fn is_module_import(&self) -> bool {
        self.info.is_module_local()
    }

    pub const fn is_import(&self) -> bool {
        self.info.is_import()
    }
}

#[derive(Debug, Clone)]
pub struct ObjModuleView<'a> {
    pub header: &'a ObjHeader,
    pub string_pool: Rc<[&'a str]>,
    pub symbol_table: &'a [SymbolEntry],
    pub typedef_table: &'a [TypeEntry],
    pub type_fields: &'a [TypeFieldInfo],
    pub function_table: &'a [FuncEntry],
    pub func_params: &'a [FuncParam],
    pub bytecode: &'a [u8],
}

// impl<'a, 'b: 'a> From<&'b Module> for ModuleView<'a> {
//     fn from(value: &'b Module) -> Self {
//         let header = ObjHeader::view_from_bytes(value.buf.as_ref());
//         let string_pool =
//     }
// }

/// Owner struct for a single bytecode Object Module
/// Used mainly for reading modules. For writing modules
/// @see [ModuleBuilder]
#[derive(Debug, Clone)]
pub struct ObjModule {
    buf: bytes::Bytes,
}

impl ObjModule {
    pub fn from_file(path: &Path) -> anyhow::Result<Self> {
        let buf = std::fs::read_to_string(path)?;
        Self::from_memory(buf.into())
    }

    pub fn from_memory(mem: Vec<u8>) -> anyhow::Result<Self> {
        let buf = bytes::Bytes::from(mem);
        let header = ObjHeader::view_from_bytes(&buf)?;
        ZealId::verify(header.magic.0)?;
        let s = Self { buf };
        Ok(s)
    }

    pub fn as_view(&self) -> anyhow::Result<ObjModuleView> {
        let header: &ObjHeader = self.section_view(&ByteChunk::header());
        let string_pool = {
            let pool = self.section_as_utf8_str(&header.strings_loc)?;
            let strs = pool
                .split(StringPool::DELIM)
                .take_while(|s| s.len() > 0)
                .collect::<Rc<[&str]>>();
            strs
        };
        let symbol_table: &[SymbolEntry] = self.section_view_list(&header.symbols_loc);
        let typedef_table: &[TypeEntry] = self.section_view_list(&header.typedefs_loc);
        let type_fields: &[TypeFieldInfo] = self.section_view_list(&header.type_fields_loc);
        let function_table: &[FuncEntry] = self.section_view_list(&header.functions_loc);
        let func_params: &[FuncParam] = self.section_view_list(&header.func_params_loc);
        let bytecode = {
            // NOTE: We could just take the range of bytecode_loc.begin.. but
            // we aren't doing that just in case there is another Object Module
            // immediately after this one in memeory.
            // We can also check that we dont encounter a magic header value at any time inside
            // the object module itself.
            let begin = header.bytecode_loc.begin as usize;
            let end = begin + header.bytecode_loc.size_bytes as usize;
            &self.buf[begin..end]
        };

        let mv = ObjModuleView {
            header,
            string_pool,
            symbol_table,
            typedef_table,
            type_fields,
            function_table,
            func_params,
            bytecode,
        };
        Ok(mv)
    }

    fn section_as_utf8_str(&self, chunk: &ByteChunk) -> anyhow::Result<&str> {
        let begin = chunk.begin as usize;
        let end = begin + chunk.size_bytes as usize;
        let bytes = &self.buf[begin..end];
        if let Ok(s) = std::str::from_utf8(bytes) {
            anyhow::Result::Ok(s)
        } else {
            bail!("Failed to convert byte slice to a UTF8 String slice! Got: {bytes:#?}");
        }
    }

    fn section_view_list<T>(&self, chunk: &ByteChunk) -> &[T]
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let begin = chunk.begin as usize;
        let end = begin + chunk.size_bytes as usize;
        let bytes = &self.buf[begin..end];
        bytemuck::cast_slice(bytes)
    }

    fn section_view<T>(&self, chunk: &ByteChunk) -> &T
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let begin = chunk.begin as usize;
        let end = begin + chunk.size_bytes as usize;
        let bytes = &self.buf[begin..end];
        bytemuck::from_bytes(bytes)
    }
}
