use std::{marker::PhantomData, path::Path, rc::Rc};

use anyhow::{bail, ensure, Context};
use bytes::Bytes;

/// ZealVM Bytecode Object format
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
    pub len_bytes: u32,
}

impl ByteChunk {
    const PLACEHOLDER_IMPORT: u32 = u32::MAX - 1;

    pub const PLACEHOLDER: Self = Self {
        begin: 0,
        len_bytes: Self::PLACEHOLDER_IMPORT,
    };

    pub const fn placeholder() -> Self {
        Self::PLACEHOLDER
    }

    pub const fn new(begin: usize, len_bytes: usize) -> Self {
        let begin = begin as u32;
        let len_bytes = len_bytes as u32;
        Self { begin, len_bytes }
    }

    pub const fn is_import(&self) -> bool {
        self == Self::PLACEHOLDER
    }

    pub const fn is_emtpy(&self) -> bool {
        self.begin == 0 && self.len_bytes == 0
    }
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct ZealId(u32);

impl ZealId {
    pub const MAGIC: Self = Self(0x7A65616C);

    pub const fn verify(n: u32) -> bool {
        n == Self::MAGIC.0
    }

    pub fn verify_bytes(bytes: &[u8]) -> bool {
        if bytes.len() < 4 {
            false
        } else {
            let bs = [bytes[0], bytes[1], bytes[2], bytes[3]];
            let n = u32::from_le_bytes(bs);
            Self::verify(n)
        }
    }
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct ObjHeader {
    pub magic: ZealId,

    pub imports_loc: ByteChunk,
    pub imports_len: u32,

    pub exports_loc: ByteChunk,
    pub exports_len: u32,

    pub strings_loc: ByteChunk,
    pub strings_len: u32,

    pub symbols_loc: ByteChunk,
    pub symbols_len: u32,

    pub typedefs_loc: ByteChunk,
    pub typedefs_loc: u32,

    pub type_fields_loc: ByteChunk,
    pub type_fields_len: u32,

    pub constants_loc: ByteChunk,
    pub constants_len: u32,

    pub functions_loc: ByteChunk,
    pub functions_len: u32,

    pub bytecode_loc: ByteChunk,
}

impl ObjHeader {
    pub const SIZE_BYTES: usize = std::mem::size_of::<Self>();

    pub const fn module_size_bytes(&self) -> usize {
        let mut size = std::mem::size_of::<u32>() as u32;
        size += self.imports_loc.len_bytes;
        size += self.exports_loc.len_bytes;
        size += self.symbols_loc.len_bytes;
        size += self.typedefs_loc.len_bytes;
        size += self.constants_loc.len_bytes;
        size += self.functions_loc.len_bytes;
        size as usize
    }

    pub fn from_bytes(buf: &[u8]) -> anyhow::Result<Self> {
        ensure!(
            buf.len() >= Self::SIZE_BYTES,
            "buffer is too small to parse to Bytecode Object Header"
        );
        let slice = &buf[..Self::SIZE_BYTES];
        let s = bytemuck::from_bytes(slice);
        Ok(*s)
    }
}

#[derive(Debug, Clone)]
pub struct ObjectView<'a> {
    pub header: &'a ObjHeader,
    pub string_pool: &'a str,
    pub symbols: &'a [SymbolEntry],
    pub typedefs: &'a [TypeEntry],
    pub type_fields: &'a [TypeFieldInfo],
    pub functions: &'a [FuncEntry],
    pub bytecode: &'a [u8],
}

impl<'a> ObjectView<'a> {
    pub fn new(buf: &'a [u8]) -> anyhow::Result<Self> {
        ensure!(
            buf.len() >= ObjHeader::SIZE_BYTES,
            "Given buffer to small to interpret as Bytecode Module Object"
        );
        let header: &ObjHeader = bytemuck::from_bytes(&buf[..ObjHeader::SIZE_BYTES]);
        ensure!(buf.len() >= header.module_size_bytes());
        let string_pool: &str = Self::section_list(buf, header.strings_loc);
        let symbols: &[SymbolEntry] = Self::section_list(buf, header.symbols_loc);
        let typedefs: &[TypeEntry] = Self::section_list(buf, header.typedefs_loc);
        let type_fields: &[TypeFieldInfo] = Self::section_list(buf, header.type_fields_loc);
        let functions: &[FuncEntry] = Self::section_list(buf, header.functions_loc);
        let bytecode: &[u8] = &buf[header.bytecode_loc..];

        let s = Self {
            header,
            string_pool,
            symbols,
            typedefs,
            type_fields,
            functions,
            bytecode,
        };
        Ok(s)
    }

    pub fn section_list<T>(buf: &[u8], range: ByteChunk) -> &[T]
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let begin = range.begin as usize;
        let end = begin + range.len_bytes as usize;
        let slice = &buf[begin..end];
        let vals: &[T] = bytemuck::cast_slice(slice);
        vals
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ConstantView<'a> {
    typeinfo: &'a TypeInfo,
    value_bytes: &'a [u8],
}

#[derive(Debug, Clone)]
pub struct ObjectBuf {
    pub header: ObjHeader,
    pub buf: Rc<[u8]>,
}

#[derive(Debug, Clone)]
pub struct StringPool(bytes::Bytes);

impl StringPool {
    pub const DELIM: char = '|';
}

#[derive(Debug, Clone, Copy)]
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

// #[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
// #[repr(transparent)]
// pub struct SymbolInfo(u8);
//
// impl SymbolInfo {
//     pub const fn new(vis: Visibility, ty: LookupType) -> Self {
//         let v = vis as u8;
//         let t = ty as u8;
//         let t = t << 4;
//         Self(v | t)
//     }
// }

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct TypeFieldInfo {
    pub parent_entry: u32,
    /// Position of field in struct. i.e. the first field in struct definiton, is position 0
    pub position: u32,
    // pub visibility: Visibility,
    pub name_loc: ByteChunk,
    pub byte_offset: u32,
    pub value_ty: ValueType,
    pub type_symbol_loc: u32,
    /// Only valid values are 0 (for private) or 1 (for public)
    /// needs to be u32 for alignment reasons...
    pub is_public: u32,
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
}
impl FuncEntry {
    /// Gets size of full function definiton in bytes. Includes params and bytecode instructions
    pub const fn size_bytes(&self) -> usize {
        let param_len = self.params_loc.len_bytes as usize;
        let bytecode_len = self.bytecode_loc.len_bytes as usize;
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
    pub position: u32,
    pub value_ty: ValueType,
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
pub struct Module {
    buf: bytes::Bytes,
}

impl Module {
    pub const MAGIC_BEGIN: usize = 0;
    pub const MAGIC_END: usize = 4;
    pub const HEADER_END: usize = Self::MAGIC_END + std::mem::size_of::<ObjHeader>();

    pub fn from_file(path: &Path) -> anyhow::Result<Self> {
        let buf = std::fs::read_to_string(path)?;
        Self::from_memory(buf.into())
    }

    pub fn from_memory(mem: Vec<u8>) -> anyhow::Result<Self> {
        let buf = bytes::Bytes::from(mem);
        let mut c = 4;
        let valid_magic = ZealId::verify_bytes(&buf[..c]);
        if !valid_magic {
            bail!("Invalid first 4 magic bytes. Expected 'ZEAL'");
        }
        c += 1;

        let header_end = c + std::mem::size_of::<ObjHeader>();
        let header_buf = buf.slice(c..header_end);

        todo!()
    }
}

pub struct ModuleView<'a> {
    header: ObjHeader,
    buf: &'a [u8],
}

impl<'a> ModuleView<'a> {
    pub fn new(buf: &'a [u8]) -> anyhow::Result<Self> {
        let header = ObjHeader::from_bytes(buf)?;
        let s = Self { header, buf };
        Ok(s)
    }

    pub const fn header(&self) -> &ObjHeader {
        &self.header
    }

    pub fn section_list<T>(&self, range: ByteChunk) -> &[T]
    where
        T: bytemuck::Pod + bytemuck::Zeroable,
    {
        let begin = range.begin as usize;
        let end = begin + range.len_bytes as usize;
        let slice = &self.buf[begin..end];
        let vals: &[T] = bytemuck::cast_slice(slice);
        vals
    }

    #[inline]
    pub fn imports(&self) -> &[Import] {
        self.section_list(self.header.imports_loc)
    }

    #[inline]
    pub fn exports(&self) -> &[Export] {
        self.section_list(self.header.exports_loc)
    }

    #[inline]
    pub fn symbols(&self) -> &[SymbolEntry] {
        self.section_list(self.header.symbols_loc)
    }

    #[inline]
    pub fn typeinfo(&self) -> &[TypeInfo] {
        self.section_list(self.header.typedefs_loc)
    }

    #[inline]
    pub fn struct_fields(&self) -> &[TypeFieldInfo] {
        self.section_list(self.header.type_fields_loc)
    }

    pub fn get_func_view(&self, index: usize) -> Option<FuncView> {
        if index > self.header.functions_len as usize {
            None
        } else {
            let begin = self.header.functions_loc.begin as usize;
            let end = begin + std::mem::size_of::<FuncHeader>();
            let header = &self.buf[begin..end];
            let header: &FuncHeader = bytemuck::from_bytes(header);

            let begin = header.params_loc.begin as usize;
            let end = begin + header.params_loc.len_bytes as usize;
            let params = &self.buf[begin..end];
            let params: &[FuncParam] = bytemuck::cast_slice(params);

            let begin = header.name_loc.begin as usize;
            let end = header.name_loc.len_bytes as usize;
            let name = &self.buf[begin..end];
            let name = std::str::from_utf8(name)
                .expect("Failed to convert byte slice: {name:?} into string!");

            let begin = header.return_ty_loc.begin as usize;
            let end = begin + header.return_ty_loc.len_bytes as usize;
            let return_ty = &self.buf[begin..end];
            let return_ty: &TypeInfo = bytemuck::from_bytes(return_ty);

            let begin = header.opcodes_loc.begin as usize;
            let end = begin + header.opcodes_loc.len_bytes as usize;
            let bytecode = &self.buf[begin..end];

            let fv = FuncView {
                header,
                name,
                params,
                return_ty,
                bytecode,
            };

            Some(fv)
        }
    }
}
//
// #[derive(Debug, Clone, Copy)]
// struct FuncIter<'a> {
//     iter: usize,
//     end: usize,
//     module_buf: &'a [u8],
// }
//
// impl<'a> Iterator for FuncIter<'a> {
//     type Item = FuncView<'a>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         if self.iter >= self.end {
//             None
//         } else {
//             let header = unsafe {
//                 self.iter
//                     .cast::<FuncHeader>()
//                     .as_ref()
//                     .expect("Invalid pointer cast!")
//             };
//
//             let begin = header.params_loc.begin as usize;
//             let end = begin + header.params_loc.len_bytes as usize;
//             let params = &self.buf[begin..end];
//             let params: &[FuncParam] = bytemuck::cast_slice(params);
//
//             let begin = header.name_loc.begin as usize;
//             let end = header.name_loc.len_bytes as usize;
//             let name = &self.buf[begin..end];
//             let name = std::str::from_utf8(name)
//                 .expect("Failed to convert byte slice: {name:?} into string!");
//
//             let begin = header.return_ty_loc.begin as usize;
//             let end = begin + header.return_ty_loc.len_bytes as usize;
//             let return_ty = &self.buf[begin..end];
//             let return_ty: &TypeInfo = bytemuck::from_bytes(return_ty);
//
//             let begin = header.opcodes_loc.begin as usize;
//             let end = begin + header.opcodes_loc.len_bytes as usize;
//             let bytecode = &self.buf[begin..end];
//
//             let fv = FuncView {
//                 header,
//                 name,
//                 params,
//                 return_ty,
//                 bytecode,
//             };
//         }
//     }
// }

fn bytes_to_u32(buf: &[u8]) -> anyhow::Result<u32> {
    if buf.len() != std::mem::size_of::<u32>() {
        bail!("Failed to convert buffer: {buf:?} to u32!");
    } else {
        let bytes = buf
            .try_into()
            .expect("Failed to convert byte slice: {bytes:?} to u32!");
        let n = u32::from_le_bytes(bytes);
        Ok(n)
    }
}

fn bytes_to_u32_pair(buf: &[u8]) -> anyhow::Result<(u32, u32)> {
    const SIZE_U32: usize = std::mem::size_of::<u32>();
    if buf.len() != std::mem::size_of::<u64>() {
        bail!("Failed to convert buffer: {buf:?} to u64!");
    } else {
        let first = &buf[..SIZE_U32];
        let second = &buf[SIZE_U32..SIZE_U32 + 4];
        let a = bytes_to_u32(first)?;
        let b = bytes_to_u32(second)?;
        Ok((a, b))
    }
}

fn parse_magic(buf: &[u8]) -> anyhow::Result<u32> {
    let magic = bytes_to_u32(&buf[..4]).context("Failed to get next u32 in byte buffer")?;
    let valid = ZealId::verify(magic);
    if !valid {
        bail!("Invalid magic header bytes. Got: {magic}");
    } else {
        Ok(magic)
    }
}
