use std::{marker::PhantomData, path::Path, rc::Rc};

use anyhow::{bail, ensure, Context};
use bytes::Bytes;

/// ZealVM Bytecode Object format

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable, PartialEq, Eq)]
#[repr(C)]
pub struct ByteRange {
    pub begin: u32,
    pub len_bytes: u32,
}

impl ByteRange {
    pub const fn new(begin: usize, len_bytes: usize) -> Self {
        let begin = begin as u32;
        let len_bytes = len_bytes as u32;
        Self { begin, len_bytes }
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

    pub imports_loc: ByteRange,
    pub imports_len: u32,

    pub exports_loc: ByteRange,
    pub exports_len: u32,

    pub labels_loc: ByteRange,
    pub labels_len: u32,

    pub symbols_loc: ByteRange,
    pub symbols_len: u32,

    pub typeinfo_loc: ByteRange,
    pub typeinfo_len: u32,

    pub struct_fields_loc: ByteRange,
    pub struct_fields_len: u32,

    pub constants_loc: ByteRange,
    pub constants_len: u32,

    pub functions_loc: ByteRange,
    pub functions_len: u32,
}

impl ObjHeader {
    pub const SIZE_BYTES: usize = std::mem::size_of::<Self>();

    pub const fn module_size_bytes(&self) -> usize {
        let mut size = std::mem::size_of::<u32>() as u32;
        size += self.imports_loc.len_bytes;
        size += self.exports_loc.len_bytes;
        size += self.symbols_loc.len_bytes;
        size += self.typeinfo_loc.len_bytes;
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
    pub imports: &'a [Import],
    pub exports: &'a [Export],
    pub labels: Rc<[&'a str]>,
    pub constants: Rc<[ConstantView<'a>]>,
    pub symbols: &'a [SymbolEntry],
    pub typeinfo: &'a [TypeInfo],
    pub struct_fields: &'a [StructField],
    pub funcs: Rc<[FuncView<'a>]>,
}

impl<'a> ObjectView<'a> {
    pub fn new(buf: &'a [u8]) -> anyhow::Result<Self> {
        let header: &ObjHeader = bytemuck::from_bytes(&buf[..ObjHeader::SIZE_BYTES]);
        let imports: &[Import] = Self::section_list(buf, header.imports_loc);
        let exports: &[Export] = Self::section_list(buf, header.exports_loc);
        let labels: &[&str] = Self::section_list(buf, header.labels_loc);

        todo!()
    }

    pub fn section_list<T>(buf: &[u8], range: ByteRange) -> &[T]
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

/// Value Type Primitive of symbol
/// NOTE: Was going to have this be a u8, but making it a u32 for
/// bytemuck and alignment reasons.
#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(transparent)]
pub struct ValueType(u32);

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
pub struct StructField {
    pub parent: u32,
    /// Position of field in struct. i.e. the first field in struct definiton, is position 0
    pub position: u32,
    // pub visibility: Visibility,
    pub name_loc: ByteRange,
    pub byte_offset: u32,
    pub type_loc: ByteRange,
    /// Only valid values are 0 (for private) or 1 (for public)
    /// needs to be u32 for alignment reasons...
    pub is_public: u32,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct TypeInfo {
    pub value_ty: ValueType,
    pub size_bytes: u32,
    pub name_loc: ByteRange,
    pub fields_loc: ByteRange,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct FuncHeader {
    pub name_loc: ByteRange,
    pub params_loc: ByteRange,
    pub return_ty_loc: ByteRange,
    pub opcodes_loc: ByteRange,
}

impl FuncHeader {
    /// Gets size of full function definiton in bytes. Includes params and bytecode instructions
    pub const fn size_bytes(&self) -> usize {
        let param_len = self.params_loc.len_bytes as usize;
        let bytecode_len = self.opcodes_loc.len_bytes as usize;
        param_len + bytecode_len
    }
}

#[derive(Debug, Clone, Copy)]
pub struct FuncView<'a> {
    pub header: &'a FuncHeader,
    pub name: &'a str,
    pub params: &'a [FuncParam],
    pub return_ty: &'a TypeInfo,
    pub bytecode: &'a [u8],
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct FuncParam {
    pub position: u32,
    pub type_loc: ByteRange,
    pub name_loc: ByteRange,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct Export {
    pub name_loc: ByteRange,
    pub symbol_loc: ByteRange,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct Import {
    pub path_loc: ByteRange,
    pub name_loc: ByteRange,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct SymbolEntry {
    pub name_loc: ByteRange,
    /// Section type this symbol resides in
    pub section_ty: SectionType,
    pub location_ty: SymbolLocation,
    pub typeinfo: TypeInfo,
    // pub typeinfo_loc: ByteRange,
    pub value_loc: ByteRange,
}

#[derive(Debug, Clone, Copy, bytemuck::Pod, bytemuck::Zeroable)]
#[repr(C)]
pub struct SectionType(u16);

impl SectionType {
    pub const HEADER: Self = Self(0);
    pub const IMPORTS: Self = Self(1);
    pub const EXPORTS: Self = Self(2);
    pub const CONSTANTS: Self = Self(3);
    pub const SYMBOLS: Self = Self(4);
    pub const TYPEINFO: Self = Self(5);
    pub const STRUCT_FIELDS: Self = Self(6);
    pub const FUNCS: Self = Self(7);
}

#[derive(Debug, Clone)]
pub struct Module {
    header: ObjHeader,
    buf: bytes::Bytes,
    imports: ImportSection,
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

    pub fn section_list<T>(&self, range: ByteRange) -> &[T]
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
        self.section_list(self.header.typeinfo_loc)
    }

    #[inline]
    pub fn struct_fields(&self) -> &[StructField] {
        self.section_list(self.header.struct_fields_loc)
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

    // pub fn funcs(&self) -> Rc<[FuncView]> {
    //     let mut views = Vec::new();
    //     let begin = self.header.functions_loc.begin;
    //     let end = self.buf.len();
    //     let bytes = &self.buf[begin..end];
    // }
}

#[derive(Debug, Clone, Copy)]
struct FuncIter<'a> {
    iter: usize,
    end: usize,
    module_buf: &'a [u8],
}

impl<'a> Iterator for FuncIter<'a> {
    type Item = FuncView<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iter >= self.end {
            None
        } else {
            let header = unsafe {
                self.iter
                    .cast::<FuncHeader>()
                    .as_ref()
                    .expect("Invalid pointer cast!")
            };

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
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportSection {
    buf: bytes::Bytes,
    loc: ByteRange,
    num_imports: usize,
}

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
