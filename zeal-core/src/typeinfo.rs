use crate::string::StrBuf;

#[derive(Debug, Clone, Copy)]
pub struct StructMeta {}

pub struct TypeInfo {
    pub typeid: usize,
    size_bytes: usize,
    align: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct FieldInfo {
    name: StrBuf,
    offset: usize,
    typeinfo: TypeInfo,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    meta: StructMeta,
}

#[derive(Debug, Clone)]
pub struct StructBuilder {}
