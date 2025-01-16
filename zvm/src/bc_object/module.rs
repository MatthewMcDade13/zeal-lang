use std::path::PathBuf;

use zeal_core::buf::ShortStr;

#[derive(Debug, Clone)]
pub struct Module {}

#[derive(Debug, Clone)]
pub struct Import {
    pub module_path: PathBuf,
    pub symbol_name: ShortStr,
}

#[derive(Debug, Clone)]
pub struct Export {
    pub symbol_name: ShortStr,
    // pub symbol_entry: SymbolEntry,
}

#[derive(Debug, Clone)]
pub struct SymbolEntry {
    pub name: ShortStr,
}

#[derive(Debug, Clone)]
pub struct TypeInfo {}
