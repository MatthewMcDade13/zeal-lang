use std::{ops::Deref, ptr::NonNull};

use anyhow::ensure;

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: String,
}

impl SymbolTable {
    pub fn lookup_str(&self, range: SymbolRange) -> anyhow::Result<&str> {
        let SymbolRange { slot, len } = range;
        ensure!((slot + len) < self.symbols.len());
        let bs = self.symbols.as_bytes();
        let begin = slot;
        let end = slot + len;
        let bytes = &bs[begin..end];
        let s = std::str::from_utf8(bytes)?;
        Ok(s)
    }
}

impl Deref for SymbolTable {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.symbols.as_str()
    }
}

#[derive(Debug, Clone, Copy, zerocopy_derive::FromBytes, zerocopy_derive::IntoBytes)]
#[repr(C)]
pub struct SymbolRange {
    slot: usize,
    len: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct Symbol {
    range: SymbolRange,
    table: NonNull<SymbolTable>,
}

impl Deref for Symbol {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        let t = unsafe { self.table.as_ref() };
        t.lookup_str(self.range)
            .expect("Invalid symbol lookup. Most likely out of range or invalid")
    }
}
