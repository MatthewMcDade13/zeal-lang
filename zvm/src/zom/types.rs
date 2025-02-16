// TODO: FUCK THIS USE SERDE MESSAGE PACK IM NOT GETTING ANYTNIGN DONE
// WHAT THE FUCK

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u32)]
pub enum TypeTag {
    Prim(PrimType),
    Struct(u32),
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u32)]
pub enum PrimType {
    #[default]
    Unit = 0,
    /// [bool]
    Bool,
    /// [u8]
    Byte,
    /// [i8]
    Char,
    /// [i16]
    Short,
    /// [u16]
    Word,
    /// [i32]
    Int,
    /// [u32]
    Uint,
    /// [i64]
    Int64,
    /// [u64]
    Uint64,
    /// [f32]
    Float,
    /// [f64]
    Float64,
    /// [usize]
    Slot,
    /// [isize]
    Offset,
    String,
    Symbol,
    Rune,
    Count,
}

pub mod bc {
    use zeal_core::mem::str::PoolStringAddr;

    use super::*;

    #[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
    #[repr(transparent)]
    pub struct TypeTagBc(u32);

    impl TypeTagBc {}

    // #[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
    // #[repr(transparent)]
    // pub struct InlineSize(u32);
    //
    // impl InlineSize {
    //     pub const fn prim_type(ptype: PrimType) -> Self {
    //         let v = ptype as u32;
    //
    //     }
    // }

    #[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
    #[repr(C)]
    pub struct StructDef {
        pub name: PoolStringAddr,
    }

    #[derive(Debug, Clone, Copy, Default, bytemuck::Pod, bytemuck::Zeroable)]
    #[repr(C)]
    pub struct TypeDefBc {
        pub tag: TypeTagBc,
        pub size_bytes: u32,
        pub inline_size: u32,
    }
}
