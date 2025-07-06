/// Index of AST Node in Flat NodeList / AST
pub const Slot = i32;

/// AST Expression Atom
pub const Value = union(enum) {
    Bool: bool,
    Byte: u8,
    Char: i8,
    Int32: i32,
    Int64: i64,
    Float32: f32,
    Float64: f64,
};
