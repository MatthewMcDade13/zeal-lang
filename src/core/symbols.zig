//! A interned string symbol similar to Elixir's Atoms

pub const Slot = i32;

pub const MAX_INLINE_SIZE = 24;

const Rune = union(enum) {
    Ref: []const u8,
    Inline: [MAX_INLINE_SIZE]u8,
    Handle: Slot,
    Empty,
};

const Dyn = struct {
    x: i32,
    y: [0]u8,
};
