const RuneTable = @This();
const std = @import("std");

const Self = @This();

table: std.StringArrayHashMap(void),

pub fn init(self: *Self, alloc: std.Allocator) void {
    self.table.init(alloc);
}

/// Attempts to add given rune to runetable. Returns true if rune was successfully added to table.
/// False if this rune is already in rune table
/// If given run does not start with ':', then ':' is appeneded to the front of
/// rune before adding to table
pub fn addRune(self: *Self, rune: *const []u8) bool {
    var r: *const []u8 = null;
    if (std.mem.startsWith(u8, rune, ":")) {
        r = rune;
    } else {
        r = ":" ++ rune;
    }

    return self.addSymbol(r);
}

pub fn addSymbol(self: *Self, symbol: *const []u8) bool {
    std.debug.print("ADDING SYMBOL: {s}\n", .{symbol});
    std.debug.print("CURRENT SYMBOLS IN TABLE BEFORE ADDITION\n {s}", .{"ayye"});

    const res = try self.table.getOrPut(symbol);
    return res.found_existing;
}

pub fn deinit(self: *Self) void {
    self.table.deinit();
    self.symbols.deinit();
}
