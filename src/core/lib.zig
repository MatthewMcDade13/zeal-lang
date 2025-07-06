const std = @import("std");
pub const sys = @import("sys.zig");
pub const symbols = @import("symbols.zig");

pub fn core() void {
    std.debug.print("{s}", .{"hello from core.zig!"});
}
