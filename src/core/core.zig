const std = @import("std");

pub fn core() void {
    std.debug.print("{s}", .{"hello from core.zig!"});
}

