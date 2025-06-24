pub const opcode = @import("opcode.zig");

const Zvm = @This();

const std = @import("std");

pub fn zvm_test() void {
    std.debug.print("{s}", .{"hello from zvm.zig!"});
}
