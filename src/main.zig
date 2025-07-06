const std = @import("std");
const zl = @import("zl");

pub fn main() !void {
    try zl.ast.parse_file("test_scripts/calculator.zl");
    // // Prints to stderr, ignoring potential errors.
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    // try zeal.bufferedPrint();
}
