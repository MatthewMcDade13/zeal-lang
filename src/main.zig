const std = @import("std");
const zl = @import("zl");

pub fn main() !void {
    zl.ast.parser.parser();
    zl.core.core();
    // // Prints to stderr, ignoring potential errors.
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    // try zeal.bufferedPrint();
}
