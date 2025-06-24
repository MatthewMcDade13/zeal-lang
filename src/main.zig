const std = @import("std");
const zl = @import("zl");

pub fn main() !void {
    _ = zl.ast.Parser.init();
    zl.core.core();
    zl.Zvm.zvm_test();
    // // Prints to stderr, ignoring potential errors.
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});
    // try zeal.bufferedPrint();
}
