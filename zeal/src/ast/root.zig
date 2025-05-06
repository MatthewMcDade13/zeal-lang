const std = @import("std");
pub const tok = @import("token.zig");

pub fn ast_root() void {
    std.debug.print("Hello from {s}!", .{"src/ast/root.zig"});
    tok.hello();
}
