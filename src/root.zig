//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

pub const ast = @import("ast/ast.zig");
pub const core = @import("core/core.zig");
pub const compiler = @import("compiler/compiler.zig");
