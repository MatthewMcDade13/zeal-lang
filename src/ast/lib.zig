pub const node = @import("node.zig");

const Parser = struct {
    pos: usize,
};

pub fn parse_source(src_bytes: []const u8) !void {
    var fbs = std.io.fixedBufferStream(src_bytes);
    const stream = fbs.reader();

    try parse(stream);
}

pub fn parse_file(path: []const u8) !void {
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());

    const stream = buf_reader.reader();
    try parse(stream);
}

fn parse(stream: anytype) !void {
    var buf: [1024]u8 = undefined;

    while (try stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("{s}\n", .{line});
    }
}

const std = @import("std");
