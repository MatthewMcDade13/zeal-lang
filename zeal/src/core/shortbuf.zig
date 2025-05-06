const std = @import("std");

// pub const ShortBuf = union(enum) {
//     pub const MAX_SHORT = 24;

//     short: u8[MAX_SHORT],
//     tall: *const []u8,
// };

pub fn ShortBuf(comptime T: type) type {
    if (T) {
        return struct {};
    } else {
        return struct {};
    }
}
