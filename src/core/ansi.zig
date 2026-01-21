const std = @import("std");

pub const Color = enum { auto, always, never };

pub const Style = enum {
    reset,
    path,
    line_no,
    match,
    add,
    del,
    heading,
    dim,
};

pub fn enabled(color: Color, is_tty: bool, no_color: bool) bool {
    return switch (color) {
        .never => false,
        .always => true,
        .auto => is_tty and !no_color,
    };
}

pub fn code(style: Style) []const u8 {
    return switch (style) {
        .reset => "\x1b[0m",
        .path => "\x1b[35m",
        .line_no => "\x1b[32m",
        .match => "\x1b[1;31m",
        .add => "\x1b[32m",
        .del => "\x1b[31m",
        .heading => "\x1b[1;36m",
        .dim => "\x1b[2m",
    };
}

// Colors for multiple patterns: red, green, yellow, blue, magenta, cyan
const match_colors = [_][]const u8{
    "\x1b[1;31m", // red
    "\x1b[1;32m", // green
    "\x1b[1;33m", // yellow
    "\x1b[1;34m", // blue
    "\x1b[1;35m", // magenta
    "\x1b[1;36m", // cyan
};

pub fn matchColor(pattern_idx: usize) []const u8 {
    return match_colors[pattern_idx % match_colors.len];
}

pub fn styled(on: bool, comptime style: Style, value: anytype) Styled(@TypeOf(value)) {
    return .{ .on = on, .value = value, .style = style };
}

fn Styled(comptime T: type) type {
    return struct {
        on: bool,
        value: T,
        style: Style,

        pub fn format(self: @This(), writer: anytype) !void {
            if (self.on) try writer.writeAll(code(self.style));

            const info = @typeInfo(T);
            switch (info) {
                .int, .comptime_int => try writer.print("{d}", .{self.value}),
                .pointer => |p| {
                    if (p.size == .one and @typeInfo(p.child) == .array) {
                        try writer.writeAll(self.value);
                    } else if (p.size == .slice and p.child == u8) {
                        try writer.writeAll(self.value);
                    } else {
                        @compileError("unsupported pointer type for Styled: " ++ @typeName(T));
                    }
                },
                .array => |a| {
                    if (a.child == u8) {
                        try writer.writeAll(&self.value);
                    } else {
                        @compileError("unsupported array type for Styled: " ++ @typeName(T));
                    }
                },
                else => @compileError("unsupported type for Styled: " ++ @typeName(T)),
            }

            if (self.on) try writer.writeAll(code(.reset));
        }
    };
}

test "enabled" {
    try std.testing.expect(!enabled(.never, true, false));
    try std.testing.expect(enabled(.always, false, true));
    try std.testing.expect(!enabled(.auto, false, false));
    try std.testing.expect(!enabled(.auto, true, true));
    try std.testing.expect(enabled(.auto, true, false));
}
