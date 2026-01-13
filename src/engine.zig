const std = @import("std");
const pcre2 = @import("pcre2.zig");
const ansi = @import("core/ansi.zig");
const bmh = @import("core/bmh.zig");

pub const Pattern = union(enum) {
    literal: bmh.Matcher,
    regex: pcre2.Code,

    pub fn deinit(self: *Pattern) void {
        switch (self.*) {
            .literal => {},
            .regex => |*c| c.deinit(),
        }
        self.* = .{ .literal = bmh.Matcher.init("", false) };
    }
};

pub fn compile(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool) !Pattern {
    if (isRegex(pat)) return .{ .regex = try pcre2.compile(allocator, pat, ignore_case) };
    return .{ .literal = bmh.Matcher.init(pat, ignore_case) };
}

pub fn matchAny(pat: *const Pattern, hay: []const u8, ignore_case: bool) bool {
    _ = ignore_case; // baked into pattern at compile time
    switch (pat.*) {
        .literal => |*m| return m.contains(hay),
        .regex => |*code| return pcre2.matchAny(code, hay),
    }
}

pub fn countMatches(pat: *const Pattern, hay: []const u8, ignore_case: bool) usize {
    _ = ignore_case;
    switch (pat.*) {
        .literal => |*m| return m.count(hay),
        .regex => |*code| return pcre2.countMatches(code, hay),
    }
}

pub const Replace = struct {
    out: []u8,
    n: usize,
};

pub fn replaceAll(allocator: std.mem.Allocator, pat: *const Pattern, hay: []const u8, repl: []const u8, ignore_case: bool) !Replace {
    _ = ignore_case;
    switch (pat.*) {
        .literal => |*m| {
            const rr = try replaceLiteral(allocator, hay, m, repl);
            return .{ .out = rr.out, .n = rr.n };
        },
        .regex => |*code| {
            const rr = try pcre2.replaceAll(allocator, code, hay, repl);
            return .{ .out = rr.out, .n = rr.n };
        },
    }
}

pub fn writeHighlighted(on: bool, pat: *const Pattern, writer: anytype, hay: []const u8, ignore_case: bool) !void {
    _ = ignore_case;
    switch (pat.*) {
        .literal => |*m| try writeHighlightedLiteral(on, writer, hay, m),
        .regex => |*code| try pcre2.writeHighlighted(on, code, writer, hay),
    }
}

fn isRegex(pat: []const u8) bool {
    for (pat) |c| switch (c) {
        '\\', '.', '+', '*', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$' => return true,
        else => {},
    };
    return false;
}

fn replaceLiteral(allocator: std.mem.Allocator, hay: []const u8, m: *const bmh.Matcher, repl: []const u8) !Replace {
    const needle_len = m.needle.len;
    if (needle_len == 0) return .{ .out = try allocator.dupe(u8, hay), .n = 0 };

    var out: std.ArrayListUnmanaged(u8) = .{};
    errdefer out.deinit(allocator);

    var off: usize = 0;
    var n: usize = 0;
    while (off <= hay.len) {
        const idx = m.indexOfPos(hay, off) orelse break;
        try out.appendSlice(allocator, hay[off..idx]);
        try out.appendSlice(allocator, repl);
        n += 1;
        off = idx + needle_len;
    }
    try out.appendSlice(allocator, hay[off..]);
    return .{ .out = try out.toOwnedSlice(allocator), .n = n };
}

fn writeHighlightedLiteral(on: bool, writer: anytype, hay: []const u8, m: *const bmh.Matcher) !void {
    const needle_len = m.needle.len;
    if (needle_len == 0) return writer.writeAll(hay);
    var off: usize = 0;
    while (true) {
        const idx = m.indexOfPos(hay, off) orelse break;
        try writer.writeAll(hay[off..idx]);
        try ansi.styled(on, .match, hay[idx .. idx + needle_len]).format(writer);
        off = idx + needle_len;
    }
    try writer.writeAll(hay[off..]);
}

test "literal vs regex detection" {
    var p = try compile(std.testing.allocator, "foo", false);
    defer p.deinit();
    try std.testing.expect(matchAny(&p, "xx foo yy", false));

    var r = try compile(std.testing.allocator, "f.o", false);
    defer r.deinit();
    try std.testing.expect(matchAny(&r, "xx foo yy", false));
}

test "literal replace" {
    var p = try compile(std.testing.allocator, "Foo", true);
    defer p.deinit();
    const rr = try replaceAll(std.testing.allocator, &p, "xx foo Foo yy", "bar", true);
    defer std.testing.allocator.free(rr.out);
    try std.testing.expectEqual(@as(usize, 2), rr.n);
    try std.testing.expectEqualStrings("xx bar bar yy", rr.out);
}
