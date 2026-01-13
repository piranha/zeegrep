const std = @import("std");
const pcre2 = @import("pcre2.zig");
const ansi = @import("core/ansi.zig");

pub const Pattern = union(enum) {
    literal: []const u8,
    regex: pcre2.Code,

    pub fn deinit(self: *Pattern) void {
        switch (self.*) {
            .literal => {},
            .regex => |*c| c.deinit(),
        }
        self.* = .{ .literal = "" };
    }
};

pub fn compile(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool) !Pattern {
    if (isRegex(pat)) return .{ .regex = try pcre2.compile(allocator, pat, ignore_case) };
    return .{ .literal = pat };
}

pub fn matchAny(pat: *const Pattern, hay: []const u8, ignore_case: bool) bool {
    switch (pat.*) {
        .literal => |needle| return contains(hay, needle, ignore_case),
        .regex => |*code| return pcre2.matchAny(code, hay),
    }
}

pub fn countMatches(pat: *const Pattern, hay: []const u8, ignore_case: bool) usize {
    switch (pat.*) {
        .literal => |needle| return countSubstr(hay, needle, ignore_case),
        .regex => |*code| return pcre2.countMatches(code, hay),
    }
}

pub const Replace = struct {
    out: []u8,
    n: usize,
};

pub fn replaceAll(allocator: std.mem.Allocator, pat: *const Pattern, hay: []const u8, repl: []const u8, ignore_case: bool) !Replace {
    switch (pat.*) {
        .literal => |needle| {
            const rr = try replaceLiteral(allocator, hay, needle, repl, ignore_case);
            return .{ .out = rr.out, .n = rr.n };
        },
        .regex => |*code| {
            const rr = try pcre2.replaceAll(allocator, code, hay, repl);
            return .{ .out = rr.out, .n = rr.n };
        },
    }
}

pub fn writeHighlighted(on: bool, pat: *const Pattern, writer: anytype, hay: []const u8, ignore_case: bool) !void {
    switch (pat.*) {
        .literal => |needle| try writeHighlightedLiteral(on, writer, hay, needle, ignore_case),
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

fn contains(hay: []const u8, needle: []const u8, ignore_case: bool) bool {
    if (!ignore_case) return std.mem.indexOf(u8, hay, needle) != null;
    if (needle.len == 0) return true;
    if (needle.len > hay.len) return false;
    var i: usize = 0;
    while (i + needle.len <= hay.len) : (i += 1) {
        if (eqAsciiFold(hay[i .. i + needle.len], needle)) return true;
    }
    return false;
}

fn countSubstr(hay: []const u8, needle: []const u8, ignore_case: bool) usize {
    if (needle.len == 0) return 0;
    var off: usize = 0;
    var n: usize = 0;
    while (off + needle.len <= hay.len) {
        const idx = if (!ignore_case)
            std.mem.indexOfPos(u8, hay, off, needle)
        else
            indexOfPosAsciiFold(hay, off, needle);
        if (idx == null) break;
        n += 1;
        off = idx.? + needle.len;
    }
    return n;
}

fn replaceLiteral(allocator: std.mem.Allocator, hay: []const u8, needle: []const u8, repl: []const u8, ignore_case: bool) !Replace {
    if (needle.len == 0) return .{ .out = try allocator.dupe(u8, hay), .n = 0 };

    var out: std.ArrayListUnmanaged(u8) = .{};
    errdefer out.deinit(allocator);

    var off: usize = 0;
    var n: usize = 0;
    while (off <= hay.len) {
        const idx = if (!ignore_case)
            std.mem.indexOfPos(u8, hay, off, needle)
        else
            indexOfPosAsciiFold(hay, off, needle);
        if (idx == null) break;
        const i = idx.?;
        try out.appendSlice(allocator, hay[off..i]);
        try out.appendSlice(allocator, repl);
        n += 1;
        off = i + needle.len;
    }
    try out.appendSlice(allocator, hay[off..]);
    return .{ .out = try out.toOwnedSlice(allocator), .n = n };
}

fn indexOfPosAsciiFold(hay: []const u8, start: usize, needle: []const u8) ?usize {
    if (needle.len == 0) return start;
    if (needle.len > hay.len) return null;
    var i: usize = start;
    while (i + needle.len <= hay.len) : (i += 1) {
        if (eqAsciiFold(hay[i .. i + needle.len], needle)) return i;
    }
    return null;
}

fn eqAsciiFold(a: []const u8, b: []const u8) bool {
    if (a.len != b.len) return false;
    for (a, b) |x, y| {
        if (std.ascii.toLower(x) != std.ascii.toLower(y)) return false;
    }
    return true;
}

fn writeHighlightedLiteral(on: bool, writer: anytype, hay: []const u8, needle: []const u8, ignore_case: bool) !void {
    if (needle.len == 0) return writer.writeAll(hay);
    var off: usize = 0;
    while (off + needle.len <= hay.len) {
        const idx = if (!ignore_case)
            std.mem.indexOfPos(u8, hay, off, needle)
        else
            indexOfPosAsciiFold(hay, off, needle);
        if (idx == null) break;
        const i = idx.?;
        try writer.writeAll(hay[off..i]);
        try ansi.styled(on, .match, hay[i .. i + needle.len]).format(writer);
        off = i + needle.len;
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
