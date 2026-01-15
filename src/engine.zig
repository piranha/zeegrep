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

    /// For literal patterns, expand $0 in replacement to needle (match is always needle).
    /// Returns repl unchanged for regex patterns or if no $0 present.
    pub fn expandReplace(self: *const Pattern, allocator: std.mem.Allocator, repl: []const u8) ![]const u8 {
        return switch (self.*) {
            .literal => |*m| expandRef(allocator, repl, m.needle),
            .regex => repl,
        };
    }
};

pub fn compile(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool, dotall: bool) !Pattern {
    if (isRegex(pat)) return .{ .regex = try pcre2.compile(allocator, pat, ignore_case, dotall) };
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

pub const Span = pcre2.Span;

pub const MatchIterator = union(enum) {
    literal: LiteralIterator,
    regex: pcre2.MatchIterator,

    pub fn init(pat: *const Pattern, hay: []const u8) MatchIterator {
        return switch (pat.*) {
            .literal => |*m| .{ .literal = LiteralIterator.init(m, hay) },
            .regex => |*code| .{ .regex = pcre2.MatchIterator.init(code, hay) },
        };
    }

    pub fn deinit(self: *MatchIterator) void {
        switch (self.*) {
            .literal => {},
            .regex => |*it| it.deinit(),
        }
    }

    pub fn next(self: *MatchIterator) ?Span {
        return switch (self.*) {
            .literal => |*it| it.next(),
            .regex => |*it| it.next(),
        };
    }
};

const LiteralIterator = struct {
    m: *const bmh.Matcher,
    hay: []const u8,
    pos: usize = 0,

    fn init(m: *const bmh.Matcher, hay: []const u8) LiteralIterator {
        return .{ .m = m, .hay = hay };
    }

    fn next(self: *LiteralIterator) ?Span {
        if (self.m.needle.len == 0) return null;
        const idx = self.m.indexOfPos(self.hay, self.pos) orelse return null;
        self.pos = idx + self.m.needle.len;
        return .{ .start = idx, .end = self.pos };
    }
};

pub fn findMatches(allocator: std.mem.Allocator, pat: *const Pattern, hay: []const u8) ![]Span {
    switch (pat.*) {
        .literal => |*m| return findMatchesLiteral(allocator, hay, m),
        .regex => |*code| return pcre2.findMatches(allocator, code, hay),
    }
}

fn findMatchesLiteral(allocator: std.mem.Allocator, hay: []const u8, m: *const bmh.Matcher) ![]Span {
    const needle_len = m.needle.len;
    if (needle_len == 0) return allocator.alloc(Span, 0);

    var spans: std.ArrayListUnmanaged(Span) = .{};
    errdefer spans.deinit(allocator);

    var off: usize = 0;
    while (off <= hay.len) {
        const idx = m.indexOfPos(hay, off) orelse break;
        try spans.append(allocator, .{ .start = idx, .end = idx + needle_len });
        off = idx + needle_len;
    }
    return spans.toOwnedSlice(allocator);
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

pub fn interpretEscapes(allocator: std.mem.Allocator, pat: []const u8) ![]u8 {
    var out: std.ArrayListUnmanaged(u8) = .{};
    errdefer out.deinit(allocator);
    var i: usize = 0;
    while (i < pat.len) : (i += 1) {
        if (pat[i] == '\\' and i + 1 < pat.len) {
            switch (pat[i + 1]) {
                'n' => {
                    try out.append(allocator, '\n');
                    i += 1;
                },
                'r' => {
                    try out.append(allocator, '\r');
                    i += 1;
                },
                't' => {
                    try out.append(allocator, '\t');
                    i += 1;
                },
                '\\' => {
                    try out.append(allocator, '\\');
                    i += 1;
                },
                else => try out.append(allocator, pat[i]),
            }
        } else {
            try out.append(allocator, pat[i]);
        }
    }
    return out.toOwnedSlice(allocator);
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

/// Expand $0 references in replacement string. Returns repl unchanged if no $0 present.
fn expandRef(allocator: std.mem.Allocator, repl: []const u8, match: []const u8) ![]const u8 {
    if (std.mem.indexOf(u8, repl, "$0") == null) return repl;

    var out: std.ArrayListUnmanaged(u8) = .{};
    errdefer out.deinit(allocator);
    var i: usize = 0;
    while (i < repl.len) {
        if (i + 1 < repl.len and repl[i] == '$' and repl[i + 1] == '0') {
            try out.appendSlice(allocator, match);
            i += 2;
        } else {
            try out.append(allocator, repl[i]);
            i += 1;
        }
    }
    return out.toOwnedSlice(allocator);
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
    var p = try compile(std.testing.allocator, "foo", false, false);
    defer p.deinit();
    try std.testing.expect(matchAny(&p, "xx foo yy", false));

    var r = try compile(std.testing.allocator, "f.o", false, false);
    defer r.deinit();
    try std.testing.expect(matchAny(&r, "xx foo yy", false));
}

test "literal replace" {
    var p = try compile(std.testing.allocator, "Foo", true, false);
    defer p.deinit();
    const rr = try replaceAll(std.testing.allocator, &p, "xx foo Foo yy", "bar", true);
    defer std.testing.allocator.free(rr.out);
    try std.testing.expectEqual(@as(usize, 2), rr.n);
    try std.testing.expectEqualStrings("xx bar bar yy", rr.out);
}

test "literal replace with $0" {
    var p = try compile(std.testing.allocator, "foo", false, false);
    defer p.deinit();
    const repl = try p.expandReplace(std.testing.allocator, "$0bar");
    defer if (repl.ptr != "$0bar".ptr) std.testing.allocator.free(repl);
    const rr = try replaceAll(std.testing.allocator, &p, "xx foo yy foo zz", repl, false);
    defer std.testing.allocator.free(rr.out);
    try std.testing.expectEqual(@as(usize, 2), rr.n);
    try std.testing.expectEqualStrings("xx foobar yy foobar zz", rr.out);
}

test "bench iterator high match count" {
    const alloc = std.testing.allocator;

    // ~1MB with "match" every ~100 bytes = ~10k matches
    const chunk = "some random text here match and more text_";
    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);
    for (0..25000) |_| try hay.appendSlice(alloc, chunk);
    const data = hay.items;

    const expected_matches: usize = 25000;
    std.debug.print("\nbench iterator: {d}KB, {d} expected matches\n", .{ data.len / 1024, expected_matches });

    // Literal pattern
    var lit = try compile(alloc, "match", false, false);
    defer lit.deinit();

    var timer = try std.time.Timer.start();
    var count: usize = 0;
    var it = MatchIterator.init(&lit, data);
    defer it.deinit();
    while (it.next()) |_| count += 1;
    const lit_ns = timer.read();

    try std.testing.expectEqual(expected_matches, count);
    std.debug.print("  literal: {d}ms ({d} matches)\n", .{ lit_ns / std.time.ns_per_ms, count });

    // Regex pattern
    var rgx = try compile(alloc, "ma.ch", false, false);
    defer rgx.deinit();

    timer.reset();
    count = 0;
    var it2 = MatchIterator.init(&rgx, data);
    defer it2.deinit();
    while (it2.next()) |_| count += 1;
    const rgx_ns = timer.read();

    try std.testing.expectEqual(expected_matches, count);
    std.debug.print("  regex: {d}ms ({d} matches)\n", .{ rgx_ns / std.time.ns_per_ms, count });

    // Sanity: should complete in reasonable time (<500ms for 25k matches)
    try std.testing.expect(lit_ns < 500 * std.time.ns_per_ms);
    try std.testing.expect(rgx_ns < 500 * std.time.ns_per_ms);
}
