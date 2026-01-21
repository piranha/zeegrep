const std = @import("std");
const pcre2 = @import("pcre2.zig");
const ansi = @import("core/ansi.zig");
const bmh = @import("core/bmh.zig");

/// SIMD-accelerated backward search for newline. stdlib's lastIndexOfScalar is scalar-only.
fn lastIndexOfNewline(hay: []const u8) ?usize {
    const Vec = @Vector(32, u8);
    const nl: Vec = @splat('\n');

    var i = hay.len;
    while (i >= 32) {
        const chunk: Vec = hay[i - 32 ..][0..32].*;
        const matches = chunk == nl;
        const mask: u32 = @bitCast(matches);
        if (mask != 0) {
            return i - 32 + (31 - @clz(mask));
        }
        i -= 32;
    }
    // Scalar head
    while (i > 0) {
        i -= 1;
        if (hay[i] == '\n') return i;
    }
    return null;
}

pub const Pattern = union(enum) {
    literal: bmh.Matcher,
    regex: OptimizedRegex,

    pub fn deinit(self: *Pattern) void {
        switch (self.*) {
            .literal => {},
            .regex => |*r| r.deinit(),
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

/// Regex with optional literal optimization.
/// Seeks literal with BMH, then runs regex on that line only.
/// If multiple literals exist, uses second as additional filter.
pub const OptimizedRegex = struct {
    code: pcre2.Code,
    literal: ?bmh.Matcher = null, // primary - used for seeking
    literal2: ?bmh.Matcher = null, // secondary - additional filter (must also contain)
    allocator: ?std.mem.Allocator = null, // for freeing literal copies
    literal_copy: ?[]const u8 = null, // owned copy of literal string
    literal2_copy: ?[]const u8 = null, // owned copy of literal2 string

    pub fn deinit(self: *OptimizedRegex) void {
        self.code.deinit();
        if (self.allocator) |alloc| {
            if (self.literal_copy) |lc| alloc.free(lc);
            if (self.literal2_copy) |lc| alloc.free(lc);
        }
        self.literal = null;
        self.literal2 = null;
        self.literal_copy = null;
        self.literal2_copy = null;
    }
};

pub fn compile(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool, dotall: bool) !Pattern {
    return compileOpts(allocator, pat, ignore_case, dotall, false);
}

pub fn compileOpts(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool, dotall: bool, force_literal: bool) !Pattern {
    if (!force_literal and isRegex(pat)) {
        const code = try pcre2.compile(allocator, pat, ignore_case, dotall);
        const lits = extractLiterals(pat);
        const is_multiline = containsNewline(pat) or dotall;

        // Literal optimization: seek literal with BMH, run regex on that line only.
        // Use second literal as additional filter if available.
        // Disabled for multiline patterns (can't use line-based optimization).
        var literal: ?bmh.Matcher = null;
        var literal2: ?bmh.Matcher = null;
        var literal_copy: ?[]const u8 = null;
        var literal2_copy: ?[]const u8 = null;
        if (!is_multiline and lits.first.len >= 3) {
            // Must copy literals - extractLiterals uses static buffer that gets overwritten
            literal_copy = try allocator.dupe(u8, lits.first);
            literal = bmh.Matcher.init(literal_copy.?, ignore_case);
            if (lits.second.len >= 3) {
                literal2_copy = try allocator.dupe(u8, lits.second);
                literal2 = bmh.Matcher.init(literal2_copy.?, ignore_case);
            }
        }
        return .{ .regex = .{
            .code = code,
            .literal = literal,
            .literal2 = literal2,
            .allocator = allocator,
            .literal_copy = literal_copy,
            .literal2_copy = literal2_copy,
        } };
    }
    return .{ .literal = bmh.Matcher.init(pat, ignore_case) };
}

/// Check if pattern contains actual newline - our line-based optimization won't work.
fn containsNewline(pat: []const u8) bool {
    return std.mem.indexOfScalar(u8, pat, '\n') != null;
}

/// Extract literal prefix from regex pattern (bytes before first metachar).
/// Returns empty slice if pattern starts with metachar.
fn extractLiteralPrefix(pat: []const u8) []const u8 {
    var i: usize = 0;
    while (i < pat.len) : (i += 1) {
        const c = pat[i];
        switch (c) {
            // Metacharacters that end literal prefix
            '.', '+', '*', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$' => return pat[0..i],
            '\\' => {
                // Escape sequence - check what follows
                if (i + 1 >= pat.len) return pat[0..i];
                const next = pat[i + 1];
                switch (next) {
                    // Literal escapes - include the escaped char
                    'n', 'r', 't', '\\', '.', '+', '*', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$' => {
                        // Can't easily include \n etc in prefix without allocation, stop here
                        return pat[0..i];
                    },
                    // Character classes like \d, \w, \s - these are regex metacharacters
                    'd', 'D', 'w', 'W', 's', 'S', 'b', 'B' => return pat[0..i],
                    // Anything else - stop to be safe
                    else => return pat[0..i],
                }
            },
            else => {},
        }
    }
    return pat; // Entire pattern is literal (shouldn't happen if isRegex passed)
}

/// Decode escaped character, returns the literal char or null if it's a metachar class.
fn decodeEscape(c: u8) ?u8 {
    return switch (c) {
        // Metachar classes - not literal
        'd', 'D', 'w', 'W', 's', 'S', 'b', 'B', 'A', 'Z', 'z' => null,
        // Special escapes
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        'a' => 0x07, // bell
        'e' => 0x1B, // escape
        'f' => 0x0C, // form feed
        'v' => 0x0B, // vertical tab
        '0' => 0, // null (simple case, not full octal)
        // Everything else is literal (escaped metachar or redundant escape)
        else => c,
    };
}

/// Result of literal extraction - up to two literals for filtering
const ExtractedLiterals = struct {
    first: []const u8 = "",
    second: []const u8 = "",
};

/// Extract up to two longest literal substrings from a regex pattern.
/// Used for multi-literal filtering (line must contain both).
fn extractLiterals(pat: []const u8) ExtractedLiterals {
    const S = struct {
        var best1: [256]u8 = undefined;
        var best2: [256]u8 = undefined;
        var current: [256]u8 = undefined;
    };

    var best1_len: usize = 0;
    var best2_len: usize = 0;
    var cur_len: usize = 0;
    var i: usize = 0;

    while (i < pat.len) {
        const c = pat[i];

        const advance: struct { skip: usize, literal: ?u8 } = switch (c) {
            '.', '+', '*', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$' => .{ .skip = 1, .literal = null },
            '\\' => blk: {
                if (i + 1 >= pat.len) break :blk .{ .skip = 1, .literal = null };
                const next = pat[i + 1];
                if (decodeEscape(next)) |lit| {
                    break :blk .{ .skip = 2, .literal = lit };
                }
                break :blk .{ .skip = 2, .literal = null };
            },
            else => .{ .skip = 1, .literal = c },
        };

        if (advance.literal) |lit| {
            if (cur_len < S.current.len) {
                S.current[cur_len] = lit;
                cur_len += 1;
            }
            i += advance.skip;
        } else {
            // End of run - update best1/best2
            if (cur_len > best1_len) {
                // Demote best1 to best2
                @memcpy(S.best2[0..best1_len], S.best1[0..best1_len]);
                best2_len = best1_len;
                // New best1
                @memcpy(S.best1[0..cur_len], S.current[0..cur_len]);
                best1_len = cur_len;
            } else if (cur_len > best2_len) {
                @memcpy(S.best2[0..cur_len], S.current[0..cur_len]);
                best2_len = cur_len;
            }
            cur_len = 0;
            i += advance.skip;
        }
    }

    // Check final run
    if (cur_len > best1_len) {
        @memcpy(S.best2[0..best1_len], S.best1[0..best1_len]);
        best2_len = best1_len;
        @memcpy(S.best1[0..cur_len], S.current[0..cur_len]);
        best1_len = cur_len;
    } else if (cur_len > best2_len) {
        @memcpy(S.best2[0..cur_len], S.current[0..cur_len]);
        best2_len = cur_len;
    }

    return .{
        .first = S.best1[0..best1_len],
        .second = S.best2[0..best2_len],
    };
}

/// Extract the longest literal substring (convenience wrapper)
fn extractLongestLiteral(pat: []const u8) []const u8 {
    return extractLiterals(pat).first;
}

pub fn matchAny(pat: *const Pattern, hay: []const u8, ignore_case: bool) bool {
    _ = ignore_case; // baked into pattern at compile time
    switch (pat.*) {
        .literal => |*m| return m.contains(hay),
        .regex => |*r| {
            // Fast path: if literal filter exists and not found, skip regex
            const lit = r.literal;
            if (lit) |*p| {
                if (!p.contains(hay)) return false;
            }
            return pcre2.matchAny(&r.code, hay);
        },
    }
}

pub fn countMatches(pat: *const Pattern, hay: []const u8, ignore_case: bool) usize {
    _ = ignore_case;
    switch (pat.*) {
        .literal => |*m| return m.count(hay),
        .regex => |*r| {
            const lit = r.literal;
            if (lit) |*p| {
                if (!p.contains(hay)) return 0;
            }
            return pcre2.countMatches(&r.code, hay);
        },
    }
}

pub const Span = pcre2.Span;

pub const MatchIterator = union(enum) {
    literal: LiteralIterator,
    regex: RegexIterator,

    pub fn init(pat: *const Pattern, hay: []const u8) MatchIterator {
        return switch (pat.*) {
            .literal => |*m| .{ .literal = LiteralIterator.init(m, hay) },
            .regex => |*r| .{ .regex = RegexIterator.init(r, hay) },
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

const RegexIterator = struct {
    inner: pcre2.MatchIterator,
    literal: ?*const bmh.Matcher,
    literal2: ?*const bmh.Matcher, // secondary filter
    hay: []const u8,
    pos: usize = 0,

    fn init(r: *const OptimizedRegex, hay: []const u8) RegexIterator {
        return .{
            .inner = pcre2.MatchIterator.init(&r.code, hay),
            .literal = if (r.literal) |*p| p else null,
            .literal2 = if (r.literal2) |*p| p else null,
            .hay = hay,
        };
    }

    fn deinit(self: *RegexIterator) void {
        self.inner.deinit();
    }

    fn next(self: *RegexIterator) ?Span {
        // Literal optimization: seek literal, run regex on that line only
        if (self.literal) |lit| return self.nextWithLiteral(lit);
        // No optimization: raw pcre2 iteration
        return self.inner.next();
    }

    /// Seek literal with BMH, run regex on that line only.
    fn nextWithLiteral(self: *RegexIterator, lit: *const bmh.Matcher) ?Span {
        while (true) {
            const lit_pos = lit.indexOfPos(self.hay, self.pos) orelse return null;

            const line_start = if (lastIndexOfNewline(self.hay[0..lit_pos])) |nl|
                nl + 1
            else
                0;
            const line_end = if (std.mem.indexOfScalarPos(u8, self.hay, lit_pos, '\n')) |nl|
                nl
            else
                self.hay.len;

            const line = self.hay[line_start..line_end];

            // If we have a second literal, check it's present before running regex
            if (self.literal2) |lit2| {
                if (!lit2.contains(line)) {
                    self.pos = line_end + 1;
                    continue;
                }
            }

            // Run regex on this line only, reusing match data
            if (self.inner.matchSlice(line)) |span| {
                self.pos = line_end + 1;
                return .{
                    .start = line_start + span.start,
                    .end = line_start + span.end,
                };
            }

            self.pos = line_end + 1;
        }
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
        .regex => |*r| {
            const lit = r.literal;
            if (lit) |*p| {
                if (!p.contains(hay)) return allocator.alloc(Span, 0);
            }
            return pcre2.findMatches(allocator, &r.code, hay);
        },
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
        .regex => |*r| {
            // For replace, skip if literal not found (no matches possible)
            const lit = r.literal;
            if (lit) |*p| {
                if (!p.contains(hay)) return .{ .out = try allocator.dupe(u8, hay), .n = 0 };
            }
            const rr = try pcre2.replaceAll(allocator, &r.code, hay, repl);
            return .{ .out = rr.out, .n = rr.n };
        },
    }
}

pub fn writeHighlighted(on: bool, pat: *const Pattern, writer: anytype, hay: []const u8, ignore_case: bool) !void {
    _ = ignore_case;
    switch (pat.*) {
        .literal => |*m| try writeHighlightedLiteral(on, writer, hay, m),
        .regex => |*r| try pcre2.writeHighlighted(on, &r.code, writer, hay),
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

test "extractLiteralPrefix" {
    try std.testing.expectEqualStrings("meta", extractLiteralPrefix("meta.+se"));
    try std.testing.expectEqualStrings("foo", extractLiteralPrefix("foo\\d+bar"));
    try std.testing.expectEqualStrings("hello", extractLiteralPrefix("hello(world)?"));
    try std.testing.expectEqualStrings("", extractLiteralPrefix(".+foo"));
    try std.testing.expectEqualStrings("", extractLiteralPrefix("^foo"));
    try std.testing.expectEqualStrings("abc", extractLiteralPrefix("abc[def]"));
    try std.testing.expectEqualStrings("test", extractLiteralPrefix("test*"));
}

test "lastIndexOfNewline SIMD" {
    // Empty
    try std.testing.expectEqual(@as(?usize, null), lastIndexOfNewline(""));
    // No newline
    try std.testing.expectEqual(@as(?usize, null), lastIndexOfNewline("hello world"));
    // Single newline
    try std.testing.expectEqual(@as(?usize, 5), lastIndexOfNewline("hello\nworld"));
    // Multiple newlines - returns last
    try std.testing.expectEqual(@as(?usize, 11), lastIndexOfNewline("hello\nworld\nfoo"));
    // At end
    try std.testing.expectEqual(@as(?usize, 5), lastIndexOfNewline("hello\n"));
    // At start
    try std.testing.expectEqual(@as(?usize, 0), lastIndexOfNewline("\nhello"));
    // Longer than 32 bytes (SIMD path)
    const long = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\nbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\n";
    try std.testing.expectEqual(@as(?usize, 67), lastIndexOfNewline(long));
    // Verify matches stdlib
    try std.testing.expectEqual(std.mem.lastIndexOfScalar(u8, long, '\n'), lastIndexOfNewline(long));
}

test "extractLiterals" {
    // Single literal
    var lits = extractLiterals("..tabase");
    try std.testing.expectEqualStrings("tabase", lits.first);
    try std.testing.expectEqualStrings("", lits.second);

    // Two literals - longest first
    lits = extractLiterals(".+met.+ase");
    try std.testing.expectEqualStrings("met", lits.first);
    try std.testing.expectEqualStrings("ase", lits.second);

    // Three literals - two longest kept
    lits = extractLiterals("foo.+bar.+baz");
    try std.testing.expectEqualStrings("foo", lits.first);
    try std.testing.expectEqualStrings("bar", lits.second); // or baz, both len 3

    // Different lengths
    lits = extractLiterals(".+metabase.+foo");
    try std.testing.expectEqualStrings("metabase", lits.first);
    try std.testing.expectEqualStrings("foo", lits.second);
}

test "extractLongestLiteral" {
    // Inner literals
    try std.testing.expectEqualStrings("tabase", extractLongestLiteral("..tabase"));
    try std.testing.expectEqualStrings("taba", extractLongestLiteral("..taba.."));
    try std.testing.expectEqualStrings("metabase", extractLongestLiteral(".+metabase.+"));
    // Prefix is longest
    try std.testing.expectEqualStrings("meta", extractLongestLiteral("meta.+se"));
    // No literal
    try std.testing.expectEqualStrings("", extractLongestLiteral(".+"));

    // Escaped metacharacters become literals
    try std.testing.expectEqualStrings("foo.bar", extractLongestLiteral("foo\\.bar"));
    try std.testing.expectEqualStrings("foo.bar.baz", extractLongestLiteral("\\d+foo\\.bar\\.baz"));
    try std.testing.expectEqualStrings("(config)", extractLongestLiteral("\\(config\\)"));
    try std.testing.expectEqualStrings("path/to/file.txt", extractLongestLiteral("path/to/file\\.txt"));
    try std.testing.expectEqualStrings("hello-world", extractLongestLiteral("hello\\-world"));
    // Metachar classes break the run
    try std.testing.expectEqualStrings("error: ", extractLongestLiteral("error: \\d+"));
    try std.testing.expectEqualStrings(" in foo.bar", extractLongestLiteral("error: \\d+ in foo\\.bar"));
    // Special escapes become their chars
    try std.testing.expectEqualStrings("foo\tbar", extractLongestLiteral("foo\\tbar"));
    try std.testing.expectEqualStrings("line1\nline2", extractLongestLiteral("line1\\nline2"));
}

test "multiline pattern disables optimization" {
    // Regex patterns with actual newline should not use line-based optimization
    // Note: CLI uses interpretEscapes to convert \n to actual newline before compile
    // Use .+ to ensure it's treated as regex
    var pat = try compile(std.testing.allocator, "fo+\nbar", false, false);
    defer pat.deinit();
    // Should be regex and have no literal prefix optimization (multiline match)
    try std.testing.expect(pat == .regex);
    try std.testing.expect(pat.regex.literal == null);

    // Should match across lines
    const hay = "foo\nbar\nbaz";
    var it = MatchIterator.init(&pat, hay);
    defer it.deinit();
    const span = it.next();
    try std.testing.expect(span != null);
    try std.testing.expectEqualStrings("foo\nbar", hay[span.?.start..span.?.end]);
}

test "multiline match with semicolon" {
    // Pattern with actual newline (as CLI would process it)
    // Use regex escape \} to ensure it's treated as regex
    const alloc = std.testing.allocator;
    var pat = try compile(alloc, "return 1;\n\\}", false, false);
    defer pat.deinit();

    // Should have no literal prefix optimization (multiline)
    try std.testing.expect(pat == .regex);
    try std.testing.expect(pat.regex.literal == null);

    // Test with actual newline in data
    const data = "return 1;\n}";

    // matchAny should work
    try std.testing.expect(matchAny(&pat, data, false));

    // Iterator should work
    var it = MatchIterator.init(&pat, data);
    defer it.deinit();
    const span = it.next();
    try std.testing.expect(span != null);
    try std.testing.expectEqualStrings("return 1;\n}", data[span.?.start..span.?.end]);
}

test "inner literal optimization" {
    // Pattern "..tabase" should extract "tabase" as longest literal
    var pat2 = try compile(std.testing.allocator, "..tabase", false, false);
    defer pat2.deinit();
    try std.testing.expect(pat2.regex.literal != null);
    try std.testing.expectEqualStrings("tabase", pat2.regex.literal.?.needle);
}

test "prefix optimization filters non-matches" {
    // Pattern "meta.+se" should not match "foobar" (no literal "meta")
    var pat = try compile(std.testing.allocator, "meta.+se", false, false);
    defer pat.deinit();

    // Verify literal was extracted
    try std.testing.expect(pat.regex.literal != null);
    try std.testing.expectEqualStrings("meta", pat.regex.literal.?.needle);

    // Should match
    try std.testing.expect(matchAny(&pat, "metabase", false));
    try std.testing.expect(matchAny(&pat, "xx metaverse yy", false));

    // Should NOT match (fast path - prefix not found)
    try std.testing.expect(!matchAny(&pat, "foobar", false));
    try std.testing.expect(!matchAny(&pat, "metallica", false)); // has "meta" but no "se" after
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

test "bench regex literal optimization" {
    const alloc = std.testing.allocator;

    // Build haystack with lines containing different patterns:
    // - "metabase" (actual match for ..tabase)
    // - "database" (contains "tabase" but different prefix - false positive for literal scan)
    // - "notable" (no "tabase" - should skip)
    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);

    const lines_per_type = 10000;
    // Mix of matching and non-matching lines
    for (0..lines_per_type) |i| {
        // Line with actual match
        try hay.writer(alloc).print("line {d}: the metabase server is running\n", .{i * 3});
        // Line with literal but no match (database has "tabase" but not ..tabase pattern)
        try hay.writer(alloc).print("line {d}: connect to database system\n", .{i * 3 + 1});
        // Line without literal
        try hay.writer(alloc).print("line {d}: some random text here\n", .{i * 3 + 2});
    }
    const data = hay.items;

    std.debug.print("\nbench regex literal opt: {d}KB, {d} lines\n", .{ data.len / 1024, lines_per_type * 3 });

    // Pattern: ..tabase - should match "metabase" and "database"
    // Literal extracted: "tabase"
    var pat = try compile(alloc, "..tabase", false, false);
    defer pat.deinit();

    // Verify literal was extracted
    try std.testing.expect(pat.regex.literal != null);
    try std.testing.expectEqualStrings("tabase", pat.regex.literal.?.needle);

    var timer = try std.time.Timer.start();
    var count: usize = 0;
    var it = MatchIterator.init(&pat, data);
    defer it.deinit();
    while (it.next()) |_| count += 1;
    const ns = timer.read();

    // Should find metabase AND database (both match ..tabase)
    const expected = lines_per_type * 2;
    try std.testing.expectEqual(expected, count);
    std.debug.print("  ..tabase: {d}ms ({d} matches, {d} literal false positives)\n", .{
        ns / std.time.ns_per_ms,
        count,
        lines_per_type, // "notable" lines skipped by literal filter
    });

    // Threshold: should be < 200ms for 30k lines on reasonable hardware
    try std.testing.expect(ns < 500 * std.time.ns_per_ms);
}

test "bench inner literal seek" {
    const alloc = std.testing.allocator;

    // Test pattern: xx.+tabase (prefix "xx", inner ".+", then "tabase")
    // This tests inner literal when there's a safe prefix before the greedy part
    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);

    const total_lines = 30000;
    for (0..total_lines) |i| {
        if (i % 100 == 0) {
            // Actual match
            try hay.writer(alloc).print("line {d}: xxmetabase server\n", .{i});
        } else if (i % 10 == 0) {
            // Has "tabase" but no "xx" prefix
            try hay.writer(alloc).print("line {d}: database system\n", .{i});
        } else {
            // No literal
            try hay.writer(alloc).print("line {d}: random text content\n", .{i});
        }
    }
    const data = hay.items;

    std.debug.print("\nbench inner literal seek: {d}KB, {d} lines\n", .{ data.len / 1024, total_lines });

    // Pattern that benefits from inner literal: ..tabase (fixed prefix dots)
    var pat = try compile(alloc, "..tabase", false, false);
    defer pat.deinit();

    try std.testing.expect(pat.regex.literal != null);
    try std.testing.expectEqualStrings("tabase", pat.regex.literal.?.needle);

    var timer = try std.time.Timer.start();
    var count: usize = 0;
    var it = MatchIterator.init(&pat, data);
    defer it.deinit();
    while (it.next()) |_| count += 1;
    const ns = timer.read();

    // Matches xxmetabase (i % 100 == 0 → 300) and database (i % 10 == 0 but not % 100 → 2700)
    const expected = total_lines / 100 + (total_lines / 10 - total_lines / 100);
    try std.testing.expectEqual(expected, count);
    std.debug.print("  ..tabase: {d}ms ({d} matches)\n", .{ ns / std.time.ns_per_ms, count });

    try std.testing.expect(ns < 500 * std.time.ns_per_ms);
}

test "literal optimization for various patterns" {
    const alloc = std.testing.allocator;

    // All patterns with 3+ char literal get prefix optimization
    var pat = try compile(alloc, ".+metabase.+", false, false);
    defer pat.deinit();
    try std.testing.expect(pat.regex.literal != null);
    try std.testing.expectEqualStrings("metabase", pat.regex.literal.?.needle);

    var pat2 = try compile(alloc, ".*foo", false, false);
    defer pat2.deinit();
    try std.testing.expect(pat2.regex.literal != null);
    try std.testing.expectEqualStrings("foo", pat2.regex.literal.?.needle);

    var pat3 = try compile(alloc, "..tabase", false, false);
    defer pat3.deinit();
    try std.testing.expect(pat3.regex.literal != null);
    try std.testing.expectEqualStrings("tabase", pat3.regex.literal.?.needle);

    var pat4 = try compile(alloc, "meta.+se", false, false);
    defer pat4.deinit();
    try std.testing.expect(pat4.regex.literal != null);
    // "meta" is longer than "se"
    try std.testing.expectEqualStrings("meta", pat4.regex.literal.?.needle);
}

test "bench greedy pattern optimization" {
    // Greedy patterns (.+X.+) use literal optimization:
    // - BMH seeks literal position
    // - PCRE2 runs only on that line
    // Compare with raw PCRE2 (no optimization) to show speedup.
    const alloc = std.testing.allocator;

    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);

    const total_lines = 5000;
    for (0..total_lines) |i| {
        if (i % 100 == 0) {
            // Matches: has chars before AND after "metabase"
            try hay.writer(alloc).print("line {d}: the metabase server\n", .{i});
        } else if (i % 20 == 0) {
            // Has literal but won't match (at line start, no .+ before)
            try hay.writer(alloc).print("metabase documentation {d}\n", .{i});
        } else {
            try hay.writer(alloc).print("line {d}: random content\n", .{i});
        }
    }
    const data = hay.items;

    const expected = total_lines / 100; // Only lines with chars before AND after

    // With literal optimization (current behavior)
    var pat = try compile(alloc, ".+metabase.+", false, false);
    defer pat.deinit();
    try std.testing.expect(pat.regex.literal != null);

    var timer = try std.time.Timer.start();
    var count: usize = 0;
    var it = MatchIterator.init(&pat, data);
    defer it.deinit();
    while (it.next()) |_| count += 1;
    const opt_ns = timer.read();
    try std.testing.expectEqual(expected, count);

    // Without optimization: raw PCRE2 iteration (simulated by using pcre2 directly)
    timer.reset();
    count = 0;
    var raw_it = pcre2.MatchIterator.init(&pat.regex.code, data);
    defer raw_it.deinit();
    while (raw_it.next()) |_| count += 1;
    const raw_ns = timer.read();
    try std.testing.expectEqual(expected, count);

    const speedup = @as(f64, @floatFromInt(raw_ns)) / @as(f64, @floatFromInt(@max(opt_ns, 1)));

    std.debug.print("\nbench greedy prefix: {d}KB, {d} lines\n", .{ data.len / 1024, total_lines });
    std.debug.print("  .+metabase.+: optimized={d}ms raw={d}ms speedup={d:.1}x ({d} matches)\n", .{
        opt_ns / std.time.ns_per_ms,
        raw_ns / std.time.ns_per_ms,
        speedup,
        count,
    });

    // Should be at least 5x faster with filter optimization
    try std.testing.expect(speedup > 5.0);
}

test "bench prefix vs inner literal" {
    const alloc = std.testing.allocator;

    // Compare: prefix literal (meta.+se) vs inner literal (..tabase)
    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);

    const total_lines = 30000;
    for (0..total_lines) |i| {
        if (i % 50 == 0) {
            try hay.writer(alloc).print("line {d}: metabase metaverse metadata\n", .{i});
        } else {
            try hay.writer(alloc).print("line {d}: unrelated content here\n", .{i});
        }
    }
    const data = hay.items;

    std.debug.print("\nbench prefix vs inner: {d}KB, {d} lines\n", .{ data.len / 1024, total_lines });

    // Prefix literal: meta.+se
    var pat1 = try compile(alloc, "meta.+se", false, false);
    defer pat1.deinit();

    var timer = try std.time.Timer.start();
    var count1: usize = 0;
    var it1 = MatchIterator.init(&pat1, data);
    defer it1.deinit();
    while (it1.next()) |_| count1 += 1;
    const ns1 = timer.read();

    // Inner literal: ..tabase
    var pat2 = try compile(alloc, "..tabase", false, false);
    defer pat2.deinit();

    timer.reset();
    var count2: usize = 0;
    var it2 = MatchIterator.init(&pat2, data);
    defer it2.deinit();
    while (it2.next()) |_| count2 += 1;
    const ns2 = timer.read();

    std.debug.print("  meta.+se: {d}ms ({d} matches)\n", .{ ns1 / std.time.ns_per_ms, count1 });
    std.debug.print("  ..tabase: {d}ms ({d} matches)\n", .{ ns2 / std.time.ns_per_ms, count2 });

    // Both patterns match "metabase" - should have same count
    try std.testing.expectEqual(count1, count2);
}
