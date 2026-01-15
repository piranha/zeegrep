const std = @import("std");
const pcre2 = @import("pcre2.zig");
const ansi = @import("core/ansi.zig");
const bmh = @import("core/bmh.zig");

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
/// - prefix: seek literal then run regex from nearby (for non-greedy patterns)
/// - filter: check if line contains literal before running regex (for greedy patterns)
pub const OptimizedRegex = struct {
    code: pcre2.Code,
    prefix: ?bmh.Matcher = null, // seek optimization (backtrack-safe patterns)
    filter: ?bmh.Matcher = null, // line filter (greedy patterns - filter only, no seek)

    pub fn deinit(self: *OptimizedRegex) void {
        self.code.deinit();
        self.prefix = null;
        self.filter = null;
    }
};

pub fn compile(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool, dotall: bool) !Pattern {
    return compileOpts(allocator, pat, ignore_case, dotall, false);
}

pub fn compileOpts(allocator: std.mem.Allocator, pat: []const u8, ignore_case: bool, dotall: bool, force_literal: bool) !Pattern {
    if (!force_literal and isRegex(pat)) {
        const code = try pcre2.compile(allocator, pat, ignore_case, dotall);
        const literal_str = extractLongestLiteral(pat);
        const has_literal = literal_str.len >= 3;
        const is_greedy = startsWithGreedy(pat);
        const is_multiline = containsNewline(pat) or dotall;

        // Choose optimization strategy:
        // - prefix: safe to seek literal and backtrack (non-greedy, single-line)
        // - filter: only filter lines by literal, run regex per-line (greedy patterns)
        // - none: multiline patterns can't use line-based optimization
        var prefix: ?bmh.Matcher = null;
        var filter: ?bmh.Matcher = null;

        if (has_literal and !is_multiline) {
            if (is_greedy) {
                filter = bmh.Matcher.init(literal_str, ignore_case);
            } else {
                prefix = bmh.Matcher.init(literal_str, ignore_case);
            }
        }
        return .{ .regex = .{ .code = code, .prefix = prefix, .filter = filter } };
    }
    return .{ .literal = bmh.Matcher.init(pat, ignore_case) };
}

/// Check if pattern contains actual newline - our line-based optimization won't work.
fn containsNewline(pat: []const u8) bool {
    return std.mem.indexOfScalar(u8, pat, '\n') != null;
}

/// Check if pattern starts with greedy quantifier that can cause backtracking.
fn startsWithGreedy(pat: []const u8) bool {
    if (pat.len < 2) return false;
    // .+ .* at start
    if (pat[0] == '.' and (pat[1] == '+' or pat[1] == '*')) return true;
    // [...]+ [...]*  at start
    if (pat[0] == '[') {
        if (std.mem.indexOfScalar(u8, pat, ']')) |end| {
            if (end + 1 < pat.len and (pat[end + 1] == '+' or pat[end + 1] == '*')) return true;
        }
    }
    // ^.+ ^.*
    if (pat[0] == '^' and pat.len >= 3 and pat[1] == '.' and (pat[2] == '+' or pat[2] == '*')) return true;
    return false;
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

/// Extract the longest literal substring from a regex pattern.
/// Scans for all literal runs and returns the longest one.
fn extractLongestLiteral(pat: []const u8) []const u8 {
    var best_start: usize = 0;
    var best_len: usize = 0;
    var run_start: usize = 0;
    var i: usize = 0;

    while (i < pat.len) {
        const c = pat[i];
        const is_meta = switch (c) {
            '.', '+', '*', '?', '(', ')', '[', ']', '{', '}', '|', '^', '$' => true,
            '\\' => blk: {
                // Check if escape sequence is a metachar class
                if (i + 1 < pat.len) {
                    const next = pat[i + 1];
                    break :blk switch (next) {
                        'd', 'D', 'w', 'W', 's', 'S', 'b', 'B' => true, // char classes
                        else => true, // treat all escapes as breaking literal for simplicity
                    };
                }
                break :blk true;
            },
            else => false,
        };

        if (is_meta) {
            // End of literal run
            const run_len = i - run_start;
            if (run_len > best_len) {
                best_start = run_start;
                best_len = run_len;
            }
            // Skip past metachar
            if (c == '\\' and i + 1 < pat.len) {
                i += 2;
            } else {
                i += 1;
            }
            run_start = i;
        } else {
            i += 1;
        }
    }

    // Check final run
    const run_len = i - run_start;
    if (run_len > best_len) {
        best_start = run_start;
        best_len = run_len;
    }

    return pat[best_start .. best_start + best_len];
}

pub fn matchAny(pat: *const Pattern, hay: []const u8, ignore_case: bool) bool {
    _ = ignore_case; // baked into pattern at compile time
    switch (pat.*) {
        .literal => |*m| return m.contains(hay),
        .regex => |*r| {
            // Fast path: if literal filter exists and not found, skip regex
            const lit = r.prefix orelse r.filter;
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
            const lit = r.prefix orelse r.filter;
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
    prefix: ?*const bmh.Matcher, // seek optimization
    filter: ?*const bmh.Matcher, // line filter optimization
    hay: []const u8,
    pos: usize = 0,
    // Max chars the pattern can match before the literal (for backtrack limit)
    // Conservative estimate: 256 chars should cover most practical patterns
    const MAX_BACKTRACK: usize = 256;

    fn init(r: *const OptimizedRegex, hay: []const u8) RegexIterator {
        return .{
            .inner = pcre2.MatchIterator.init(&r.code, hay),
            .prefix = if (r.prefix) |*p| p else null,
            .filter = if (r.filter) |*f| f else null,
            .hay = hay,
        };
    }

    fn deinit(self: *RegexIterator) void {
        self.inner.deinit();
    }

    fn next(self: *RegexIterator) ?Span {
        // Prefix optimization: seek literal, backtrack limited amount, run regex
        if (self.prefix) |lit| return self.nextWithPrefix(lit);

        // Filter optimization: scan lines for literal, run regex only on matching lines
        if (self.filter) |lit| return self.nextWithFilter(lit);

        // No optimization: raw pcre2 iteration
        return self.inner.next();
    }

    /// Seek literal with BMH, back up limited amount, run regex from there.
    fn nextWithPrefix(self: *RegexIterator, lit: *const bmh.Matcher) ?Span {
        while (true) {
            const lit_pos = lit.indexOfPos(self.hay, self.pos) orelse return null;

            const line_start = if (std.mem.lastIndexOfScalar(u8, self.hay[0..lit_pos], '\n')) |nl|
                nl + 1
            else
                0;
            const line_end = if (std.mem.indexOfScalarPos(u8, self.hay, lit_pos, '\n')) |nl|
                nl
            else
                self.hay.len;

            const backtrack_limit = if (lit_pos > MAX_BACKTRACK) lit_pos - MAX_BACKTRACK else 0;
            const search_start = @max(@max(line_start, backtrack_limit), self.inner.pos);

            if (search_start <= lit_pos) {
                if (self.inner.nextFrom(search_start)) |span| {
                    if (span.end <= line_end + 1) {
                        self.pos = if (span.end > span.start) span.end else span.start + 1;
                        self.inner.setPos(self.pos);
                        return span;
                    }
                }
            }

            self.pos = line_end + 1;
            self.inner.setPos(self.pos);
        }
    }

    /// Filter lines by literal, run regex only on lines containing the literal.
    /// For greedy patterns (.+foo.+) where seek optimization would cause backtracking.
    fn nextWithFilter(self: *RegexIterator, lit: *const bmh.Matcher) ?Span {
        while (self.pos < self.hay.len) {
            // Find next line
            const line_start = self.pos;
            const line_end = std.mem.indexOfScalarPos(u8, self.hay, line_start, '\n') orelse self.hay.len;
            const line = self.hay[line_start..line_end];

            // Move to next line for next iteration
            self.pos = if (line_end < self.hay.len) line_end + 1 else self.hay.len;

            // Skip line if literal not present
            if (!lit.contains(line)) continue;

            // Run regex on this line only
            // Create a temporary match on the line slice, but need to return offsets into original hay
            if (pcre2.matchAny(self.inner.code, line)) {
                // Get actual match position within line
                var line_iter = pcre2.MatchIterator.init(self.inner.code, line);
                defer line_iter.deinit();
                if (line_iter.next()) |span| {
                    return .{
                        .start = line_start + span.start,
                        .end = line_start + span.end,
                    };
                }
            }
        }
        return null;
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
            const lit = r.prefix orelse r.filter;
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
            const lit = r.prefix orelse r.filter;
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

test "extractLongestLiteral" {
    // Inner literals
    try std.testing.expectEqualStrings("tabase", extractLongestLiteral("..tabase"));
    try std.testing.expectEqualStrings("taba", extractLongestLiteral("..taba.."));
    try std.testing.expectEqualStrings("metabase", extractLongestLiteral(".+metabase.+"));
    // Prefix is longest
    try std.testing.expectEqualStrings("meta", extractLongestLiteral("meta.+se"));
    // No literal
    try std.testing.expectEqualStrings("", extractLongestLiteral(".+"));
}

test "multiline pattern disables optimization" {
    // Regex patterns with actual newline should not use line-based optimization
    // Note: CLI uses interpretEscapes to convert \n to actual newline before compile
    // Use .+ to ensure it's treated as regex
    var pat = try compile(std.testing.allocator, "fo+\nbar", false, false);
    defer pat.deinit();
    // Should be regex and have no literal prefix optimization (multiline match)
    try std.testing.expect(pat == .regex);
    try std.testing.expect(pat.regex.prefix == null);

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
    try std.testing.expect(pat.regex.prefix == null);

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
    try std.testing.expect(pat2.regex.prefix != null);
    try std.testing.expectEqualStrings("tabase", pat2.regex.prefix.?.needle);
}

test "prefix optimization filters non-matches" {
    // Pattern "meta.+se" should not match "foobar" (no literal "meta")
    var pat = try compile(std.testing.allocator, "meta.+se", false, false);
    defer pat.deinit();

    // Verify literal was extracted
    try std.testing.expect(pat.regex.prefix != null);
    try std.testing.expectEqualStrings("meta", pat.regex.prefix.?.needle);

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
    try std.testing.expect(pat.regex.prefix != null);
    try std.testing.expectEqualStrings("tabase", pat.regex.prefix.?.needle);

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

    try std.testing.expect(pat.regex.prefix != null);
    try std.testing.expectEqualStrings("tabase", pat.regex.prefix.?.needle);

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

test "greedy prefix uses filter optimization" {
    const alloc = std.testing.allocator;

    // .+metabase.+ should use FILTER (not prefix) - greedy prefix causes backtracking
    var pat = try compile(alloc, ".+metabase.+", false, false);
    defer pat.deinit();
    try std.testing.expect(pat.regex.prefix == null);
    try std.testing.expect(pat.regex.filter != null);
    try std.testing.expectEqualStrings("metabase", pat.regex.filter.?.needle);

    // .*foo should use filter
    var pat2 = try compile(alloc, ".*foo", false, false);
    defer pat2.deinit();
    try std.testing.expect(pat2.regex.prefix == null);
    try std.testing.expect(pat2.regex.filter != null);

    // ..tabase SHOULD use prefix (fixed width, safe to backtrack)
    var pat3 = try compile(alloc, "..tabase", false, false);
    defer pat3.deinit();
    try std.testing.expect(pat3.regex.prefix != null);
    try std.testing.expect(pat3.regex.filter == null);

    // meta.+se SHOULD use prefix (literal prefix, safe)
    var pat4 = try compile(alloc, "meta.+se", false, false);
    defer pat4.deinit();
    try std.testing.expect(pat4.regex.prefix != null);
    try std.testing.expect(pat4.regex.filter == null);
}

test "bench greedy prefix filter optimization" {
    // Greedy prefix patterns (.+X.+) use line filter optimization:
    // - BMH scans for literal to filter lines
    // - PCRE2 runs only on lines containing the literal
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

    // With filter optimization (current behavior)
    var pat = try compile(alloc, ".+metabase.+", false, false);
    defer pat.deinit();
    try std.testing.expect(pat.regex.filter != null);

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
