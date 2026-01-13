const std = @import("std");

pub const Matcher = struct {
    needle: []const u8,
    ignore_case: bool,
    shift: [256]u32,

    pub fn init(needle: []const u8, ignore_case: bool) Matcher {
        var self: Matcher = .{
            .needle = needle,
            .ignore_case = ignore_case,
            .shift = undefined,
        };
        const n: u32 = @intCast(@min(needle.len, std.math.maxInt(u32)));
        @memset(&self.shift, n);
        if (needle.len >= 2) {
            const last = needle.len - 1;
            for (0..needle.len) |i| {
                const c = self.fold(needle[i]);
                self.shift[c] = @intCast(last -| i);
            }
        }
        return self;
    }

    pub fn contains(self: *const Matcher, hay: []const u8) bool {
        return self.indexOfPos(hay, 0) != null;
    }

    pub fn indexOfPos(self: *const Matcher, hay: []const u8, start: usize) ?usize {
        const needle = self.needle;
        if (needle.len == 0) return start;
        if (needle.len > hay.len or start > hay.len - needle.len) return null;

        // SIMD fast path for case-sensitive
        if (!self.ignore_case) {
            return simdIndexOf(hay, start, needle);
        }

        // BMH for case-insensitive
        if (needle.len == 1) {
            const n0 = self.fold(needle[0]);
            var i: usize = start;
            while (i < hay.len) : (i += 1) {
                if (self.fold(hay[i]) == n0) return i;
            }
            return null;
        }

        const last: usize = needle.len - 1;
        var i: usize = start + last;
        while (i < hay.len) {
            const s: u32 = self.shift[self.fold(hay[i])];
            if (s != 0) {
                i += s;
                continue;
            }

            const pos = i - last;
            if (self.eqAt(hay, pos)) return pos;
            i += 1;
        }
        return null;
    }

    fn simdIndexOf(hay: []const u8, start: usize, needle: []const u8) ?usize {
        const Vec = @Vector(32, u8);
        const first: Vec = @splat(needle[0]);
        const needle_len = needle.len;

        var i: usize = start;
        // SIMD loop: process 32 bytes at a time
        while (i + 32 + needle_len - 1 <= hay.len) {
            const chunk: Vec = hay[i..][0..32].*;
            const eq = chunk == first;
            var mask: u32 = @bitCast(eq);
            while (mask != 0) {
                const bit: u5 = @truncate(@ctz(mask));
                const pos = i + bit;
                if (std.mem.startsWith(u8, hay[pos..], needle)) return pos;
                mask &= mask - 1;
            }
            i += 32;
        }
        // Scalar tail
        return std.mem.indexOfPos(u8, hay, i, needle);
    }

    pub fn count(self: *const Matcher, hay: []const u8) usize {
        const needle = self.needle;
        if (needle.len == 0) return 0;
        var off: usize = 0;
        var n: usize = 0;
        while (true) {
            const idx = self.indexOfPos(hay, off) orelse break;
            n += 1;
            off = idx + needle.len;
            if (off > hay.len) break;
        }
        return n;
    }

    inline fn fold(self: *const Matcher, b: u8) u8 {
        return if (self.ignore_case) std.ascii.toLower(b) else b;
    }

    fn eqAt(self: *const Matcher, hay: []const u8, pos: usize) bool {
        const needle = self.needle;
        if (pos + needle.len > hay.len) return false;
        if (!self.ignore_case) return std.mem.eql(u8, hay[pos .. pos + needle.len], needle);
        for (needle, 0..) |c, j| {
            if (std.ascii.toLower(hay[pos + j]) != std.ascii.toLower(c)) return false;
        }
        return true;
    }
};

test "bmh basics" {
    const m = Matcher.init("needle", false);
    try std.testing.expectEqual(@as(?usize, 8), m.indexOfPos("hay hay needle ok", 0));
    try std.testing.expectEqual(@as(usize, 2), Matcher.init("aa", false).count("aaaaa"));
    // case-sensitive must NOT match wrong case
    try std.testing.expect(!Matcher.init("Foo", false).contains("foo bar"));
    try std.testing.expect(Matcher.init("Foo", false).contains("Foo bar"));
}

test "bmh ignore-case" {
    const m = Matcher.init("Foo", true);
    try std.testing.expect(m.contains("xx foo yy"));
    try std.testing.expectEqual(@as(usize, 2), m.count("FOO foo"));
}

// Naive search for benchmark comparison
fn naiveIndexOf(hay: []const u8, needle: []const u8, ignore_case: bool) ?usize {
    if (needle.len == 0) return 0;
    if (needle.len > hay.len) return null;
    var i: usize = 0;
    outer: while (i + needle.len <= hay.len) : (i += 1) {
        for (needle, 0..) |c, j| {
            const h = hay[i + j];
            const n = c;
            const eq = if (ignore_case) std.ascii.toLower(h) == std.ascii.toLower(n) else h == n;
            if (!eq) continue :outer;
        }
        return i;
    }
    return null;
}

fn naiveCount(hay: []const u8, needle: []const u8, ignore_case: bool) usize {
    if (needle.len == 0) return 0;
    var off: usize = 0;
    var n: usize = 0;
    while (true) {
        const idx = naiveIndexOf(hay[off..], needle, ignore_case) orelse break;
        n += 1;
        off += idx + needle.len;
        if (off > hay.len) break;
    }
    return n;
}

test "bench bmh vs naive" {
    const alloc = std.testing.allocator;

    // Generate ~1MB haystack with pattern every ~1KB
    const chunk = "abcdefghijklmnopqrstuvwxyz0123456789_" ** 28; // ~1KB
    const needle = "PATTERN_HERE";
    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);
    for (0..1000) |_| {
        try hay.appendSlice(alloc, chunk);
        try hay.appendSlice(alloc, needle);
    }
    const data = hay.items;
    std.debug.print("\nhaystack: {d}KB, needle: {d}B, expected matches: 1000\n", .{ data.len / 1024, needle.len });

    const iters: u32 = 10;
    var bmh_ns: u64 = 0;
    var naive_ns: u64 = 0;

    // case-sensitive
    {
        const m = Matcher.init(needle, false);
        var timer = try std.time.Timer.start();
        for (0..iters) |_| _ = m.count(data);
        bmh_ns = timer.read();

        timer.reset();
        for (0..iters) |_| _ = naiveCount(data, needle, false);
        naive_ns = timer.read();

        std.debug.print("case-sensitive ({d} iters): bmh={d}ms naive={d}ms speedup={d:.1}x\n", .{
            iters,
            bmh_ns / std.time.ns_per_ms,
            naive_ns / std.time.ns_per_ms,
            @as(f64, @floatFromInt(naive_ns)) / @as(f64, @floatFromInt(@max(bmh_ns, 1))),
        });
        try std.testing.expectEqual(@as(usize, 1000), m.count(data));
    }

    // case-insensitive
    {
        const m = Matcher.init(needle, true);
        var timer = try std.time.Timer.start();
        for (0..iters) |_| _ = m.count(data);
        bmh_ns = timer.read();

        timer.reset();
        for (0..iters) |_| _ = naiveCount(data, needle, true);
        naive_ns = timer.read();

        std.debug.print("case-insensitive ({d} iters): bmh={d}ms naive={d}ms speedup={d:.1}x\n", .{
            iters,
            bmh_ns / std.time.ns_per_ms,
            naive_ns / std.time.ns_per_ms,
            @as(f64, @floatFromInt(naive_ns)) / @as(f64, @floatFromInt(@max(bmh_ns, 1))),
        });
        try std.testing.expectEqual(@as(usize, 1000), m.count(data));
    }
}
