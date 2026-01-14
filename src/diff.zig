const std = @import("std");
const ansi = @import("core/ansi.zig");

/// Print diff showing changed regions as separate hunks.
/// Returns number of replacement operations.
pub fn printChangedLines(
    writer: anytype,
    color: bool,
    path: []const u8,
    old: []const u8,
    new: []const u8,
    ctx_before: usize,
    ctx_after: usize,
) !usize {
    const old_lines = try lineSlices(old);
    const new_lines = try lineSlices(new);

    // Same line count: show each differing line as separate hunk
    if (old_lines.len == new_lines.len) {
        var n: usize = 0;
        var i: usize = 0;
        while (i < old_lines.len) {
            if (!std.mem.eql(u8, old_lines.items()[i], new_lines.items()[i])) {
                // Find end of contiguous change
                var end = i + 1;
                while (end < old_lines.len and
                    !std.mem.eql(u8, old_lines.items()[end], new_lines.items()[end]))
                {
                    end += 1;
                }
                try printHunk(writer, color, path, old_lines, new_lines, i, end, i, end, ctx_before, ctx_after);
                n += 1;
                i = end;
            } else {
                i += 1;
            }
        }
        return n;
    }

    // Different line counts: find changed region
    var first_diff: usize = 0;
    while (first_diff < old_lines.len and first_diff < new_lines.len and
        std.mem.eql(u8, old_lines.items()[first_diff], new_lines.items()[first_diff]))
    {
        first_diff += 1;
    }

    if (first_diff == old_lines.len and first_diff == new_lines.len) return 0;

    var old_end = old_lines.len;
    var new_end = new_lines.len;
    while (old_end > first_diff and new_end > first_diff and
        std.mem.eql(u8, old_lines.items()[old_end - 1], new_lines.items()[new_end - 1]))
    {
        old_end -= 1;
        new_end -= 1;
    }

    try printHunk(writer, color, path, old_lines, new_lines, first_diff, old_end, first_diff, new_end, ctx_before, ctx_after);
    return 1;
}

fn printHunk(
    writer: anytype,
    color: bool,
    path: []const u8,
    old_lines: LineSlices,
    new_lines: LineSlices,
    old_start: usize,
    old_end: usize,
    new_start: usize,
    new_end: usize,
    ctx_before: usize,
    ctx_after: usize,
) !void {
    const ctx_start = if (old_start >= ctx_before) old_start - ctx_before else 0;
    const ctx_end_old = @min(old_end + ctx_after, old_lines.len);
    const ctx_end_new = @min(new_end + ctx_after, new_lines.len);

    // Header
    const first = ctx_start + 1;
    const last = ctx_end_old;
    if (first == last) {
        try writer.print("── {f}:{f} ──\n", .{ ansi.styled(color, .path, path), ansi.styled(color, .heading, first) });
    } else {
        try writer.print("── {f}:{f}-{f} ──\n", .{ ansi.styled(color, .path, path), ansi.styled(color, .heading, first), ansi.styled(color, .heading, last) });
    }

    // Context before
    for (old_lines.items()[ctx_start..old_start]) |line| {
        try writer.print("  {s}\n", .{line});
    }

    // Deletions
    for (old_lines.items()[old_start..old_end]) |line| {
        try writer.print("{f} {f}\n", .{ ansi.styled(color, .del, "-"), ansi.styled(color, .del, line) });
    }

    // Additions
    for (new_lines.items()[new_start..new_end]) |line| {
        try writer.print("{f} {f}\n", .{ ansi.styled(color, .add, "+"), ansi.styled(color, .add, line) });
    }

    // Context after
    if (ctx_end_new > new_end) {
        for (new_lines.items()[new_end..ctx_end_new]) |line| {
            try writer.print("  {s}\n", .{line});
        }
    }
}

/// Split content into lines, returning slices into the original data.
/// Uses a fixed-size buffer to avoid allocation.
fn lineSlices(data: []const u8) error{FileTooLarge}!LineSlices {
    var result = LineSlices{};
    var it = std.mem.splitScalar(u8, data, '\n');
    while (it.next()) |line| {
        if (result.len >= result.buf.len) return error.FileTooLarge;
        result.buf[result.len] = line;
        result.len += 1;
    }
    // Remove trailing empty line (artifact of trailing newline)
    if (result.len > 0 and result.buf[result.len - 1].len == 0) {
        result.len -= 1;
    }
    return result;
}

const LineSlices = struct {
    buf: [4096][]const u8 = undefined,
    len: usize = 0,

    fn items(self: *const LineSlices) []const []const u8 {
        return self.buf[0..self.len];
    }
};

test "diff simple line change" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "a\nb\nc\n", "a\nbb\nc\n", 0, 0);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, s, "x.txt:2") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "- b") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "+ bb") != null);
}

test "diff multiline to single line" {
    var buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    // Replace 3 lines with 1 line
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "a\nb\nc\nd\n", "a\nX\nd\n", 0, 0);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();

    const expected =
        \\── x.txt:2-3 ──
        \\- b
        \\- c
        \\+ X
        \\
    ;
    try std.testing.expectEqualStrings(expected, s);
}

test "diff single line to multiline" {
    var buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    // Replace 1 line with 3 lines
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "a\nX\nd\n", "a\nb\nc\nd\n", 0, 0);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();

    const expected =
        \\── x.txt:2 ──
        \\- X
        \\+ b
        \\+ c
        \\
    ;
    try std.testing.expectEqualStrings(expected, s);
}

test "diff with context" {
    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "1\n2\n3\n4\n5\n", "1\n2\nX\n4\n5\n", 1, 1);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();

    const expected =
        \\── x.txt:2-4 ──
        \\  2
        \\- 3
        \\+ X
        \\  4
        \\
    ;
    try std.testing.expectEqualStrings(expected, s);
}

test "diff multiline replacement with context" {
    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    // old has 3 lines that become 1, with context
    const old = "header\nold {\n    body\n}\nfooter\n";
    const new = "header\nnew { updated }\nfooter\n";
    const n = try printChangedLines(fbs.writer(), false, "x.txt", old, new, 1, 1);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();

    const expected =
        \\── x.txt:1-5 ──
        \\  header
        \\- old {
        \\-     body
        \\- }
        \\+ new { updated }
        \\  footer
        \\
    ;
    try std.testing.expectEqualStrings(expected, s);
}

test "diff no change" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "same\n", "same\n", 0, 0);
    try std.testing.expectEqual(@as(usize, 0), n);
    try std.testing.expectEqual(@as(usize, 0), fbs.getWritten().len);
}
