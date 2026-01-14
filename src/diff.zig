const std = @import("std");
const ansi = @import("core/ansi.zig");

const Line = struct {
    old: []const u8,
    new: []const u8,
    changed: bool,
};

pub fn printChangedLines(
    writer: anytype,
    color: bool,
    path: []const u8,
    old: []const u8,
    new: []const u8,
    before: usize,
    after: usize,
) !usize {
    // Collect all lines
    var lines: [4096]Line = undefined;
    var total: usize = 0;
    var n_changes: usize = 0;

    var it_old = std.mem.splitScalar(u8, old, '\n');
    var it_new = std.mem.splitScalar(u8, new, '\n');

    while (true) {
        const o = it_old.next();
        const nn = it_new.next();
        if (o == null and nn == null) break;
        if (total >= lines.len) break;
        const ol = o orelse "";
        const nl = nn orelse "";
        const changed = !std.mem.eql(u8, ol, nl);
        if (changed) n_changes += 1;
        lines[total] = .{ .old = ol, .new = nl, .changed = changed };
        total += 1;
    }

    if (n_changes == 0) return 0;

    // Find hunks (groups of changes with context, coalesced if overlapping)
    var hunk_start: usize = 0;
    var hunk_end: usize = 0;
    var in_hunk = false;

    var i: usize = 0;
    while (i < total) : (i += 1) {
        if (!lines[i].changed) continue;

        const ctx_start = if (i >= before) i - before else 0;
        const ctx_end = @min(i + after + 1, total);

        if (!in_hunk) {
            // Start new hunk
            hunk_start = ctx_start;
            hunk_end = ctx_end;
            in_hunk = true;
        } else if (ctx_start <= hunk_end) {
            // Overlaps/touches current hunk, extend it
            hunk_end = ctx_end;
        } else {
            // Gap - print current hunk, start new one
            try printHunk(writer, color, path, lines[0..total], hunk_start, hunk_end, before > 0 or after > 0);
            hunk_start = ctx_start;
            hunk_end = ctx_end;
        }
    }

    // Print final hunk
    if (in_hunk) {
        try printHunk(writer, color, path, lines[0..total], hunk_start, hunk_end, before > 0 or after > 0);
    }

    return n_changes;
}

fn printHunk(writer: anytype, color: bool, path: []const u8, lines: []const Line, start: usize, end: usize, has_context: bool) !void {
    // Header with line range (1-indexed)
    const first = start + 1;
    const last = end; // end is exclusive, so last line is end
    if (first == last) {
        try writer.print("── {f}:{f} ──\n", .{ ansi.styled(color, .path, path), ansi.styled(color, .heading, first) });
    } else {
        try writer.print("── {f}:{f}-{f} ──\n", .{ ansi.styled(color, .path, path), ansi.styled(color, .heading, first), ansi.styled(color, .heading, last) });
    }

    _ = has_context;
    const hunk = lines[start..end];
    var i: usize = 0;
    while (i < hunk.len) {
        if (!hunk[i].changed) {
            // Context line
            try writer.print("  {s}\n", .{hunk[i].old});
            i += 1;
        } else {
            // Find run of consecutive changes
            const run_start = i;
            while (i < hunk.len and hunk[i].changed) : (i += 1) {}
            // Print all deletions
            for (hunk[run_start..i]) |line| {
                try writer.print("{f} {f}\n", .{ ansi.styled(color, .del, "-"), ansi.styled(color, .del, line.old) });
            }
            // Print all additions
            for (hunk[run_start..i]) |line| {
                try writer.print("{f} {f}\n", .{ ansi.styled(color, .add, "+"), ansi.styled(color, .add, line.new) });
            }
        }
    }
}

test "diff prints only changed lines" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "a\nb\nc\n", "a\nbb\nc\n", 0, 0);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, s, "x.txt:2") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "- b") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "+ bb") != null);
}

test "diff groups consecutive changes no context" {
    var buf: [512]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "a\nb\nc\nd\n", "a\nB\nC\nd\n", 0, 0);
    try std.testing.expectEqual(@as(usize, 2), n);
    const s = fbs.getWritten();
    // Should have single header for lines 2-3
    try std.testing.expect(std.mem.indexOf(u8, s, "x.txt:2-3") != null);
}

test "diff with context" {
    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "1\n2\n3\n4\n5\n", "1\n2\nX\n4\n5\n", 1, 1);
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();
    // Should show context lines 2 and 4
    try std.testing.expect(std.mem.indexOf(u8, s, "  2") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "  4") != null);
}

test "diff groups consecutive changes" {
    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    // Lines 2,3 change (consecutive) - should group deletions then additions
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "1\n2\n3\n4\n", "1\nA\nB\n4\n", 1, 1);
    try std.testing.expectEqual(@as(usize, 2), n);
    const s = fbs.getWritten();

    // Consecutive changes: all deletions, then all additions
    const expected =
        \\── x.txt:1-4 ──
        \\  1
        \\- 2
        \\- 3
        \\+ A
        \\+ B
        \\  4
        \\
    ;
    try std.testing.expectEqualStrings(expected, s);
}

test "diff separates non-consecutive changes with context" {
    var buf: [1024]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    // Lines 2 and 4 change (non-consecutive) - context line 3 separates them
    const n = try printChangedLines(fbs.writer(), false, "x.txt", "1\n2\n3\n4\n5\n", "1\nA\n3\nB\n5\n", 1, 1);
    try std.testing.expectEqual(@as(usize, 2), n);
    const s = fbs.getWritten();

    // Non-consecutive: context separates the change groups
    const expected =
        \\── x.txt:1-5 ──
        \\  1
        \\- 2
        \\+ A
        \\  3
        \\- 4
        \\+ B
        \\  5
        \\
    ;
    try std.testing.expectEqualStrings(expected, s);
}
