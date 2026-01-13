const std = @import("std");

pub fn printChangedLines(writer: anytype, path: []const u8, old: []const u8, new: []const u8) !usize {
    var n: usize = 0;
    var it_old = std.mem.splitScalar(u8, old, '\n');
    var it_new = std.mem.splitScalar(u8, new, '\n');

    var line_no: usize = 1;
    while (true) : (line_no += 1) {
        const o = it_old.next();
        const nn = it_new.next();
        if (o == null and nn == null) break;
        const ol = o orelse "";
        const nl = nn orelse "";
        if (std.mem.eql(u8, ol, nl)) continue;
        n += 1;
        try writer.print("── {s}:{d} ──\n", .{ path, line_no });
        try writer.print("- {s}\n", .{ol});
        try writer.print("+ {s}\n", .{nl});
    }
    return n;
}

test "diff prints only changed lines" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const n = try printChangedLines(fbs.writer(), "x.txt", "a\nb\nc\n", "a\nbb\nc\n");
    try std.testing.expectEqual(@as(usize, 1), n);
    const s = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, s, "x.txt:2") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "- b") != null);
    try std.testing.expect(std.mem.indexOf(u8, s, "+ bb") != null);
}

