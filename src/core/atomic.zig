const std = @import("std");

var tmp_counter = std.atomic.Value(u64).init(0);

pub fn replaceFileAtomic(allocator: std.mem.Allocator, dir: std.fs.Dir, path: []const u8, data: []const u8) !void {
    const ts: i128 = std.time.nanoTimestamp();
    const cnt = tmp_counter.fetchAdd(1, .monotonic);
    const seed = @as(u64, @truncate(@as(u128, @bitCast(ts)))) +% cnt;
    var prng = std.Random.DefaultPrng.init(seed);
    const r = prng.random();

    const tmp = try std.fmt.allocPrint(allocator, ".zg-tmp-{x}", .{r.int(u64)});
    defer allocator.free(tmp);

    {
        var f = try dir.createFile(tmp, .{ .truncate = true, .read = false });
        defer f.close();
        try f.writeAll(data);
        f.sync() catch {};
    }

    dir.rename(tmp, path) catch |e| switch (e) {
        error.PathAlreadyExists => {
            dir.deleteFile(path) catch {};
            try dir.rename(tmp, path);
        },
        else => return e,
    };
}

test "atomic replace writes bytes" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.writeFile(.{ .sub_path = "x.txt", .data = "old\n" });
    try replaceFileAtomic(std.testing.allocator, td.dir, "x.txt", "new\n");

    var buf: [16]u8 = undefined;
    const got = try td.dir.readFile("x.txt", &buf);
    try std.testing.expectEqualStrings("new\n", got);
}
