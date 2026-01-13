const std = @import("std");
const ignore = @import("ignore.zig");

pub fn collectFiles(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *const ignore.Ignorer,
    paths: []const []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
) ![][]const u8 {
    var list: std.ArrayListUnmanaged([]const u8) = .{};
    errdefer {
        for (list.items) |p| allocator.free(p);
        list.deinit(allocator);
    }

    if (paths.len == 0) {
        try collectRoot(allocator, cwd, ign, &list, ".", include, exclude);
    } else for (paths) |root| {
        try collectRoot(allocator, cwd, ign, &list, root, include, exclude);
    }

    return try list.toOwnedSlice(allocator);
}

fn collectRoot(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *const ignore.Ignorer,
    list: *std.ArrayListUnmanaged([]const u8),
    root: []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
) !void {
    const st = cwd.statFile(root) catch |e| switch (e) {
        error.FileNotFound => return,
        else => return e,
    };

        if (st.kind == .directory) {
            const prefix = if (std.mem.eql(u8, root, ".") or std.mem.eql(u8, root, "./")) "" else root;
            var d = try cwd.openDir(root, .{ .iterate = true });
            defer d.close();
            try collectDir(allocator, cwd, ign, list, prefix, d, include, exclude);
        } else {
            if (!passes(root, include, exclude)) return;
            if (ign.match(root, false)) return;
            try list.append(allocator, try allocator.dupe(u8, root));
        }
}

fn collectDir(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *const ignore.Ignorer,
    list: *std.ArrayListUnmanaged([]const u8),
    prefix: []const u8,
    dir: std.fs.Dir,
    include: []const []const u8,
    exclude: []const []const u8,
) !void {
    var it = dir.iterate();
    while (try it.next()) |ent| {
        const rel = if (prefix.len == 0)
            try allocator.dupe(u8, ent.name)
        else
            try std.fs.path.join(allocator, &.{ prefix, ent.name });
        defer allocator.free(rel);

        if (!passes(rel, include, exclude)) continue;
        if (ign.match(rel, ent.kind == .directory)) continue;

        switch (ent.kind) {
            .file => try list.append(allocator, try allocator.dupe(u8, rel)),
            .directory => {
                var sub = try cwd.openDir(rel, .{ .iterate = true });
                defer sub.close();
                try collectDir(allocator, cwd, ign, list, rel, sub, include, exclude);
            },
            else => {},
        }
    }
}

fn passes(path: []const u8, include: []const []const u8, exclude: []const []const u8) bool {
    for (exclude) |x| if (std.mem.indexOf(u8, path, x) != null) return false;
    if (include.len == 0) return true;
    for (include) |g| if (std.mem.indexOf(u8, path, g) != null) return true;
    return false;
}

test "collect files with include/exclude" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("src");
    try td.dir.makeDir("test");
    try td.dir.writeFile(.{ .sub_path = "src/a.clj", .data = "x\n" });
    try td.dir.writeFile(.{ .sub_path = "test/b.clj", .data = "x\n" });

    var ign = try ignore.Ignorer.init(std.testing.allocator, td.dir);
    defer ign.deinit();

    const got = try collectFiles(std.testing.allocator, td.dir, &ign, &.{ "." }, &.{ "src" }, &.{ "test" });
    defer {
        for (got) |p| std.testing.allocator.free(p);
        std.testing.allocator.free(got);
    }
    try std.testing.expectEqual(@as(usize, 1), got.len);
    try std.testing.expect(std.mem.endsWith(u8, got[0], "src/a.clj"));
}
