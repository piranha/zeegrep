const std = @import("std");
const ignore = @import("ignore.zig");
const glob = @import("glob.zig");

pub fn collectFiles(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    paths: []const []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
) ![][]const u8 {
    var list: std.ArrayListUnmanaged([]const u8) = .{};
    errdefer {
        for (list.items) |p| allocator.free(p);
        list.deinit(allocator);
    }

    if (paths.len == 0) {
        try collectRoot(allocator, cwd, ign, &list, ".", include, exclude, hidden);
    } else for (paths) |root| {
        try collectRoot(allocator, cwd, ign, &list, root, include, exclude, hidden);
    }

    return try list.toOwnedSlice(allocator);
}

/// Walk files and call callback for each file (streaming, no collection)
pub fn walkFiles(
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    paths: []const []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    ctx: anytype,
) !void {
    if (paths.len == 0) {
        try walkRoot(cwd, ign, ".", include, exclude, hidden, ctx);
    } else for (paths) |root| {
        try walkRoot(cwd, ign, root, include, exclude, hidden, ctx);
    }
}

fn walkRoot(
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    root: []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    ctx: anytype,
) !void {
    const st = cwd.statFile(root) catch |e| switch (e) {
        error.FileNotFound => return,
        else => return e,
    };

    if (st.kind == .directory) {
        const prefix = if (std.mem.eql(u8, root, ".") or std.mem.eql(u8, root, "./")) "" else root;
        var d = try cwd.openDir(root, .{ .iterate = true });
        defer d.close();
        if (prefix.len != 0) {
            try ign.pushDir(d, prefix);
            defer ign.popDir();
        }
        try walkDir(cwd, ign, prefix, d, include, exclude, hidden, ctx);
    } else {
        if (!passesFile(root, include, exclude)) return;
        if (ign.ignored(root, false, hidden)) return;
        ctx.onFile(root);
    }
}

fn walkDir(
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    prefix: []const u8,
    dir: std.fs.Dir,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    ctx: anytype,
) !void {
    var pathbuf: [std.fs.max_path_bytes]u8 = undefined;
    var it = dir.iterate();
    while (try it.next()) |ent| {

        const rel = if (prefix.len == 0)
            ent.name
        else blk: {
            const len = prefix.len + 1 + ent.name.len;
            if (len > pathbuf.len) continue;
            @memcpy(pathbuf[0..prefix.len], prefix);
            pathbuf[prefix.len] = '/';
            @memcpy(pathbuf[prefix.len + 1 ..][0..ent.name.len], ent.name);
            break :blk pathbuf[0..len];
        };

        if (ent.kind == .directory) {
            if (excluded(rel, exclude)) continue;
        } else {
            if (!passesFile(rel, include, exclude)) continue;
        }
        if (ign.ignored(rel, ent.kind == .directory, hidden)) continue;

        switch (ent.kind) {
            .file => ctx.onFile(rel),
            .directory => {
                var sub = try cwd.openDir(rel, .{ .iterate = true });
                defer sub.close();
                try ign.pushDir(sub, rel);
                defer ign.popDir();
                try walkDir(cwd, ign, rel, sub, include, exclude, hidden, ctx);
            },
            else => {},
        }
    }
}

fn collectRoot(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    list: *std.ArrayListUnmanaged([]const u8),
    root: []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
) !void {
    const st = cwd.statFile(root) catch |e| switch (e) {
        error.FileNotFound => return,
        else => return e,
    };

    if (st.kind == .directory) {
        const prefix = if (std.mem.eql(u8, root, ".") or std.mem.eql(u8, root, "./")) "" else root;
        var d = try cwd.openDir(root, .{ .iterate = true });
        defer d.close();
        if (prefix.len != 0) {
            try ign.pushDir(d, prefix);
            defer ign.popDir();
        }
        try collectDir(allocator, cwd, ign, list, prefix, d, include, exclude, hidden);
    } else {
        if (!passesFile(root, include, exclude)) return;
        if (ign.ignored(root, false, hidden)) return;
        try list.append(allocator, try allocator.dupe(u8, root));
    }
}

fn collectDir(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    list: *std.ArrayListUnmanaged([]const u8),
    prefix: []const u8,
    dir: std.fs.Dir,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
) !void {
    var it = dir.iterate();
    while (try it.next()) |ent| {
        const rel = if (prefix.len == 0)
            try allocator.dupe(u8, ent.name)
        else
            try std.fs.path.join(allocator, &.{ prefix, ent.name });
        defer allocator.free(rel);

        if (ent.kind == .directory) {
            if (excluded(rel, exclude)) continue;
        } else {
            if (!passesFile(rel, include, exclude)) continue;
        }
        if (ign.ignored(rel, ent.kind == .directory, hidden)) continue;

        switch (ent.kind) {
            .file => try list.append(allocator, try allocator.dupe(u8, rel)),
            .directory => {
                var sub = try cwd.openDir(rel, .{ .iterate = true });
                defer sub.close();
                try ign.pushDir(sub, rel);
                defer ign.popDir();
                try collectDir(allocator, cwd, ign, list, rel, sub, include, exclude, hidden);
            },
            else => {},
        }
    }
}

fn passesFile(path: []const u8, include: []const []const u8, exclude: []const []const u8) bool {
    if (excluded(path, exclude)) return false;
    if (include.len == 0) return true;
    for (include) |g| {
        if (glob.isGlob(g)) {
            if (glob.matchPath(g, path)) return true;
        } else if (std.mem.indexOf(u8, path, g) != null) return true;
    }
    return false;
}

fn excluded(path: []const u8, exclude: []const []const u8) bool {
    for (exclude) |x| {
        if (glob.isGlob(x)) {
            if (glob.matchPath(x, path)) return true;
        } else if (std.mem.indexOf(u8, path, x) != null) return true;
    }
    return false;
}

test "collect files with include/exclude" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("src");
    try td.dir.makeDir("test");
    try td.dir.writeFile(.{ .sub_path = "src/a.clj", .data = "x\n" });
    try td.dir.writeFile(.{ .sub_path = "test/b.clj", .data = "x\n" });

    var ign = ignore.Stack.init(std.testing.allocator);
    defer ign.deinit();
    try ign.pushDir(td.dir, "");
    defer ign.popDir();

    const got = try collectFiles(std.testing.allocator, td.dir, &ign, &.{ "." }, &.{ "src" }, &.{ "test" }, false);
    defer {
        for (got) |p| std.testing.allocator.free(p);
        std.testing.allocator.free(got);
    }
    try std.testing.expectEqual(@as(usize, 1), got.len);
    try std.testing.expect(std.mem.endsWith(u8, got[0], "src/a.clj"));
}

test "include does not prune directories" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("src");
    try td.dir.writeFile(.{ .sub_path = "src/run.zig", .data = "x\n" });

    var ign = ignore.Stack.init(std.testing.allocator);
    defer ign.deinit();
    try ign.pushDir(td.dir, "");
    defer ign.popDir();

    const got = try collectFiles(std.testing.allocator, td.dir, &ign, &.{ "." }, &.{ "run" }, &.{}, false);
    defer {
        for (got) |p| std.testing.allocator.free(p);
        std.testing.allocator.free(got);
    }
    try std.testing.expectEqual(@as(usize, 1), got.len);
    try std.testing.expect(std.mem.endsWith(u8, got[0], "src/run.zig"));
}
