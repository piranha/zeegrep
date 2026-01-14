const std = @import("std");
const ignore = @import("ignore.zig");
const glob = @import("glob.zig");

/// Walk files and call callback for each file (streaming)
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

test "walk files with include/exclude" {
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

    const Collector = struct {
        files: std.ArrayListUnmanaged([]const u8) = .{},
        alloc: std.mem.Allocator,

        pub fn onFile(self: *@This(), path: []const u8) void {
            self.files.append(self.alloc, self.alloc.dupe(u8, path) catch return) catch {};
        }

        pub fn deinit(self: *@This()) void {
            for (self.files.items) |p| self.alloc.free(p);
            self.files.deinit(self.alloc);
        }
    };

    var collector = Collector{ .alloc = std.testing.allocator };
    defer collector.deinit();

    try walkFiles(td.dir, &ign, &.{"."}, &.{"src"}, &.{"test"}, false, &collector);

    try std.testing.expectEqual(@as(usize, 1), collector.files.items.len);
    try std.testing.expect(std.mem.endsWith(u8, collector.files.items[0], "src/a.clj"));
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

    const Collector = struct {
        files: std.ArrayListUnmanaged([]const u8) = .{},
        alloc: std.mem.Allocator,

        pub fn onFile(self: *@This(), path: []const u8) void {
            self.files.append(self.alloc, self.alloc.dupe(u8, path) catch return) catch {};
        }

        pub fn deinit(self: *@This()) void {
            for (self.files.items) |p| self.alloc.free(p);
            self.files.deinit(self.alloc);
        }
    };

    var collector = Collector{ .alloc = std.testing.allocator };
    defer collector.deinit();

    try walkFiles(td.dir, &ign, &.{"."}, &.{"run"}, &.{}, false, &collector);

    try std.testing.expectEqual(@as(usize, 1), collector.files.items.len);
    try std.testing.expect(std.mem.endsWith(u8, collector.files.items[0], "src/run.zig"));
}
