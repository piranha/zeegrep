const std = @import("std");
const ignore = @import("ignore.zig");
const glob = @import("glob.zig");

/// Walk files and call callback for each file (streaming)
pub fn walkFiles(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    paths: []const []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    sorted: bool,
    ctx: anytype,
) !void {
    if (paths.len == 0) {
        try walkRoot(allocator, cwd, ign, ".", include, exclude, hidden, sorted, ctx);
    } else for (paths) |root| {
        try walkRoot(allocator, cwd, ign, root, include, exclude, hidden, sorted, ctx);
    }
}

fn walkRoot(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    root: []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    sorted: bool,
    ctx: anytype,
) !void {
    const st = cwd.statFile(root) catch |e| switch (e) {
        error.FileNotFound => return,
        else => return e,
    };

    if (st.kind == .directory) {
        // Strip trailing slashes to avoid double-slash paths like "src//file.zig"
        const trimmed = std.mem.trimRight(u8, root, "/");
        const prefix = if (trimmed.len == 0 or std.mem.eql(u8, trimmed, ".")) "" else trimmed;
        var d = try cwd.openDir(root, .{ .iterate = true });
        defer d.close();
        // prefix == "" means cwd, whose ignore frame the caller already pushed
        try walkDir(allocator, cwd, ign, prefix, d, include, exclude, hidden, sorted, prefix.len != 0, ctx);
    } else {
        if (!passesFile(root, include, exclude)) return;
        if (ign.ignored(root, false, hidden)) return;
        ctx.onFile(root);
    }
}

/// Name stored as offset into a shared byte buffer to keep allocations per-dir, not per-entry.
const Entry = struct {
    off: u32,
    len: u32,
    kind: std.fs.Dir.Entry.Kind,

    fn name(self: Entry, buf: []const u8) []const u8 {
        return buf[self.off..][0..self.len];
    }
};

fn walkDir(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    prefix: []const u8,
    dir: std.fs.Dir,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    sorted: bool,
    push_ignore: bool,
    ctx: anytype,
) anyerror!void {
    var pathbuf: [std.fs.max_path_bytes]u8 = undefined;

    // One listing pass: entries are buffered so ignore files can be detected
    // before any is opened (a blind openat per missing ignore file is ~1us).
    var names: std.ArrayListUnmanaged(u8) = .{};
    defer names.deinit(allocator);
    var entries: std.ArrayListUnmanaged(Entry) = .{};
    defer entries.deinit(allocator);
    var found: ignore.Found = .{false} ** ignore.names.len;

    var it = dir.iterate();
    while (try it.next()) |ent| {
        if (ent.kind != .file and ent.kind != .directory) continue;
        if (ent.kind == .file) {
            if (ignore.nameIndex(ent.name)) |i| found[i] = true;
        }
        try entries.append(allocator, .{
            .off = @intCast(names.items.len),
            .len = @intCast(ent.name.len),
            .kind = ent.kind,
        });
        try names.appendSlice(allocator, ent.name);
    }

    if (sorted) {
        const Sorter = struct {
            buf: []const u8,
            fn lessThan(self: @This(), a: Entry, b: Entry) bool {
                return std.mem.lessThan(u8, a.name(self.buf), b.name(self.buf));
            }
        };
        std.mem.sort(Entry, entries.items, Sorter{ .buf = names.items }, Sorter.lessThan);
    }

    if (push_ignore) try ign.pushDir(dir, prefix, found);
    defer if (push_ignore) ign.popDir();

    for (entries.items) |ent| {
        try processEntry(allocator, cwd, ign, prefix, ent.name(names.items), ent.kind, &pathbuf, include, exclude, hidden, sorted, ctx);
    }
}

fn processEntry(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    ign: *ignore.Stack,
    prefix: []const u8,
    name: []const u8,
    kind: std.fs.Dir.Entry.Kind,
    pathbuf: *[std.fs.max_path_bytes]u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    sorted: bool,
    ctx: anytype,
) anyerror!void {
    const rel = if (prefix.len == 0)
        name
    else blk: {
        const len = prefix.len + 1 + name.len;
        if (len > pathbuf.len) return;
        @memcpy(pathbuf[0..prefix.len], prefix);
        pathbuf[prefix.len] = '/';
        @memcpy(pathbuf[prefix.len + 1 ..][0..name.len], name);
        break :blk pathbuf[0..len];
    };

    if (kind == .directory) {
        if (excluded(rel, exclude)) return;
    } else {
        if (!passesFile(rel, include, exclude)) return;
    }
    if (ign.ignored(rel, kind == .directory, hidden)) return;

    switch (kind) {
        .file => ctx.onFile(rel),
        .directory => {
            var sub = cwd.openDir(rel, .{ .iterate = true }) catch return;
            defer sub.close();
            walkDir(allocator, cwd, ign, rel, sub, include, exclude, hidden, sorted, true, ctx) catch {};
        },
        else => {},
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
    try ign.pushDir(td.dir, "", ignore.all_found);
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

    try walkFiles(std.testing.allocator, td.dir, &ign, &.{"."}, &.{"src"}, &.{"test"}, false, false, &collector);

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
    try ign.pushDir(td.dir, "", ignore.all_found);
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

    try walkFiles(std.testing.allocator, td.dir, &ign, &.{"."}, &.{"run"}, &.{}, false, false, &collector);

    try std.testing.expectEqual(@as(usize, 1), collector.files.items.len);
    try std.testing.expect(std.mem.endsWith(u8, collector.files.items[0], "src/run.zig"));
}
