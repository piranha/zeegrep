const std = @import("std");
const ignore = @import("ignore.zig");
const glob = @import("glob.zig");

pub const max_walkers = 8;

/// A directory handed to another worker. `path` is owned by the queue ("" = cwd).
const Task = struct {
    path: []u8,
    node: ?*const ignore.Node,
};

/// LIFO work queue with sleep/wake and quiescence detection: the walk is done
/// when nothing is queued and no worker is still scanning.
const Queue = struct {
    allocator: std.mem.Allocator,
    mutex: std.Thread.Mutex = .{},
    cond: std.Thread.Condition = .{},
    items: std.ArrayListUnmanaged(Task) = .{},
    busy: usize = 0,
    /// Workers blocked in pop(). Read without the lock to decide whether
    /// sharing a subdirectory is worth the path copy.
    idle: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),

    fn deinit(self: *Queue) void {
        for (self.items.items) |t| self.allocator.free(t.path);
        self.items.deinit(self.allocator);
    }

    fn hasIdleWorker(self: *const Queue) bool {
        return self.idle.load(.monotonic) > 0;
    }

    /// Takes ownership of `path`.
    fn push(self: *Queue, path: []u8, node: ?*const ignore.Node) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.items.append(self.allocator, .{ .path = path, .node = node }) catch {
            self.allocator.free(path);
            return;
        };
        self.cond.signal();
    }

    /// Blocks until work is available; null when the whole walk is finished.
    fn pop(self: *Queue) ?Task {
        self.mutex.lock();
        defer self.mutex.unlock();
        while (true) {
            if (self.items.pop()) |t| {
                self.busy += 1;
                return t;
            }
            if (self.busy == 0) {
                self.cond.broadcast(); // wake the others so they exit too
                return null;
            }
            _ = self.idle.fetchAdd(1, .monotonic);
            self.cond.wait(&self.mutex);
            _ = self.idle.fetchSub(1, .monotonic);
        }
    }

    fn done(self: *Queue) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        self.busy -= 1;
        if (self.busy == 0 and self.items.items.len == 0) self.cond.broadcast();
    }
};

const Walk = struct {
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    set: *ignore.Set,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    q: Queue,
};

/// Walk files and call `ctx.onFile(rel_path)` for each match.
/// `onFile` runs on up to `threads` threads concurrently and must be
/// thread-safe; `rel_path` is only valid for the duration of the call.
pub fn walkFiles(
    allocator: std.mem.Allocator,
    cwd: std.fs.Dir,
    set: *ignore.Set,
    node: ?*const ignore.Node,
    paths: []const []const u8,
    include: []const []const u8,
    exclude: []const []const u8,
    hidden: bool,
    threads: usize,
    ctx: anytype,
) !void {
    var w = Walk{
        .allocator = allocator,
        .cwd = cwd,
        .set = set,
        .include = include,
        .exclude = exclude,
        .hidden = hidden,
        .q = .{ .allocator = allocator },
    };
    defer w.q.deinit();

    if (paths.len == 0) {
        try seedRoot(&w, node, ".", ctx);
    } else {
        // Overlapping roots (`zg pat . src`) would otherwise list a directory
        // twice, which for --replace means replacing in the same file twice.
        const keep = try rootsToWalk(allocator, cwd, paths);
        defer allocator.free(keep);
        for (paths, keep) |root, wanted| {
            if (wanted) try seedRoot(&w, node, root, ctx);
        }
    }
    if (w.q.items.items.len == 0) return;

    const Worker = struct {
        fn run(walk: *Walk, c: @TypeOf(ctx)) void {
            // Reused across tasks: holds the path of the directory being scanned.
            var path: std.ArrayListUnmanaged(u8) = .{};
            defer path.deinit(walk.allocator);

            while (walk.q.pop()) |task| {
                path.clearRetainingCapacity();
                path.appendSlice(walk.allocator, task.path) catch {};
                scanTask(walk, &path, task.node, c) catch {};
                walk.allocator.free(task.path);
                walk.q.done();
            }
        }
    };

    const n = @max(1, @min(threads, max_walkers));
    var extra: [max_walkers - 1]std.Thread = undefined;
    var spawned: usize = 0;
    while (spawned + 1 < n) : (spawned += 1) {
        extra[spawned] = std.Thread.spawn(.{}, Worker.run, .{ &w, ctx }) catch break;
    }
    Worker.run(&w, ctx);
    for (extra[0..spawned]) |t| t.join();
}

/// Marks which roots to actually walk: drops exact duplicates and roots nested
/// inside another root. Comparison is on resolved paths, so "src", "./src/" and
/// an absolute path to it are recognized as the same place.
fn rootsToWalk(allocator: std.mem.Allocator, cwd: std.fs.Dir, paths: []const []const u8) ![]bool {
    const keep = try allocator.alloc(bool, paths.len);
    @memset(keep, true);
    if (paths.len < 2) return keep;

    const real = try allocator.alloc(?[]u8, paths.len);
    defer {
        for (real) |r| if (r) |p| allocator.free(p);
        allocator.free(real);
    }
    for (paths, real) |p, *r| r.* = cwd.realpathAlloc(allocator, p) catch null;

    for (real, 0..) |ri, i| {
        const a = ri orelse continue; // unresolvable: leave it to seedRoot to report
        for (real, 0..) |rj, j| {
            if (i == j) continue;
            const b = rj orelse continue;
            // Equal paths: keep the first occurrence only
            const nested = if (std.mem.eql(u8, a, b)) j < i and keep[j] else contains(b, a);
            if (nested) {
                keep[i] = false;
                break;
            }
        }
    }
    return keep;
}

/// True when `child` is strictly inside directory `parent` (both resolved).
fn contains(parent: []const u8, child: []const u8) bool {
    if (child.len <= parent.len) return false;
    if (!std.mem.startsWith(u8, child, parent)) return false;
    if (std.mem.eql(u8, parent, "/")) return true;
    return child[parent.len] == '/';
}

fn seedRoot(w: *Walk, node: ?*const ignore.Node, root: []const u8, ctx: anytype) !void {
    const st = w.cwd.statFile(root) catch |e| switch (e) {
        error.FileNotFound => return,
        else => return e,
    };

    if (st.kind != .directory) {
        if (!passesFile(root, w.include, w.exclude)) return;
        if (ignore.Node.ignored(node, root, false, w.hidden)) return;
        ctx.onFile(root);
        return;
    }

    // Strip trailing slashes to avoid double-slash paths like "src//file.zig"
    const trimmed = std.mem.trimRight(u8, root, "/");
    const prefix = if (trimmed.len == 0 or std.mem.eql(u8, trimmed, ".")) "" else trimmed;
    w.q.push(try w.allocator.dupe(u8, prefix), node);
}

/// Opens a queued directory by its full path (once per handoff) and scans it.
fn scanTask(w: *Walk, path: *std.ArrayListUnmanaged(u8), node: ?*const ignore.Node, ctx: anytype) !void {
    const rel = if (path.items.len == 0) "." else path.items;
    var dir = try w.cwd.openDir(rel, .{ .iterate = true });
    defer dir.close();
    try scanDir(w, dir, path, node, ctx);
}

/// Recurses into subdirectories with `openat(dir, name)`, which is much cheaper
/// than re-resolving a deep path from cwd. Subdirs are only handed to the queue
/// when another worker is idle, so the handoff cost is paid only when it buys
/// parallelism.
fn scanDir(w: *Walk, dir: std.fs.Dir, path: *std.ArrayListUnmanaged(u8), parent_node: ?*const ignore.Node, ctx: anytype) anyerror!void {
    const allocator = w.allocator;
    const prefix_len = path.items.len;

    // One listing pass, buffered: ignore files are detected from the entries
    // instead of a blind openat() per candidate, and the names must outlive the
    // iterator because rules load between listing and processing.
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

    // This dir's own rules apply to its entries. An empty path means cwd,
    // whose rules the caller already loaded.
    const node = if (prefix_len == 0) parent_node else try w.set.push(parent_node, dir, path.items, found);

    for (entries.items) |ent| {
        const name = ent.name(names.items);
        path.shrinkRetainingCapacity(prefix_len);
        if (prefix_len != 0) try path.append(allocator, '/');
        try path.appendSlice(allocator, name);
        const rel = path.items;

        const is_dir = ent.kind == .directory;
        if (is_dir) {
            if (excluded(rel, w.exclude)) continue;
        } else {
            if (!passesFile(rel, w.include, w.exclude)) continue;
        }
        if (ignore.Node.ignored(node, rel, is_dir, w.hidden)) continue;

        if (!is_dir) {
            ctx.onFile(rel);
            continue;
        }
        if (w.q.hasIdleWorker()) {
            w.q.push(allocator.dupe(u8, rel) catch continue, node);
        } else {
            var sub = dir.openDir(name, .{ .iterate = true }) catch continue;
            defer sub.close();
            scanDir(w, sub, path, node, ctx) catch {};
        }
    }
    path.shrinkRetainingCapacity(prefix_len);
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

const TestCollector = struct {
    files: std.ArrayListUnmanaged([]const u8) = .{},
    alloc: std.mem.Allocator,
    mu: std.Thread.Mutex = .{},

    pub fn onFile(self: *@This(), path: []const u8) void {
        self.mu.lock();
        defer self.mu.unlock();
        self.files.append(self.alloc, self.alloc.dupe(u8, path) catch return) catch {};
    }

    pub fn deinit(self: *@This()) void {
        for (self.files.items) |p| self.alloc.free(p);
        self.files.deinit(self.alloc);
    }
};

test "walk files with include/exclude" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("src");
    try td.dir.makeDir("test");
    try td.dir.writeFile(.{ .sub_path = "src/a.clj", .data = "x\n" });
    try td.dir.writeFile(.{ .sub_path = "test/b.clj", .data = "x\n" });

    var set = ignore.Set.init(std.testing.allocator);
    defer set.deinit();
    const root = try set.push(null, td.dir, "", ignore.all_found);

    var collector = TestCollector{ .alloc = std.testing.allocator };
    defer collector.deinit();

    try walkFiles(std.testing.allocator, td.dir, &set, root, &.{"."}, &.{"src"}, &.{"test"}, false, 4, &collector);

    try std.testing.expectEqual(@as(usize, 1), collector.files.items.len);
    try std.testing.expect(std.mem.endsWith(u8, collector.files.items[0], "src/a.clj"));
}

test "include does not prune directories" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("src");
    try td.dir.writeFile(.{ .sub_path = "src/run.zig", .data = "x\n" });

    var set = ignore.Set.init(std.testing.allocator);
    defer set.deinit();
    const root = try set.push(null, td.dir, "", ignore.all_found);

    var collector = TestCollector{ .alloc = std.testing.allocator };
    defer collector.deinit();

    try walkFiles(std.testing.allocator, td.dir, &set, root, &.{"."}, &.{"run"}, &.{}, false, 1, &collector);

    try std.testing.expectEqual(@as(usize, 1), collector.files.items.len);
    try std.testing.expect(std.mem.endsWith(u8, collector.files.items[0], "src/run.zig"));
}

test "overlapping roots are walked once" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makePath("sub/deep");
    try td.dir.writeFile(.{ .sub_path = "top.txt", .data = "x\n" });
    try td.dir.writeFile(.{ .sub_path = "sub/a.txt", .data = "x\n" });
    try td.dir.writeFile(.{ .sub_path = "sub/deep/b.txt", .data = "x\n" });

    const cases = [_][]const []const u8{
        &.{ ".", "sub" },
        &.{ "sub", "." },
        &.{ "sub", "sub", "sub/deep" },
        &.{ "./sub/", "sub" },
    };
    const expected = [_]usize{ 3, 3, 2, 2 };

    for (cases, expected) |roots, want| {
        var set = ignore.Set.init(std.testing.allocator);
        defer set.deinit();
        const root = try set.push(null, td.dir, "", ignore.all_found);

        var collector = TestCollector{ .alloc = std.testing.allocator };
        defer collector.deinit();

        try walkFiles(std.testing.allocator, td.dir, &set, root, roots, &.{}, &.{}, false, 4, &collector);
        try std.testing.expectEqual(want, collector.files.items.len);
    }
}

test "parallel walk finds every file in a deep tree" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    var buf: [64]u8 = undefined;
    var expected: usize = 0;
    for (0..12) |i| {
        const d = try std.fmt.bufPrint(&buf, "d{d}/sub/deeper", .{i});
        try td.dir.makePath(d);
        for (0..7) |j| {
            const f = try std.fmt.bufPrint(&buf, "d{d}/sub/deeper/f{d}.txt", .{ i, j });
            try td.dir.writeFile(.{ .sub_path = f, .data = "x\n" });
            expected += 1;
        }
    }

    for ([_]usize{ 1, 2, 8, 64 }) |threads| {
        var set = ignore.Set.init(std.testing.allocator);
        defer set.deinit();
        const root = try set.push(null, td.dir, "", ignore.all_found);

        var collector = TestCollector{ .alloc = std.testing.allocator };
        defer collector.deinit();

        try walkFiles(std.testing.allocator, td.dir, &set, root, &.{"."}, &.{}, &.{}, false, threads, &collector);
        try std.testing.expectEqual(expected, collector.files.items.len);
    }
}
