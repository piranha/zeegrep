const std = @import("std");
const engine = @import("engine.zig");
const diff = @import("diff.zig");
const core_pool = @import("core/pool.zig");
const core_atomic = @import("core/atomic.zig");
const core_walk = @import("core/walk.zig");
const core_ignore = @import("core/ignore.zig");

pub fn run(allocator: std.mem.Allocator, writer: anytype, options: anytype, pattern: []const u8, paths: []const []const u8) !void {
    const cwd = std.fs.cwd();

    if (options.file_names and options.replace != null) return error.InvalidArgs;

    var ign = try core_ignore.Ignorer.init(allocator, cwd);
    defer ign.deinit();

    var files: [][]const u8 = try core_walk.collectFiles(
        allocator,
        cwd,
        &ign,
        paths,
        options.include.constSlice(),
        options.exclude.constSlice(),
    );
    defer {
        for (files) |p| allocator.free(p);
        allocator.free(files);
    }

    if (options.abs) {
        for (files, 0..) |p, i| {
            const abs = cwd.realpathAlloc(allocator, p) catch continue;
            allocator.free(p);
            files[i] = abs;
        }
    }

    var pat = try engine.compile(allocator, pattern, options.ignore_case);
    defer pat.deinit();

    const before: u32 = if (options.context > 0) options.context else options.before;
    const after: u32 = if (options.context > 0) options.context else options.after;

    var ts = std.heap.ThreadSafeAllocator{ .child_allocator = allocator };
    const ta = ts.allocator();

    var pool: core_pool.Pool = .{};
    try pool.init(allocator, null);
    defer pool.deinit();

    var shared = Shared.init(ta, before, after);
    defer shared.deinit();

    var scope = pool.scope();
    for (files) |p| scope.spawn(Job(@TypeOf(options)).run, .{ &shared, p, options, &pat });
    scope.wait();

    if (options.sort) std.mem.sort(Result, shared.results.items, {}, Result.less);

    for (shared.results.items) |r| {
        defer ta.free(r.out);
        if (!options.quiet) try writer.writeAll(r.out);
    }

    if (options.count and !options.quiet and options.replace == null) {
        try writer.print("{d}\n", .{shared.total_matches});
    }

    if (options.replace != null and !options.quiet) {
        try writer.print("{d} files, {d} replacements\n", .{ shared.files_changed, shared.total_repls });
    }

    if (shared.had_error) return error.JobFailed;
    if (shared.total_matches == 0 and shared.total_repls == 0) return error.NoMatches;
}

const Result = struct {
    path: []const u8,
    out: []u8,

    fn less(_: void, a: Result, b: Result) bool {
        return std.mem.lessThan(u8, a.path, b.path);
    }
};

const Shared = struct {
    allocator: std.mem.Allocator,
    before: u32,
    after: u32,

    mu: std.Thread.Mutex = .{},
    results: std.ArrayListUnmanaged(Result) = .{},

    total_matches: usize = 0,
    total_repls: usize = 0,
    files_changed: usize = 0,
    had_error: bool = false,

    fn init(allocator: std.mem.Allocator, before: u32, after: u32) Shared {
        return .{ .allocator = allocator, .before = before, .after = after };
    }

    fn deinit(self: *Shared) void {
        self.results.deinit(self.allocator);
    }

    fn add(self: *Shared, matches: usize, repls: usize, changed: bool) void {
        self.mu.lock();
        defer self.mu.unlock();
        self.total_matches += matches;
        self.total_repls += repls;
        if (changed) self.files_changed += 1;
    }

    fn push(self: *Shared, r: Result) void {
        self.mu.lock();
        defer self.mu.unlock();
        self.results.append(self.allocator, r) catch {
            self.had_error = true;
        };
    }

    fn fail(self: *Shared) void {
        self.mu.lock();
        self.had_error = true;
        self.mu.unlock();
    }
};

fn Job(comptime Opts: type) type {
    return struct {
        fn run(shared: *Shared, path: []const u8, options: Opts, pat: *const engine.Pattern) void {
            work(shared, path, options, pat) catch shared.fail();
        }

        fn work(shared: *Shared, path: []const u8, options: Opts, pat: *const engine.Pattern) !void {
            if (options.file_names) {
                const base = std.fs.path.basename(path);
                if (!engine.matchAny(pat, base, options.ignore_case)) return;
                shared.add(1, 0, false);
                if (!options.quiet and !options.count) {
                    shared.push(.{
                        .path = path,
                        .out = try std.fmt.allocPrint(shared.allocator, "{s}\n", .{path}),
                    });
                }
                return;
            }

            var f = try std.fs.cwd().openFile(path, .{});
            defer f.close();

            const data = f.readToEndAlloc(shared.allocator, 64 * 1024 * 1024) catch |e| switch (e) {
                error.FileTooBig => return,
                else => return e,
            };
            defer shared.allocator.free(data);

            if (isBinary(data)) return;

            if (options.replace) |repl| {
                const rr = try engine.replaceAll(shared.allocator, pat, data, repl, options.ignore_case);
                defer shared.allocator.free(rr.out);
                if (rr.n == 0) return;

                if (!options.dry_run) try writeAtomic(shared.allocator, path, rr.out);

                shared.add(0, rr.n, true);
                if (options.dry_run and !options.quiet) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    defer buf.deinit(shared.allocator);
                    _ = try diff.printChangedLines(buf.writer(shared.allocator), path, data, rr.out);
                    shared.push(.{ .path = path, .out = try buf.toOwnedSlice(shared.allocator) });
                }
                return;
            }

            const lines = try splitLines(shared.allocator, data);
            defer shared.allocator.free(lines);

            var emit = try shared.allocator.alloc(bool, lines.len);
            defer shared.allocator.free(emit);
            @memset(emit, false);

            var matches: usize = 0;
            for (lines, 0..) |ln, i| {
                if (!engine.matchAny(pat, ln, options.ignore_case)) continue;
                matches += engine.countMatches(pat, ln, options.ignore_case);

                const lo: usize = if (shared.before > i) 0 else i - shared.before;
                var hi: usize = i + shared.after;
                if (hi >= lines.len) hi = lines.len - 1;
                var j: usize = lo;
                while (j <= hi) : (j += 1) emit[j] = true;
            }

            if (matches == 0) return;
            shared.add(matches, 0, false);

            if (options.count or options.quiet) return;

            var out: std.ArrayListUnmanaged(u8) = .{};
            defer out.deinit(shared.allocator);
            for (lines, 0..) |ln, i| {
                if (!emit[i]) continue;
                try out.writer(shared.allocator).print("{s}:{d}: {s}\n", .{ path, i + 1, ln });
            }
            shared.push(.{ .path = path, .out = try out.toOwnedSlice(shared.allocator) });
        }
    };
}

fn splitLines(allocator: std.mem.Allocator, data: []const u8) ![]const []const u8 {
    var list: std.ArrayListUnmanaged([]const u8) = .{};
    errdefer list.deinit(allocator);
    var it = std.mem.splitScalar(u8, data, '\n');
    while (it.next()) |line| try list.append(allocator, line);
    return try list.toOwnedSlice(allocator);
}

fn isBinary(data: []const u8) bool {
    const n = @min(data.len, 4096);
    return std.mem.indexOfScalar(u8, data[0..n], 0) != null;
}

fn writeAtomic(allocator: std.mem.Allocator, path: []const u8, data: []const u8) !void {
    const parent = std.fs.path.dirname(path) orelse ".";
    const base = std.fs.path.basename(path);

    var d = try std.fs.cwd().openDir(parent, .{});
    defer d.close();
    try core_atomic.replaceFileAtomic(allocator, d, base, data);
}

test "search and replace tmpdir" {
    const cwd = std.fs.cwd();

    var name_buf: [64]u8 = undefined;
    const root = try std.fmt.bufPrint(&name_buf, "tmp-zg-test-{d}", .{std.time.milliTimestamp()});
    cwd.makeDir(root) catch {};
    defer cwd.deleteTree(root) catch {};

    var pbuf: [128]u8 = undefined;
    const a = try std.fmt.bufPrint(&pbuf, "{s}/a.txt", .{root});
    try cwd.writeFile(.{ .sub_path = a, .data = "foo1\nxx\nfoo2\n" });
    const b = try std.fmt.bufPrint(&pbuf, "{s}/b.txt", .{root});
    try cwd.writeFile(.{ .sub_path = b, .data = "nope\n" });

    const Opt = struct {
        replace: ?[]const u8 = null,
        dry_run: bool = false,
        ignore_case: bool = false,
        context: u32 = 0,
        after: u32 = 0,
        before: u32 = 0,
        quiet: bool = false,
        count: bool = false,
        abs: bool = false,
        sort: bool = true,
        file_names: bool = false,
        include: @import("core/opt.zig").Multi([]const u8, 8) = .{},
        exclude: @import("core/opt.zig").Multi([]const u8, 8) = .{},
    };

    var o = Opt{};
    try o.include.append(".txt");

    var buf: [2048]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);

    // Search
    _ = run(std.testing.allocator, fbs.writer(), o, "foo", &.{root}) catch |e| {
        try std.testing.expect(e != error.NoMatches);
        return e;
    };
    var out = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt:1:") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt:3:") != null);

    // Replace dry-run
    fbs.reset();
    o.replace = "bar$1";
    o.dry_run = true;
    _ = run(std.testing.allocator, fbs.writer(), o, "foo(\\d+)", &.{root}) catch {};
    out = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, out, "replacements") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "──") != null);
}
