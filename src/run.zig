const std = @import("std");
const builtin = @import("builtin");
const engine = @import("engine.zig");
const diff = @import("diff.zig");
const core_pool = @import("core/pool.zig");
const core_atomic = @import("core/atomic.zig");
const core_walk = @import("core/walk.zig");
const core_ignore = @import("core/ignore.zig");
const ansi = @import("core/ansi.zig");

pub fn run(allocator: std.mem.Allocator, writer: anytype, options: anytype, pattern: []const u8, paths: []const []const u8) !void {
    const cwd = std.fs.cwd();

    if (options.file_names and options.replace != null) return error.InvalidArgs;

    var pat = try engine.compile(allocator, pattern, options.ignore_case);
    defer pat.deinit();

    const before: u32 = if (options.context > 0) options.context else options.before;
    const after: u32 = if (options.context > 0) options.context else options.after;

    // Jobs use per-thread arenas for temp work; only final output uses shared allocator
    var ts = std.heap.ThreadSafeAllocator{ .child_allocator = allocator };
    const shared_alloc = ts.allocator();

    var pool: core_pool.Pool = .{};
    try pool.init(allocator, null);
    defer pool.deinit();

    const is_tty = std.fs.File.stdout().isTty();
    const no_color = std.process.getEnvVarOwned(allocator, "NO_COLOR") catch null;
    defer if (no_color) |v| allocator.free(v);
    const use_color = ansi.enabled(options.color, is_tty, no_color != null);
    const use_heading = is_tty and options.replace == null and !options.quiet and !options.count and !options.file_names and isMultiPathSearch(cwd, paths);

    var shared = Shared.init(shared_alloc, before, after, use_color, use_heading);
    defer shared.deinit();

    var scope = pool.scope();

    // Walk and spawn jobs simultaneously (instead of collect-then-search)
    var ign = core_ignore.Stack.init(allocator);
    defer ign.deinit();
    try ign.pushDir(cwd, "");
    defer ign.popDir();

    const Walker = struct {
        scope: *core_pool.Scope,
        shared: *Shared,
        pat: *const engine.Pattern,
        options: @TypeOf(options),
        allocator: std.mem.Allocator,
        cwd: std.fs.Dir,

        pub fn onFile(self: *@This(), path: []const u8) void {
            const p = self.allocator.dupe(u8, path) catch return;
            if (self.options.abs) {
                const abs = self.cwd.realpathAlloc(self.allocator, p) catch {
                    self.allocator.free(p);
                    return;
                };
                self.allocator.free(p);
                self.scope.spawn(Job(@TypeOf(self.options)).run, .{ self.shared, abs, self.options, self.pat });
            } else {
                self.scope.spawn(Job(@TypeOf(self.options)).run, .{ self.shared, p, self.options, self.pat });
            }
        }
    };
    var walker = Walker{
        .scope = &scope,
        .shared = &shared,
        .pat = &pat,
        .options = options,
        .allocator = shared_alloc,
        .cwd = cwd,
    };
    try core_walk.walkFiles(
        cwd,
        &ign,
        paths,
        options.include.constSlice(),
        options.exclude.constSlice(),
        options.hidden,
        &walker,
    );
    scope.wait();

    if (options.sort) std.mem.sort(Result, shared.results.items, {}, Result.less);

    var first_out = true;
    for (shared.results.items) |r| {
        defer shared_alloc.free(r.out);
        if (!options.quiet) {
            if (shared.heading and !first_out) try writer.writeByte('\n');
            first_out = false;
            try writer.writeAll(r.out);
        }
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
    color: bool,
    heading: bool,

    mu: std.Thread.Mutex = .{},
    results: std.ArrayListUnmanaged(Result) = .{},

    total_matches: usize = 0,
    total_repls: usize = 0,
    files_changed: usize = 0,
    had_error: bool = false,

    fn init(allocator: std.mem.Allocator, before: u32, after: u32, color: bool, heading: bool) Shared {
        return .{ .allocator = allocator, .before = before, .after = after, .color = color, .heading = heading };
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
            defer shared.allocator.free(path); // path was duped by walker
            // Per-job arena for temp work - avoids contention on shared allocator
            var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
            defer arena.deinit();
            work(arena.allocator(), shared, path, options, pat) catch |e| {
                if (options.debug) debugSkip(path, @errorName(e));
                shared.fail();
            };
        }

        fn debugSkip(path: []const u8, reason: []const u8) void {
            std.fs.File.stderr().deprecatedWriter().print("DEBUG: {s}: {s}\n", .{ path, reason }) catch {};
        }

        fn work(tmp: std.mem.Allocator, shared: *Shared, path: []const u8, options: Opts, pat: *const engine.Pattern) !void {
            if (options.file_names) {
                if (!engine.matchAny(pat, path, options.ignore_case)) return;
                const n = engine.countMatches(pat, path, options.ignore_case);
                shared.add(n, 0, false);
                if (options.count and !options.quiet) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    const w = buf.writer(tmp);
                    try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, n) });
                    shared.push(.{ .path = path, .out = try shared.allocator.dupe(u8, buf.items) });
                    return;
                }
                if (!options.quiet and !options.count) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    const w = buf.writer(tmp);
                    try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
                    shared.push(.{ .path = path, .out = try shared.allocator.dupe(u8, buf.items) });
                }
                return;
            }

            var f = std.fs.cwd().openFile(path, .{}) catch |e| {
                if (options.debug) debugSkip(path, @errorName(e));
                return;
            };
            defer f.close();

            const stat = f.stat() catch |e| {
                if (options.debug) debugSkip(path, @errorName(e));
                return;
            };
            if (stat.size == 0) return; // empty files aren't interesting
            if (stat.size > 64 * 1024 * 1024) {
                if (options.debug) debugSkip(path, "file too large (>64MB)");
                return;
            }

            const file_data = mapFile(tmp, f, stat.size) catch |e| {
                if (options.debug) debugSkip(path, @errorName(e));
                return;
            };
            defer unmapFile(tmp, file_data);
            const data = file_data.ptr;

            if (isBinary(data)) {
                if (options.debug) debugSkip(path, "binary file");
                return;
            }

            if (options.replace) |repl| {
                const rr = try engine.replaceAll(tmp, pat, data, repl, options.ignore_case);
                if (rr.n == 0) return;

                if (!options.dry_run) try writeAtomic(tmp, path, rr.out);

                shared.add(0, rr.n, true);
                if (options.dry_run and !options.quiet) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    _ = try diff.printChangedLines(buf.writer(tmp), shared.color, path, data, rr.out);
                    shared.push(.{ .path = path, .out = try shared.allocator.dupe(u8, buf.items) });
                }
                return;
            }

            // Fast path: no context, stream output directly
            if (shared.before == 0 and shared.after == 0) {
                var out: std.ArrayListUnmanaged(u8) = .{};
                const w = out.writer(tmp);

                var matches: usize = 0;
                var line_no: usize = 1;
                var it = std.mem.splitScalar(u8, data, '\n');
                var wrote_heading = false;

                while (it.next()) |ln| : (line_no += 1) {
                    const n = engine.countMatches(pat, ln, options.ignore_case);
                    if (n == 0) continue;
                    matches += n;

                    if (options.quiet) continue;
                    if (options.count) continue;

                    if (shared.heading and !wrote_heading) {
                        try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
                        wrote_heading = true;
                    }
                    if (!shared.heading) {
                        try w.print("{f}:", .{ansi.styled(shared.color, .path, path)});
                    }
                    try w.print("{f}:", .{ansi.styled(shared.color, .line_no, line_no)});
                    try engine.writeHighlighted(shared.color, pat, w, ln, options.ignore_case);
                    try w.writeByte('\n');
                }

                if (matches == 0) return;
                shared.add(matches, 0, false);

                if (options.count and !options.quiet) {
                    try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, matches) });
                }
                if (!options.quiet and out.items.len > 0) {
                    shared.push(.{ .path = path, .out = try shared.allocator.dupe(u8, out.items) });
                }
                return;
            }

            // Slow path: context lines needed
            const lines = try splitLines(tmp, data);

            const emit = try tmp.alloc(bool, lines.len);
            @memset(emit, false);

            const is_match = try tmp.alloc(bool, lines.len);
            @memset(is_match, false);

            var matches: usize = 0;
            for (lines, 0..) |ln, i| {
                const n = engine.countMatches(pat, ln, options.ignore_case);
                if (n == 0) continue;
                is_match[i] = true;
                matches += n;

                const lo: usize = if (shared.before > i) 0 else i - shared.before;
                var hi: usize = i + shared.after;
                if (hi >= lines.len) hi = lines.len - 1;
                var j: usize = lo;
                while (j <= hi) : (j += 1) emit[j] = true;
            }

            if (matches == 0) return;
            shared.add(matches, 0, false);

            if (options.count) {
                if (options.quiet) return;
                var buf: std.ArrayListUnmanaged(u8) = .{};
                const w = buf.writer(tmp);
                try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, matches) });
                shared.push(.{ .path = path, .out = try shared.allocator.dupe(u8, buf.items) });
                return;
            }
            if (options.quiet) return;

            var out: std.ArrayListUnmanaged(u8) = .{};
            const w = out.writer(tmp);

            var last: ?usize = null;
            if (shared.heading) {
                try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
            }
            for (lines, 0..) |ln, i| {
                if (!emit[i]) continue;

                if (last != null and i > last.? + 1) {
                    try w.print("{f}\n", .{ansi.styled(shared.color, .dim, "--")});
                }
                last = i;

                const sep: u8 = if (is_match[i]) ':' else '-';

                if (!shared.heading) {
                    try w.print("{f}{c}", .{ ansi.styled(shared.color, .path, path), sep });
                }
                try w.print("{f}{c}", .{ ansi.styled(shared.color, .line_no, i + 1), sep });

                if (is_match[i]) try engine.writeHighlighted(shared.color, pat, w, ln, options.ignore_case) else try w.writeAll(ln);
                try w.writeByte('\n');
            }
            shared.push(.{ .path = path, .out = try shared.allocator.dupe(u8, out.items) });
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

const FileData = struct {
    ptr: []const u8,
};

fn mapFile(allocator: std.mem.Allocator, f: std.fs.File, size: u64) !FileData {
    if (comptime builtin.os.tag == .windows) {
        const buf = try allocator.alloc(u8, size);
        const n = try f.readAll(buf);
        return .{ .ptr = buf[0..n] };
    } else {
        const data = try std.posix.mmap(null, size, std.c.PROT.READ, .{ .TYPE = .PRIVATE }, f.handle, 0);
        return .{ .ptr = data };
    }
}

fn unmapFile(allocator: std.mem.Allocator, fd: FileData) void {
    if (comptime builtin.os.tag == .windows) {
        allocator.free(fd.ptr);
    } else {
        std.posix.munmap(@alignCast(@constCast(fd.ptr)));
    }
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
        hidden: bool = false,
        debug: bool = false,
        color: ansi.Color = .never,
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
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "1:foo1") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "3:foo2") != null);

    // Replace dry-run
    fbs.reset();
    o.replace = "bar$1";
    o.dry_run = true;
    _ = run(std.testing.allocator, fbs.writer(), o, "foo(\\d+)", &.{root}) catch {};
    out = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, out, "replacements") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "──") != null);

    // Count (per-file)
    fbs.reset();
    o.replace = null;
    o.dry_run = false;
    o.count = true;
    _ = run(std.testing.allocator, fbs.writer(), o, "foo", &.{root}) catch {};
    out = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt:2") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "b.txt") == null);

    // -f (path match)
    fbs.reset();
    o.count = false;
    o.file_names = true;
    _ = run(std.testing.allocator, fbs.writer(), o, "a.txt", &.{root}) catch {};
    out = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt") != null);
}

fn isMultiPathSearch(cwd: std.fs.Dir, paths: []const []const u8) bool {
    if (paths.len == 0) return true;
    for (paths) |p| {
        const st = cwd.statFile(p) catch continue;
        if (st.kind == .directory) return true;
    }
    return false;
}
