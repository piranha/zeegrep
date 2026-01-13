const std = @import("std");
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

    var ign = core_ignore.Stack.init(allocator);
    defer ign.deinit();
    try ign.pushDir(cwd, "");
    defer ign.popDir();

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

    const is_tty = std.posix.isatty(std.posix.STDOUT_FILENO);
    const no_color = std.process.getEnvVarOwned(allocator, "NO_COLOR") catch null;
    defer if (no_color) |v| allocator.free(v);
    const use_color = ansi.enabled(options.color, is_tty, no_color != null);
    const use_heading = options.replace == null and !options.quiet and !options.count and !options.file_names and isMultiPathSearch(cwd, paths);

    var shared = Shared.init(ta, before, after, use_color, use_heading);
    defer shared.deinit();

    var scope = pool.scope();
    for (files) |p| scope.spawn(Job(@TypeOf(options)).run, .{ &shared, p, options, &pat });
    scope.wait();

    if (options.sort) std.mem.sort(Result, shared.results.items, {}, Result.less);

    var first_out = true;
    for (shared.results.items) |r| {
        defer ta.free(r.out);
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
            work(shared, path, options, pat) catch shared.fail();
        }

        fn work(shared: *Shared, path: []const u8, options: Opts, pat: *const engine.Pattern) !void {
            if (options.file_names) {
                if (!engine.matchAny(pat, path, options.ignore_case)) return;
                const n = engine.countMatches(pat, path, options.ignore_case);
                shared.add(n, 0, false);
                if (options.count and !options.quiet) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    defer buf.deinit(shared.allocator);
                    const w = buf.writer(shared.allocator);
                    try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, n) });
                    shared.push(.{ .path = path, .out = try buf.toOwnedSlice(shared.allocator) });
                    return;
                }
                if (!options.quiet and !options.count) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    defer buf.deinit(shared.allocator);
                    const w = buf.writer(shared.allocator);
                    try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
                    shared.push(.{ .path = path, .out = try buf.toOwnedSlice(shared.allocator) });
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
                    _ = try diff.printChangedLines(buf.writer(shared.allocator), shared.color, path, data, rr.out);
                    shared.push(.{ .path = path, .out = try buf.toOwnedSlice(shared.allocator) });
                }
                return;
            }

            const lines = try splitLines(shared.allocator, data);
            defer shared.allocator.free(lines);

            var emit = try shared.allocator.alloc(bool, lines.len);
            defer shared.allocator.free(emit);
            @memset(emit, false);

            var is_match = try shared.allocator.alloc(bool, lines.len);
            defer shared.allocator.free(is_match);
            @memset(is_match, false);

            var matches: usize = 0;
            for (lines, 0..) |ln, i| {
                if (!engine.matchAny(pat, ln, options.ignore_case)) continue;
                is_match[i] = true;
                matches += engine.countMatches(pat, ln, options.ignore_case);

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
                defer buf.deinit(shared.allocator);
                const w = buf.writer(shared.allocator);
                try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, matches) });
                shared.push(.{ .path = path, .out = try buf.toOwnedSlice(shared.allocator) });
                return;
            }
            if (options.quiet) return;

            var out: std.ArrayListUnmanaged(u8) = .{};
            defer out.deinit(shared.allocator);
            const w = out.writer(shared.allocator);

            var last: ?usize = null;
            if (shared.heading) {
                try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
            }
            for (lines, 0..) |ln, i| {
                if (!emit[i]) continue;

                if ((shared.before > 0 or shared.after > 0) and last != null and i > last.? + 1) {
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
