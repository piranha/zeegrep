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

    // Interpret escape sequences in pattern and replacement
    const processed_pattern = try engine.interpretEscapes(allocator, pattern);
    defer allocator.free(processed_pattern);
    const processed_replace = if (options.replace) |r| try engine.interpretEscapes(allocator, r) else null;
    defer if (processed_replace) |r| allocator.free(r);

    var pat = try engine.compile(allocator, processed_pattern, options.ignore_case, options.multiline);
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

    var shared = Shared.init(shared_alloc, before, after, use_color, use_heading, options.sort, processed_replace, writer.any());
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

    // When sorting, results are buffered - sort and output them
    // When not sorting, results were already streamed to stdout
    if (options.sort) {
        std.mem.sort(Result, shared.results.items, {}, Result.less);
        var first_out = true;
        for (shared.results.items) |r| {
            defer shared_alloc.free(r.path);
            defer shared_alloc.free(r.out);
            if (!options.quiet) {
                if (shared.heading and !first_out) try writer.writeByte('\n');
                first_out = false;
                try writer.writeAll(r.out);
            }
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
    sort: bool,
    replace: ?[]const u8,
    writer: std.io.AnyWriter,

    mu: std.Thread.Mutex = .{},
    results: std.ArrayListUnmanaged(Result) = .{},
    first_out: bool = true,

    total_matches: usize = 0,
    total_repls: usize = 0,
    files_changed: usize = 0,
    had_error: bool = false,

    fn init(allocator: std.mem.Allocator, before: u32, after: u32, color: bool, heading: bool, sort: bool, replace: ?[]const u8, writer: std.io.AnyWriter) Shared {
        return .{ .allocator = allocator, .before = before, .after = after, .color = color, .heading = heading, .sort = sort, .replace = replace, .writer = writer };
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

    /// Output result: buffer if sorting, stream to writer if not
    fn output(self: *Shared, path: []const u8, out: []const u8) void {
        self.mu.lock();
        defer self.mu.unlock();
        if (self.sort) {
            self.results.append(self.allocator, .{
                .path = self.allocator.dupe(u8, path) catch {
                    self.had_error = true;
                    return;
                },
                .out = self.allocator.dupe(u8, out) catch {
                    self.had_error = true;
                    return;
                },
            }) catch {
                self.had_error = true;
            };
        } else {
            if (self.heading and !self.first_out) self.writer.writeByte('\n') catch {};
            self.first_out = false;
            self.writer.writeAll(out) catch {
                self.had_error = true;
            };
        }
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
            std.debug.print("DEBUG: {s}: {s}\n", .{ path, reason });
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
                    shared.output(path, buf.items);
                    return;
                }
                if (!options.quiet and !options.count) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    const w = buf.writer(tmp);
                    try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
                    shared.output(path, buf.items);
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

            if (shared.replace) |repl| {
                const rr = try engine.replaceAll(tmp, pat, data, repl, options.ignore_case);
                if (rr.n == 0) return;

                if (!options.dry_run) try writeAtomic(tmp, path, rr.out);

                shared.add(0, rr.n, true);
                if (options.dry_run and !options.quiet) {
                    var buf: std.ArrayListUnmanaged(u8) = .{};
                    _ = try diff.printChangedLines(buf.writer(tmp), shared.color, path, data, rr.out, shared.before, shared.after);
                    shared.output(path, buf.items);
                }
                return;
            }

            // Context lines: use line-based approach
            if (shared.before > 0 or shared.after > 0) {
                try searchWithContext(tmp, shared, path, data, pat, options);
                return;
            }

            // Whole-file matching with iterator (zero allocation)
            var matches: usize = 0;
            var out: std.ArrayListUnmanaged(u8) = .{};
            const w = out.writer(tmp);
            var wrote_heading = false;
            var last_end_line: ?usize = null;

            var it = engine.MatchIterator.init(pat, data);
            defer it.deinit();

            // Incremental line tracking to avoid O(n) offsetToLine calls
            var line_tracker = LineTracker{};

            while (it.next()) |span| {
                matches += 1;
                const start_line = line_tracker.lineAt(data, span.start);
                // Skip output if we already output this line (but still count)
                if (last_end_line != null and start_line <= last_end_line.?) continue;

                if (options.quiet or options.count) {
                    last_end_line = start_line;
                    continue;
                }

                const match_text = data[span.start..span.end];
                const is_multiline_match = std.mem.indexOfScalar(u8, match_text, '\n') != null;

                if (is_multiline_match) {
                    const end_line = line_tracker.lineAt(data, span.end);
                    if (last_end_line != null and start_line > last_end_line.? + 1)
                        try w.print("{f}\n", .{ansi.styled(shared.color, .dim, "--")});
                    try w.print("{f} {f}:{f}-{f} {f}\n", .{
                        ansi.styled(shared.color, .dim, "──"),
                        ansi.styled(shared.color, .path, path),
                        ansi.styled(shared.color, .line_no, start_line),
                        ansi.styled(shared.color, .line_no, end_line),
                        ansi.styled(shared.color, .dim, "──"),
                    });
                    const block_start = lineStartEnd(data, span.start).start;
                    const block_end = lineStartEnd(data, if (span.end > 0) span.end - 1 else 0).end;
                    var line_start = block_start;
                    var cur_line = start_line;
                    while (line_start <= block_end) {
                        const le = lineStartEnd(data, line_start);
                        try w.print("{f}│ ", .{ansi.styled(shared.color, .line_no, cur_line)});
                        if (line_start >= span.start and le.end <= span.end) {
                            try w.print("{f}", .{ansi.styled(shared.color, .match, data[line_start..le.end])});
                        } else if (span.start > line_start and span.start < le.end) {
                            try w.writeAll(data[line_start..span.start]);
                            try w.print("{f}", .{ansi.styled(shared.color, .match, data[span.start..@min(span.end, le.end)])});
                            if (span.end < le.end) try w.writeAll(data[span.end..le.end]);
                        } else if (span.end > line_start and span.end <= le.end) {
                            try w.print("{f}", .{ansi.styled(shared.color, .match, data[line_start..span.end])});
                            try w.writeAll(data[span.end..le.end]);
                        } else try w.writeAll(data[line_start..le.end]);
                        try w.writeByte('\n');
                        cur_line += 1;
                        line_start = le.end + 1;
                    }
                    last_end_line = end_line;
                } else {
                    const le = lineStartEnd(data, span.start);
                    if (last_end_line != null and start_line > last_end_line.? + 1)
                        try w.print("{f}\n", .{ansi.styled(shared.color, .dim, "--")});
                    if (shared.heading and !wrote_heading) {
                        try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
                        wrote_heading = true;
                    }
                    if (!shared.heading) try w.print("{f}:", .{ansi.styled(shared.color, .path, path)});
                    try w.print("{f}:", .{ansi.styled(shared.color, .line_no, start_line)});
                    try engine.writeHighlighted(shared.color, pat, w, data[le.start..le.end], options.ignore_case);
                    try w.writeByte('\n');
                    last_end_line = start_line;
                }
            }
            if (matches == 0) return;
            shared.add(matches, 0, false);
            if (options.count and !options.quiet)
                try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, matches) });
            if (!options.quiet and out.items.len > 0) shared.output(path, out.items);
        }

        fn searchWithContext(tmp: std.mem.Allocator, shared: *Shared, path: []const u8, data: []const u8, pat: *const engine.Pattern, options: Opts) !void {
            // Context lines needed
            const lines = try splitLines(tmp, data);
            const emit = try tmp.alloc(bool, lines.len);
            @memset(emit, false);
            const is_match = try tmp.alloc(bool, lines.len);
            @memset(is_match, false);

            var matches: usize = 0;
            for (lines, 0..) |ln, idx| {
                const n = engine.countMatches(pat, ln, options.ignore_case);
                if (n == 0) continue;
                is_match[idx] = true;
                matches += n;
                const lo: usize = if (shared.before > idx) 0 else idx - shared.before;
                var hi: usize = idx + shared.after;
                if (hi >= lines.len) hi = lines.len - 1;
                var j: usize = lo;
                while (j <= hi) : (j += 1) emit[j] = true;
            }
            if (matches == 0) return;
            shared.add(matches, 0, false);

            if (options.count) {
                if (options.quiet) return;
                var buf: std.ArrayListUnmanaged(u8) = .{};
                try buf.writer(tmp).print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, matches) });
                shared.output(path, buf.items);
                return;
            }
            if (options.quiet) return;

            var out: std.ArrayListUnmanaged(u8) = .{};
            const w = out.writer(tmp);
            var last: ?usize = null;
            if (shared.heading) try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
            for (lines, 0..) |ln, idx| {
                if (!emit[idx]) continue;
                if (last != null and idx > last.? + 1) try w.print("{f}\n", .{ansi.styled(shared.color, .dim, "--")});
                last = idx;
                const sep: u8 = if (is_match[idx]) ':' else '-';
                if (!shared.heading) try w.print("{f}{c}", .{ ansi.styled(shared.color, .path, path), sep });
                try w.print("{f}{c}", .{ ansi.styled(shared.color, .line_no, idx + 1), sep });
                if (is_match[idx]) try engine.writeHighlighted(shared.color, pat, w, ln, options.ignore_case) else try w.writeAll(ln);
                try w.writeByte('\n');
            }
            shared.output(path, out.items);
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

fn offsetToLine(data: []const u8, offset: usize) usize {
    var line: usize = 1;
    for (data[0..@min(offset, data.len)]) |c| {
        if (c == '\n') line += 1;
    }
    return line;
}

// Incremental line tracker - O(delta) instead of O(n) for sequential offsets
const LineTracker = struct {
    pos: usize = 0,
    line: usize = 1,

    fn lineAt(self: *LineTracker, data: []const u8, offset: usize) usize {
        if (offset >= self.pos) {
            // Forward scan from current position
            for (data[self.pos..@min(offset, data.len)]) |c| {
                if (c == '\n') self.line += 1;
            }
        } else {
            // Backward scan (rare, but handle it)
            var i = self.pos;
            while (i > offset) {
                i -= 1;
                if (data[i] == '\n') self.line -= 1;
            }
        }
        self.pos = offset;
        return self.line;
    }
};

fn lineStartEnd(data: []const u8, offset: usize) struct { start: usize, end: usize } {
    var start = offset;
    while (start > 0 and data[start - 1] != '\n') start -= 1;
    var end = offset;
    while (end < data.len and data[end] != '\n') end += 1;
    return .{ .start = start, .end = end };
}

fn isBinary(data: []const u8) bool {
    const n = @min(data.len, 4096);
    return std.mem.indexOfScalar(u8, data[0..n], 0) != null;
}

const FileData = struct {
    ptr: []const u8,
    section_handle: if (builtin.os.tag == .windows) std.os.windows.HANDLE else void = if (builtin.os.tag == .windows) undefined else {},
};

fn mapFile(allocator: std.mem.Allocator, f: std.fs.File, size: u64) !FileData {
    _ = allocator;
    if (comptime builtin.os.tag == .windows) {
        const w = std.os.windows;
        var section_handle: w.HANDLE = undefined;
        const create_rc = w.ntdll.NtCreateSection(
            &section_handle,
            w.STANDARD_RIGHTS_REQUIRED | w.SECTION_QUERY | w.SECTION_MAP_READ,
            null,
            null,
            w.PAGE_READONLY,
            w.SEC_COMMIT,
            f.handle,
        );
        if (create_rc != .SUCCESS) return error.MmapFailed;
        errdefer w.CloseHandle(section_handle);

        var view_size: usize = 0;
        var base_ptr: usize = 0;
        const map_rc = w.ntdll.NtMapViewOfSection(
            section_handle,
            w.GetCurrentProcess(),
            @ptrCast(&base_ptr),
            null,
            0,
            null,
            &view_size,
            .ViewUnmap,
            0,
            w.PAGE_READONLY,
        );
        if (map_rc != .SUCCESS) return error.MmapFailed;

        return .{
            .ptr = @as([*]const u8, @ptrFromInt(base_ptr))[0..size],
            .section_handle = section_handle,
        };
    } else {
        const data = try std.posix.mmap(null, size, std.c.PROT.READ, .{ .TYPE = .PRIVATE }, f.handle, 0);
        return .{ .ptr = data };
    }
}

fn unmapFile(allocator: std.mem.Allocator, fd: FileData) void {
    _ = allocator;
    if (comptime builtin.os.tag == .windows) {
        const w = std.os.windows;
        _ = w.ntdll.NtUnmapViewOfSection(w.GetCurrentProcess(), @ptrFromInt(@intFromPtr(fd.ptr.ptr)));
        w.CloseHandle(fd.section_handle);
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
        multiline: bool = false,
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

    // Streaming (sort=false) - output goes directly to writer
    fbs.reset();
    o.file_names = false;
    o.sort = false;
    _ = run(std.testing.allocator, fbs.writer(), o, "foo", &.{root}) catch {};
    out = fbs.getWritten();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "foo1") != null);
}

fn isMultiPathSearch(cwd: std.fs.Dir, paths: []const []const u8) bool {
    if (paths.len == 0) return true;
    for (paths) |p| {
        const st = cwd.statFile(p) catch continue;
        if (st.kind == .directory) return true;
    }
    return false;
}

test "bench LineTracker vs naive offsetToLine" {
    const alloc = std.testing.allocator;

    // Generate ~1MB with ~10k lines
    var hay: std.ArrayListUnmanaged(u8) = .{};
    defer hay.deinit(alloc);
    for (0..10000) |_| try hay.appendSlice(alloc, "this is a line of text with some content here\n");
    const data = hay.items;
    const line_count: usize = 10000;

    // Generate offsets spread across the file (simulating match positions)
    var offsets: [1000]usize = undefined;
    for (&offsets, 0..) |*o, i| o.* = (i * data.len) / 1000;

    std.debug.print("\nbench LineTracker: {d}KB, {d} lines, {d} lookups\n", .{ data.len / 1024, line_count, offsets.len });

    // Naive: O(n) per lookup
    var timer = try std.time.Timer.start();
    var sum: usize = 0;
    for (offsets) |off| sum += offsetToLine(data, off);
    const naive_ns = timer.read();

    // LineTracker: O(delta) per lookup
    timer.reset();
    var sum2: usize = 0;
    var tracker = LineTracker{};
    for (offsets) |off| sum2 += tracker.lineAt(data, off);
    const tracker_ns = timer.read();

    try std.testing.expectEqual(sum, sum2);
    const speedup = @as(f64, @floatFromInt(naive_ns)) / @as(f64, @floatFromInt(@max(tracker_ns, 1)));
    std.debug.print("  naive: {d}ms, tracker: {d}ms, speedup: {d:.1}x\n", .{
        naive_ns / std.time.ns_per_ms,
        tracker_ns / std.time.ns_per_ms,
        speedup,
    });

    // Tracker should be significantly faster for sequential access
    try std.testing.expect(speedup > 10.0);
}
