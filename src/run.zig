const std = @import("std");
const builtin = @import("builtin");
const engine = @import("engine.zig");
const diff = @import("diff.zig");
const core_pool = @import("core/pool.zig");
const core_atomic = @import("core/atomic.zig");
const core_walk = @import("core/walk.zig");
const core_ignore = @import("core/ignore.zig");
const ansi = @import("core/ansi.zig");
const opt = @import("core/opt.zig");

pub const Options = struct {
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
    sort: bool = false,
    file_names: bool = false,
    hidden: bool = false,
    debug: bool = false,
    color: ansi.Color = .auto,
    include: opt.Multi([]const u8, 64) = .{},
    exclude: opt.Multi([]const u8, 64) = .{},
    version: bool = false,

    pub const meta = .{
        .replace = .{ .short = 'r', .help = "Replacement string (enables replace)" },
        .dry_run = .{ .short = 'n', .help = "Dry-run (show diff, no writes)" },
        .ignore_case = .{ .short = 'i', .help = "Case-insensitive search" },
        .multiline = .{ .short = 'M', .help = "Multiline mode (. matches newlines)" },
        .context = .{ .short = 'C', .help = "Context lines" },
        .after = .{ .short = 'A', .help = "Context lines after match" },
        .before = .{ .short = 'B', .help = "Context lines before match" },
        .quiet = .{ .short = 'q', .help = "Quiet (exit code only)" },
        .count = .{ .help = "Only print match count" },
        .abs = .{ .help = "Print absolute paths" },
        .sort = .{ .help = "Sort output by path" },
        .file_names = .{ .short = 'f', .help = "Match against file names (not contents)" },
        .hidden = .{ .help = "Search hidden files and directories" },
        .debug = .{ .help = "Show skipped files and reasons" },
        .color = .{ .help = "Colorize output (auto|always|never)" },
        .include = .{ .short = 'g', .help = "Only paths containing substring (repeatable)" },
        .exclude = .{ .short = 'x', .help = "Skip paths containing substring (repeatable)" },
        .version = .{ .short = 'V', .help = "Show version" },
    };

    pub const about = .{
        .name = "zeegrep",
        .desc = "zee search & replace tool",
        .usage =
        \\Usage: zg <pattern> [path] [options]
        \\       zg <pattern> -r <replacement> [path] [options]
        \\
        \\Examples:
        \\  zg pattern                 Search in current directory
        \\  zg 'fn\s+\w+' src/         Regex search in specific path
        \\  zg old -r new              Replace in-place
        \\  zg old -r new -n           Dry-run, show diff
        \\  zg 'foo(\d+)' -r 'bar$1'   Replace with capture groups
        \\  zg pattern -x test -g clj  Filter paths by substring
        \\
        ,
    };
};

pub fn run(allocator: std.mem.Allocator, writer: *std.io.Writer, options: anytype, pattern: []const u8, paths: []const []const u8) !void {
    const cwd = std.fs.cwd();

    if (options.file_names and options.replace != null) return error.InvalidArgs;

    // Interpret escape sequences in pattern and replacement
    const processed_pattern = try engine.interpretEscapes(allocator, pattern);
    defer allocator.free(processed_pattern);
    const escaped_replace = if (options.replace) |r| try engine.interpretEscapes(allocator, r) else null;
    defer if (escaped_replace) |r| allocator.free(r);

    var pat = try engine.compile(allocator, processed_pattern, options.ignore_case, options.multiline);
    defer pat.deinit();

    // For literal patterns, expand $0 to needle once (not per-file)
    const processed_replace = if (escaped_replace) |r| try pat.expandReplace(allocator, r) else null;
    defer if (processed_replace) |r| if (r.ptr != (escaped_replace orelse r).ptr) allocator.free(r);

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

    var shared = Shared.init(shared_alloc, before, after, use_color, use_heading, options.sort, processed_replace, writer);
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
            if (!options.quiet) {
                if (shared.heading and !first_out) try writer.writeByte('\n');
                first_out = false;
                try writer.writeAll(r.out);
            }
        }
    }

    if (options.replace != null and !options.quiet) {
        try writer.print("{d} files, {d} replacements\n", .{ shared.files_changed.load(.monotonic), shared.total_repls.load(.monotonic) });
    }

    if (shared.had_error.load(.monotonic)) return error.JobFailed;
    if (shared.total_matches.load(.monotonic) == 0 and shared.total_repls.load(.monotonic) == 0) return error.NoMatches;
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
    writer: *std.io.Writer,

    mu: std.Thread.Mutex = .{},
    results: std.ArrayListUnmanaged(Result) = .{},
    first_out: bool = true,

    total_matches: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    total_repls: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    files_changed: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    had_error: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),

    fn init(allocator: std.mem.Allocator, before: u32, after: u32, color: bool, heading: bool, sort: bool, replace: ?[]const u8, writer: *std.io.Writer) Shared {
        return .{ .allocator = allocator, .before = before, .after = after, .color = color, .heading = heading, .sort = sort, .replace = replace, .writer = writer };
    }

    fn deinit(self: *Shared) void {
        for (self.results.items) |r| {
            self.allocator.free(r.path);
            self.allocator.free(r.out);
        }
        self.results.deinit(self.allocator);
    }

    fn add(self: *Shared, matches: usize, repls: usize, changed: bool) void {
        _ = self.total_matches.fetchAdd(matches, .monotonic);
        _ = self.total_repls.fetchAdd(repls, .monotonic);
        if (changed) _ = self.files_changed.fetchAdd(1, .monotonic);
    }

    /// Output result: buffer if sorting, stream to writer if not
    fn output(self: *Shared, path: []const u8, out: []const u8) void {
        if (self.sort) {
            const d_path = self.allocator.dupe(u8, path) catch {
                self.fail();
                return;
            };
            const d_out = self.allocator.dupe(u8, out) catch {
                self.allocator.free(d_path);
                self.fail();
                return;
            };

            self.mu.lock();
            defer self.mu.unlock();
            self.results.append(self.allocator, .{
                .path = d_path,
                .out = d_out,
            }) catch {
                self.allocator.free(d_path);
                self.allocator.free(d_out);
                self.had_error.store(true, .monotonic);
            };
        } else {
            self.mu.lock();
            defer self.mu.unlock();
            if (self.heading and !self.first_out) self.writer.writeByte('\n') catch {};
            self.first_out = false;
            self.writer.writeAll(out) catch {
                self.had_error.store(true, .monotonic);
            };
        }
    }

    fn fail(self: *Shared) void {
        self.had_error.store(true, .monotonic);
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
            if (stat.size > 1024 * 1024 * 1024) {
                if (options.debug) debugSkip(path, "file too large (>1GB)");
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

            // Collect matches with context
            const result = try collectMatches(tmp, pat, data, shared.before, shared.after);
            if (result.match_count == 0) return;
            shared.add(result.match_count, 0, false);

            if (options.quiet) return;
            if (options.count) {
                var buf: std.ArrayListUnmanaged(u8) = .{};
                try buf.writer(tmp).print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, result.match_count) });
                shared.output(path, buf.items);
                return;
            }

            var out: std.ArrayListUnmanaged(u8) = .{};
            try formatMatches(out.writer(tmp), shared.color, shared.heading, path, result.lines, pat, options.ignore_case);
            if (out.items.len > 0) shared.output(path, out.items);
        }
    };
}

// Intermediate match result for unified formatting
const MatchLine = struct {
    line_no: usize, // 1-indexed
    line_end: usize = 0, // 0 = single line, >0 = multiline match ends here
    text: []const u8, // slice into file data
    is_context: bool = false, // context line vs actual match
};

fn collectMatches(
    allocator: std.mem.Allocator,
    pat: *const engine.Pattern,
    data: []const u8,
    before: u32,
    after: u32,
) !struct { lines: []MatchLine, match_count: usize } {
    var results: std.ArrayListUnmanaged(MatchLine) = .{};
    errdefer results.deinit(allocator);

    var it = engine.MatchIterator.init(pat, data);
    defer it.deinit();

    var tracker = LineTracker{};
    var match_count: usize = 0;
    var last_line: usize = 0;

    while (it.next()) |span| {
        match_count += 1;
        const start_line = tracker.lineAt(data, span.start);

        // Skip if same line as previous match (but still count)
        if (start_line == last_line) continue;

        const match_text = data[span.start..span.end];
        const is_multiline = std.mem.indexOfScalar(u8, match_text, '\n') != null;
        const end_line = if (is_multiline) tracker.lineAt(data, span.end) else 0;

        // Add context lines before (if needed and not already emitted)
        if (before > 0) {
            const ctx_start = if (before >= start_line) 1 else start_line - before;
            var ctx_line = ctx_start;
            while (ctx_line < start_line) : (ctx_line += 1) {
                if (ctx_line > last_line) {
                    const le = lineAtIndex(data, ctx_line);
                    try results.append(allocator, .{
                        .line_no = ctx_line,
                        .text = le.text,
                        .is_context = true,
                    });
                }
            }
        }

        // Add the match line(s)
        if (is_multiline) {
            // For multiline, store all lines in the span
            const block_start = lineStartEnd(data, span.start).start;
            const block_end = lineStartEnd(data, if (span.end > 0) span.end - 1 else 0).end;
            var line_start = block_start;
            var cur_line = start_line;
            while (line_start <= block_end) {
                const le = lineStartEnd(data, line_start);
                try results.append(allocator, .{
                    .line_no = cur_line,
                    .line_end = if (cur_line == start_line) end_line else 0,
                    .text = data[le.start..le.end],
                });
                cur_line += 1;
                line_start = le.end + 1;
            }
            last_line = end_line;
        } else {
            const le = lineStartEnd(data, span.start);
            try results.append(allocator, .{
                .line_no = start_line,
                .text = data[le.start..le.end],
            });
            last_line = start_line;
        }

        // Add context lines after
        if (after > 0) {
            const match_end = if (end_line > 0) end_line else start_line;
            var ctx_line = match_end + 1;
            const ctx_end = match_end + after;
            while (ctx_line <= ctx_end) : (ctx_line += 1) {
                const le = lineAtIndex(data, ctx_line);
                if (le.text.len == 0 and ctx_line > 1) break; // EOF
                try results.append(allocator, .{
                    .line_no = ctx_line,
                    .text = le.text,
                    .is_context = true,
                });
            }
            last_line = @max(last_line, ctx_end);
        }
    }

    return .{ .lines = try results.toOwnedSlice(allocator), .match_count = match_count };
}

fn formatMatches(
    writer: anytype,
    color: bool,
    heading: bool,
    path: []const u8,
    lines: []const MatchLine,
    pat: *const engine.Pattern,
    ignore_case: bool,
) !void {
    var wrote_heading = false;
    var last_line: usize = 0;
    var multiline_end: usize = 0;

    for (lines) |line| {
        const in_multiline = multiline_end > 0 and line.line_no <= multiline_end;

        // Separator for non-contiguous lines
        if (last_line > 0 and line.line_no > last_line + 1 and !in_multiline) {
            try writer.print("{f}\n", .{ansi.styled(color, .dim, "--")});
        }

        // Multiline match header
        if (line.line_end > 0) {
            try writer.print("{f} {f}:{f}-{f} {f}\n", .{
                ansi.styled(color, .dim, "──"),
                ansi.styled(color, .path, path),
                ansi.styled(color, .line_no, line.line_no),
                ansi.styled(color, .line_no, line.line_end),
                ansi.styled(color, .dim, "──"),
            });
            multiline_end = line.line_end;
        }

        // Line content
        if (in_multiline or line.line_end > 0) {
            try writer.print("{f}│ ", .{ansi.styled(color, .line_no, line.line_no)});
            try writer.print("{f}", .{ansi.styled(color, .match, line.text)});
        } else {
            if (heading and !wrote_heading) {
                try writer.print("{f}\n", .{ansi.styled(color, .path, path)});
                wrote_heading = true;
            }
            const sep: u8 = if (line.is_context) '-' else ':';
            if (!heading) try writer.print("{f}{c}", .{ ansi.styled(color, .path, path), sep });
            try writer.print("{f}{c}", .{ ansi.styled(color, .line_no, line.line_no), sep });
            if (line.is_context) {
                try writer.writeAll(line.text);
            } else {
                try engine.writeHighlighted(color, pat, writer, line.text, ignore_case);
            }
        }
        try writer.writeByte('\n');
        last_line = line.line_no;
    }
}

fn lineAtIndex(data: []const u8, line_no: usize) struct { text: []const u8 } {
    var current_line: usize = 1;
    var start: usize = 0;
    for (data, 0..) |c, i| {
        if (current_line == line_no) {
            const end = std.mem.indexOfScalarPos(u8, data, i, '\n') orelse data.len;
            return .{ .text = data[start..end] };
        }
        if (c == '\n') {
            current_line += 1;
            start = i + 1;
        }
    }
    // Line beyond EOF
    return .{ .text = "" };
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
            for (data[self.pos..@min(offset, data.len)]) |c| {
                if (c == '\n') self.line += 1;
            }
        } else {
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

    var o = Options{ .sort = true, .color = .never };
    try o.include.append(".txt");

    var buf: [2048]u8 = undefined;
    var w = std.io.Writer.fixed(&buf);

    // Search
    _ = run(std.testing.allocator, &w, o, "foo", &.{root}) catch |e| {
        try std.testing.expect(e != error.NoMatches);
        return e;
    };
    var out = w.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "1:foo1") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "3:foo2") != null);

    // Replace dry-run
    w.end = 0;
    o.replace = "bar$1";
    o.dry_run = true;
    _ = run(std.testing.allocator, &w, o, "foo(\\d+)", &.{root}) catch {};
    out = w.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "replacements") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "──") != null);

    // Count (per-file)
    w.end = 0;
    o.replace = null;
    o.dry_run = false;
    o.count = true;
    _ = run(std.testing.allocator, &w, o, "foo", &.{root}) catch {};
    out = w.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt:2") != null);
    try std.testing.expect(std.mem.indexOf(u8, out, "b.txt") == null);

    // -f (path match)
    w.end = 0;
    o.count = false;
    o.file_names = true;
    _ = run(std.testing.allocator, &w, o, "a.txt", &.{root}) catch {};
    out = w.buffered();
    try std.testing.expect(std.mem.indexOf(u8, out, "a.txt") != null);

    // Streaming (sort=false) - output goes directly to writer
    w.end = 0;
    o.file_names = false;
    o.sort = false;
    _ = run(std.testing.allocator, &w, o, "foo", &.{root}) catch {};
    out = w.buffered();
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
