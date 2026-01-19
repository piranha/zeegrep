const std = @import("std");
const builtin = @import("builtin");
const engine = @import("engine.zig");
const diff = @import("diff.zig");
const core_pool = @import("core/pool.zig");
const core_atomic = @import("core/atomic.zig");
const core_walk = @import("core/walk.zig");
const core_ignore = @import("core/ignore.zig");
const ansi = @import("core/ansi.zig");
const opt = @import("opt");

const max_file_size = 1 << 30; // 1GB
const max_parallelism = 3;

pub const Options = struct {
    replace: ?[]const u8 = null,
    dry_run: bool = false,
    ignore_case: bool = false,
    multiline: bool = false,
    literal: bool = false,
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
        .literal = .{ .short = 'F', .help = "Treat pattern as literal string" },
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

pub fn run(allocator: std.mem.Allocator, writer: *std.io.Writer, options: Options, pattern: []const u8, paths: []const []const u8) !void {
    const cwd = std.fs.cwd();

    if (options.file_names and options.replace != null) return error.InvalidArgs;

    // Interpret escape sequences in pattern and replacement
    const processed_pattern = try engine.interpretEscapes(allocator, pattern);
    defer allocator.free(processed_pattern);
    const escaped_replace = if (options.replace) |r| try engine.interpretEscapes(allocator, r) else null;
    defer if (escaped_replace) |r| allocator.free(r);

    var pat = try engine.compileOpts(allocator, processed_pattern, options.ignore_case, options.multiline, options.literal);
    defer pat.deinit();

    // For literal patterns, expand $0 to needle once (not per-file)
    const processed_replace = if (escaped_replace) |r| try pat.expandReplace(allocator, r) else null;
    defer if (processed_replace) |r| if (r.ptr != (escaped_replace orelse r).ptr) allocator.free(r);

    const before: u32 = if (options.context > 0) options.context else options.before;
    const after: u32 = if (options.context > 0) options.context else options.after;

    const is_tty = std.fs.File.stdout().isTty();
    const no_color = std.process.getEnvVarOwned(allocator, "NO_COLOR") catch null;
    defer if (no_color) |v| allocator.free(v);
    const use_color = ansi.enabled(options.color, is_tty, no_color != null);
    const use_heading = is_tty and options.replace == null and !options.quiet and !options.count and !options.file_names and isMultiPathSearch(cwd, paths);

    var ign = core_ignore.Stack.init(allocator);
    defer ign.deinit();
    try ign.pushDir(cwd, "");
    defer ign.popDir();

    // Sorted mode: collect paths, sort, process sequentially (streaming output, low memory)
    // Parallel mode: walk and spawn jobs simultaneously
    if (options.sort) {
        var collected_paths: std.ArrayListUnmanaged([]const u8) = .{};
        defer {
            for (collected_paths.items) |p| allocator.free(p);
            collected_paths.deinit(allocator);
        }

        const PathCollector = struct {
            paths: *std.ArrayListUnmanaged([]const u8),
            alloc: std.mem.Allocator,
            cwd: std.fs.Dir,
            use_abs: bool,

            pub fn onFile(self: *@This(), path: []const u8) void {
                const p = if (self.use_abs)
                    self.cwd.realpathAlloc(self.alloc, path) catch return
                else
                    self.alloc.dupe(u8, path) catch return;
                self.paths.append(self.alloc, p) catch self.alloc.free(p);
            }
        };
        var collector = PathCollector{ .paths = &collected_paths, .alloc = allocator, .cwd = cwd, .use_abs = options.abs };
        try core_walk.walkFiles(allocator, cwd, &ign, paths, options.include.constSlice(), options.exclude.constSlice(), options.hidden, true, &collector);

        std.mem.sort([]const u8, collected_paths.items, {}, struct {
            fn lt(_: void, a: []const u8, b: []const u8) bool {
                return std.mem.lessThan(u8, a, b);
            }
        }.lt);

        var state = StreamState{
            .total_matches = 0,
            .total_repls = 0,
            .files_changed = 0,
            .had_error = false,
            .first_out = true,
        };
        var arena = std.heap.ArenaAllocator.init(allocator);
        defer arena.deinit();
        for (collected_paths.items) |path| {
            processFile(arena.allocator(), &state, path, &pat, options, use_color, use_heading, before, after, writer);
            _ = arena.reset(.retain_capacity);
        }

        if (options.replace != null and !options.quiet) {
            try writer.print("{d} files, {d} replacements\n", .{ state.files_changed, state.total_repls });
        }
        if (state.had_error) return error.JobFailed;
        if (state.total_matches == 0 and state.total_repls == 0) return error.NoMatches;
        return;
    }

    // Parallel mode
    var ts = std.heap.ThreadSafeAllocator{ .child_allocator = allocator };
    const shared_alloc = ts.allocator();

    var pool: core_pool.Pool = .{};
    const n_jobs = @min(max_parallelism, std.Thread.getCpuCount() catch max_parallelism);
    try pool.init(allocator, n_jobs);
    defer pool.deinit();

    var shared = Shared.init(shared_alloc, &pool, before, after, use_color, use_heading, processed_replace, writer);

    var scope = pool.scope();

    const Walker = struct {
        scope: *core_pool.Scope,
        shared: *Shared,
        pat: *const engine.Pattern,
        opts: Options,
        allocator: std.mem.Allocator,
        cwd: std.fs.Dir,

        pub fn onFile(self: *@This(), path: []const u8) void {
            const p = self.allocator.dupe(u8, path) catch return;
            if (self.opts.abs) {
                const abs = self.cwd.realpathAlloc(self.allocator, p) catch {
                    self.allocator.free(p);
                    return;
                };
                self.allocator.free(p);
                self.scope.spawn(Job.run, .{ self.shared, abs, self.opts, self.pat });
            } else {
                self.scope.spawn(Job.run, .{ self.shared, p, self.opts, self.pat });
            }
        }
    };
    var walker = Walker{
        .scope = &scope,
        .shared = &shared,
        .pat = &pat,
        .opts = options,
        .allocator = shared_alloc,
        .cwd = cwd,
    };
    try core_walk.walkFiles(allocator, cwd, &ign, paths, options.include.constSlice(), options.exclude.constSlice(), options.hidden, false, &walker);
    scope.wait();

    if (options.replace != null and !options.quiet) {
        try writer.print("{d} files, {d} replacements\n", .{ shared.files_changed.load(.monotonic), shared.total_repls.load(.monotonic) });
    }

    if (shared.had_error.load(.monotonic)) return error.JobFailed;
    if (shared.total_matches.load(.monotonic) == 0 and shared.total_repls.load(.monotonic) == 0) return error.NoMatches;
}

const StreamState = struct {
    total_matches: usize,
    total_repls: usize,
    files_changed: usize,
    had_error: bool,
    first_out: bool,
};

const Shared = struct {
    allocator: std.mem.Allocator,
    pool: *core_pool.Pool,
    before: u32,
    after: u32,
    color: bool,
    heading: bool,
    replace: ?[]const u8,
    writer: *std.io.Writer,

    mu: std.Thread.Mutex = .{},
    first_out: bool = true,

    total_matches: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    total_repls: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    files_changed: std.atomic.Value(usize) = std.atomic.Value(usize).init(0),
    had_error: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),

    fn init(allocator: std.mem.Allocator, pool: *core_pool.Pool, before: u32, after: u32, color: bool, heading: bool, replace: ?[]const u8, writer: *std.io.Writer) Shared {
        return .{ .allocator = allocator, .pool = pool, .before = before, .after = after, .color = color, .heading = heading, .replace = replace, .writer = writer };
    }

    fn add(self: *Shared, matches: usize, repls: usize, changed: bool) void {
        _ = self.total_matches.fetchAdd(matches, .monotonic);
        _ = self.total_repls.fetchAdd(repls, .monotonic);
        if (changed) _ = self.files_changed.fetchAdd(1, .monotonic);
    }

    fn output(self: *Shared, out: []const u8) void {
        self.mu.lock();
        defer self.mu.unlock();

        if (!self.first_out) {
            if (self.heading) {
                self.writer.writeByte('\n') catch {};
            } else if (self.before > 0 or self.after > 0) {
                self.writer.print("{f}\n", .{ansi.styled(self.color, .dim, "--")}) catch {};
            }
        }
        self.first_out = false;
        self.writer.writeAll(out) catch {
            self.had_error.store(true, .monotonic);
        };
    }

    fn fail(self: *Shared) void {
        self.had_error.store(true, .monotonic);
    }
};

const Job = struct {
    fn run(thread_id: usize, shared: *Shared, path: []const u8, options: Options, pat: *const engine.Pattern) void {
        defer shared.allocator.free(path); // path was duped by walker
        const arena = shared.pool.arena(thread_id);
        defer _ = arena.reset(.retain_capacity);
        work(arena.allocator(), shared, path, options, pat) catch |e| {
            if (options.debug) debugSkip(path, @errorName(e));
            shared.fail();
        };
    }

    fn debugSkip(path: []const u8, reason: []const u8) void {
        std.debug.print("DEBUG: {s}: {s}\n", .{ path, reason });
    }

    fn work(tmp: std.mem.Allocator, shared: *Shared, path: []const u8, options: Options, pat: *const engine.Pattern) !void {
        if (options.file_names) {
            if (!engine.matchAny(pat, path, options.ignore_case)) return;
            const n = engine.countMatches(pat, path, options.ignore_case);
            shared.add(n, 0, false);
            if (options.count and !options.quiet) {
                var buf: std.ArrayListUnmanaged(u8) = .{};
                const w = buf.writer(tmp);
                try w.print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, n) });
                shared.output(buf.items);
                return;
            }
            if (!options.quiet and !options.count) {
                var buf: std.ArrayListUnmanaged(u8) = .{};
                const w = buf.writer(tmp);
                try w.print("{f}\n", .{ansi.styled(shared.color, .path, path)});
                shared.output(buf.items);
                return;
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
        if (stat.size == 0) return;
        if (stat.size > max_file_size) {
            if (options.debug) debugSkip(path, "file too large (>1GB)");
            return;
        }

        const file_data = mapFile(f, stat.size) catch |e| {
            if (options.debug) debugSkip(path, @errorName(e));
            return;
        };
        defer unmapFile(file_data);
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
                shared.output(buf.items);
                return;
            }
            return;
        }

        const has_context = shared.before > 0 or shared.after > 0;

        // Stream matches to buffer, then output
        var out: std.ArrayListUnmanaged(u8) = .{};
        const match_count = try streamMatches(out.writer(tmp), shared.color, shared.heading, has_context, path, pat, data, shared.before, shared.after, options.ignore_case, options.quiet or options.count, null);
        if (match_count == 0) return;
        shared.add(match_count, 0, false);
        if (options.quiet) return;
        if (options.count) {
            out.clearRetainingCapacity();
            try out.writer(tmp).print("{f}:{f}\n", .{ ansi.styled(shared.color, .path, path), ansi.styled(shared.color, .line_no, match_count) });
        }
        if (out.items.len > 0) shared.output(out.items);
    }
};

/// Single-threaded file processing for sorted mode - streams directly to writer
fn processFile(
    tmp: std.mem.Allocator,
    state: *StreamState,
    path: []const u8,
    pat: *const engine.Pattern,
    options: Options,
    color: bool,
    heading: bool,
    before: u32,
    after: u32,
    writer: *std.io.Writer,
) void {
    processFileInner(tmp, state, path, pat, options, color, heading, before, after, writer) catch |e| {
        if (options.debug) std.debug.print("DEBUG: {s}: {s}\n", .{ path, @errorName(e) });
        state.had_error = true;
    };
}

fn processFileInner(
    tmp: std.mem.Allocator,
    state: *StreamState,
    path: []const u8,
    pat: *const engine.Pattern,
    options: Options,
    color: bool,
    heading: bool,
    before: u32,
    after: u32,
    writer: *std.io.Writer,
) !void {
    if (options.file_names) {
        if (!engine.matchAny(pat, path, options.ignore_case)) return;
        const n = engine.countMatches(pat, path, options.ignore_case);
        state.total_matches += n;
        if (!options.quiet) {
            try writeSeparator(state, writer, color, heading, before, after);
            if (options.count) {
                try writer.print("{f}:{f}\n", .{ ansi.styled(color, .path, path), ansi.styled(color, .line_no, n) });
            } else {
                try writer.print("{f}\n", .{ansi.styled(color, .path, path)});
            }
        }
        return;
    }

    var f = std.fs.cwd().openFile(path, .{}) catch return;
    defer f.close();

    const stat = f.stat() catch return;
    if (stat.size == 0) return;
    if (stat.size > max_file_size) return;

    const file_data = mapFile(f, stat.size) catch return;
    defer unmapFile(file_data);
    const data = file_data.ptr;

    if (isBinary(data)) return;

    if (options.replace) |repl| {
        const rr = try engine.replaceAll(tmp, pat, data, repl, options.ignore_case);
        if (rr.n == 0) return;

        if (!options.dry_run) try writeAtomic(tmp, path, rr.out);

        state.total_repls += rr.n;
        state.files_changed += 1;
        if (options.dry_run and !options.quiet) {
            try writeSeparator(state, writer, color, heading, before, after);
            _ = try diff.printChangedLines(writer, color, path, data, rr.out, before, after);
        }
        return;
    }

    const has_context = before > 0 or after > 0;

    if (options.count) {
        const match_count = try streamMatches(writer, color, heading, has_context, path, pat, data, before, after, options.ignore_case, true, &state.first_out);
        if (match_count == 0) return;
        state.total_matches += match_count;
        if (options.quiet) return;
        try writeSeparator(state, writer, color, heading, before, after);
        try writer.print("{f}:{f}\n", .{ ansi.styled(color, .path, path), ansi.styled(color, .line_no, match_count) });
        return;
    }

    const match_count = try streamMatches(writer, color, heading, has_context, path, pat, data, before, after, options.ignore_case, options.quiet, &state.first_out);
    if (match_count == 0) return;
    state.total_matches += match_count;
    // streamMatches already wrote output with separators
}

fn writeSeparator(state: *StreamState, writer: *std.io.Writer, color: bool, heading: bool, before: u32, after: u32) !void {
    if (!state.first_out) {
        if (heading) {
            try writer.writeByte('\n');
        } else if (before > 0 or after > 0) {
            try writer.print("{f}\n", .{ansi.styled(color, .dim, "--")});
        }
    }
    state.first_out = false;
}

/// Stream matches directly to writer - no intermediate MatchLine allocation
fn streamMatches(
    writer: anytype,
    color: bool,
    heading: bool,
    has_context: bool,
    path: []const u8,
    pat: *const engine.Pattern,
    data: []const u8,
    before: u32,
    after: u32,
    ignore_case: bool,
    count_only: bool,
    first_out: ?*bool,
) !usize {
    var it = engine.MatchIterator.init(pat, data);
    defer it.deinit();

    var tracker = LineTracker{};
    var match_count: usize = 0;
    var last_emitted: usize = 0; // last line we wrote
    var last_match_line: usize = 0; // last line with a match (for dedup)
    var wrote_heading = false;
    var wrote_file_sep = false; // wrote separator before this file's output
    var multiline_end: usize = 0;
    var pending_after: usize = 0; // context lines still owed after last match

    while (it.next()) |span| {
        match_count += 1;
        const start_line = tracker.lineAt(data, span.start);

        // Skip output if same line as previous match (but still count)
        if (start_line == last_match_line) continue;

        const match_text = data[span.start..span.end];
        const is_multiline = std.mem.indexOfScalar(u8, match_text, '\n') != null;
        const end_line = if (is_multiline) tracker.lineAt(data, span.end) else 0;

        if (count_only) {
            last_match_line = if (end_line > 0) end_line else start_line;
            continue;
        }

        // Write file separator before first output of this file
        if (!wrote_file_sep) {
            if (first_out) |fo| {
                if (!fo.*) {
                    if (heading) {
                        try writer.writeByte('\n');
                    } else if (has_context) {
                        try writer.print("{f}\n", .{ansi.styled(color, .dim, "--")});
                    }
                }
                fo.* = false;
            }
            wrote_file_sep = true;
        }

        // Emit pending after-context from previous match (up to this match's before-context)
        if (pending_after > 0 and last_emitted > 0) {
            const ctx_limit = if (before > 0 and before < start_line) start_line - before else start_line;
            var ctx_line = last_emitted + 1;
            while (ctx_line < ctx_limit and pending_after > 0) : (ctx_line += 1) {
                if (lineAtIndex(data, ctx_line)) |text| {
                    try writeContextLine(writer, color, heading, path, ctx_line, text, &wrote_heading);
                    last_emitted = ctx_line;
                }
                pending_after -= 1;
            }
        }

        // Separator for non-contiguous groups
        if (has_context and last_emitted > 0 and start_line > last_emitted + 1) {
            const in_ml = multiline_end > 0 and start_line <= multiline_end;
            if (!in_ml) try writer.print("{f}\n", .{ansi.styled(color, .dim, "--")});
        }

        // Before-context
        if (before > 0) {
            const ctx_start = if (before >= start_line) 1 else start_line - before;
            var ctx_line = ctx_start;
            while (ctx_line < start_line) : (ctx_line += 1) {
                if (ctx_line > last_emitted) {
                    if (lineAtIndex(data, ctx_line)) |text| {
                        try writeContextLine(writer, color, heading, path, ctx_line, text, &wrote_heading);
                        last_emitted = ctx_line;
                    }
                }
            }
        }

        // The match line(s)
        if (is_multiline) {
            try writer.print("{f} {f}:{f}-{f} {f}\n", .{
                ansi.styled(color, .dim, "──"),
                ansi.styled(color, .path, path),
                ansi.styled(color, .line_no, start_line),
                ansi.styled(color, .line_no, end_line),
                ansi.styled(color, .dim, "──"),
            });
            multiline_end = end_line;

            const block_start = lineStartEnd(data, span.start).start;
            const block_end = lineStartEnd(data, if (span.end > 0) span.end - 1 else 0).end;
            var line_start = block_start;
            var cur_line = start_line;
            while (line_start <= block_end) {
                const le = lineStartEnd(data, line_start);
                try writer.print("{f}│ {f}\n", .{
                    ansi.styled(color, .line_no, cur_line),
                    ansi.styled(color, .match, data[le.start..le.end]),
                });
                cur_line += 1;
                line_start = le.end + 1;
            }
            last_emitted = end_line;
            last_match_line = end_line;
        } else {
            const le = lineStartEnd(data, span.start);
            try writeMatchLine(writer, color, heading, path, start_line, data[le.start..le.end], pat, ignore_case, &wrote_heading);
            last_emitted = start_line;
            last_match_line = start_line;
        }

        pending_after = after;
    }

    // Trailing after-context
    if (!count_only and pending_after > 0 and last_emitted > 0) {
        var ctx_line = last_emitted + 1;
        while (pending_after > 0) : (pending_after -= 1) {
            if (lineAtIndex(data, ctx_line)) |text| {
                try writeContextLine(writer, color, heading, path, ctx_line, text, &wrote_heading);
                ctx_line += 1;
            } else break;
        }
    }

    return match_count;
}

fn writeContextLine(writer: anytype, color: bool, heading: bool, path: []const u8, line_no: usize, text: []const u8, wrote_heading: *bool) !void {
    if (heading and !wrote_heading.*) {
        try writer.print("{f}\n", .{ansi.styled(color, .path, path)});
        wrote_heading.* = true;
    }
    if (!heading) try writer.print("{f}-", .{ansi.styled(color, .path, path)});
    try writer.print("{f}-", .{ansi.styled(color, .line_no, line_no)});
    try writer.writeAll(text);
    try writer.writeByte('\n');
}

fn writeMatchLine(writer: anytype, color: bool, heading: bool, path: []const u8, line_no: usize, text: []const u8, pat: *const engine.Pattern, ignore_case: bool, wrote_heading: *bool) !void {
    if (heading and !wrote_heading.*) {
        try writer.print("{f}\n", .{ansi.styled(color, .path, path)});
        wrote_heading.* = true;
    }
    if (!heading) try writer.print("{f}:", .{ansi.styled(color, .path, path)});
    try writer.print("{f}:", .{ansi.styled(color, .line_no, line_no)});
    try engine.writeHighlighted(color, pat, writer, text, ignore_case);
    try writer.writeByte('\n');
}

fn lineAtIndex(data: []const u8, line_no: usize) ?[]const u8 {
    var current_line: usize = 1;
    var start: usize = 0;
    for (data, 0..) |c, i| {
        if (current_line == line_no) {
            const end = std.mem.indexOfScalarPos(u8, data, i, '\n') orelse data.len;
            return data[start..end];
        }
        if (c == '\n') {
            current_line += 1;
            start = i + 1;
        }
    }
    return null;
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
            var i = self.pos;
            const limit = @min(offset, data.len);

            // SIMD forward
            const Vec = @Vector(32, u8);
            const nl: Vec = @splat('\n');
            while (i + 32 <= limit) {
                const chunk: Vec = data[i..][0..32].*;
                const matches = chunk == nl;
                self.line += @popCount(@as(u32, @bitCast(matches)));
                i += 32;
            }

            // Scalar tail
            while (i < limit) : (i += 1) {
                if (data[i] == '\n') self.line += 1;
            }
        } else {
            // Backward scan (rarely used but needed for correctness)
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

fn mapFile(f: std.fs.File, size: u64) !FileData {
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

fn unmapFile(fd: FileData) void {
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
