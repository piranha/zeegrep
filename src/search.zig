//! Transducer-based search pipeline
//!
//! Pipeline: paths -> FileProcessor -> OutputSink
//!
//! Each file is processed as a unit: open, mmap, search/replace, format, close.
//! This keeps resource management simple (arena per file).
//!
//! ## Making FileProcessor more granular (future work)
//!
//! FileProcessor is itself a mini-pipeline:
//!   path -> open -> mmap -> binary_check -> search/replace -> format -> cleanup
//!
//! This could be expressed as composed transducers:
//! ```
//! const FilePipeline = Compose(.{
//!     Keep(openAndMmap),      // path -> ?FileCtx
//!     Filter(notBinary),      // skip binary files
//!     Keep(searchOrReplace),  // FileCtx -> ?Matches
//!     Map(format),            // Matches -> FormattedOutput
//! });
//! ```
//!
//! The challenge is **resource cleanup** (mmap lifetime, arena reuse):
//!
//! Option A: Bracket transducer - `Bracket(acquire, release, inner)`
//!   Wraps inner pipeline, guarantees release even on error/halt.
//!   Like Haskell's bracket or Python's context manager.
//!
//! Option B: Resource-carrying context - FileCtx flows through with arena,
//!   final stage calls cleanup. Risk: early termination skips cleanup.
//!
//! Option C: RAII wrapper types - MappedFile has deinit(), transducer
//!   infrastructure calls deinit on scope exit. Needs language support
//!   or explicit defer in each transform function.
//!
//! For now, FileProcessor handles this imperatively with defer.
//! The granular approach would improve composability (swap search strategy,
//! add caching, etc.) but adds complexity. Profile first.

const std = @import("std");
const xf = @import("transducer.zig");
const engine = @import("engine.zig");
const diff = @import("diff.zig");
const ansi = @import("core/ansi.zig");

// ============================================================================
// Types
// ============================================================================

/// Result of processing a single file
pub const FileResult = struct {
    output: []const u8, // formatted output to write (empty = nothing to write)
    matches: usize,
    replacements: usize,
    changed: bool,
};

/// Comptime options for pipeline construction
pub const PipelineOptions = struct {
    replace: bool,
    before: u32,
    after: u32,
    color: bool,
    heading: bool,
    ignore_case: bool,
    quiet: bool,
    count: bool,
    file_names: bool,
    dry_run: bool,
    debug: bool,
};

/// Runtime context for processing
pub const RuntimeContext = struct {
    pat: *const engine.Pattern,
    replace_str: ?[]const u8,
};

// ============================================================================
// File Processing Transducer
// ============================================================================

/// A transducer that processes files. Takes path, emits FileResult.
/// Runtime context is stored in the transducer instance.
pub fn FileProcessor(comptime opts: PipelineOptions) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                ctx: RuntimeContext,

                const Self = @This();

                // Thread-local arena, reset after each file
                threadlocal var tl_arena: ?std.heap.ArenaAllocator = null;

                fn getArena() *std.heap.ArenaAllocator {
                    if (tl_arena == null) {
                        tl_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
                    }
                    return &tl_arena.?;
                }

                pub fn step(self: *Self, path: []const u8) !void {
                    const arena = getArena();
                    defer _ = arena.reset(.retain_capacity);

                    if (processFile(arena.allocator(), path, self.ctx)) |result| {
                        try self.downstream.step(result);
                    }
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }

                fn processFile(tmp: std.mem.Allocator, path: []const u8, ctx: RuntimeContext) ?FileResult {
                    return processInner(tmp, path, ctx) catch |e| {
                        if (opts.debug) {
                            std.debug.print("DEBUG: {s}: {s}\n", .{ path, @errorName(e) });
                        }
                        return null;
                    };
                }

                fn processInner(tmp: std.mem.Allocator, path: []const u8, ctx: RuntimeContext) !?FileResult {
                    // File name matching mode
                    if (opts.file_names) {
                        if (!engine.matchAny(ctx.pat, path, opts.ignore_case)) return null;
                        const n = engine.countMatches(ctx.pat, path, opts.ignore_case);

                        if (opts.quiet) return FileResult{ .output = "", .matches = n, .replacements = 0, .changed = false };

                        var buf: std.ArrayListUnmanaged(u8) = .{};
                        const w = buf.writer(tmp);
                        if (opts.count) {
                            try w.print("{f}:{f}\n", .{ ansi.styled(opts.color, .path, path), ansi.styled(opts.color, .line_no, n) });
                        } else {
                            try w.print("{f}\n", .{ ansi.styled(opts.color, .path, path) });
                        }
                        const output = try tmp.dupe(u8, buf.items);
                        return FileResult{ .output = output, .matches = n, .replacements = 0, .changed = false };
                    }

                    // Open and map file
                    var f = std.fs.cwd().openFile(path, .{}) catch return null;
                    defer f.close();

                    const stat = f.stat() catch return null;
                    if (stat.size == 0) return null;
                    if (stat.size > 1024 * 1024 * 1024) return null; // >1GB

                    const file_data = mapFile(tmp, f, stat.size) catch return null;
                    defer unmapFile(tmp, file_data);
                    const data = file_data.ptr;

                    if (isBinary(data)) return null;

                    // Replace mode
                    if (opts.replace) {
                        const repl = ctx.replace_str orelse return null;
                        const rr = try engine.replaceAll(tmp, ctx.pat, data, repl, opts.ignore_case);
                        if (rr.n == 0) return null;

                        if (!opts.dry_run) try writeAtomic(tmp, path, rr.out);

                        if (opts.quiet) return FileResult{ .output = "", .matches = 0, .replacements = rr.n, .changed = true };

                        if (opts.dry_run) {
                            var buf: std.ArrayListUnmanaged(u8) = .{};
                            _ = try diff.printChangedLines(buf.writer(tmp), opts.color, path, data, rr.out, opts.before, opts.after);
                            const output = try tmp.dupe(u8, buf.items);
                            return FileResult{ .output = output, .matches = 0, .replacements = rr.n, .changed = true };
                        }

                        return FileResult{ .output = "", .matches = 0, .replacements = rr.n, .changed = true };
                    }

                    // Search mode
                    const has_context = opts.before > 0 or opts.after > 0;
                    var out: std.ArrayListUnmanaged(u8) = .{};
                    const result = try streamMatches(out.writer(tmp), opts.color, opts.heading, has_context, path, ctx.pat, data, opts.before, opts.after, opts.ignore_case, opts.quiet or opts.count, null);

                    if (result.match_count == 0) return null;
                    if (opts.quiet) return FileResult{ .output = "", .matches = result.match_count, .replacements = 0, .changed = false };

                    if (opts.count) {
                        out.clearRetainingCapacity();
                        try out.writer(tmp).print("{f}:{f}\n", .{ ansi.styled(opts.color, .path, path), ansi.styled(opts.color, .line_no, result.match_count) });
                    }

                    const output = try tmp.dupe(u8, out.items);
                    return FileResult{ .output = output, .matches = result.match_count, .replacements = 0, .changed = false };
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink, ctx: RuntimeContext) apply(Sink) {
            return .{ .downstream = sink, .ctx = ctx };
        }
    };
}

// ============================================================================
// Output Sink
// ============================================================================

/// Sink that writes output and accumulates stats
pub fn OutputSink(comptime opts: PipelineOptions, comptime Writer: type) type {
    return struct {
        writer: Writer,
        mutex: ?*std.Thread.Mutex, // null for single-threaded

        // Stats
        total_matches: usize = 0,
        total_repls: usize = 0,
        files_changed: usize = 0,
        first_out: bool = true,
        had_error: bool = false,

        const Self = @This();

        pub fn step(self: *Self, result: FileResult) !void {
            if (self.mutex) |m| m.lock();
            defer if (self.mutex) |m| m.unlock();

            self.total_matches += result.matches;
            self.total_repls += result.replacements;
            if (result.changed) self.files_changed += 1;

            if (result.output.len > 0) {
                // Write separator if needed
                if (!self.first_out) {
                    if (opts.heading) {
                        self.writer.writeByte('\n') catch {
                            self.had_error = true;
                            return;
                        };
                    } else if (opts.before > 0 or opts.after > 0) {
                        self.writer.print("{f}\n", .{ansi.styled(opts.color, .dim, "--")}) catch {
                            self.had_error = true;
                            return;
                        };
                    }
                }
                self.first_out = false;

                self.writer.writeAll(result.output) catch {
                    self.had_error = true;
                };
            }
        }

        pub fn finish(_: *Self) !void {}
    };
}

// ============================================================================
// Helper functions (copied from run.zig - could be shared)
// ============================================================================

const FileMap = struct { ptr: []const u8, is_mmap: bool };

fn mapFile(allocator: std.mem.Allocator, file: std.fs.File, size: u64) !FileMap {
    if (@import("builtin").os.tag == .windows) {
        const buf = try allocator.alloc(u8, @intCast(size));
        const n = try file.readAll(buf);
        return .{ .ptr = buf[0..n], .is_mmap = false };
    }
    const ptr = try std.posix.mmap(null, @intCast(size), std.posix.PROT.READ, .{ .TYPE = .PRIVATE }, file.handle, 0);
    return .{ .ptr = ptr, .is_mmap = true };
}

fn unmapFile(allocator: std.mem.Allocator, fm: FileMap) void {
    if (fm.is_mmap) {
        std.posix.munmap(@alignCast(@constCast(fm.ptr)));
    } else {
        allocator.free(fm.ptr);
    }
}

fn isBinary(data: []const u8) bool {
    const check_len = @min(data.len, 8192);
    return std.mem.indexOfScalar(u8, data[0..check_len], 0) != null;
}

fn writeAtomic(allocator: std.mem.Allocator, path: []const u8, data: []const u8) !void {
    const dir = std.fs.cwd();
    var atomic = try dir.atomicFile(path, .{});
    defer atomic.deinit();
    try atomic.file.writeAll(data);
    try atomic.finish();
    _ = allocator;
}

// Import from run.zig
const run_impl = @import("run.zig");
const streamMatches = run_impl.streamMatches;

// ============================================================================
// Driving the Pipeline
// ============================================================================

/// Run search/replace on a list of paths
pub fn search(
    comptime opts: PipelineOptions,
    paths: []const []const u8,
    writer: anytype,
    ctx: RuntimeContext,
) !struct { matches: usize, replacements: usize, files_changed: usize } {
    const Processor = FileProcessor(opts);
    const Sink = OutputSink(opts, @TypeOf(writer));

    const sink = Sink{
        .writer = writer,
        .mutex = null,
    };

    var pipeline = Processor.wrap(Sink, sink, ctx);

    for (paths) |path| {
        pipeline.step(path) catch |err| {
            if (err == error.TransducerHalt) break;
            return err;
        };
    }
    try pipeline.finish();

    if (pipeline.downstream.had_error) return error.OutputFailed;

    return .{
        .matches = pipeline.downstream.total_matches,
        .replacements = pipeline.downstream.total_repls,
        .files_changed = pipeline.downstream.files_changed,
    };
}

// ============================================================================
// Tests
// ============================================================================

test "search basic" {
    const alloc = std.testing.allocator;

    // Create a temp file
    var tmp_dir = std.testing.tmpDir(.{});
    defer tmp_dir.cleanup();

    const file_content = "hello world\nfoo bar\nhello again\n";
    try tmp_dir.dir.writeFile(.{ .sub_path = "test.txt", .data = file_content });

    // Get the path
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const path = try tmp_dir.dir.realpath("test.txt", &path_buf);

    // Compile pattern
    var pat = try engine.compile(alloc, "hello", false, false);
    defer pat.deinit();

    // Run search
    var output: std.ArrayListUnmanaged(u8) = .{};
    defer output.deinit(alloc);
    const writer = output.writer(alloc);

    const paths = [_][]const u8{path};

    const result = try search(
        .{
            .replace = false,
            .before = 0,
            .after = 0,
            .color = false,
            .heading = false,
            .ignore_case = false,
            .quiet = false,
            .count = false,
            .file_names = false,
            .dry_run = false,
            .debug = false,
        },
        &paths,
        &writer,
        .{ .pat = &pat, .replace_str = null },
    );

    try std.testing.expectEqual(@as(usize, 2), result.matches);
    try std.testing.expect(output.items.len > 0);
}
