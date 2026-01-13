const std = @import("std");
const opt = @import("core/opt.zig");
const run = @import("run.zig");

const Options = struct {
    replace: ?[]const u8 = null,
    dry_run: bool = false,
    ignore_case: bool = false,
    context: u32 = 0,
    after: u32 = 0,
    before: u32 = 0,
    quiet: bool = false,
    count: bool = false,
    abs: bool = false,
    sort: bool = false,
    file_names: bool = false,
    include: opt.Multi([]const u8, 64) = .{},
    exclude: opt.Multi([]const u8, 64) = .{},

    pub const meta = .{
        .replace = .{ .short = 'r', .help = "Replacement string (enables replace)" },
        .dry_run = .{ .short = 'n', .help = "Dry-run (show diff, no writes)" },
        .ignore_case = .{ .short = 'i', .help = "Case-insensitive search" },
        .context = .{ .short = 'C', .help = "Context lines" },
        .after = .{ .short = 'A', .help = "Context lines after match" },
        .before = .{ .short = 'B', .help = "Context lines before match" },
        .quiet = .{ .short = 'q', .help = "Quiet (exit code only)" },
        .count = .{ .help = "Only print match count" },
        .abs = .{ .help = "Print absolute paths" },
        .sort = .{ .help = "Sort output by path" },
        .file_names = .{ .short = 'f', .help = "Match against file names (not contents)" },
        .include = .{ .short = 'g', .help = "Only paths containing substring (repeatable)" },
        .exclude = .{ .short = 'x', .help = "Skip paths containing substring (repeatable)" },
    };

    pub const about = .{ .name = "zeegrep", .desc = "zee search & replace tool" };
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    const args = argv[1..];
    if (args.len == 0) {
        opt.usage(Options);
        return;
    }

    var options = Options{};
    const rest = opt.parse(Options, &options, args) catch |e| switch (e) {
        error.Help => {
            opt.usage(Options);
            return;
        },
        else => return e,
    };

    if (rest.len == 0) {
        opt.usage(Options);
        return;
    }
    const pattern = rest[0];

    // Allow a limited form of "options after pattern" by parsing once more on
    // the args after the pattern (until the first path positional).
    const paths = opt.parse(Options, &options, rest[1..]) catch |e| switch (e) {
        error.Help => {
            opt.usage(Options);
            return;
        },
        else => return e,
    };

    const stdout = std.fs.File.stdout().deprecatedWriter();
    run.run(allocator, stdout, options, pattern, paths) catch |e| switch (e) {
        error.NoMatches => std.process.exit(1),
        else => std.process.exit(2),
    };
}
