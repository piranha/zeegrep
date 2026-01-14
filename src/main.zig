const std = @import("std");
const opt = @import("core/opt.zig");
const run = @import("run.zig");
const ansi = @import("core/ansi.zig");
const build_options = @import("build_options");

const version = build_options.version;

const Options = struct {
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

    if (options.version) {
        std.debug.print("zeegrep {s}\n", .{version});
        return;
    }

    if (rest.len == 0) {
        opt.usage(Options);
        return;
    }
    const pattern = rest[0];

    // Parse options from remaining args (options can appear anywhere, mixed with paths)
    const remaining = opt.parse(Options, &options, rest[1..]) catch |e| switch (e) {
        error.Help => {
            opt.usage(Options);
            return;
        },
        else => return e,
    };

    // Filter out parsed options from remaining args to get actual paths
    var path_buf: [64][]const u8 = undefined;
    var n_paths: usize = 0;
    for (remaining) |arg| {
        if (arg.len > 0 and arg[0] != '-') {
            path_buf[n_paths] = arg;
            n_paths += 1;
        }
    }
    const paths = path_buf[0..n_paths];

    const stdout = std.fs.File.stdout().deprecatedWriter();
    run.run(allocator, stdout, options, pattern, paths) catch |e| switch (e) {
        error.NoMatches => std.process.exit(1),
        else => std.process.exit(2),
    };
}
