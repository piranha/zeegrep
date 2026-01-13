const std = @import("std");
const opt = @import("core/opt.zig");

const Options = struct {
    replace: ?[]const u8 = null,
    dry_run: bool = false,
    ignore_case: bool = false,
    context: u32 = 0,
    include: opt.Multi([]const u8, 64) = .{},
    exclude: opt.Multi([]const u8, 64) = .{},

    pub const meta = .{
        .replace = .{ .short = 'r', .help = "Replacement string (enables replace)" },
        .dry_run = .{ .short = 'n', .help = "Dry-run (show diff, no writes)" },
        .ignore_case = .{ .short = 'i', .help = "Case-insensitive search" },
        .context = .{ .short = 'C', .help = "Context lines" },
        .include = .{ .short = 'g', .help = "Only paths containing substring (repeatable)" },
        .exclude = .{ .short = 'x', .help = "Skip paths containing substring (repeatable)" },
    };

    pub const about = .{ .name = "zg", .desc = "zeegrep (WIP)" };
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

    var stdout = std.fs.File.stdout().deprecatedWriter();
    try stdout.print("pattern: {s}\n", .{pattern});
    try printList(stdout, "exclude", options.exclude.constSlice());
    try printList(stdout, "include", options.include.constSlice());
    try printList(stdout, "paths", paths);
}

fn printList(writer: anytype, label: []const u8, items: []const []const u8) !void {
    if (items.len == 0) return;
    try writer.print("{s}:", .{label});
    for (items) |it| try writer.print(" {s}", .{it});
    try writer.writeByte('\n');
}
