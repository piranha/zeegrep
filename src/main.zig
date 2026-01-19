const std = @import("std");
const opt = @import("opt");
const run = @import("run.zig");
const build_options = @import("build_options");

const version = build_options.version;
const Options = run.Options;

fn usage() void {
    var buf: [4096]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&buf);
    opt.printUsage(Options, &stdout.interface);
    stdout.interface.flush() catch {};
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const argv = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, argv);

    const args = argv[1..];
    if (args.len == 0) {
        usage();
        return;
    }

    var options = Options{};
    const rest = opt.parse(Options, &options, args) catch |e| switch (e) {
        error.Help => {
            usage();
            return;
        },
        error.UnknownOption, error.MissingValue, error.InvalidValue => std.process.exit(1),
        else => return e,
    };

    if (options.version) {
        var buf: [64]u8 = undefined;
        var stdout = std.fs.File.stdout().writer(&buf);
        stdout.interface.print("zeegrep {s}\n", .{version}) catch {};
        stdout.interface.flush() catch {};
        return;
    }

    if (rest.len == 0) {
        usage();
        return;
    }
    const pattern = rest[0];

    // Parse options from remaining args (options can appear anywhere, mixed with paths)
    const remaining = opt.parse(Options, &options, rest[1..]) catch |e| switch (e) {
        error.Help => {
            usage();
            return;
        },
        error.UnknownOption, error.MissingValue, error.InvalidValue => std.process.exit(1),
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

    var write_buf: [64 * 1024]u8 = undefined;
    var stdout = std.fs.File.stdout().writer(&write_buf);
    run.run(allocator, &stdout.interface, options, pattern, paths) catch |e| switch (e) {
        error.NoMatches => {
            stdout.interface.flush() catch {};
            std.process.exit(1);
        },
        else => {
            stdout.interface.flush() catch {};
            std.process.exit(2);
        },
    };
    stdout.interface.flush() catch {};
}
