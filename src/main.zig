const std = @import("std");
const opt = @import("core/opt.zig");
const run = @import("run.zig");
const build_options = @import("build_options");

const version = build_options.version;
const Options = run.Options;

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
        error.UnknownOption, error.MissingValue, error.InvalidValue => std.process.exit(1),
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

    var stdout = std.fs.File.stdout().writer(&.{});
    run.run(allocator, &stdout.interface, options, pattern, paths) catch |e| switch (e) {
        error.NoMatches => std.process.exit(1),
        else => std.process.exit(2),
    };
}
