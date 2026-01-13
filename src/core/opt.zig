//! Declarative CLI option parsing
//!
//! Usage:
//!   const TxOpts = struct {
//!       interface: []const u8 = "wlan0",
//!       port: u16 = 5600,
//!       fhss: bool = false,
//!
//!       pub const meta = .{
//!           .interface = .{ .short = 'i', .help = "WiFi interface" },
//!           .port = .{ .short = 'p', .help = "UDP port" },
//!           .fhss = .{ .help = "Enable FHSS" },
//!       };
//!       pub const about = .{ .name = "tx", .desc = "Low-level TX (debug)" };
//!   };
//!
//!   var opts = TxOpts{};
//!   const rest = opt.parse(TxOpts, &opts, args) catch |e| {
//!       opt.printUsage(TxOpts);
//!       return e;
//!   };

const std = @import("std");

pub const ParseError = error{
    MissingValue,
    InvalidValue,
    UnknownOption,
    Help,
    OutOfMemory,
    TooManyValues,
};

/// Parse args into opts struct. Returns remaining positional args.
pub fn parse(comptime T: type, opts: *T, args: []const []const u8) ParseError![]const []const u8 {
    return parseInternal(T, void, opts, undefined, args, false);
}

/// Parse args into two structs (global + subcommand). Options can appear anywhere.
/// First positional is subcommand name (skipped). Returns remaining positional args.
pub fn parseMerged(
    comptime G: type,
    comptime S: type,
    global: *G,
    sub: *S,
    args: []const []const u8,
) ParseError![]const []const u8 {
    return parseInternal(G, S, global, sub, args, true);
}

fn parseInternal(
    comptime G: type,
    comptime S: type,
    global: *G,
    sub: anytype,
    args: []const []const u8,
    comptime merged: bool,
) ParseError![]const []const u8 {
    const g_fields = @typeInfo(G).@"struct".fields;
    const g_has_meta = @hasDecl(G, "meta");
    const s_fields = if (S != void) @typeInfo(S).@"struct".fields else &[_]std.builtin.Type.StructField{};
    const s_has_meta = if (S != void) @hasDecl(S, "meta") else false;

    var i: usize = 0;
    var found_subcmd = false;
    var first_pos: usize = args.len;

    while (i < args.len) : (i += 1) {
        const arg = args[i];

        if (arg.len == 0) continue;

        // Help flag
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            return ParseError.Help;
        }

        // Long option: --name or --name=value
        if (arg.len > 2 and arg[0] == '-' and arg[1] == '-') {
            const rest = arg[2..];
            var name: []const u8 = rest;
            var inline_value: ?[]const u8 = null;

            if (std.mem.indexOf(u8, rest, "=")) |eq| {
                name = rest[0..eq];
                inline_value = rest[eq + 1 ..];
            }

            var name_buf: [64]u8 = undefined;
            const norm_name = normalizeName(name, &name_buf);

            // Try global first, then subcommand
            if (setField(G, g_fields, g_has_meta, global, norm_name, inline_value, args, &i)) {
                continue;
            }
            if (merged and S != void) {
                if (setField(S, s_fields, s_has_meta, sub, norm_name, inline_value, args, &i)) {
                    continue;
                }
            }
            // In merged mode with empty subcommand struct, skip unknown options
            // (allows config system to handle --radio-* style options)
            if (merged and s_fields.len == 0) {
                // Skip value if next arg isn't an option
                if (inline_value == null and i + 1 < args.len and args[i + 1].len > 0 and args[i + 1][0] != '-') {
                    i += 1;
                }
                continue;
            }
            return ParseError.UnknownOption;
        }

        // Short option: -x or -xvalue
        if (arg.len >= 2 and arg[0] == '-' and arg[1] != '-') {
            const short = arg[1];
            const inline_value: ?[]const u8 = if (arg.len > 2) arg[2..] else null;

            // Try global first, then subcommand
            if (setFieldByShort(G, g_fields, g_has_meta, global, short, inline_value, args, &i)) {
                continue;
            }
            if (merged and S != void) {
                if (setFieldByShort(S, s_fields, s_has_meta, sub, short, inline_value, args, &i)) {
                    continue;
                }
            }
            // In merged mode with empty subcommand struct, skip unknown short options
            if (merged and s_fields.len == 0) {
                if (inline_value == null and i + 1 < args.len and args[i + 1].len > 0 and args[i + 1][0] != '-') {
                    i += 1;
                }
                continue;
            }
            return ParseError.UnknownOption;
        }

        // Positional
        if (merged and !found_subcmd) {
            // First positional in merged mode is subcommand name - skip it
            found_subcmd = true;
            continue;
        }
        if (first_pos == args.len) first_pos = i;
        // Continue parsing to handle trailing options after positionals
    }

    return args[first_pos..];
}

fn normalizeName(name: []const u8, buf: []u8) []const u8 {
    var j: usize = 0;
    for (name) |c| {
        if (j >= buf.len) break;
        buf[j] = if (c == '-') '_' else c;
        j += 1;
    }
    return buf[0..j];
}

fn setField(
    comptime T: type,
    comptime fields: []const std.builtin.Type.StructField,
    comptime has_meta: bool,
    opts: *T,
    name: []const u8,
    inline_value: ?[]const u8,
    args: []const []const u8,
    i: *usize,
) bool {
    inline for (fields) |field| {
        if (std.mem.eql(u8, field.name, name)) {
            setValue(T, opts, field, has_meta, inline_value, args, i) catch return false;
            return true;
        }
    }
    return false;
}

fn setFieldByShort(
    comptime T: type,
    comptime fields: []const std.builtin.Type.StructField,
    comptime has_meta: bool,
    opts: *T,
    short: u8,
    inline_value: ?[]const u8,
    args: []const []const u8,
    i: *usize,
) bool {
    if (!has_meta) return false;

    inline for (fields) |field| {
        if (@hasField(@TypeOf(T.meta), field.name)) {
            const field_meta = @field(T.meta, field.name);
            if (@hasField(@TypeOf(field_meta), "short")) {
                if (field_meta.short == short) {
                    setValue(T, opts, field, has_meta, inline_value, args, i) catch return false;
                    return true;
                }
            }
        }
    }
    return false;
}

fn setValue(
    comptime T: type,
    opts: *T,
    comptime field: std.builtin.Type.StructField,
    comptime has_meta: bool,
    inline_value: ?[]const u8,
    args: []const []const u8,
    i: *usize,
) ParseError!void {
    const F = field.type;

    // Bool fields are flags (no value needed)
    if (F == bool) {
        @field(opts, field.name) = true;
        return;
    }

    // Get value from inline or next arg
    const value = inline_value orelse blk: {
        if (i.* + 1 >= args.len) return ParseError.MissingValue;
        i.* += 1;
        break :blk args[i.*];
    };

    // Repeatable options: append when the field is a list-like type.
    if (listElemType(F)) |Elem| {
        const parsed = try parseScalar(Elem, value);
        var list_ptr = &@field(opts, field.name);
        list_ptr.append(parsed) catch |e| {
            const name = @errorName(e);
            if (std.mem.eql(u8, name, "OutOfMemory")) return ParseError.OutOfMemory;
            if (std.mem.eql(u8, name, "Overflow")) return ParseError.TooManyValues;
            return ParseError.InvalidValue;
        };
        return;
    }

    // Parse based on type
    @field(opts, field.name) = try parseScalar(F, value);

    _ = has_meta;
}

fn parseScalar(comptime T: type, value: []const u8) ParseError!T {
    if (T == []const u8) return value;

    switch (@typeInfo(T)) {
        .optional => {
            const Child = @typeInfo(T).optional.child;
            const parsed = try parseScalar(Child, value);
            return @as(T, parsed);
        },
        .int => return std.fmt.parseInt(T, value, 0) catch return ParseError.InvalidValue,
        .@"enum" => return std.meta.stringToEnum(T, value) orelse return ParseError.InvalidValue,
        else => return ParseError.InvalidValue,
    }
}

/// Fixed-capacity list type for repeatable CLI options (no allocator required).
///
/// Example:
///   const Opts = struct {
///       exclude: opt.Multi([]const u8, 32) = .{},
///       pub const meta = .{ .exclude = .{ .short = 'x' } };
///   };
pub fn Multi(comptime Elem: type, comptime capacity: usize) type {
    return struct {
        const Self = @This();

        buffer: [capacity]Elem = undefined,
        len: usize = 0,

        pub fn append(self: *Self, item: Elem) error{Overflow}!void {
            if (self.len >= capacity) return error.Overflow;
            self.buffer[self.len] = item;
            self.len += 1;
        }

        pub fn slice(self: *Self) []Elem {
            return self.buffer[0..self.len];
        }

        pub fn constSlice(self: *const Self) []const Elem {
            return self.buffer[0..self.len];
        }
    };
}

fn listElemType(comptime ListT: type) ?type {
    if (@typeInfo(ListT) != .@"struct") return null;

    if (!@hasDecl(ListT, "append")) return null;
    const append_info = @typeInfo(@TypeOf(ListT.append));
    if (append_info != .@"fn") return null;
    const params = append_info.@"fn".params;
    if (params.len != 2) return null;

    const fields = @typeInfo(ListT).@"struct".fields;
    var elem: ?type = null;
    inline for (fields) |field| {
        if (std.mem.eql(u8, field.name, "items")) {
            const info = @typeInfo(field.type);
            if (info == .pointer and info.pointer.size == .slice) elem = info.pointer.child;
        }

        if (std.mem.eql(u8, field.name, "buffer")) {
            const info = @typeInfo(field.type);
            if (info == .array) elem = info.array.child;
        }
    }

    const found = elem orelse return null;
    const item_param = params[1].type orelse return null;
    if (item_param != found) return null;

    return found;
}

/// Print usage/help for opts struct
pub fn printUsage(comptime T: type, writer: anytype) void {
    const fields = @typeInfo(T).@"struct".fields;
    const has_meta = @hasDecl(T, "meta");
    const has_about = @hasDecl(T, "about");

    // Header
    if (has_about) {
        writer.print("{s}", .{T.about.name}) catch return;
        if (@hasField(@TypeOf(T.about), "desc")) {
            writer.print(" - {s}", .{T.about.desc}) catch return;
        }
        writer.writeAll("\n\n") catch return;
        if (@hasField(@TypeOf(T.about), "usage")) {
            writer.writeAll(T.about.usage) catch return;
            writer.writeAll("\n") catch return;
        }
    }

    writer.writeAll("Options:\n") catch return;

    // Options
    inline for (fields) |field| {
        const short: ?u8 = if (has_meta and @hasField(@TypeOf(T.meta), field.name)) blk: {
            const m = @field(T.meta, field.name);
            break :blk if (@hasField(@TypeOf(m), "short")) m.short else null;
        } else null;

        const help: ?[]const u8 = if (has_meta and @hasField(@TypeOf(T.meta), field.name)) blk: {
            const m = @field(T.meta, field.name);
            break :blk if (@hasField(@TypeOf(m), "help")) m.help else null;
        } else null;

        // Format: "  -x, --long-name <type>  Help text (default: val)"
        writer.writeAll("  ") catch return;

        if (short) |s| {
            writer.print("-{c}, ", .{s}) catch return;
        } else {
            writer.writeAll("    ") catch return;
        }

        // Convert field_name to --field-name
        writer.writeAll("--") catch return;
        for (field.name) |c| {
            writer.writeByte(if (c == '_') '-' else c) catch return;
        }

        // Type hint
        if (field.type != bool) {
            writer.print(" <{s}>", .{typeName(field.type)}) catch return;
        }

        // Padding
        const name_len = field.name.len + 2 + (if (field.type != bool) typeName(field.type).len + 3 else 0);
        const pad = if (name_len < 24) 24 - name_len else 1;
        writer.writeByteNTimes(' ', pad) catch return;

        // Help text
        if (help) |h| {
            writer.writeAll(h) catch return;
        }

        // Default value
        if (field.defaultValue()) |def| {
            if (field.type == bool) {
                // Don't show default for flags
            } else if (field.type == []const u8) {
                writer.print(" (default: {s})", .{def}) catch return;
            } else if (@typeInfo(field.type) == .optional) {
                // Don't show default for optionals
            } else if (@typeInfo(field.type) == .int) {
                writer.print(" (default: {d})", .{def}) catch return;
            } else if (@typeInfo(field.type) == .@"enum") {
                writer.print(" (default: {s})", .{@tagName(def)}) catch return;
            }
        }

        writer.writeAll("\n") catch return;
    }

    writer.writeAll("  -h, --help                  Show this help\n") catch return;
}

fn typeName(comptime T: type) [:0]const u8 {
    if (T == []const u8) return "str";
    if (@typeInfo(T) == .optional) {
        return typeName(@typeInfo(T).optional.child);
    }
    if (listElemType(T)) |Elem| {
        return typeName(Elem);
    }
    if (@typeInfo(T) == .int) return @typeName(T);
    if (@typeInfo(T) == .@"enum") return "enum";
    return "?";
}

/// Convenience: print to stderr
pub fn usage(comptime T: type) void {
    var buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    printUsage(T, fbs.writer());
    std.fs.File.stderr().writeAll(fbs.getWritten()) catch {};
}

/// Find subcommand name (first positional arg)
pub fn findSubcmd(comptime E: type, args: []const []const u8) ?E {
    for (args) |arg| {
        if (arg.len > 0 and arg[0] != '-') {
            return std.meta.stringToEnum(E, arg);
        }
    }
    return null;
}

/// Print merged usage (global + subcommand opts)
pub fn printMergedUsage(comptime G: type, comptime S: type, writer: anytype) void {
    const has_about = @hasDecl(S, "about");

    // Header from subcommand
    if (has_about) {
        writer.print("{s}", .{S.about.name}) catch return;
        if (@hasField(@TypeOf(S.about), "desc")) {
            writer.print(" - {s}", .{S.about.desc}) catch return;
        }
        writer.writeAll("\n\n") catch return;
    }

    // Subcommand options first
    writer.writeAll("Options:\n") catch return;
    printFields(S, writer);

    // Global options
    writer.writeAll("\nGlobal options:\n") catch return;
    printFields(G, writer);

    writer.writeAll("  -h, --help              Show this help\n") catch return;
}

fn printFields(comptime T: type, writer: anytype) void {
    const fields = @typeInfo(T).@"struct".fields;
    const has_meta = @hasDecl(T, "meta");

    inline for (fields) |field| {
        const short: ?u8 = if (has_meta and @hasField(@TypeOf(T.meta), field.name)) blk: {
            const m = @field(T.meta, field.name);
            break :blk if (@hasField(@TypeOf(m), "short")) m.short else null;
        } else null;

        const help: ?[]const u8 = if (has_meta and @hasField(@TypeOf(T.meta), field.name)) blk: {
            const m = @field(T.meta, field.name);
            break :blk if (@hasField(@TypeOf(m), "help")) m.help else null;
        } else null;

        writer.writeAll("  ") catch return;

        if (short) |s| {
            writer.print("-{c}, ", .{s}) catch return;
        } else {
            writer.writeAll("    ") catch return;
        }

        writer.writeAll("--") catch return;
        for (field.name) |c| {
            writer.writeByte(if (c == '_') '-' else c) catch return;
        }

        if (field.type != bool) {
            writer.print(" <{s}>", .{typeName(field.type)}) catch return;
        }

        const name_len = field.name.len + 2 + (if (field.type != bool) typeName(field.type).len + 3 else 0);
        const pad = if (name_len < 24) 24 - name_len else 1;
        writer.writeByteNTimes(' ', pad) catch return;

        if (help) |h| {
            writer.writeAll(h) catch return;
        }

        if (field.defaultValue()) |def| {
            if (field.type == bool) {
                // skip
            } else if (field.type == []const u8) {
                writer.print(" (default: {s})", .{def}) catch return;
            } else if (@typeInfo(field.type) == .optional) {
                // skip
            } else if (@typeInfo(field.type) == .int) {
                writer.print(" (default: {d})", .{def}) catch return;
            } else if (@typeInfo(field.type) == .@"enum") {
                writer.print(" (default: {s})", .{@tagName(def)}) catch return;
            }
        }

        writer.writeAll("\n") catch return;
    }
}

/// Convenience: print merged usage to stderr
pub fn mergedUsage(comptime G: type, comptime S: type) void {
    var buf: [4096]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    printMergedUsage(G, S, fbs.writer());
    std.fs.File.stderr().writeAll(fbs.getWritten()) catch {};
}

// ============ Tests ============

test "parse basic options" {
    const Opts = struct {
        name: []const u8 = "default",
        count: u32 = 0,
        verbose: bool = false,

        pub const meta = .{
            .name = .{ .short = 'n', .help = "Name" },
            .count = .{ .short = 'c', .help = "Count" },
            .verbose = .{ .short = 'v', .help = "Verbose" },
        };
    };

    var opts = Opts{};
    _ = try parse(Opts, &opts, &.{ "-n", "foo", "-c", "42", "-v" });

    try std.testing.expectEqualStrings("foo", opts.name);
    try std.testing.expectEqual(@as(u32, 42), opts.count);
    try std.testing.expect(opts.verbose);
}

test "parse long options" {
    const Opts = struct {
        input_port: u16 = 5600,
        output_addr: []const u8 = "127.0.0.1",
    };

    var opts = Opts{};
    _ = try parse(Opts, &opts, &.{ "--input-port", "1234", "--output-addr=192.168.1.1" });

    try std.testing.expectEqual(@as(u16, 1234), opts.input_port);
    try std.testing.expectEqualStrings("192.168.1.1", opts.output_addr);
}

test "parse returns positional args" {
    const Opts = struct {
        verbose: bool = false,

        pub const meta = .{ .verbose = .{ .short = 'v' } };
    };

    var opts = Opts{};
    const rest = try parse(Opts, &opts, &.{ "-v", "file1", "file2" });

    try std.testing.expect(opts.verbose);
    try std.testing.expectEqual(@as(usize, 2), rest.len);
    try std.testing.expectEqualStrings("file1", rest[0]);
}

test "help flag returns error" {
    const Opts = struct {
        name: []const u8 = "x",
    };

    var opts = Opts{};
    const result = parse(Opts, &opts, &.{"--help"});
    try std.testing.expectError(ParseError.Help, result);
}

test "printUsage output" {
    const Opts = struct {
        replace: []const u8 = "",
        context: u8 = 2,
        dry_run: bool = false,

        pub const meta = .{
            .replace = .{ .short = 'r', .help = "Replacement string" },
            .context = .{ .short = 'C', .help = "Lines of context" },
            .dry_run = .{ .short = 'n', .help = "Preview changes" },
        };
        pub const about = .{ .name = "zg", .desc = "Fast search and replace" };
    };

    var buf: [2048]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    printUsage(Opts, fbs.writer());
    const output = fbs.getWritten();

    try std.testing.expect(std.mem.indexOf(u8, output, "zg - Fast search and replace") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "-r, --replace") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "(default: 2)") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "--dry-run") != null);
}

test "optional field" {
    const Opts = struct {
        config: ?[]const u8 = null,
        limit: ?u32 = null,
    };

    var opts = Opts{};
    _ = try parse(Opts, &opts, &.{ "--config", "foo.toml", "--limit", "100" });

    try std.testing.expectEqualStrings("foo.toml", opts.config.?);
    try std.testing.expectEqual(@as(u32, 100), opts.limit.?);
}

test "enum field" {
    const Mode = enum { fast, slow, auto };
    const Opts = struct {
        mode: Mode = .auto,
    };

    var opts = Opts{};
    _ = try parse(Opts, &opts, &.{ "--mode", "fast" });

    try std.testing.expectEqual(Mode.fast, opts.mode);
}

test "findSubcmd" {
    const Cmd = enum { tx, rx, drone, gs };

    try std.testing.expectEqual(Cmd.tx, findSubcmd(Cmd, &.{ "-v", "tx", "-i", "wlan0" }));
    try std.testing.expectEqual(Cmd.rx, findSubcmd(Cmd, &.{"rx"}));
    try std.testing.expectEqual(null, findSubcmd(Cmd, &.{ "-v", "--quiet" }));
    try std.testing.expectEqual(null, findSubcmd(Cmd, &.{"unknown"}));
}

test "parseMerged global opts anywhere" {
    const Global = struct {
        verbose: bool = false,
        quiet: bool = false,

        pub const meta = .{
            .verbose = .{ .short = 'v', .help = "Verbose" },
            .quiet = .{ .short = 'q', .help = "Quiet" },
        };
    };

    const TxOpts = struct {
        interface: []const u8 = "wlan0",
        port: u16 = 5600,

        pub const meta = .{
            .interface = .{ .short = 'i', .help = "Interface" },
            .port = .{ .short = 'p', .help = "Port" },
        };
    };

    // Global before subcommand
    {
        var global = Global{};
        var sub = TxOpts{};
        _ = try parseMerged(Global, TxOpts, &global, &sub, &.{ "-v", "tx", "-i", "wlan1" });
        try std.testing.expect(global.verbose);
        try std.testing.expectEqualStrings("wlan1", sub.interface);
    }

    // Global after subcommand
    {
        var global = Global{};
        var sub = TxOpts{};
        _ = try parseMerged(Global, TxOpts, &global, &sub, &.{ "tx", "-i", "wlan1", "-v" });
        try std.testing.expect(global.verbose);
        try std.testing.expectEqualStrings("wlan1", sub.interface);
    }

    // Mixed order
    {
        var global = Global{};
        var sub = TxOpts{};
        _ = try parseMerged(Global, TxOpts, &global, &sub, &.{ "tx", "-v", "-i", "wlan1", "--quiet", "-p", "1234" });
        try std.testing.expect(global.verbose);
        try std.testing.expect(global.quiet);
        try std.testing.expectEqualStrings("wlan1", sub.interface);
        try std.testing.expectEqual(@as(u16, 1234), sub.port);
    }
}

test "parseMerged returns positional after subcmd" {
    const Global = struct { verbose: bool = false };
    const Sub = struct { name: []const u8 = "x" };

    var global = Global{};
    var sub = Sub{};
    const rest = try parseMerged(Global, Sub, &global, &sub, &.{ "cmd", "--name", "foo", "pos1", "pos2" });

    try std.testing.expectEqualStrings("foo", sub.name);
    try std.testing.expectEqual(@as(usize, 2), rest.len);
    try std.testing.expectEqualStrings("pos1", rest[0]);
}

test "repeatable option appends (fixed-capacity list)" {
    const Opts = struct {
        exclude: Multi([]const u8, 8) = .{},

        pub const meta = .{ .exclude = .{ .short = 'x', .help = "Exclude" } };
    };

    var opts = Opts{};
    _ = try parse(Opts, &opts, &.{ "-x", "log", "-x", "pyc" });

    const got = opts.exclude.constSlice();
    try std.testing.expectEqual(@as(usize, 2), got.len);
    try std.testing.expectEqualStrings("log", got[0]);
    try std.testing.expectEqualStrings("pyc", got[1]);
}

test "trailing options after positionals" {
    const Opts = struct {
        verbose: bool = false,
        count: bool = false,

        pub const meta = .{
            .verbose = .{ .short = 'v' },
            .count = .{},
        };
    };

    var opts = Opts{};
    const rest = try parse(Opts, &opts, &.{ "path1", "--count", "path2", "-v" });

    try std.testing.expect(opts.verbose);
    try std.testing.expect(opts.count);
    // Returned slice includes positionals starting from first one
    try std.testing.expectEqual(@as(usize, 4), rest.len);
    try std.testing.expectEqualStrings("path1", rest[0]);
}
