const std = @import("std");
const ansi = @import("core/ansi.zig");

pub const c = @cImport({
    @cDefine("PCRE2_CODE_UNIT_WIDTH", "8");
    @cInclude("pcre2.h");
});

pub const Code = struct {
    code: ?*c.pcre2_code_8 = null,

    pub fn deinit(self: *Code) void {
        if (self.code) |p| c.pcre2_code_free_8(p);
        self.* = .{};
    }
};

pub fn compile(allocator: std.mem.Allocator, pattern: []const u8, ignore_case: bool) !Code {
    var err_code: c_int = 0;
    var err_off: c.PCRE2_SIZE = 0;

    var opts: c_uint = c.PCRE2_MULTILINE;
    if (ignore_case) opts |= c.PCRE2_CASELESS;

    const code = c.pcre2_compile_8(
        pattern.ptr,
        pattern.len,
        opts,
        &err_code,
        &err_off,
        null,
    ) orelse {
        const msg = try errorMessage(allocator, err_code);
        defer allocator.free(msg);
        std.log.err("pcre2 compile failed at {d}: {s}", .{ err_off, msg });
        return error.InvalidPattern;
    };

    _ = c.pcre2_jit_compile_8(code, c.PCRE2_JIT_COMPLETE);

    return .{ .code = code };
}

pub fn matchAny(code: *const Code, subject: []const u8) bool {
    const md = c.pcre2_match_data_create_from_pattern_8(code.code.?, null) orelse return false;
    defer c.pcre2_match_data_free_8(md);

    const rc = c.pcre2_match_8(
        code.code.?,
        subject.ptr,
        subject.len,
        0,
        0,
        md,
        null,
    );
    return rc >= 0;
}

pub fn countMatches(code: *const Code, subject: []const u8) usize {
    const md = c.pcre2_match_data_create_from_pattern_8(code.code.?, null) orelse return 0;
    defer c.pcre2_match_data_free_8(md);

    var off: usize = 0;
    var n: usize = 0;
    while (off <= subject.len) {
        const rc = c.pcre2_match_8(
            code.code.?,
            subject.ptr,
            subject.len,
            off,
            0,
            md,
            null,
        );
        if (rc < 0) break;
        const ovec = c.pcre2_get_ovector_pointer_8(md);
        const start: usize = @intCast(ovec[0]);
        const end: usize = @intCast(ovec[1]);
        n += 1;
        off = if (end > start) end else start + 1;
    }
    return n;
}

pub fn writeHighlighted(code: *const Code, writer: anytype, subject: []const u8) !void {
    const md = c.pcre2_match_data_create_from_pattern_8(code.code.?, null) orelse return writer.writeAll(subject);
    defer c.pcre2_match_data_free_8(md);

    var off: usize = 0;
    while (off <= subject.len) {
        const rc = c.pcre2_match_8(
            code.code.?,
            subject.ptr,
            subject.len,
            off,
            0,
            md,
            null,
        );
        if (rc < 0) break;
        const ovec = c.pcre2_get_ovector_pointer_8(md);
        const start: usize = @intCast(ovec[0]);
        const end: usize = @intCast(ovec[1]);
        if (start > subject.len or end > subject.len or end < start) break;

        try writer.writeAll(subject[off..start]);
        try writer.print("{f}", .{ansi.styled(true, .match, subject[start..end])});

        off = if (end > start) end else start + 1;
    }
    if (off < subject.len) try writer.writeAll(subject[off..]);
}

pub const ReplaceResult = struct {
    out: []u8,
    n: usize,
};

pub fn replaceAll(allocator: std.mem.Allocator, code: *const Code, subject: []const u8, repl: []const u8) !ReplaceResult {
    const opts: c_uint = c.PCRE2_SUBSTITUTE_GLOBAL | c.PCRE2_SUBSTITUTE_OVERFLOW_LENGTH;

    const cap: usize = subject.len + 64;
    var out = try allocator.alloc(u8, cap);
    errdefer allocator.free(out);

    var out_len: c.PCRE2_SIZE = @intCast(out.len);
    var rc = c.pcre2_substitute_8(
        code.code.?,
        subject.ptr,
        subject.len,
        0,
        opts,
        null,
        null,
        repl.ptr,
        repl.len,
        out.ptr,
        &out_len,
    );
    if (rc >= 0) {
        const used: usize = @intCast(out_len);
        out = shrink(allocator, out, used) catch out;
        return .{ .out = out, .n = @intCast(rc) };
    }
    if (rc != c.PCRE2_ERROR_NOMEMORY) return error.ReplaceFailed;

    allocator.free(out);
    out = try allocator.alloc(u8, @intCast(out_len));
    errdefer allocator.free(out);
    out_len = @intCast(out.len);
    rc = c.pcre2_substitute_8(
        code.code.?,
        subject.ptr,
        subject.len,
        0,
        opts,
        null,
        null,
        repl.ptr,
        repl.len,
        out.ptr,
        &out_len,
    );
    if (rc < 0) {
        allocator.free(out);
        return error.ReplaceFailed;
    }

    const used: usize = @intCast(out_len);
    out = shrink(allocator, out, used) catch out;
    return .{ .out = out, .n = @intCast(rc) };
}

fn errorMessage(allocator: std.mem.Allocator, err_code: c_int) ![]u8 {
    var buf: [256]u8 = undefined;
    const rc = c.pcre2_get_error_message_8(err_code, &buf, buf.len);
    const n: usize = if (rc >= 0) @intCast(rc) else 0;
    return allocator.dupe(u8, buf[0..n]);
}

fn shrink(allocator: std.mem.Allocator, buf: []u8, used: usize) ![]u8 {
    if (used == buf.len) return buf;
    if (used == 0) {
        allocator.free(buf);
        return allocator.alloc(u8, 0);
    }
    return try allocator.realloc(buf, used);
}

test "pcre2 replace and count" {
    var code = try compile(std.testing.allocator, "foo(\\d+)", false);
    defer code.deinit();

    try std.testing.expect(matchAny(&code, "xx foo123 yy"));
    try std.testing.expectEqual(@as(usize, 2), countMatches(&code, "foo1 foo2"));

    const rr = try replaceAll(std.testing.allocator, &code, "foo12 bar foo34", "bar$1");
    defer std.testing.allocator.free(rr.out);
    try std.testing.expectEqual(@as(usize, 2), rr.n);
    try std.testing.expectEqualStrings("bar12 bar bar34", rr.out);
}
