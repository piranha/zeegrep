const std = @import("std");

pub fn isGlob(pat: []const u8) bool {
    return std.mem.indexOfAny(u8, pat, "*?[") != null;
}

pub fn match(pat: []const u8, s: []const u8) bool {
    return matchAt(pat, 0, s, 0);
}

pub fn matchPath(pat: []const u8, path: []const u8) bool {
    var p = pat;
    if (p.len > 0 and p[0] == std.fs.path.sep) p = p[1..];
    if (std.mem.indexOfScalar(u8, p, std.fs.path.sep) == null) return match(p, std.fs.path.basename(path));
    return match(p, path);
}

fn matchAt(pat: []const u8, pi0: usize, s: []const u8, si0: usize) bool {
    var pi = pi0;
    var si = si0;
    var star_pi: ?usize = null;
    var star_si: usize = 0;
    var star_slash = false;

    while (true) {
        if (si == s.len) break;

        if (pi < pat.len and pat[pi] == '\\' and pi + 1 < pat.len) {
            if (pat[pi + 1] == s[si]) {
                pi += 2;
                si += 1;
                continue;
            }
        }

        if (pi < pat.len and pat[pi] == '[') {
            if (classEnd(pat, pi)) |end| {
                if (s[si] != std.fs.path.sep and classHas(pat[pi + 1 .. end], s[si])) {
                    pi = end + 1;
                    si += 1;
                    continue;
                }
            }
        }

        if (pi < pat.len and pat[pi] == '?') {
            if (s[si] != std.fs.path.sep) {
                pi += 1;
                si += 1;
                continue;
            }
        }

        if (pi < pat.len and pat[pi] == '*') {
            star_slash = pi + 1 < pat.len and pat[pi + 1] == '*';
            star_pi = if (star_slash) pi + 2 else pi + 1;
            pi = star_pi.?;
            star_si = si;
            continue;
        }

        if (pi < pat.len and pat[pi] == s[si]) {
            pi += 1;
            si += 1;
            continue;
        }

        if (star_pi) |spi| {
            if (!star_slash and star_si < s.len and s[star_si] == std.fs.path.sep) return false;
            star_si += 1;
            if (star_si > s.len) return false;
            si = star_si;
            pi = spi;
            continue;
        }

        return false;
    }

    while (pi < pat.len) {
        if (pat[pi] == '*') {
            pi += 1;
            if (pi < pat.len and pat[pi] == '*') pi += 1;
            continue;
        }
        if (pat[pi] == '\\' and pi + 1 < pat.len) return false;
        break;
    }
    return pi == pat.len;
}

fn classEnd(pat: []const u8, start: usize) ?usize {
    if (start >= pat.len or pat[start] != '[') return null;
    var i = start + 1;
    if (i < pat.len and (pat[i] == '!' or pat[i] == '^')) i += 1;
    if (i < pat.len and pat[i] == ']') i += 1;
    while (i < pat.len) : (i += 1) {
        if (pat[i] == '\\' and i + 1 < pat.len) {
            i += 1;
            continue;
        }
        if (pat[i] == ']') return i;
    }
    return null;
}

fn classHas(body: []const u8, c: u8) bool {
    if (body.len == 0) return false;
    var i: usize = 0;
    var neg = false;
    if (body[0] == '!' or body[0] == '^') {
        neg = true;
        i = 1;
    }

    var ok = false;
    while (i < body.len) : (i += 1) {
        var a = body[i];
        if (a == '\\' and i + 1 < body.len) {
            i += 1;
            a = body[i];
        }

        if (i + 2 < body.len and body[i + 1] == '-') {
            var b = body[i + 2];
            if (b == '\\' and i + 3 < body.len) b = body[i + 3];
            if (a <= c and c <= b) ok = true;
        } else if (a == c) {
            ok = true;
        }
    }
    return if (neg) !ok else ok;
}

test "glob basics" {
    try std.testing.expect(matchPath("*.zig", "src/main.zig"));
    try std.testing.expect(!matchPath("*.zig", "src/main.c"));
    try std.testing.expect(match("src/**", "src/core/opt.zig"));
    try std.testing.expect(match("src/*/opt.zig", "src/core/opt.zig"));
    try std.testing.expect(!match("src/*/opt.zig", "src/core/x/opt.zig"));
}

test "glob classes" {
    try std.testing.expect(match("file[0-9].txt", "file7.txt"));
    try std.testing.expect(!match("file[0-9].txt", "filex.txt"));
    try std.testing.expect(match("file[!0-9].txt", "filex.txt"));
}
