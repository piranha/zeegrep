const std = @import("std");

pub const Ignorer = struct {
    allocator: std.mem.Allocator,
    pats: std.ArrayListUnmanaged(Pat) = .{},

    pub fn init(allocator: std.mem.Allocator, dir: std.fs.Dir) !Ignorer {
        var self: Ignorer = .{ .allocator = allocator };
        errdefer self.deinit();
        try self.load(dir, ".gitignore");
        try self.load(dir, ".ignore");
        return self;
    }

    pub fn deinit(self: *Ignorer) void {
        for (self.pats.items) |p| self.allocator.free(p.s);
        self.pats.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn match(self: *const Ignorer, rel_path: []const u8, is_dir: bool) bool {
        if (defaultSkip(rel_path)) return true;
        var ignored = false;
        for (self.pats.items) |p| {
            if (!p.matches(rel_path, is_dir)) continue;
            ignored = !p.neg;
        }
        return ignored;
    }

    fn load(self: *Ignorer, dir: std.fs.Dir, name: []const u8) !void {
        const data = dir.readFileAlloc(self.allocator, name, 1 << 20) catch |e| switch (e) {
            error.FileNotFound => return,
            else => return e,
        };
        defer self.allocator.free(data);

        var it = std.mem.splitScalar(u8, data, '\n');
        while (it.next()) |raw| {
            const line = std.mem.trim(u8, raw, " \t\r");
            if (line.len == 0) continue;
            if (line[0] == '#') continue;

            var neg = false;
            var s = line;
            if (s[0] == '!') {
                neg = true;
                s = std.mem.trimLeft(u8, s[1..], " \t");
                if (s.len == 0) continue;
            }
            try self.pats.append(self.allocator, .{ .s = try self.allocator.dupe(u8, s), .neg = neg });
        }
    }
};

const Pat = struct {
    s: []const u8,
    neg: bool = false,

    fn matches(self: Pat, rel_path: []const u8, is_dir: bool) bool {
        _ = is_dir;
        var pat = self.s;
        var anchored = false;
        if (pat.len > 0 and pat[0] == '/') {
            anchored = true;
            pat = pat[1..];
        }

        const dir_only = pat.len > 0 and pat[pat.len - 1] == '/';
        if (dir_only) pat = pat[0 .. pat.len - 1];

        if (pat.len == 0) return false;

        if (hasGlob(pat)) {
            if (anchored) return globMatch(pat, rel_path);
            return globMatchAny(pat, rel_path);
        }

        if (dir_only) {
            if (anchored) {
                if (std.mem.eql(u8, rel_path, pat)) return true;
                return rel_path.len > pat.len and std.mem.startsWith(u8, rel_path, pat) and rel_path[pat.len] == std.fs.path.sep;
            }
            return hasPathComponent(rel_path, pat);
        }

        if (anchored) return std.mem.startsWith(u8, rel_path, pat);
        return std.mem.indexOf(u8, rel_path, pat) != null;
    }
};

fn hasPathComponent(rel_path: []const u8, name: []const u8) bool {
    if (std.mem.eql(u8, rel_path, name)) return true;
    var i: usize = 0;
    while (i < rel_path.len) {
        const start = i;
        while (i < rel_path.len and rel_path[i] != std.fs.path.sep) i += 1;
        if (std.mem.eql(u8, rel_path[start..i], name)) return true;
        i += 1;
    }
    return false;
}

fn hasGlob(pat: []const u8) bool {
    return std.mem.indexOfAny(u8, pat, "*?") != null;
}

fn globMatchAny(pat: []const u8, rel_path: []const u8) bool {
    if (globMatch(pat, rel_path)) return true;
    var i: usize = 0;
    while (i < rel_path.len) : (i += 1) {
        if (rel_path[i] == std.fs.path.sep) {
            const rest = rel_path[i + 1 ..];
            if (globMatch(pat, rest)) return true;
        }
    }
    return false;
}

fn globMatch(pat: []const u8, s: []const u8) bool {
    var pi: usize = 0;
    var si: usize = 0;
    var star_pi: ?usize = null;
    var star_si: usize = 0;

    while (si < s.len) {
        if (pi < pat.len and (pat[pi] == '?' or pat[pi] == s[si])) {
            pi += 1;
            si += 1;
            continue;
        }
        if (pi < pat.len and pat[pi] == '*') {
            star_pi = pi;
            pi += 1;
            star_si = si;
            continue;
        }
        if (star_pi) |spi| {
            pi = spi + 1;
            star_si += 1;
            si = star_si;
            continue;
        }
        return false;
    }

    while (pi < pat.len and pat[pi] == '*') pi += 1;
    return pi == pat.len;
}

fn defaultSkip(rel_path: []const u8) bool {
    if (std.mem.startsWith(u8, rel_path, ".git")) return true;
    if (std.mem.startsWith(u8, rel_path, ".zig-cache")) return true;
    if (std.mem.startsWith(u8, rel_path, ".zig-global-cache")) return true;
    if (std.mem.startsWith(u8, rel_path, "zig-out")) return true;
    return std.mem.indexOf(u8, rel_path, "/.git/") != null or
        std.mem.indexOf(u8, rel_path, "/.zig-cache/") != null or
        std.mem.indexOf(u8, rel_path, "/.zig-global-cache/") != null or
        std.mem.indexOf(u8, rel_path, "/zig-out/") != null;
}

test "ignore basic patterns" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.writeFile(.{ .sub_path = ".gitignore", .data = "*.log\n!keep.log\nnode_modules/\n" });

    var ign = try Ignorer.init(std.testing.allocator, td.dir);
    defer ign.deinit();

    try std.testing.expect(ign.match("a.log", false));
    try std.testing.expect(!ign.match("keep.log", false));
    try std.testing.expect(ign.match("node_modules", true));
    try std.testing.expect(ign.match("node_modules/x.js", false));
}
