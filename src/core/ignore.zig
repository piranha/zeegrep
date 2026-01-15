const std = @import("std");
const glob = @import("glob.zig");

pub const Stack = struct {
    allocator: std.mem.Allocator,
    frames: std.ArrayListUnmanaged(Frame) = .{},
    rules: std.ArrayListUnmanaged(Rule) = .{},

    pub fn init(allocator: std.mem.Allocator) Stack {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Stack) void {
        while (self.frames.items.len > 0) self.popDir();
        self.frames.deinit(self.allocator);
        self.rules.deinit(self.allocator);
        self.* = undefined;
    }

    pub fn pushDir(self: *Stack, dir: std.fs.Dir, base_rel: []const u8) !void {
        const base = try self.allocator.dupe(u8, base_rel);
        errdefer self.allocator.free(base);

        const frame: Frame = .{ .base = base, .rules_start = self.rules.items.len };
        try self.frames.append(self.allocator, frame);
        errdefer {
            _ = self.frames.pop();
            self.allocator.free(base);
        }

        try self.load(dir, ".gitignore");
        try self.load(dir, ".rgignore");
        try self.load(dir, ".ignore");
    }

    pub fn popDir(self: *Stack) void {
        const frame = self.frames.pop().?;
        while (self.rules.items.len > frame.rules_start) {
            const r = self.rules.pop().?;
            self.allocator.free(r.pat);
        }
        self.allocator.free(frame.base);
    }

    pub fn ignored(self: *const Stack, rel_path: []const u8, is_dir: bool, hidden: bool) bool {
        if (defaultSkip(rel_path)) return true;

        // Dotfiles start as "ignored" unless --hidden; gitignore negation can override
        const name = std.fs.path.basename(rel_path);
        var ignored_: bool = !hidden and name.len > 0 and name[0] == '.';

        for (self.rules.items) |r| {
            if (r.dir_only and !is_dir) continue;

            const base = self.frames.items[r.frame].base;
            const target = r.target(rel_path, base) orelse continue;

            if (!matchPat(r.pat, r.kind, target, r.anchored or r.has_slash)) continue;
            ignored_ = !r.neg;
        }
        return ignored_;
    }

    fn load(self: *Stack, dir: std.fs.Dir, name: []const u8) !void {
        const data = dir.readFileAlloc(self.allocator, name, 1 << 20) catch |e| switch (e) {
            error.FileNotFound => return,
            else => return e,
        };
        defer self.allocator.free(data);

        var it = std.mem.splitScalar(u8, data, '\n');
        while (it.next()) |raw| {
            var line = std.mem.trim(u8, raw, " \t\r");
            if (line.len == 0) continue;

            if (line[0] == '\\' and line.len >= 2 and (line[1] == '#' or line[1] == '!')) {
                line = line[1..];
            } else if (line[0] == '#') continue;

            var neg = false;
            if (line[0] == '!') {
                neg = true;
                line = line[1..];
                if (line.len == 0) continue;
            }

            var anchored = false;
            if (line[0] == '/') {
                anchored = true;
                line = line[1..];
                if (line.len == 0) continue;
            }

            var dir_only = false;
            if (line.len > 0 and line[line.len - 1] == '/') {
                dir_only = true;
                line = line[0 .. line.len - 1];
                if (line.len == 0) continue;
            }

            try self.rules.append(self.allocator, .{
                .frame = self.frames.items.len - 1,
                .pat = try self.allocator.dupe(u8, line),
                .neg = neg,
                .dir_only = dir_only,
                .anchored = anchored,
                .has_slash = std.mem.indexOfScalar(u8, line, std.fs.path.sep) != null,
                .kind = glob.classify(line),
            });
        }
    }
};

const Frame = struct {
    base: []const u8,
    rules_start: usize,
};

const Rule = struct {
    frame: usize,
    pat: []const u8,
    neg: bool,
    dir_only: bool,
    anchored: bool,
    has_slash: bool,
    kind: glob.PatKind,

    fn target(self: Rule, rel_path: []const u8, base: []const u8) ?[]const u8 {
        _ = self;
        if (base.len == 0) return if (std.mem.startsWith(u8, rel_path, "./")) rel_path[2..] else rel_path;
        if (std.mem.eql(u8, rel_path, base)) return "";
        if (rel_path.len <= base.len + 1) return null;
        if (!std.mem.startsWith(u8, rel_path, base)) return null;
        if (rel_path[base.len] != std.fs.path.sep) return null;
        return rel_path[base.len + 1 ..];
    }
};

fn matchPat(pat: []const u8, kind: glob.PatKind, target: []const u8, full_path: bool) bool {
    if (!full_path and std.mem.indexOfScalar(u8, pat, std.fs.path.sep) == null) {
        const base = std.fs.path.basename(target);
        return glob.fastMatch(kind, pat, base);
    }
    return glob.fastMatch(kind, pat, target);
}

fn defaultSkip(rel_path: []const u8) bool {
    // Only skip VCS internals - everything else via ignore files
    const vcs = .{ ".git", ".hg", ".svn" };
    inline for (vcs) |v| {
        if (std.mem.startsWith(u8, rel_path, v)) return true;
        if (std.mem.indexOf(u8, rel_path, "/" ++ v ++ "/") != null) return true;
    }
    return false;
}

test "stack honors per-dir ignore" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("a");
    try td.dir.makeDir("a/b");
    try td.dir.writeFile(.{ .sub_path = ".gitignore", .data = "*.log\n" });
    try td.dir.writeFile(.{ .sub_path = "a/.gitignore", .data = "!keep.log\n" });

    var st = Stack.init(std.testing.allocator);
    defer st.deinit();
    try st.pushDir(td.dir, "");

    try std.testing.expect(st.ignored("x.log", false, true));
    try std.testing.expect(st.ignored("a/x.log", false, true));

    var a = try td.dir.openDir("a", .{ .iterate = true });
    defer a.close();
    try st.pushDir(a, "a");
    defer st.popDir();

    try std.testing.expect(!st.ignored("a/keep.log", false, true));
    try std.testing.expect(st.ignored("a/nope.log", false, true));
}

