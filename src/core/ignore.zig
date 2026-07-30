const std = @import("std");
const glob = @import("glob.zig");

/// Per-directory ignore files, in load order.
pub const names = [_][]const u8{ ".gitignore", ".rgignore", ".ignore" };
pub const Found = [names.len]bool;
pub const all_found: Found = .{true} ** names.len;

/// Index into `names` if `name` is an ignore file.
pub fn nameIndex(name: []const u8) ?usize {
    for (names, 0..) |n, i| {
        if (std.mem.eql(u8, name, n)) return i;
    }
    return null;
}

/// One directory's ignore rules, linked to the enclosing directory's node.
/// Immutable once built, so subtrees can be walked concurrently: a walker
/// only carries the leaf pointer, no shared mutable stack.
pub const Node = struct {
    parent: ?*const Node,
    base: []const u8,
    rules: []const Rule,

    pub fn ignored(node: ?*const Node, rel_path: []const u8, is_dir: bool, hidden: bool) bool {
        if (defaultSkip(rel_path)) return true;

        // Dotfiles start as "ignored" unless --hidden; gitignore negation can override
        const name = std.fs.path.basename(rel_path);
        const start: bool = !hidden and name.len > 0 and name[0] == '.';
        return apply(node, rel_path, is_dir, start);
    }

    /// Outermost rules first, last match wins (gitignore precedence). Recursion
    /// depth is the number of ancestors that actually have ignore files.
    fn apply(node: ?*const Node, rel_path: []const u8, is_dir: bool, start: bool) bool {
        const n = node orelse return start;
        var ignored_ = apply(n.parent, rel_path, is_dir, start);
        for (n.rules) |r| {
            if (r.dir_only and !is_dir) continue;
            const target = r.target(rel_path, n.base) orelse continue;
            if (!matchPat(r.pat, r.kind, target, r.anchored or r.has_slash)) continue;
            ignored_ = !r.neg;
        }
        return ignored_;
    }
};

/// Owns every Node for the run. Nodes are never freed individually: only
/// directories that actually contain rules allocate one, so a tree with 169
/// gitignores costs tens of KB.
pub const Set = struct {
    arena: std.heap.ArenaAllocator,
    mutex: std.Thread.Mutex = .{},

    pub fn init(allocator: std.mem.Allocator) Set {
        return .{ .arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Set) void {
        self.arena.deinit();
        self.* = undefined;
    }

    /// Returns the node covering `dir`, or `parent` when `dir` adds no rules.
    /// `found` tells which ignore files the caller saw in the directory listing,
    /// so we don't pay an openat() per missing one.
    pub fn push(self: *Set, parent: ?*const Node, dir: std.fs.Dir, base_rel: []const u8, found: Found) !?*const Node {
        var any = false;
        for (found) |f| any = any or f;
        if (!any) return parent;

        var rules: std.ArrayListUnmanaged(Rule) = .{};
        const scratch = self.arena.child_allocator;

        // Rules live in the arena, so hold the lock while parsing. Only dirs that
        // actually have ignore files get here, so contention is negligible.
        self.mutex.lock();
        defer self.mutex.unlock();
        const alloc = self.arena.allocator();

        for (names, found) |name, present| {
            if (present) try load(alloc, scratch, &rules, dir, name);
        }
        if (rules.items.len == 0) return parent;

        const node = try alloc.create(Node);
        node.* = .{
            .parent = parent,
            .base = try alloc.dupe(u8, base_rel),
            .rules = try rules.toOwnedSlice(alloc),
        };
        return node;
    }

    fn load(
        alloc: std.mem.Allocator,
        scratch: std.mem.Allocator,
        rules: *std.ArrayListUnmanaged(Rule),
        dir: std.fs.Dir,
        name: []const u8,
    ) !void {
        const data = dir.readFileAlloc(scratch, name, 1 << 20) catch |e| switch (e) {
            error.FileNotFound => return,
            else => return e,
        };
        defer scratch.free(data);

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

            try rules.append(alloc, .{
                .pat = try alloc.dupe(u8, line),
                .neg = neg,
                .dir_only = dir_only,
                .anchored = anchored,
                .has_slash = std.mem.indexOfScalar(u8, line, std.fs.path.sep) != null,
                .kind = glob.classify(line),
            });
        }
    }
};

const Rule = struct {
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

test "chain honors per-dir ignore" {
    var td = std.testing.tmpDir(.{});
    defer td.cleanup();

    try td.dir.makeDir("a");
    try td.dir.makeDir("a/b");
    try td.dir.writeFile(.{ .sub_path = ".gitignore", .data = "*.log\n" });
    try td.dir.writeFile(.{ .sub_path = "a/.gitignore", .data = "!keep.log\n" });

    var set = Set.init(std.testing.allocator);
    defer set.deinit();
    const root = try set.push(null, td.dir, "", all_found);

    try std.testing.expect(Node.ignored(root, "x.log", false, true));
    try std.testing.expect(Node.ignored(root, "a/x.log", false, true));

    var a = try td.dir.openDir("a", .{ .iterate = true });
    defer a.close();
    const node_a = try set.push(root, a, "a", all_found);

    try std.testing.expect(!Node.ignored(node_a, "a/keep.log", false, true));
    try std.testing.expect(Node.ignored(node_a, "a/nope.log", false, true));

    // A dir without ignore files must not allocate a node
    var b = try a.openDir("b", .{ .iterate = true });
    defer b.close();
    try std.testing.expectEqual(node_a, try set.push(node_a, b, "a/b", .{false} ** names.len));
}

