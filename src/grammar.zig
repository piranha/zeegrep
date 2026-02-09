const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("tree_sitter/api.h");
});
const dl = @cImport({
    @cInclude("dlfcn.h");
});

pub const TSLanguage = c.TSLanguage;
pub const TSParser = c.TSParser;
pub const TSTree = c.TSTree;
pub const TSNode = c.TSNode;
pub const TSPoint = c.TSPoint;

const release_version = "0.13.22";
const release_base = "https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/" ++ release_version;

const so_ext = switch (builtin.os.tag) {
    .macos => "dylib",
    .windows => "dll",
    else => "so",
};

const archive_triple = switch (builtin.os.tag) {
    .macos => switch (builtin.cpu.arch) {
        .aarch64 => "aarch64-apple-darwin",
        .x86_64 => "x86_64-apple-darwin",
        else => @compileError("unsupported arch"),
    },
    .linux => switch (builtin.cpu.arch) {
        .aarch64 => "aarch64-unknown-linux-gnu",
        .x86_64 => "x86_64-unknown-linux-gnu",
        else => @compileError("unsupported arch"),
    },
    .windows => "x86_64-pc-windows-msvc",
    else => @compileError("unsupported os"),
};

// Supported languages: name -> C symbol
const LangEntry = struct { []const u8, []const u8 };
const languages = [_]LangEntry{
    .{ "python", "tree_sitter_python" },
    .{ "javascript", "tree_sitter_javascript" },
    .{ "typescript", "tree_sitter_typescript" },
    .{ "tsx", "tree_sitter_tsx" },
    .{ "rust", "tree_sitter_rust" },
    .{ "go", "tree_sitter_go" },
    .{ "c", "tree_sitter_c" },
    .{ "cpp", "tree_sitter_cpp" },
    .{ "java", "tree_sitter_java" },
    .{ "ruby", "tree_sitter_ruby" },
    .{ "clojure", "tree_sitter_clojure" },
    .{ "zig", "tree_sitter_zig" },
    .{ "bash", "tree_sitter_bash" },
    .{ "json", "tree_sitter_json" },
    .{ "toml", "tree_sitter_toml" },
    .{ "yaml", "tree_sitter_yaml" },
    .{ "html", "tree_sitter_html" },
    .{ "css", "tree_sitter_css" },
    .{ "lua", "tree_sitter_lua" },
    .{ "elixir", "tree_sitter_elixir" },
    .{ "haskell", "tree_sitter_haskell" },
    .{ "scala", "tree_sitter_scala" },
    .{ "swift", "tree_sitter_swift" },
    .{ "kotlin", "tree_sitter_kotlin" },
    .{ "php", "tree_sitter_php" },
};

// Extension -> language name mapping
pub fn langFromExt(ext: []const u8) ?[]const u8 {
    const map = .{
        .{ ".py", "python" },
        .{ ".pyi", "python" },
        .{ ".js", "javascript" },
        .{ ".mjs", "javascript" },
        .{ ".cjs", "javascript" },
        .{ ".jsx", "javascript" },
        .{ ".ts", "typescript" },
        .{ ".tsx", "tsx" },
        .{ ".rs", "rust" },
        .{ ".go", "go" },
        .{ ".c", "c" },
        .{ ".h", "c" },
        .{ ".cpp", "cpp" },
        .{ ".cc", "cpp" },
        .{ ".cxx", "cpp" },
        .{ ".hpp", "cpp" },
        .{ ".hh", "cpp" },
        .{ ".java", "java" },
        .{ ".rb", "ruby" },
        .{ ".clj", "clojure" },
        .{ ".cljs", "clojure" },
        .{ ".cljc", "clojure" },
        .{ ".zig", "zig" },
        .{ ".sh", "bash" },
        .{ ".bash", "bash" },
        .{ ".json", "json" },
        .{ ".toml", "toml" },
        .{ ".yaml", "yaml" },
        .{ ".yml", "yaml" },
        .{ ".html", "html" },
        .{ ".htm", "html" },
        .{ ".css", "css" },
        .{ ".lua", "lua" },
        .{ ".ex", "elixir" },
        .{ ".exs", "elixir" },
        .{ ".hs", "haskell" },
        .{ ".scala", "scala" },
        .{ ".sc", "scala" },
        .{ ".swift", "swift" },
        .{ ".kt", "kotlin" },
        .{ ".kts", "kotlin" },
        .{ ".php", "php" },
    };
    inline for (map) |entry| {
        if (std.mem.eql(u8, ext, entry[0])) return entry[1];
    }
    return null;
}

fn findSymbol(name: []const u8) ?[]const u8 {
    for (&languages) |*entry| {
        if (std.mem.eql(u8, entry[0], name)) return entry[1];
    }
    return null;
}

/// Get the grammar cache directory
pub fn getCacheDir(allocator: std.mem.Allocator, override: ?[]const u8) ![]const u8 {
    if (override) |dir| return allocator.dupe(u8, dir);
    if (std.process.getEnvVarOwned(allocator, "XDG_CACHE_HOME")) |xdg| {
        defer allocator.free(xdg);
        return std.fmt.allocPrint(allocator, "{s}/zeegrep/grammars", .{xdg});
    } else |_| {}
    if (std.process.getEnvVarOwned(allocator, "HOME")) |home| {
        defer allocator.free(home);
        return std.fmt.allocPrint(allocator, "{s}/.cache/zeegrep/grammars", .{home});
    } else |_| {}
    return allocator.dupe(u8, "/tmp/zeegrep/grammars");
}

/// Download a single grammar .dylib/.so from emacs-tree-sitter releases
fn downloadGrammar(allocator: std.mem.Allocator, lang_name: []const u8, cache_dir: []const u8) !void {
    std.fs.cwd().makePath(cache_dir) catch {};

    const url = try std.fmt.allocPrint(allocator,
        "{s}/tree-sitter-grammars.{s}.v{s}.tar.gz",
        .{ release_base, archive_triple, release_version },
    );
    defer allocator.free(url);

    const lib_name = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ lang_name, so_ext });
    defer allocator.free(lib_name);

    const dest = try std.fmt.allocPrint(allocator, "{s}/{s}", .{ cache_dir, lib_name });
    defer allocator.free(dest);

    std.debug.print("Downloading {s} grammar...\n", .{lang_name});

    // curl | tar to extract just the one file we need
    const tar_filter = try allocator.dupeZ(u8, lib_name);
    defer allocator.free(tar_filter);
    const url_z = try allocator.dupeZ(u8, url);
    defer allocator.free(url_z);
    const cache_z = try allocator.dupeZ(u8, cache_dir);
    defer allocator.free(cache_z);

    // Use: curl -fsSL <url> | tar xzf - -C <cache_dir> <lib_name>
    const script = try std.fmt.allocPrint(allocator,
        "curl -fsSL '{s}' | tar xzf - -C '{s}' '{s}'",
        .{ url, cache_dir, lib_name },
    );
    defer allocator.free(script);
    const script_z = try allocator.dupeZ(u8, script);
    defer allocator.free(script_z);

    const argv = [_][]const u8{ "sh", "-c", script };
    var child = std.process.Child.init(&argv, allocator);
    child.stderr_behavior = .Pipe;
    child.stdout_behavior = .Ignore;
    try child.spawn();
    const term = try child.wait();
    switch (term) {
        .Exited => |code| if (code != 0) return error.DownloadFailed,
        else => return error.DownloadFailed,
    }
}

/// Fetch grammar for --fetch command (just download, don't load)
pub fn fetchGrammar(allocator: std.mem.Allocator, lang_name: []const u8, cache_dir: []const u8) !void {
    if (findSymbol(lang_name) == null) return error.UnknownLanguage;
    try downloadGrammar(allocator, lang_name, cache_dir);
}

/// Cached grammar pointers — survives for process lifetime (languages leak by design)
var grammar_cache: std.StringHashMapUnmanaged(*const TSLanguage) = .{};
var grammar_cache_mu: std.Thread.Mutex = .{};

/// Load a grammar: return cached if available, else download if needed, dlopen, dlsym
pub fn loadGrammar(
    allocator: std.mem.Allocator,
    lang_name: []const u8,
    cache_dir: []const u8,
) !*const TSLanguage {
    grammar_cache_mu.lock();
    if (grammar_cache.get(lang_name)) |cached| {
        grammar_cache_mu.unlock();
        return cached;
    }
    grammar_cache_mu.unlock();

    const symbol = findSymbol(lang_name) orelse return error.UnknownLanguage;

    const lib_path = try std.fmt.allocPrint(allocator, "{s}/{s}.{s}", .{ cache_dir, lang_name, so_ext });
    defer allocator.free(lib_path);

    // Download if not cached on disk
    std.fs.cwd().access(lib_path, .{}) catch {
        try downloadGrammar(allocator, lang_name, cache_dir);
    };

    const lib_path_z = try allocator.dupeZ(u8, lib_path);
    defer allocator.free(lib_path_z);
    const handle = dl.dlopen(lib_path_z.ptr, dl.RTLD_NOW) orelse return error.DlopenFailed;
    // Intentionally leak — language code must stay loaded

    const sym_z = try allocator.dupeZ(u8, symbol);
    defer allocator.free(sym_z);
    const sym = dl.dlsym(handle, sym_z.ptr) orelse return error.SymbolNotFound;

    const LangFn = *const fn () callconv(.c) *const TSLanguage;
    const lang_fn: LangFn = @ptrCast(@alignCast(sym));
    const lang = lang_fn();

    // Cache it — dupe key since lang_name may be caller-owned
    grammar_cache_mu.lock();
    defer grammar_cache_mu.unlock();
    const key = allocator.dupe(u8, lang_name) catch return lang;
    grammar_cache.put(allocator, key, lang) catch {};

    return lang;
}

// Tree-sitter parser wrapper
pub const Parser = struct {
    parser: *TSParser,

    pub fn init() !Parser {
        const p = c.ts_parser_new() orelse return error.ParserInitFailed;
        return .{ .parser = p };
    }

    pub fn deinit(self: *Parser) void {
        c.ts_parser_delete(self.parser);
    }

    pub fn setLanguage(self: *Parser, lang: *const TSLanguage) !void {
        if (!c.ts_parser_set_language(self.parser, lang)) return error.SetLanguageFailed;
    }

    pub fn parseString(self: *Parser, source: []const u8) !*TSTree {
        return c.ts_parser_parse_string(
            self.parser,
            null,
            source.ptr,
            @intCast(source.len),
        ) orelse return error.ParseFailed;
    }
};

pub fn treeDelete(tree: *TSTree) void {
    c.ts_tree_delete(tree);
}

pub fn rootNode(tree: *TSTree) TSNode {
    return c.ts_tree_root_node(tree);
}

pub fn nodeType(node: TSNode) []const u8 {
    const t = c.ts_node_type(node);
    if (t == null) return "";
    return std.mem.span(t);
}

pub fn nodeParent(node: TSNode) TSNode {
    return c.ts_node_parent(node);
}

pub fn nodeIsNull(node: TSNode) bool {
    return c.ts_node_is_null(node);
}

pub fn nodeStartByte(node: TSNode) u32 {
    return c.ts_node_start_byte(node);
}

pub fn nodeEndByte(node: TSNode) u32 {
    return c.ts_node_end_byte(node);
}

pub fn nodeStartPoint(node: TSNode) TSPoint {
    return c.ts_node_start_point(node);
}

pub fn nodeIsNamed(node: TSNode) bool {
    return c.ts_node_is_named(node);
}

pub fn nodeEndPoint(node: TSNode) TSPoint {
    return c.ts_node_end_point(node);
}

pub fn nodeDescendant(node: TSNode, start: u32, end: u32) TSNode {
    return c.ts_node_descendant_for_byte_range(node, start, end);
}

pub fn nodeChildCount(node: TSNode) u32 {
    return c.ts_node_child_count(node);
}

pub fn nodeChild(node: TSNode, index: u32) TSNode {
    return c.ts_node_child(node, index);
}

/// List available languages and their cache status
pub fn listLanguages(_: std.mem.Allocator, writer: anytype, cache_dir: []const u8) !void {
    try writer.writeAll("Available languages:\n\n");
    var check_buf: [1024]u8 = undefined;
    for (&languages) |*entry| {
        const name = entry[0];
        const filepath = std.fmt.bufPrint(&check_buf, "{s}/{s}.{s}", .{ cache_dir, name, so_ext }) catch continue;
        const cached = if (std.fs.cwd().access(filepath, .{})) |_| true else |_| false;
        const status: []const u8 = if (cached) " [cached]" else "";
        try writer.print("  {s}{s}\n", .{ name, status });
    }
}
