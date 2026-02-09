const std = @import("std");
const grammar = @import("grammar.zig");

/// Walk up from a node to find the enclosing structural block.
/// Prefers the closest multi-line named ancestor strictly larger than
/// the match. Falls back to the closest single-line ancestor if no
/// multi-line one exists (e.g. one-liner functions).
/// Never returns the root node (whole file).
pub fn findEnclosingBlock(node: grammar.TSNode, match_start: u32, match_end: u32) ?grammar.TSNode {
    var current = grammar.nodeParent(node);
    var first_larger: ?grammar.TSNode = null;
    while (!grammar.nodeIsNull(current)) {
        const is_root = grammar.nodeIsNull(grammar.nodeParent(current));
        if (!is_root and grammar.nodeIsNamed(current) and grammar.nodeEndByte(current) >= match_end) {
            const strictly_larger = grammar.nodeStartByte(current) < match_start or
                grammar.nodeEndByte(current) > match_end;
            if (strictly_larger) {
                if (first_larger == null) first_larger = current;
                // Multi-line: this is the ideal block
                if (grammar.nodeStartPoint(current).row < grammar.nodeEndPoint(current).row)
                    return current;
            }
        }
        current = grammar.nodeParent(current);
    }
    return first_larger;
}

/// A resolved block: byte range in the source file
pub const Block = struct {
    start_byte: u32,
    end_byte: u32,
    start_line: u32,
    node_type: []const u8,
};

/// Given a match at a byte offset, find the enclosing structural block.
/// Anchors on the leaf at match_start, then walks up to first named ancestor
/// that also covers match_end.
pub fn resolveBlock(
    tree: *grammar.TSTree,
    match_start: u32,
    match_end: u32,
) ?Block {
    const root = grammar.rootNode(tree);
    const leaf = grammar.nodeDescendant(root, match_start, match_start);
    if (grammar.nodeIsNull(leaf)) return null;

    const block_node = findEnclosingBlock(leaf, match_start, match_end) orelse return null;
    const start_point = grammar.nodeStartPoint(block_node);

    return .{
        .start_byte = grammar.nodeStartByte(block_node),
        .end_byte = grammar.nodeEndByte(block_node),
        .start_line = start_point.row,
        .node_type = grammar.nodeType(block_node),
    };
}

/// Resolve all blocks for matches in a file, deduplicating overlapping blocks.
/// Returns blocks sorted by start_byte.
pub fn resolveBlocks(
    allocator: std.mem.Allocator,
    tree: *grammar.TSTree,
    match_offsets: []const [2]u32,
) ![]Block {
    var seen: std.AutoHashMapUnmanaged([2]u32, void) = .{};
    defer seen.deinit(allocator);
    var blocks: std.ArrayListUnmanaged(Block) = .{};
    errdefer blocks.deinit(allocator);

    for (match_offsets) |offsets| {
        if (resolveBlock(tree, offsets[0], offsets[1])) |block| {
            const key = .{ block.start_byte, block.end_byte };
            const entry = try seen.getOrPut(allocator, key);
            if (!entry.found_existing)
                try blocks.append(allocator, block);
        }
    }

    std.mem.sort(Block, blocks.items, {}, struct {
        fn lt(_: void, a: Block, b: Block) bool {
            return a.start_byte < b.start_byte;
        }
    }.lt);

    return blocks.toOwnedSlice(allocator);
}
