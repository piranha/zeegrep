//! Comptime Transducers for Zig
//!
//! A transducer is a composable transformation that doesn't know about its source or sink.
//! In Clojure: (fn [rf] (fn [acc x] ...))
//! In Zig: comptime type that wraps a Downstream type and forwards transformed items.
//!
//! Protocol:
//!   step(item) - process one item, may call downstream 0, 1, or N times
//!   finish()   - signal completion, flush any buffered state
//!
//! Errors propagate up immediately (pipeline halts on first error).
//!
//! ## Backpressure
//!
//! Like Clojure transducers, backpressure is NOT built into the protocol.
//! This is push-based: upstream calls downstream.step() as fast as it can.
//! For zeegrep's use case (buffered stdout), this is fine.
//!
//! If you need backpressure:
//! 1. Use bounded channels between stages (complex, needs async)
//! 2. Use a Buffer transducer that errors when full
//! 3. Process in batches where batch size controls throughput
//!
//! ## Example: zeegrep pipeline
//!
//! ```zig
//! // Conceptually, zeegrep's pipeline is:
//! //   walk(dir) -> paths -> filter(not_binary) -> mmap -> matches -> format -> stdout
//! //
//! // With transducers:
//! const Pipeline = Compose5(
//!     FilterBinary,           // skip binary files
//!     FlatMap(findMatches),   // path -> []Match
//!     TrackLineNumbers,       // attach line numbers
//!     AddContext(3),          // before/after lines
//!     FormatMatch,            // -> []u8
//! );
//!
//! // Parallel variant:
//! const ParallelPipeline = Compose(
//!     Parallel(3, Compose(FilterBinary, FlatMap(findMatches)), Path, Match),
//!     Compose(TrackLineNumbers, Compose(AddContext(3), FormatMatch)),
//! );
//! ```

const std = @import("std");

// ============================================================================
// Core Transducers
// ============================================================================

/// Map: transform each item
pub fn Map(comptime f: anytype) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    try self.downstream.step(f(item));
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Filter: keep items matching predicate
pub fn Filter(comptime pred: anytype) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    if (pred(item)) {
                        try self.downstream.step(item);
                    }
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Take: pass through first N items, then halt
pub fn Take(comptime n: usize) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                count: usize = 0,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    if (self.count >= n) {
                        return error.TransducerHalt;
                    }
                    self.count += 1;
                    try self.downstream.step(item);
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Dedupe: skip consecutive duplicates
pub fn Dedupe(comptime T: type) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                last: ?T = null,

                const Self = @This();

                pub fn step(self: *Self, item: T) !void {
                    if (self.last == null or self.last.? != item) {
                        self.last = item;
                        try self.downstream.step(item);
                    }
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// FlatMap: for each item, emit multiple items
pub fn FlatMap(comptime f: anytype) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    const items = f(item);
                    for (items) |sub_item| {
                        try self.downstream.step(sub_item);
                    }
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Batch: collect N items, emit as slice
pub fn Batch(comptime T: type, comptime n: usize) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                buffer: [n]T = undefined,
                len: usize = 0,

                const Self = @This();

                pub fn step(self: *Self, item: T) !void {
                    self.buffer[self.len] = item;
                    self.len += 1;
                    if (self.len == n) {
                        try self.downstream.step(self.buffer[0..n]);
                        self.len = 0;
                    }
                }

                pub fn finish(self: *Self) !void {
                    if (self.len > 0) {
                        try self.downstream.step(self.buffer[0..self.len]);
                    }
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

// ============================================================================
// Composition
// ============================================================================

/// Compose two transducers: A then B
/// Items flow: source -> A -> B -> sink
pub fn Compose(comptime A: type, comptime B: type) type {
    return struct {
        /// Get the composed result type
        pub fn apply(comptime Downstream: type) type {
            return A.apply(B.apply(Downstream));
        }

        /// Build the composed chain from a sink value
        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            // B wraps sink, A wraps that
            const b_wrapped = B.wrap(Sink, sink);
            return A.wrap(@TypeOf(b_wrapped), b_wrapped);
        }
    };
}

/// Compose 3 transducers
pub fn Compose3(comptime A: type, comptime B: type, comptime C: type) type {
    return Compose(A, Compose(B, C));
}

/// Compose 4 transducers
pub fn Compose4(comptime A: type, comptime B: type, comptime C: type, comptime D: type) type {
    return Compose(A, Compose3(B, C, D));
}

/// Compose 5 transducers
pub fn Compose5(comptime A: type, comptime B: type, comptime C: type, comptime D: type, comptime E: type) type {
    return Compose(A, Compose4(B, C, D, E));
}

/// Identity transducer - passes through unchanged
pub const Identity = struct {
    pub fn apply(comptime Downstream: type) type {
        return Downstream;
    }

    pub fn wrap(comptime Sink: type, sink: Sink) Sink {
        return sink;
    }
};

// ============================================================================
// Parallel Transducer
// ============================================================================

/// Parallel: process items concurrently, collect results
/// This is a "higher-order" transducer - it takes another transducer as parameter.
///
/// Items are distributed to N workers, each running the inner transducer.
/// Results are collected and forwarded to downstream.
///
/// Note: Order is NOT preserved. Use for embarrassingly parallel workloads.
pub fn Parallel(
    comptime n_workers: usize,
    comptime InnerXf: type,
    comptime Item: type,
    comptime Result: type,
) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                allocator: std.mem.Allocator,

                const Self = @This();

                // Worker collects results into a thread-local buffer
                const WorkerSink = struct {
                    results: *std.ArrayList(Result),

                    pub fn step(self: *WorkerSink, item: Result) !void {
                        try self.results.append(item);
                    }

                    pub fn finish(_: *WorkerSink) !void {}
                };

                pub fn step(self: *Self, items: []const Item) !void {
                    // Simple parallel: divide work among threads
                    var threads: [n_workers]?std.Thread = [_]?std.Thread{null} ** n_workers;
                    var results: [n_workers]std.ArrayList(Result) = undefined;
                    var errors: [n_workers]?anyerror = [_]?anyerror{null} ** n_workers;

                    // Init result buffers
                    for (&results) |*r| {
                        r.* = std.ArrayList(Result).init(self.allocator);
                    }
                    defer for (&results) |*r| r.deinit();

                    // Spawn workers
                    const chunk_size = (items.len + n_workers - 1) / n_workers;
                    for (0..n_workers) |i| {
                        const start = i * chunk_size;
                        if (start >= items.len) break;
                        const end = @min(start + chunk_size, items.len);
                        const chunk = items[start..end];

                        threads[i] = std.Thread.spawn(.{}, workerFn, .{
                            chunk,
                            &results[i],
                            &errors[i],
                        }) catch |err| {
                            errors[i] = err;
                            continue;
                        };
                    }

                    // Join and collect
                    for (threads, 0..) |maybe_thread, i| {
                        if (maybe_thread) |thread| {
                            thread.join();
                        }
                        if (errors[i]) |err| return err;

                        // Forward results to downstream
                        for (results[i].items) |result| {
                            try self.downstream.step(result);
                        }
                    }
                }

                fn workerFn(
                    items: []const Item,
                    result_list: *std.ArrayList(Result),
                    err_out: *?anyerror,
                ) void {
                    const sink = WorkerSink{ .results = result_list };
                    var reducer = InnerXf.wrap(WorkerSink, sink);

                    for (items) |item| {
                        reducer.step(item) catch |e| {
                            if (e != error.TransducerHalt) {
                                err_out.* = e;
                            }
                            return;
                        };
                    }
                    reducer.finish() catch |e| {
                        err_out.* = e;
                    };
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink, allocator: std.mem.Allocator) apply(Sink) {
            return .{ .downstream = sink, .allocator = allocator };
        }
    };
}

// ============================================================================
// Stateful Transducers (need runtime init)
// ============================================================================

/// Enumerate: attach index to each item
pub fn Enumerate(comptime T: type) type {
    return struct {
        pub const Item = struct { index: usize, value: T };

        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                index: usize = 0,

                const Self = @This();

                pub fn step(self: *Self, item: T) !void {
                    try self.downstream.step(Item{ .index = self.index, .value = item });
                    self.index += 1;
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Scan: like fold but emits intermediate results
pub fn Scan(comptime T: type, comptime f: anytype, comptime init: T) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                acc: T = init,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    self.acc = f(self.acc, item);
                    try self.downstream.step(self.acc);
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// TakeWhile: pass items while predicate is true, then halt
pub fn TakeWhile(comptime pred: anytype) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    if (!pred(item)) {
                        return error.TransducerHalt;
                    }
                    try self.downstream.step(item);
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// DropWhile: skip items while predicate is true, then pass all
pub fn DropWhile(comptime pred: anytype) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                dropping: bool = true,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    if (self.dropping) {
                        if (pred(item)) return;
                        self.dropping = false;
                    }
                    try self.downstream.step(item);
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Drop: skip first N items
pub fn Drop(comptime n: usize) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                count: usize = 0,

                const Self = @This();

                pub fn step(self: *Self, item: anytype) !void {
                    if (self.count < n) {
                        self.count += 1;
                        return;
                    }
                    try self.downstream.step(item);
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Interpose: insert separator between items
pub fn Interpose(comptime T: type, comptime sep: T) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                first: bool = true,

                const Self = @This();

                pub fn step(self: *Self, item: T) !void {
                    if (!self.first) {
                        try self.downstream.step(sep);
                    }
                    self.first = false;
                    try self.downstream.step(item);
                }

                pub fn finish(self: *Self) !void {
                    try self.downstream.finish();
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

// ============================================================================
// Sources / Driving
// ============================================================================

/// Drive a pipeline from a slice
pub fn transduce(
    comptime Xform: type,
    comptime Sink: type,
    sink: Sink,
    source: anytype,
) !void {
    var reducer = Xform.wrap(Sink, sink);

    for (source) |item| {
        reducer.step(item) catch |err| {
            if (err == error.TransducerHalt) break;
            return err;
        };
    }

    try reducer.finish();
}

/// Drive from an iterator (anything with next() -> ?T)
pub fn transduceIter(
    comptime Xform: type,
    comptime Sink: type,
    sink: Sink,
    iter: anytype,
) !void {
    var reducer = Xform.wrap(Sink, sink);

    while (iter.next()) |item| {
        reducer.step(item) catch |err| {
            if (err == error.TransducerHalt) break;
            return err;
        };
    }

    try reducer.finish();
}

// ============================================================================
// Sinks
// ============================================================================

/// Collect items into an ArrayList
pub fn ToArrayList(comptime T: type) type {
    return struct {
        list: *std.ArrayList(T),

        const Self = @This();

        pub fn step(self: *Self, item: T) !void {
            try self.list.append(item);
        }

        pub fn finish(_: *Self) !void {}
    };
}

/// Count items
pub fn Counter(comptime T: type) type {
    _ = T;
    return struct {
        count: *usize,

        const Self = @This();

        pub fn step(self: *Self, _: anytype) !void {
            self.count.* += 1;
        }

        pub fn finish(_: *Self) !void {}
    };
}

/// Discard (like /dev/null, for benchmarking)
pub const Discard = struct {
    pub fn step(_: *Discard, _: anytype) !void {}
    pub fn finish(_: *Discard) !void {}
};

/// Write formatted lines to any writer
pub fn ToWriter(comptime Writer: type) type {
    return struct {
        writer: Writer,

        const Self = @This();

        pub fn step(self: *Self, item: []const u8) !void {
            try self.writer.writeAll(item);
        }

        pub fn finish(_: *Self) !void {}
    };
}

/// Fold/reduce into a single value
pub fn Fold(comptime T: type, comptime f: anytype) type {
    return struct {
        acc: *T,

        const Self = @This();

        pub fn step(self: *Self, item: anytype) !void {
            self.acc.* = f(self.acc.*, item);
        }

        pub fn finish(_: *Self) !void {}
    };
}

// ============================================================================
// Tests
// ============================================================================

test "map" {
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Map(double), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 2, 4, 6 }, result.items);
}

test "filter" {
    const even = struct {
        fn f(x: i32) bool {
            return @mod(x, 2) == 0;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Filter(even), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 2, 4 }, result.items);
}

test "compose map and filter" {
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;
    const even = struct {
        fn f(x: i32) bool {
            return @mod(x, 2) == 0;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    // double then filter even (all pass since doubled)
    const Xf = Compose(Map(double), Filter(even));

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Xf, ToArrayList(i32), sink, &[_]i32{ 1, 2, 3 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 2, 4, 6 }, result.items);
}

test "take early termination" {
    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Take(3), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3 }, result.items);
}

test "dedupe" {
    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Dedupe(i32), ToArrayList(i32), sink, &[_]i32{ 1, 1, 2, 2, 2, 3, 1, 1 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 1 }, result.items);
}

test "compose3" {
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;
    const inc = struct {
        fn f(x: i32) i32 {
            return x + 1;
        }
    }.f;
    const gt5 = struct {
        fn f(x: i32) bool {
            return x > 5;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    // (x * 2) + 1, keep if > 5
    const Xf = Compose3(Map(double), Map(inc), Filter(gt5));

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Xf, ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5 });

    // 1*2+1=3 (skip), 2*2+1=5 (skip), 3*2+1=7, 4*2+1=9, 5*2+1=11
    try std.testing.expectEqualSlices(i32, &[_]i32{ 7, 9, 11 }, result.items);
}

test "counter sink" {
    var count: usize = 0;
    const sink = Counter(i32){ .count = &count };
    try transduce(Filter(struct {
        fn f(x: i32) bool {
            return x > 3;
        }
    }.f), Counter(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 6 });

    try std.testing.expectEqual(@as(usize, 3), count);
}

test "discard sink" {
    const discard = Discard{};
    try transduce(Map(struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f), Discard, discard, &[_]i32{ 1, 2, 3, 4, 5 });
    // Just verifying it doesn't crash
}

test "identity" {
    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Identity, ToArrayList(i32), sink, &[_]i32{ 1, 2, 3 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3 }, result.items);
}

test "fold" {
    const add = struct {
        fn f(acc: i32, x: i32) i32 {
            return acc + x;
        }
    }.f;

    var sum: i32 = 0;
    const sink = Fold(i32, add){ .acc = &sum };
    try transduce(Identity, Fold(i32, add), sink, &[_]i32{ 1, 2, 3, 4, 5 });

    try std.testing.expectEqual(@as(i32, 15), sum);
}

test "map then fold" {
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;
    const add = struct {
        fn f(acc: i32, x: i32) i32 {
            return acc + x;
        }
    }.f;

    var sum: i32 = 0;
    const sink = Fold(i32, add){ .acc = &sum };
    try transduce(Map(double), Fold(i32, add), sink, &[_]i32{ 1, 2, 3, 4, 5 });

    // (1+2+3+4+5)*2 = 30
    try std.testing.expectEqual(@as(i32, 30), sum);
}

test "enumerate" {
    const E = Enumerate(i32);
    var result = std.ArrayList(E.Item).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(E.Item){ .list = &result };
    try transduce(E, ToArrayList(E.Item), sink, &[_]i32{ 10, 20, 30 });

    try std.testing.expectEqual(@as(usize, 3), result.items.len);
    try std.testing.expectEqual(@as(usize, 0), result.items[0].index);
    try std.testing.expectEqual(@as(i32, 10), result.items[0].value);
    try std.testing.expectEqual(@as(usize, 2), result.items[2].index);
    try std.testing.expectEqual(@as(i32, 30), result.items[2].value);
}

test "scan (running sum)" {
    const add = struct {
        fn f(acc: i32, x: i32) i32 {
            return acc + x;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Scan(i32, add, 0), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5 });

    // Running sums: 1, 3, 6, 10, 15
    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 3, 6, 10, 15 }, result.items);
}

test "take while" {
    const lt5 = struct {
        fn f(x: i32) bool {
            return x < 5;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(TakeWhile(lt5), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 6, 7 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 4 }, result.items);
}

test "drop while" {
    const lt5 = struct {
        fn f(x: i32) bool {
            return x < 5;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(DropWhile(lt5), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 6, 7 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 5, 6, 7 }, result.items);
}

test "drop" {
    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Drop(3), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 6, 7 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 4, 5, 6, 7 }, result.items);
}

test "interpose" {
    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Interpose(i32, 0), ToArrayList(i32), sink, &[_]i32{ 1, 2, 3 });

    try std.testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 2, 0, 3 }, result.items);
}

test "complex composition" {
    // drop 2, take while < 10, double, running sum
    const lt10 = struct {
        fn f(x: i32) bool {
            return x < 10;
        }
    }.f;
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;
    const add = struct {
        fn f(acc: i32, x: i32) i32 {
            return acc + x;
        }
    }.f;

    var result = std.ArrayList(i32).init(std.testing.allocator);
    defer result.deinit();

    const Xf = Compose4(Drop(2), TakeWhile(lt10), Map(double), Scan(i32, add, 0));

    const sink = ToArrayList(i32){ .list = &result };
    try transduce(Xf, ToArrayList(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 10, 11, 12 });

    // Input after drop(2): 3, 4, 5, 10, 11, 12
    // After takeWhile(<10): 3, 4, 5
    // After double: 6, 8, 10
    // Running sum: 6, 14, 24
    try std.testing.expectEqualSlices(i32, &[_]i32{ 6, 14, 24 }, result.items);
}
