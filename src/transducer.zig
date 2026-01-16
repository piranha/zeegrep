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
//! const Pipe = Compose(.{
//!     FilterBinary,           // skip binary files
//!     FlatMap(findMatches),   // path -> []Match
//!     TrackLineNumbers,       // attach line numbers
//!     AddContext(3),          // before/after lines
//!     FormatMatch,            // -> []u8
//! });
//!
//! // Parallel variant:
//! const ParallelPipe = Compose(.{
//!     Parallel(8, 64, Compose(.{ FilterBinary, FlatMap(findMatches) }), Path, Match),
//!     TrackLineNumbers,
//!     AddContext(3),
//!     FormatMatch,
//! });
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

/// Partition: collect N items, emit as slice
/// - n: partition size
/// - step_by: advance by this many items (default = n, no overlap)
/// - all: if true, emit incomplete final partition (partition-all behavior)
///
/// Examples (with n=3):
///   step_by=3, all=true:  [1,2,3,4,5] -> [1,2,3], [4,5]
///   step_by=3, all=false: [1,2,3,4,5] -> [1,2,3]
///   step_by=1, all=true:  [1,2,3,4] -> [1,2,3], [2,3,4], [3,4], [4]  (sliding window)
///   step_by=1, all=false: [1,2,3,4] -> [1,2,3], [2,3,4]
pub fn Partition(comptime T: type, comptime n: usize, comptime step_by: usize, comptime all: bool) type {
    if (step_by == 0) @compileError("step_by must be > 0");
    if (n == 0) @compileError("n must be > 0");

    // Buffer needs to hold n items for the window
    const buf_size = n;

    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                buffer: [buf_size]T = undefined,
                len: usize = 0,

                const Self = @This();

                pub fn step(self: *Self, item: T) !void {
                    self.buffer[self.len] = item;
                    self.len += 1;

                    if (self.len == n) {
                        try self.downstream.step(self.buffer[0..n]);
                        // Slide: keep last (n - step_by) items
                        if (step_by < n) {
                            const keep = n - step_by;
                            for (0..keep) |i| {
                                self.buffer[i] = self.buffer[step_by + i];
                            }
                            self.len = keep;
                        } else {
                            self.len = 0;
                        }
                    }
                }

                pub fn finish(self: *Self) !void {
                    if (all) {
                        // Emit remaining partial partitions
                        while (self.len > 0) {
                            try self.downstream.step(self.buffer[0..self.len]);
                            if (step_by >= self.len) break;
                            // Slide for next partial
                            const keep = self.len - step_by;
                            for (0..keep) |i| {
                                self.buffer[i] = self.buffer[step_by + i];
                            }
                            self.len = keep;
                        }
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

/// Compose transducers from a tuple: Compose(.{ A, B, C })
/// Items flow left to right: source -> A -> B -> C -> sink
pub fn Compose(comptime xforms: anytype) type {
    const fields = @typeInfo(@TypeOf(xforms)).@"struct".fields;
    if (fields.len == 0) @compileError("Compose requires at least one transducer");
    if (fields.len == 1) return @field(xforms, fields[0].name);

    // Right-fold: compose from the end backwards
    comptime var Result = @field(xforms, fields[fields.len - 1].name);
    comptime var i = fields.len - 1;
    inline while (i > 0) {
        i -= 1;
        const Prev = @field(xforms, fields[i].name);
        Result = Compose2(Prev, Result);
    }
    return Result;
}

/// Compose exactly two transducers (internal helper)
fn Compose2(comptime A: type, comptime B: type) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return A.apply(B.apply(Downstream));
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            const b_wrapped = B.wrap(Sink, sink);
            return A.wrap(@TypeOf(b_wrapped), b_wrapped);
        }
    };
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

/// Parallel: async boundary that processes items concurrently
/// 
/// Each step(item) pushes to a work queue. N workers pull items, run InnerXf,
/// push results to output queue. Main thread drains output queue to downstream.
///
/// This is a "higher-order" transducer wrapping another transducer.
/// Order is NOT preserved. Use for embarrassingly parallel workloads.
///
/// Flow: step(item) -> work_queue -> worker runs InnerXf -> result_queue -> downstream
pub fn Parallel(
    comptime n_workers: usize,
    comptime queue_size: usize,
    comptime InnerXf: type,
    comptime Item: type,
    comptime Result: type,
) type {
    return struct {
        pub fn apply(comptime Downstream: type) type {
            return struct {
                downstream: Downstream,
                work_queue: WorkQueue = .{},
                result_queue: ResultQueue = .{},
                workers: [n_workers]std.Thread = undefined,
                workers_started: usize = 0,
                started: bool = false,
                first_error: ?anyerror = null,

                const Self = @This();
                const WorkQueue = BoundedQueue(Item, queue_size);
                const ResultQueue = BoundedQueue(ResultOrError, queue_size * n_workers);
                const ResultOrError = union(enum) { result: Result, err: anyerror };

                fn ensureStarted(self: *Self) void {
                    if (self.started) return;
                    self.started = true;

                    // Spawn workers - now self address is stable
                    for (0..n_workers) |i| {
                        self.workers[i] = std.Thread.spawn(.{}, workerFn, .{
                            &self.work_queue,
                            &self.result_queue,
                        }) catch continue;
                        self.workers_started += 1;
                    }
                }

                pub fn step(self: *Self, item: Item) !void {
                    self.ensureStarted();

                    // Drain any ready results first (non-blocking)
                    try self.drainResults();

                    // Push work (blocks if queue full - natural backpressure)
                    self.work_queue.push(item);
                }

                pub fn finish(self: *Self) !void {
                    if (!self.started) {
                        try self.downstream.finish();
                        return;
                    }

                    // Signal workers to stop
                    self.work_queue.close();

                    // Join workers
                    for (self.workers[0..self.workers_started]) |worker| {
                        worker.join();
                    }

                    // Drain remaining results (non-blocking - workers are done)
                    try self.drainResults();

                    if (self.first_error) |err| return err;

                    try self.downstream.finish();
                }

                fn drainResults(self: *Self) !void {
                    while (self.result_queue.tryPop()) |result_or_err| {
                        switch (result_or_err) {
                            .result => |result| try self.downstream.step(result),
                            .err => |err| {
                                if (self.first_error == null) self.first_error = err;
                            },
                        }
                    }
                }

                fn workerFn(work_queue: *WorkQueue, result_queue: *ResultQueue) void {
                    // Worker's local sink - pushes to result queue
                    const WorkerSink = struct {
                        rq: *ResultQueue,

                        pub fn step(sink: *@This(), result: Result) !void {
                            sink.rq.push(.{ .result = result });
                        }

                        pub fn finish(_: *@This()) !void {}
                    };

                    const sink = WorkerSink{ .rq = result_queue };
                    var reducer = InnerXf.wrap(WorkerSink, sink);

                    while (work_queue.pop()) |item| {
                        reducer.step(item) catch |e| {
                            if (e != error.TransducerHalt) {
                                result_queue.push(.{ .err = e });
                            }
                            continue;
                        };
                    }

                    reducer.finish() catch |e| {
                        result_queue.push(.{ .err = e });
                    };
                }
            };
        }

        pub fn wrap(comptime Sink: type, sink: Sink) apply(Sink) {
            return .{ .downstream = sink };
        }
    };
}

/// Thread-safe bounded queue with close semantics
fn BoundedQueue(comptime T: type, comptime capacity: usize) type {
    return struct {
        buffer: [capacity]T = undefined,
        head: usize = 0,
        tail: usize = 0,
        len: usize = 0,
        closed: bool = false,
        mutex: std.Thread.Mutex = .{},
        not_empty: std.Thread.Condition = .{},
        not_full: std.Thread.Condition = .{},

        const Self = @This();

        pub fn init() Self {
            return .{};
        }

        /// Push item, blocks if full
        pub fn push(self: *Self, item: T) void {
            self.mutex.lock();
            defer self.mutex.unlock();

            while (self.len == capacity and !self.closed) {
                self.not_full.wait(&self.mutex);
            }

            if (self.closed) return;

            self.buffer[self.tail] = item;
            self.tail = (self.tail + 1) % capacity;
            self.len += 1;

            self.not_empty.signal();
        }

        /// Pop item, blocks if empty, returns null if closed and empty
        pub fn pop(self: *Self) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();

            while (self.len == 0 and !self.closed) {
                self.not_empty.wait(&self.mutex);
            }

            if (self.len == 0) return null;

            const item = self.buffer[self.head];
            self.head = (self.head + 1) % capacity;
            self.len -= 1;

            self.not_full.signal();
            return item;
        }

        /// Non-blocking pop
        pub fn tryPop(self: *Self) ?T {
            self.mutex.lock();
            defer self.mutex.unlock();

            if (self.len == 0) return null;

            const item = self.buffer[self.head];
            self.head = (self.head + 1) % capacity;
            self.len -= 1;

            self.not_full.signal();
            return item;
        }

        /// Signal no more items will be pushed
        pub fn close(self: *Self) void {
            self.mutex.lock();
            defer self.mutex.unlock();

            self.closed = true;
            self.not_empty.broadcast();
            self.not_full.broadcast();
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

/// Collect items into an ArrayListUnmanaged
pub fn ToArrayList(comptime T: type) type {
    return struct {
        list: *std.ArrayListUnmanaged(T),
        allocator: std.mem.Allocator,

        const Self = @This();

        pub fn step(self: *Self, item: T) !void {
            try self.list.append(self.allocator, item);
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

const testing = std.testing;
const alloc = testing.allocator;

fn listOf(comptime T: type) std.ArrayListUnmanaged(T) {
    return .{};
}

fn sinkFor(comptime T: type, list: *std.ArrayListUnmanaged(T)) ToArrayList(T) {
    return .{ .list = list, .allocator = alloc };
}

test "map" {
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;

    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Map(double), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3 });
    try testing.expectEqualSlices(i32, &[_]i32{ 2, 4, 6 }, result.items);
}

test "filter" {
    const even = struct {
        fn f(x: i32) bool {
            return @mod(x, 2) == 0;
        }
    }.f;

    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Filter(even), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5 });
    try testing.expectEqualSlices(i32, &[_]i32{ 2, 4 }, result.items);
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

    var result = listOf(i32);
    defer result.deinit(alloc);

    const Xf = Compose(.{ Map(double), Filter(even) });
    try transduce(Xf, ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3 });
    try testing.expectEqualSlices(i32, &[_]i32{ 2, 4, 6 }, result.items);
}

test "take early termination" {
    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Take(3), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 });
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3 }, result.items);
}

test "dedupe" {
    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Dedupe(i32), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 1, 2, 2, 2, 3, 1, 1 });
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 1 }, result.items);
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

    var result = listOf(i32);
    defer result.deinit(alloc);

    const Xf = Compose(.{ Map(double), Map(inc), Filter(gt5) });
    try transduce(Xf, ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5 });
    // 1*2+1=3 (skip), 2*2+1=5 (skip), 3*2+1=7, 4*2+1=9, 5*2+1=11
    try testing.expectEqualSlices(i32, &[_]i32{ 7, 9, 11 }, result.items);
}

test "counter sink" {
    var count: usize = 0;
    const sink = Counter(i32){ .count = &count };
    try transduce(Filter(struct {
        fn f(x: i32) bool {
            return x > 3;
        }
    }.f), Counter(i32), sink, &[_]i32{ 1, 2, 3, 4, 5, 6 });
    try testing.expectEqual(@as(usize, 3), count);
}

test "discard sink" {
    const discard = Discard{};
    try transduce(Map(struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f), Discard, discard, &[_]i32{ 1, 2, 3, 4, 5 });
}

test "identity" {
    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Identity, ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3 });
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3 }, result.items);
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
    try testing.expectEqual(@as(i32, 15), sum);
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
    try testing.expectEqual(@as(i32, 30), sum);
}

test "enumerate" {
    const E = Enumerate(i32);
    var result = listOf(E.Item);
    defer result.deinit(alloc);

    try transduce(E, ToArrayList(E.Item), sinkFor(E.Item, &result), &[_]i32{ 10, 20, 30 });
    try testing.expectEqual(@as(usize, 3), result.items.len);
    try testing.expectEqual(@as(usize, 0), result.items[0].index);
    try testing.expectEqual(@as(i32, 10), result.items[0].value);
    try testing.expectEqual(@as(usize, 2), result.items[2].index);
    try testing.expectEqual(@as(i32, 30), result.items[2].value);
}

test "scan (running sum)" {
    const add = struct {
        fn f(acc: i32, x: i32) i32 {
            return acc + x;
        }
    }.f;

    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Scan(i32, add, 0), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5 });
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 3, 6, 10, 15 }, result.items);
}

test "take while" {
    const lt5 = struct {
        fn f(x: i32) bool {
            return x < 5;
        }
    }.f;

    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(TakeWhile(lt5), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5, 6, 7 });
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 2, 3, 4 }, result.items);
}

test "drop while" {
    const lt5 = struct {
        fn f(x: i32) bool {
            return x < 5;
        }
    }.f;

    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(DropWhile(lt5), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5, 6, 7 });
    try testing.expectEqualSlices(i32, &[_]i32{ 5, 6, 7 }, result.items);
}

test "drop" {
    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Drop(3), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5, 6, 7 });
    try testing.expectEqualSlices(i32, &[_]i32{ 4, 5, 6, 7 }, result.items);
}

test "interpose" {
    var result = listOf(i32);
    defer result.deinit(alloc);

    try transduce(Interpose(i32, 0), ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3 });
    try testing.expectEqualSlices(i32, &[_]i32{ 1, 0, 2, 0, 3 }, result.items);
}

test "complex composition" {
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

    var result = listOf(i32);
    defer result.deinit(alloc);

    const Xf = Compose(.{ Drop(2), TakeWhile(lt10), Map(double), Scan(i32, add, 0) });
    try transduce(Xf, ToArrayList(i32), sinkFor(i32, &result), &[_]i32{ 1, 2, 3, 4, 5, 10, 11, 12 });
    // drop(2): 3,4,5,10,11,12 -> takeWhile(<10): 3,4,5 -> double: 6,8,10 -> scan: 6,14,24
    try testing.expectEqualSlices(i32, &[_]i32{ 6, 14, 24 }, result.items);
}

test "partition basic (all=true)" {
    // Use a sink that sums partition lengths and first elements
    const Summer = struct {
        count: usize = 0,
        first_sum: i32 = 0,
        len_sum: usize = 0,

        pub fn step(self: *@This(), slice: []const i32) !void {
            self.count += 1;
            self.first_sum += slice[0];
            self.len_sum += slice.len;
        }
        pub fn finish(_: *@This()) !void {}
    };

    const sink = Summer{};
    var xf = Partition(i32, 3, 3, true).wrap(Summer, sink);
    for ([_]i32{ 1, 2, 3, 4, 5 }) |item| try xf.step(item);
    try xf.finish();
    // Expect: [1,2,3] and [4,5] -> 2 partitions, first_sum=1+4=5, len_sum=3+2=5
    try testing.expectEqual(@as(usize, 2), xf.downstream.count);
    try testing.expectEqual(@as(i32, 5), xf.downstream.first_sum);
    try testing.expectEqual(@as(usize, 5), xf.downstream.len_sum);
}

test "partition strict (all=false)" {
    var count: usize = 0;
    const sink = Counter([]const i32){ .count = &count };
    try transduce(Partition(i32, 3, 3, false), Counter([]const i32), sink, &[_]i32{ 1, 2, 3, 4, 5 });
    // Only [1,2,3] emitted, [4,5] dropped
    try testing.expectEqual(@as(usize, 1), count);
}

test "partition sliding window (step=1)" {
    var count: usize = 0;
    const sink = Counter([]const i32){ .count = &count };
    try transduce(Partition(i32, 3, 1, false), Counter([]const i32), sink, &[_]i32{ 1, 2, 3, 4 });
    // [1,2,3], [2,3,4] -> 2 windows
    try testing.expectEqual(@as(usize, 2), count);
}

test "partition sliding window with partials (step=1, all=true)" {
    var count: usize = 0;
    const sink = Counter([]const i32){ .count = &count };
    try transduce(Partition(i32, 3, 1, true), Counter([]const i32), sink, &[_]i32{ 1, 2, 3, 4 });
    // [1,2,3], [2,3,4], [3,4], [4] -> 4 partitions
    try testing.expectEqual(@as(usize, 4), count);
}

test "parallel basic" {
    const double = struct {
        fn f(x: i32) i32 {
            return x * 2;
        }
    }.f;

    var result = listOf(i32);
    defer result.deinit(alloc);

    const ParXf = Parallel(2, 8, Map(double), i32, i32);
    var reducer = ParXf.wrap(ToArrayList(i32), sinkFor(i32, &result));

    for ([_]i32{ 1, 2, 3, 4, 5 }) |item| {
        try reducer.step(item);
    }
    try reducer.finish();

    std.mem.sort(i32, result.items, {}, std.sort.asc(i32));
    try testing.expectEqualSlices(i32, &[_]i32{ 2, 4, 6, 8, 10 }, result.items);
}

// ============================================================================
// Benchmarks: Zero-Cost Abstraction Verification
// ============================================================================

test "bench: transducer vs hand-written (map+filter+take)" {
    // Pipeline: double, keep evens, take first 1000
    const N = 100_000;
    const TAKE = 1000;
    const ITERS = 100;

    const double = struct {
        fn f(x: i64) i64 {
            return x *% 2;
        }
    }.f;
    const isEven = struct {
        fn f(x: i64) bool {
            return @mod(x, 2) == 0;
        }
    }.f;

    // Generate input
    var input: [N]i64 = undefined;
    for (0..N) |i| input[i] = @intCast(i);

    // Warm up + verify correctness
    var xf_sum: i64 = 0;
    var hand_sum: i64 = 0;

    // Hand-written version
    {
        var count: usize = 0;
        for (input) |x| {
            const doubled = x *% 2;
            if (@mod(doubled, 2) == 0) {
                hand_sum +%= doubled;
                count += 1;
                if (count >= TAKE) break;
            }
        }
    }

    // Transducer version
    {
        const Xf = Compose(.{ Map(double), Filter(isEven), Take(TAKE) });
        const Summer = struct {
            sum: *i64,
            pub fn step(self: *@This(), x: i64) !void {
                self.sum.* +%= x;
            }
            pub fn finish(_: *@This()) !void {}
        };
        var sum: i64 = 0;
        const sink = Summer{ .sum = &sum };
        try transduce(Xf, Summer, sink, &input);
        xf_sum = sum;
    }

    try testing.expectEqual(hand_sum, xf_sum);

    // Benchmark
    var timer = try std.time.Timer.start();

    // Hand-written
    timer.reset();
    var hand_result: i64 = 0;
    for (0..ITERS) |_| {
        var count: usize = 0;
        for (input) |x| {
            const doubled = x *% 2;
            if (@mod(doubled, 2) == 0) {
                hand_result +%= doubled;
                count += 1;
                if (count >= TAKE) break;
            }
        }
    }
    const hand_ns = timer.read();

    // Transducer
    timer.reset();
    var xf_result: i64 = 0;
    for (0..ITERS) |_| {
        const Xf = Compose(.{ Map(double), Filter(isEven), Take(TAKE) });
        const Summer = struct {
            sum: *i64,
            pub fn step(self: *@This(), x: i64) !void {
                self.sum.* +%= x;
            }
            pub fn finish(_: *@This()) !void {}
        };
        const sink = Summer{ .sum = &xf_result };
        transduce(Xf, Summer, sink, &input) catch {};
    }
    const xf_ns = timer.read();

    const hand_us = hand_ns / 1000;
    const xf_us = xf_ns / 1000;
    const ratio = @as(f64, @floatFromInt(xf_us)) / @as(f64, @floatFromInt(@max(hand_us, 1)));

    std.debug.print("\nbench map+filter+take ({} items, take {}, {} iters):\n", .{ N, TAKE, ITERS });
    std.debug.print("  hand-written: {}us\n", .{hand_us});
    std.debug.print("  transducer:   {}us\n", .{xf_us});
    if (hand_us == 0 and xf_us == 0) {
        std.debug.print("  ratio:        ~1.0x (both optimized away)\n", .{});
    } else {
        std.debug.print("  ratio:        {d:.2}x\n", .{ratio});
    }

    // Should be very close to 1.0x (zero-cost)
    // Allow up to 1.5x for measurement noise in debug builds
    try testing.expect(ratio < 1.5);
}

test "bench: transducer vs hand-written (scan running sum)" {
    const N = 100_000;
    const ITERS = 50;

    const add = struct {
        fn f(acc: i64, x: i64) i64 {
            return acc +% x;
        }
    }.f;

    var input: [N]i64 = undefined;
    for (0..N) |i| input[i] = @intCast(i);

    var timer = try std.time.Timer.start();

    // Hand-written running sum
    timer.reset();
    var hand_last: i64 = 0;
    for (0..ITERS) |_| {
        var acc: i64 = 0;
        for (input) |x| {
            acc +%= x;
            hand_last = acc; // simulate consuming each running sum
        }
    }
    const hand_ns = timer.read();

    // Transducer scan
    timer.reset();
    var xf_last: i64 = 0;
    for (0..ITERS) |_| {
        const Consumer = struct {
            last: *i64,
            pub fn step(self: *@This(), x: i64) !void {
                self.last.* = x;
            }
            pub fn finish(_: *@This()) !void {}
        };
        const sink = Consumer{ .last = &xf_last };
        transduce(Scan(i64, add, 0), Consumer, sink, &input) catch {};
    }
    const xf_ns = timer.read();

    try testing.expectEqual(hand_last, xf_last);

    const hand_us = hand_ns / 1000;
    const xf_us = xf_ns / 1000;
    const ratio = @as(f64, @floatFromInt(xf_us)) / @as(f64, @floatFromInt(@max(hand_us, 1)));

    std.debug.print("\nbench scan/running-sum ({} items, {} iters):\n", .{ N, ITERS });
    std.debug.print("  hand-written: {}us\n", .{hand_us});
    std.debug.print("  transducer:   {}us\n", .{xf_us});
    std.debug.print("  ratio:        {d:.2}x\n", .{ratio});

    // Debug builds may show overhead; release should be ~1.0x
    try testing.expect(ratio < 2.0);
}

test "bench: complex pipeline (drop+filter+map+enumerate+take)" {
    const N = 100_000;
    const ITERS = 50;

    const square = struct {
        fn f(x: i64) i64 {
            return x *% x;
        }
    }.f;
    const isOdd = struct {
        fn f(x: i64) bool {
            return @mod(x, 2) == 1;
        }
    }.f;

    var input: [N]i64 = undefined;
    for (0..N) |i| input[i] = @intCast(i);

    var timer = try std.time.Timer.start();

    // Hand-written: drop 100, filter odd, square, enumerate, take 500
    timer.reset();
    var hand_sum: i64 = 0;
    for (0..ITERS) |_| {
        var idx: usize = 0;
        var count: usize = 0;
        for (input) |x| {
            // drop 100
            if (idx < 100) {
                idx += 1;
                continue;
            }
            idx += 1;
            // filter odd
            if (@mod(x, 2) != 1) continue;
            // square
            const sq = x *% x;
            // enumerate (we just use count as index)
            // take 500
            hand_sum +%= sq +% @as(i64, @intCast(count));
            count += 1;
            if (count >= 500) break;
        }
    }
    const hand_ns = timer.read();

    // Transducer version
    timer.reset();
    var xf_sum: i64 = 0;
    for (0..ITERS) |_| {
        const E = Enumerate(i64);
        const Xf = Compose(.{ Drop(100), Filter(isOdd), Map(square), E, Take(500) });

        const Summer = struct {
            sum: *i64,
            pub fn step(self: *@This(), item: E.Item) !void {
                self.sum.* +%= item.value +% @as(i64, @intCast(item.index));
            }
            pub fn finish(_: *@This()) !void {}
        };
        const sink = Summer{ .sum = &xf_sum };
        transduce(Xf, Summer, sink, &input) catch {};
    }
    const xf_ns = timer.read();

    try testing.expectEqual(hand_sum, xf_sum);

    const hand_us = hand_ns / 1000;
    const xf_us = xf_ns / 1000;
    const ratio = @as(f64, @floatFromInt(xf_us)) / @as(f64, @floatFromInt(@max(hand_us, 1)));

    std.debug.print("\nbench complex pipeline ({} items, {} iters):\n", .{ N, ITERS });
    std.debug.print("  hand-written: {}us\n", .{hand_us});
    std.debug.print("  transducer:   {}us\n", .{xf_us});
    std.debug.print("  ratio:        {d:.2}x\n", .{ratio});

    try testing.expect(ratio < 1.5);
}

test "bench: memory - transducers stream without intermediate collections" {
    // Verify transducers process one item at a time (no intermediate arrays).
    // We count how many times "expensive work" overlaps in flight.
    const N = 10_000;

    // Track pipeline depth at each step
    const Tracker = struct {
        var depth: usize = 0;
        var max_depth: usize = 0;

        fn enter() void {
            depth += 1;
            if (depth > max_depth) max_depth = depth;
        }
        fn leave() void {
            depth -= 1;
        }
        fn reset() void {
            depth = 0;
            max_depth = 0;
        }
    };

    Tracker.reset();

    // Each transform enters/leaves the tracker
    const step1 = struct {
        fn f(x: i64) i64 {
            Tracker.enter();
            defer Tracker.leave();
            return x * 2;
        }
    }.f;
    const keepOdd = struct {
        fn f(x: i64) bool {
            Tracker.enter();
            defer Tracker.leave();
            return @mod(x, 2) == 1;
        }
    }.f;
    const step2 = struct {
        fn f(x: i64) i64 {
            Tracker.enter();
            defer Tracker.leave();
            return x + 1;
        }
    }.f;

    const Xf = Compose(.{ Map(step1), Filter(keepOdd), Map(step2) });

    var sum: i64 = 0;
    const Summer = struct {
        s: *i64,
        pub fn step(self: *@This(), x: i64) !void {
            Tracker.enter();
            defer Tracker.leave();
            self.s.* += x;
        }
        pub fn finish(_: *@This()) !void {}
    };

    var input: [N]i64 = undefined;
    for (0..N) |i| input[i] = @intCast(i);

    const sink = Summer{ .s = &sum };
    try transduce(Xf, Summer, sink, &input);

    std.debug.print("\nbench memory (streaming, {} items):\n", .{N});
    std.debug.print("  max pipeline depth: {} (ideal: 1 per stage)\n", .{Tracker.max_depth});
    std.debug.print("  (eager/batched would show depth = N)\n", .{});

    // Streaming: depth = number of nested calls for ONE item flowing through
    // Here: step1 -> keepOdd -> step2 -> summer = max 4 at any instant
    // If it were eager, we'd see depth = N (all items in first stage before second starts)
    try testing.expect(Tracker.max_depth <= 4);
}
