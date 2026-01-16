const std = @import("std");

pub const max_threads = 16;

pub const Pool = struct {
    pool: std.Thread.Pool = undefined,
    n_threads: usize = 0,
    arenas: [max_threads]std.heap.ArenaAllocator = undefined,

    pub fn init(self: *Pool, allocator: std.mem.Allocator, n_jobs: ?usize) !void {
        // +1 for main thread which may run jobs on alloc failure
        self.n_threads = 1 + (n_jobs orelse @max(1, std.Thread.getCpuCount() catch 1));
        for (self.arenas[0..self.n_threads]) |*a| a.* = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        try self.pool.init(.{ .allocator = allocator, .n_jobs = n_jobs, .track_ids = true });
    }

    pub fn deinit(self: *Pool) void {
        self.pool.deinit();
        for (self.arenas[0..self.n_threads]) |*a| a.deinit();
    }

    pub fn scope(self: *Pool) Scope {
        return .{ .pool = self };
    }

    pub fn arena(self: *Pool, thread_id: usize) *std.heap.ArenaAllocator {
        return &self.arenas[thread_id];
    }
};

pub const Scope = struct {
    pool: *Pool,
    wg: std.Thread.WaitGroup = .{},

    pub fn spawn(self: *Scope, comptime func: anytype, args: anytype) void {
        self.pool.pool.spawnWgId(&self.wg, func, args);
    }

    pub fn wait(self: *Scope) void {
        self.wg.wait();
    }
};

test "scope fork join" {
    var p: Pool = .{};
    try p.init(std.testing.allocator, 2);
    defer p.deinit();

    var s = p.scope();
    var x: usize = 0;
    var m: std.Thread.Mutex = .{};

    const Job = struct {
        fn run(thread_id: usize, mutex: *std.Thread.Mutex, n: *usize) void {
            _ = thread_id;
            mutex.lock();
            n.* += 1;
            mutex.unlock();
        }
    };

    s.spawn(Job.run, .{ &m, &x });
    s.spawn(Job.run, .{ &m, &x });
    s.wait();
    try std.testing.expectEqual(@as(usize, 2), x);
}

