const std = @import("std");

pub const Pool = struct {
    pool: std.Thread.Pool = undefined,

    pub fn init(self: *Pool, allocator: std.mem.Allocator, n_jobs: ?usize) !void {
        try self.pool.init(.{ .allocator = allocator, .n_jobs = n_jobs });
    }

    pub fn deinit(self: *Pool) void {
        self.pool.deinit();
    }

    pub fn scope(self: *Pool) Scope {
        return .{ .pool = self };
    }
};

pub const Scope = struct {
    pool: *Pool,
    wg: std.Thread.WaitGroup = .{},

    pub fn spawn(self: *Scope, comptime func: anytype, args: anytype) void {
        self.pool.pool.spawnWg(&self.wg, func, args);
    }

    pub fn wait(self: *Scope) void {
        self.wg.wait();
    }
};

test "scope fork join" {
    var p: Pool = .{};
    try p.init(std.testing.allocator, 2);
    defer p.deinit();

    var scope = p.scope();
    var x: usize = 0;
    var m: std.Thread.Mutex = .{};

    const Job = struct {
        fn run(mutex: *std.Thread.Mutex, n: *usize) void {
            mutex.lock();
            n.* += 1;
            mutex.unlock();
        }
    };

    scope.spawn(Job.run, .{ &m, &x });
    scope.spawn(Job.run, .{ &m, &x });
    scope.wait();
    try std.testing.expectEqual(@as(usize, 2), x);
}

