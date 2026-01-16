const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});
    const version = b.option([]const u8, "version", "Version string") orelse "dev";
    const output = b.option([]const u8, "output", "Custom output path (e.g., dist/zg-Linux-x86_64)");

    const pcre2_dep = b.dependency("pcre2", .{
        .target = target,
        .optimize = optimize,
    });
    const pcre2_lib = pcre2_dep.artifact("pcre2-8");

    const build_options = b.addOptions();
    build_options.addOption([]const u8, "version", version);

    const exe_module = b.createModule(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
        .strip = optimize != .Debug,
    });
    exe_module.addOptions("build_options", build_options);
    const exe = b.addExecutable(.{
        .name = "zg",
        .root_module = exe_module,
    });
    exe.linkLibrary(pcre2_lib);
    if (output) |out_path| {
        const install = b.addInstallFileWithDir(exe.getEmittedBin(), .prefix, out_path);
        b.getInstallStep().dependOn(&install.step);
    } else {
        b.installArtifact(exe);
    }

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| run_cmd.addArgs(args);

    const run_step = b.step("run", "Run zg");
    run_step.dependOn(&run_cmd.step);

    const opt_test_module = b.createModule(.{
        .root_source_file = b.path("src/core/opt.zig"),
        .target = target,
        .optimize = optimize,
    });
    const opt_tests = b.addTest(.{ .root_module = opt_test_module });
    const run_opt_tests = b.addRunArtifact(opt_tests);

    const run_test_module = b.createModule(.{
        .root_source_file = b.path("src/run.zig"),
        .target = target,
        .optimize = optimize,
    });
    const run_tests = b.addTest(.{ .root_module = run_test_module });
    run_tests.linkLibrary(pcre2_lib);
    const run_run_tests = b.addRunArtifact(run_tests);

    const transducer_test_module = b.createModule(.{
        .root_source_file = b.path("src/transducer.zig"),
        .target = target,
        .optimize = optimize,
    });
    const transducer_tests = b.addTest(.{ .root_module = transducer_test_module });
    const run_transducer_tests = b.addRunArtifact(transducer_tests);

    const search_test_module = b.createModule(.{
        .root_source_file = b.path("src/search.zig"),
        .target = target,
        .optimize = optimize,
    });
    const search_tests = b.addTest(.{ .root_module = search_test_module });
    search_tests.linkLibrary(pcre2_lib);
    const run_search_tests = b.addRunArtifact(search_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_opt_tests.step);
    test_step.dependOn(&run_run_tests.step);
    test_step.dependOn(&run_transducer_tests.step);
    test_step.dependOn(&run_search_tests.step);
}
