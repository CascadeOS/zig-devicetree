// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const devicetree_mod = b.addModule("devicetree", .{
        .root_source_file = b.path("src/devicetree.zig"),
        .optimize = optimize,
        .target = target,
    });

    const test_exe = b.addTest(.{ .root_module = devicetree_mod });
    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_test.step);

    // check step
    {
        const check_test_exe = b.addTest(.{ .root_module = devicetree_mod });
        const check_test_step = b.step("check", "");
        check_test_step.dependOn(&check_test_exe.step);
    }
}

const std = @import("std");
