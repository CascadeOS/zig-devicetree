// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: Lee Cannon <leecannon@leecannon.xyz>

pub fn build(b: *std.Build) void {
    const devicetree_mod = b.addModule("DeviceTree", .{
        .root_source_file = b.path("src/DeviceTree.zig"),
    });
    _ = devicetree_mod;

    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    // docs
    {
        const docs_step = b.step("docs", "Build the package documentation");
        const docs_obj = b.addObject(.{
            .name = "DeviceTree",
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/DeviceTree.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        const install_docs = b.addInstallDirectory(.{
            .source_dir = docs_obj.getEmittedDocs(),
            .install_dir = .prefix,
            .install_subdir = "docs",
        });
        docs_step.dependOn(&install_docs.step);
    }

    // tests
    {
        const test_exe = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/DeviceTree.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });
        const run_test_exe = b.addRunArtifact(test_exe);

        const test_step = b.step("test", "Run tests");
        test_step.dependOn(&run_test_exe.step);
    }

    // check
    {
        const check_test_exe = b.addTest(.{
            .root_module = b.createModule(.{
                .root_source_file = b.path("src/DeviceTree.zig"),
                .target = target,
                .optimize = optimize,
            }),
        });

        const check_test_step = b.step("check", "");
        check_test_step.dependOn(&check_test_exe.step);
    }
}

const std = @import("std");
