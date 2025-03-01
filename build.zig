// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>

pub fn build(b: *std.Build) void {
    const optimize = b.standardOptimizeOption(.{});
    const target = b.standardTargetOptions(.{});

    const use_bundled_musl = b.option(
        bool,
        "use_bundled_musl",
        "Use bundled musl instead of linking libc (defaults to true for freestanding, false otherwise)",
    ) orelse (target.result.os.tag == .freestanding);

    const devicetree_mod = b.addModule("devicetree", .{
        .root_source_file = b.path("src/devicetree.zig"),
        .optimize = optimize,
        .target = target,
    });

    devicetree_mod.addIncludePath(b.path("libfdt-1.7.2"));
    devicetree_mod.addCSourceFiles(.{
        .root = b.path("libfdt-1.7.2"),
        .files = &.{
            "fdt_addresses.c",
            "fdt_check.c",
            "fdt_ro.c",
            "fdt.c",
        },
    });

    if (use_bundled_musl) {
        devicetree_mod.addCMacro("_USE_BUNDLED_MUSL_", "1");
        devicetree_mod.addCSourceFile(.{
            .file = b.path("musl/musl.c"),
            .flags = &.{"-fno-sanitize=undefined"},
        });
    } else {
        devicetree_mod.link_libc = true;
    }

    const test_exe = b.addTest(.{ .root_module = devicetree_mod });
    const run_test = b.addRunArtifact(test_exe);
    const test_step = b.step("test", "Run tests");
    test_step.dependOn(&run_test.step);

    // check step
    {
        // FIXME: `-fno-emit-bin` with c files is broken https://github.com/ziglang/zig/issues/22682
        // const check_test_exe = b.addTest(.{ .root_module = devicetree_mod });
        const check_test_step = b.step("check", "");
        check_test_step.dependOn(&test_exe.step);
    }
}

const std = @import("std");
