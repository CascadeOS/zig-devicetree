// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

pub const bigToNative = std.mem.bigToNative;
pub const nativeToBig = std.mem.nativeToBig;

pub const test_dtb: if (builtin.is_test)
    []align(8) const u8
else
    void = if (builtin.is_test)
test_dtb: {
    const emded = @embedFile("test.dtb");
    const buf: [emded.len]u8 align(8) = emded.*;
    break :test_dtb &buf;
} else {};

/// A version of `std.testing.expectEqual` that flips the order of `expected` and `actual` to allow
/// expected to not use `anytype`.
pub inline fn customExpectEqual(actual: anytype, expected: @TypeOf(actual)) !void {
    try std.testing.expectEqual(expected, actual);
}

const std = @import("std");
const builtin = @import("builtin");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
