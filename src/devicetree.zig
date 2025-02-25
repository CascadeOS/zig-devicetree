// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>

const std = @import("std");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
