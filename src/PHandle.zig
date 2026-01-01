// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

pub const PHandle = enum(u32) {
    _,

    /// Find a node by its phandle.
    pub fn node(phandle: PHandle, dt: DeviceTree) IteratorError!?Node.WithName {
        // can't use `.property_name` here because it could be either 'phandle' or 'linux,phandle'
        var node_iter = try dt.nodeIterator(
            .root,
            .all_children,
            .any,
        );

        while (try node_iter.next(dt)) |node_with_name| {
            if (try node_with_name.node.pHandle(dt)) |p_handle| {
                if (p_handle == phandle) return node_with_name;
            }
        }
        return null;
    }

    test node {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const handle: PHandle = @enumFromInt(0x1);

        const cpu0_node = try handle.node(dt);
        try std.testing.expectEqualStrings("cpu@0", cpu0_node.?.name);
    }
};

const std = @import("std");
const shared = @import("shared.zig");

const DeviceTree = @import("DeviceTree.zig");
const IteratorError = DeviceTree.IteratorError;
const Node = @import("Node.zig").Node;

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
