// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

/// A node in a device tree.
pub const Node = enum(u32) {
    root = 0,
    _,

    /// Iterate over the properties of a node that match `match`.
    ///
    /// See `Property.Match` for more information on the different matching types.
    pub fn propertyIterator(
        node: Node,
        dt: DeviceTree,
        match: Property.Match,
    ) IteratorError!Property.Iterator {
        var tag_iterator = Tag.iterator(dt, @intFromEnum(node));

        const match_prop_name: []const u8 = switch (match) {
            .any => undefined,
            .name => |name| name,
            .value => |prop| prop.name,
        };

        // move past `node`
        var value: Tag.Value = undefined;
        if (try tag_iterator.next(&value)) |tuple| {
            if (tuple.tag != .begin_node) {
                @branchHint(.cold);
                return error.BadOffset;
            }
        }

        return .{
            .tag_iterator = tag_iterator,
            .strings_block = dt.fdt.ptr + dt.strings_block_offset,
            .offset_limit = dt.strings_block_offset_limit,

            .match = match,
            .match_prop_name = match_prop_name,
        };
    }

    test propertyIterator {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        // any
        {
            var iter = try Node.root.propertyIterator(dt, .any);

            try std.testing.expectEqualStrings("#address-cells", (try iter.next()).?.name);
            try std.testing.expectEqualStrings("#size-cells", (try iter.next()).?.name);
            try std.testing.expectEqualStrings("compatible", (try iter.next()).?.name);
            try std.testing.expectEqualStrings("model", (try iter.next()).?.name);
            try shared.customExpectEqual(try iter.next(), null);
        }

        // by name - found
        {
            var iter = try Node.root.propertyIterator(dt, .{ .name = "#address-cells" });
            const address_cells_property = try iter.next();
            try shared.customExpectEqual(address_cells_property.?.value.toU32(), 0x02);
        }

        // by name - not found
        {
            var iter = try Node.root.propertyIterator(dt, .{ .name = "not-found" });
            const not_found_name_property = try iter.next();
            try shared.customExpectEqual(not_found_name_property, null);
        }

        // by value - found
        {
            var iter = try Node.root.propertyIterator(dt, .{
                .value = .{
                    .name = "compatible",
                    .value = .fromString("riscv-virtio"),
                },
            });
            const compatible_property = try iter.next();

            try std.testing.expectEqualStrings("riscv-virtio", compatible_property.?.value.toString());
        }

        // by value - not match
        {
            var iter = try Node.root.propertyIterator(dt, .{
                .value = .{
                    .name = "compatible",
                    .value = .fromString("blah"),
                },
            });
            const compatible_not_match_property = try iter.next();
            try shared.customExpectEqual(compatible_not_match_property, null);
        }
    }

    pub const CheckCompatibleError = IteratorError || Property.Value.StringListIterator.Error;

    /// Check if a node has a compatible property which lists the given `compatible` string.
    pub fn checkCompatible(node: Node, dt: DeviceTree, compatible: []const u8) CheckCompatibleError!bool {
        var compatible_iter = try node.compatibleIterator(dt);

        while (try compatible_iter.next()) |string| {
            if (std.mem.eql(u8, string, compatible)) return true;
        }

        return false;
    }

    test checkCompatible {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const platform_bus_node = blk: {
            var node_iter = try dt.nodeIterator(
                .root,
                .all_children,
                .{ .name = "platform-bus" },
            );
            break :blk (try node_iter.next(dt)).?.node;
        };

        // match
        try std.testing.expect(try platform_bus_node.checkCompatible(dt, "simple-bus"));

        // no match
        try std.testing.expect(!(try platform_bus_node.checkCompatible(dt, "ns16550a")));
    }

    /// Fetch an iterator over the values of the `compatible` property of a node.
    ///
    /// Returns an empty iterator if the node does not have a `compatible` property.
    pub fn compatibleIterator(node: Node, dt: DeviceTree) IteratorError!Property.Value.StringListIterator {
        var property_iterator = try node.propertyIterator(
            dt,
            .{ .name = "compatible" },
        );

        const compatible_property = (try property_iterator.next()) orelse
            return .{ .string_list = &.{} };
        return compatible_property.value.stringListIterator();
    }

    test compatibleIterator {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const test_node = blk: {
            var node_iter = try dt.nodeIterator(
                .root,
                .all_children,
                .{ .name = "test@100000" },
            );
            break :blk (try node_iter.next(dt)).?.node;
        };

        var compatible_iter = try test_node.compatibleIterator(dt);
        try std.testing.expectEqualStrings("sifive,test1", (try compatible_iter.next()).?);
        try std.testing.expectEqualStrings("sifive,test0", (try compatible_iter.next()).?);
        try std.testing.expectEqualStrings("syscon", (try compatible_iter.next()).?);
        try shared.customExpectEqual(try compatible_iter.next(), null);
    }

    /// Fetch the `#address-cells` property of a node if it exists.
    pub fn addressCells(node: Node, dt: DeviceTree) IteratorError!?u32 {
        var property_iterator = try node.propertyIterator(
            dt,
            .{ .name = "#address-cells" },
        );

        if (try property_iterator.next()) |prop| {
            return prop.value.toU32();
        }

        return null;
    }

    test addressCells {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        try shared.customExpectEqual(
            try Node.root.addressCells(dt),
            0x02,
        );
    }

    /// Fetch the `#size-cells` property of a node if it exists.
    pub fn sizeCells(node: Node, dt: DeviceTree) IteratorError!?u32 {
        var property_iterator = try node.propertyIterator(
            dt,
            .{ .name = "#size-cells" },
        );

        if (try property_iterator.next()) |prop| {
            return prop.value.toU32();
        }

        return null;
    }

    test sizeCells {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        try shared.customExpectEqual(
            try Node.root.sizeCells(dt),
            0x02,
        );
    }

    /// Fetch the `phandle` property of a node if it exists.
    ///
    /// Checks for both `phandle` and `linux,phandle` properties.
    pub fn pHandle(node: Node, dt: DeviceTree) IteratorError!?PHandle {
        {
            var property_iterator = try node.propertyIterator(
                dt,
                .{ .name = "phandle" },
            );

            if (try property_iterator.next()) |prop| {
                return prop.value.toPHandle();
            }
        }

        {
            var property_iterator = try node.propertyIterator(
                dt,
                .{ .name = "linux,phandle" },
            );

            if (try property_iterator.next()) |prop| {
                return prop.value.toPHandle();
            }
        }

        return null;
    }

    test pHandle {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const plic_node = blk: {
            var node_iter = try dt.nodeIterator(
                .root,
                .all_children,
                .{ .name = "plic@c000000" },
            );
            break :blk (try node_iter.next(dt)).?;
        };
        try std.testing.expectEqualStrings("plic@c000000", plic_node.name);

        try shared.customExpectEqual(plic_node.node.pHandle(dt), @enumFromInt(0x03));
    }

    /// Retrieve the depth of a node.
    ///
    /// The root node has depth 0, its immediate subnodes depth 1 and so forth.
    ///
    /// NOTE: This function is expensive, as it must scan the devicetree from the start to the `node`.
    pub fn depth(node: Node, dt: DeviceTree) IteratorError!usize {
        var tag_iterator = Tag.iterator(dt, @intFromEnum(Node.root));

        var value: Tag.Value = undefined; // this value is not used
        var current_depth: usize = 0;

        while (try tag_iterator.next(&value)) |tuple| {
            switch (tuple.tag) {
                .begin_node => {
                    if (tuple.offset == @intFromEnum(node)) return current_depth;

                    current_depth += 1;
                },
                .end_node => {
                    if (current_depth == 0) {
                        @branchHint(.cold);
                        return error.Truncated;
                    }
                    current_depth -= 1;
                },
                else => {},
            }
        } else {
            @branchHint(.cold);
        }

        return error.BadOffset;
    }

    test depth {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        try shared.customExpectEqual(try Node.root.depth(dt), 0);

        const pci_node = blk: {
            var node_iter = try dt.nodeIterator(
                .root,
                .all_children,
                .{ .name = "pci" },
            );
            break :blk (try node_iter.next(dt)).?.node;
        };
        try shared.customExpectEqual(try pci_node.depth(dt), 2);
    }

    /// Retrieve the parent of a node (that is, it finds the the node which contains `node` as a subnode).
    ///
    /// A `null` value is only possible for the root node.
    ///
    /// NOTE: This function is expensive, as it must scan the device tree structure from the start to
    /// `node`, *twice*.
    pub fn parent(node: Node, dt: DeviceTree) IteratorError!?Node.WithName {
        const own_depth = try node.depth(dt);

        if (own_depth == 0) return null;
        const parent_depth = own_depth - 1;

        var tag_iterator = Tag.iterator(dt, @intFromEnum(Node.root));

        var value: Tag.Value = undefined;
        var current_depth: usize = 0;
        var candidiate_parent: Node.WithName = undefined;

        while (try tag_iterator.next(&value)) |tuple| {
            switch (tuple.tag) {
                .begin_node => {
                    if (tuple.offset == @intFromEnum(node)) {
                        return candidiate_parent;
                    }

                    if (parent_depth == current_depth) {
                        candidiate_parent = .{
                            .name = value.begin_node,
                            .node = @enumFromInt(tuple.offset),
                        };
                    }

                    current_depth += 1;
                },
                .end_node => {
                    if (current_depth == 0) {
                        @branchHint(.cold);
                        return error.Truncated;
                    }
                    current_depth -= 1;
                },
                else => {},
            }
        } else {
            @branchHint(.cold);
        }

        return error.BadOffset;
    }

    test parent {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        try shared.customExpectEqual(try Node.root.parent(dt), null);

        const soc_node = blk: {
            var node_iter = try dt.nodeIterator(
                .root,
                .all_children,
                .{ .name = "soc" },
            );
            break :blk (try node_iter.next(dt)).?.node;
        };

        const soc_parent = try soc_node.parent(dt);
        try shared.customExpectEqual(soc_parent.?.node, Node.root);

        const clint_node = blk: {
            var node_iter = try dt.nodeIterator(
                soc_node,
                .direct_children,
                .{ .name = "clint" },
            );
            break :blk (try node_iter.next(dt)).?.node;
        };

        const clint_parent = try clint_node.parent(dt);
        try shared.customExpectEqual(clint_parent.?.node, soc_node);
    }

    pub const PathError = error{
        NoSpace,
    } || IteratorError;

    /// Retrieve the full path of `node`.
    ///
    /// The returned path is stored in the provided buffer `buf`.
    ///
    /// NOTE: This function is expensive, as it must scan the device tree structure from the start to `node`.
    pub fn path(node: Node, dt: DeviceTree, buf: []u8) PathError![]const u8 {
        var value: Tag.Value = undefined;
        var full_path: std.ArrayListUnmanaged(u8) = .initBuffer(buf);

        if (node == Node.root) {
            full_path.printBounded("/", .{}) catch {
                @branchHint(.cold);
                return error.NoSpace;
            };
            return full_path.items;
        }

        var tag_iterator = Tag.iterator(dt, @intFromEnum(Node.root));

        // move past `root`
        if (try tag_iterator.next(&value)) |tuple| {
            if (tuple.tag != .begin_node) {
                @branchHint(.cold);
                return error.BadOffset;
            }
        }

        while (try tag_iterator.next(&value)) |tuple| {
            switch (tuple.tag) {
                .begin_node => {
                    full_path.printBounded("/", .{}) catch {
                        @branchHint(.cold);
                        return error.NoSpace;
                    };
                    full_path.printBounded("{s}", .{value.begin_node}) catch {
                        @branchHint(.cold);
                        return error.NoSpace;
                    };

                    if (tuple.offset == @intFromEnum(node)) return full_path.items;
                },
                .end_node => {
                    const new_len = std.mem.lastIndexOfScalar(u8, full_path.items, '/') orelse
                        {
                            @branchHint(.cold);
                            break;
                        };
                    full_path.shrinkRetainingCapacity(new_len);
                },
                else => {},
            }
        } else {
            @branchHint(.cold);
        }

        return error.BadOffset;
    }

    test path {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        var buf: [256]u8 = undefined;

        const root_path = try Node.root.path(dt, buf[0..]);
        try std.testing.expectEqualStrings("/", root_path);

        const virtio_mmio_node = blk: {
            var node_iter = try dt.nodeIterator(
                .root,
                .all_children,
                .{ .name = "virtio_mmio@10001000" },
            );
            break :blk (try node_iter.next(dt)).?.node;
        };

        const virtio_mmio_path = try virtio_mmio_node.path(dt, buf[0..]);
        try std.testing.expectEqualStrings("/soc/virtio_mmio@10001000", virtio_mmio_path);
    }

    pub const WithName = struct {
        name: [:0]const u8,
        node: Node,
    };

    /// An iterator over nodes in a device tree.
    pub const Iterator = struct {
        tag_iterator: Tag.Iterator,
        iteration_type: IterationType,
        match: Match,

        /// Used only when `match` is `.name`.
        ///
        /// If `true`, the match name does not include the address specifier.
        match_name_no_address: bool,

        /// Tracks the relative depth of the current node relative to the parent node.
        ///
        /// The value is only used when `iteration_type` is `.direct_children` or `.all_children` but is unconditionally
        /// incremented and decremented.
        relative_depth: i32 = 0,

        /// Get the next node.
        pub fn next(self: *Iterator, dt: DeviceTree) IteratorError!?Node.WithName {
            // used only when `match` is `.name`
            var value: Tag.Value = undefined;
            while (try self.tag_iterator.next(&value)) |tuple| {
                switch (tuple.tag) {
                    .begin_node => {
                        self.relative_depth += 1;

                        if (self.iteration_type == .direct_children and self.relative_depth != 1) {
                            continue;
                        }

                        const node_with_name: WithName = .{
                            .name = value.begin_node,
                            .node = @enumFromInt(tuple.offset),
                        };

                        if (try self.match.isMatch(dt, node_with_name, self.match_name_no_address)) {
                            return node_with_name;
                        }
                    },
                    .end_node => {
                        if (self.iteration_type != .all) {
                            if (self.relative_depth == 0) {
                                @branchHint(.unlikely);
                                self.tag_iterator.offset = null;
                                return null;
                            }
                        }

                        self.relative_depth -= 1;
                    },
                    .nop => unreachable,
                    else => {},
                }
            }
            return null;
        }

        const IterationType = enum {
            all,
            direct_children,
            all_children,
        };
    };

    pub const Match = union(enum) {
        /// Match any node.
        any,

        /// Match a node by name.
        ///
        /// The unit address can be omitted.
        name: []const u8,

        /// Match a node by property name.
        property_name: []const u8,

        /// Match a node by property name and value.
        property_value: Property,

        inline fn isMatch(match: Match, dt: DeviceTree, node_with_name: Node.WithName, match_name_no_address: bool) !bool {
            switch (match) {
                .any => return true,
                .name => |match_name| {
                    const name = node_with_name.name;

                    if (name.len < match_name.len)
                        return false;

                    if (!std.mem.eql(u8, name[0..match_name.len], match_name))
                        return false;

                    if (name[match_name.len] == 0)
                        return true;

                    if (match_name_no_address and name[match_name.len] == '@')
                        return true;
                },
                .property_name => |property_name| {
                    var property_iter = try node_with_name.node.propertyIterator(
                        dt,
                        .{ .name = property_name },
                    );
                    if (try property_iter.next()) |_| return true;
                },
                .property_value => |property_value| {
                    var property_iter = try node_with_name.node.propertyIterator(
                        dt,
                        .{ .value = property_value },
                    );
                    if (try property_iter.next()) |_| return true;
                },
            }

            return false;
        }
    };
};

const std = @import("std");
const shared = @import("shared.zig");

const DeviceTree = @import("DeviceTree.zig");
const Tag = @import("Tag.zig").Tag;
const Property = @import("Property.zig");
const PHandle = @import("PHandle.zig").PHandle;
const IteratorError = DeviceTree.IteratorError;
const CompatibleMatchIterator = DeviceTree.CompatibleMatchIterator;

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
