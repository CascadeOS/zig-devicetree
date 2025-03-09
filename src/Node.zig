// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

/// A node in a device tree.
pub const Node = enum(u32) {
    root = 0,
    _,

    /// Get the first subnode of `parent_node` that matches `match`.
    ///
    /// See `SubnodeIterationType` for information on the different iteration types.
    ///
    /// See `Node.Match` for more information on the different matching types.
    pub fn firstMatchingSubnode(
        parent_node: Node,
        dt: DeviceTree,
        iteration_type: SubnodeIterationType,
        match: Match,
    ) IteratorError!?Node.WithName {
        var iter = try parent_node.iterateSubnodes(dt, iteration_type, match);
        return try iter.next();
    }

    test firstMatchingSubnode {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const chosen_node = try Node.root.firstMatchingSubnode(
            dt,
            .direct_children,
            .{ .property_name = "stdout-path" },
        );
        try std.testing.expectEqualStrings("chosen", chosen_node.?.name);
    }

    /// Iterate the subnodes of `parent` that match `match`.
    ///
    /// See `SubnodeIterationType` for information on the different iteration types.
    ///
    /// See `Node.Match` for more information on the different matching types.
    pub fn iterateSubnodes(
        parent_node: Node,
        dt: DeviceTree,
        iteration_type: SubnodeIterationType,
        match: Match,
    ) IteratorError!Iterator {
        var tag_iterator = Tag.iterator(dt, @intFromEnum(parent_node));

        // move past `parent_node`
        var value: Tag.Value = undefined;
        if (try tag_iterator.next(&value)) |tuple| {
            if (tuple.tag != .begin_node) {
                @branchHint(.cold);
                return error.BadOffset;
            }
        }

        return .{
            .dt = dt,
            .tag_iterator = tag_iterator,
            .iteration_type = switch (iteration_type) {
                .direct_children => .direct_children,
                .all_children => .all_children,
            },
            .match = match,
            .match_name_no_address = switch (match) {
                .name => |name| std.mem.indexOfScalar(u8, name, '@') == null,
                else => false,
            },
        };
    }

    pub const SubnodeIterationType = enum {
        /// Iterate the direct children of the parent node.
        direct_children,

        /// Iterate all children of the parent node.
        all_children,
    };

    test iterateSubnodes {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        // any - direct children
        {
            var iter = try Node.root.iterateSubnodes(
                dt,
                .direct_children,
                .any,
            );

            var number_of_nodes: usize = 0;

            while (try iter.next()) |_| {
                number_of_nodes += 1;
            }

            try shared.customExpectEqual(number_of_nodes, 11);
        }

        // any - all children
        {
            var iter = try Node.root.iterateSubnodes(
                dt,
                .all_children,
                .any,
            );

            var number_of_nodes: usize = 0;

            while (try iter.next()) |_| {
                number_of_nodes += 1;
            }

            try shared.customExpectEqual(number_of_nodes, 30);
        }

        const soc_node = (try dt.firstMatchingNode(.{ .name = "soc" })).?.node;

        // by name - with address
        {
            var iter = try soc_node.iterateSubnodes(dt, .all_children, .{
                .name = "plic@c000000",
            });

            const plic_node = try iter.next();
            try std.testing.expectEqualStrings("plic@c000000", plic_node.?.name);
        }

        // by name - without address
        {
            var iter = try soc_node.iterateSubnodes(dt, .all_children, .{
                .name = "test",
            });

            const test_node = try iter.next();
            try std.testing.expectEqualStrings("test@100000", test_node.?.name);
        }

        // by property name
        {
            var iter = try soc_node.iterateSubnodes(dt, .all_children, .{
                .property_name = "interrupt-controller",
            });

            const plic_node = try iter.next();
            try std.testing.expectEqualStrings("plic@c000000", plic_node.?.name);
        }

        // by property value
        {
            var iter = try Node.root.iterateSubnodes(dt, .all_children, .{
                .property_value = .{
                    .name = "mmu-type",
                    .value = .fromString("riscv,sv57"),
                },
            });

            const cpu0_node = try iter.next();
            try std.testing.expectEqualStrings("cpu@0", cpu0_node.?.name);
        }
    }

    /// Get the first property of a node that matches `match`.
    ///
    /// See `Property.Match` for more information on the different matching types.
    pub fn firstMatchingProperty(node: Node, dt: DeviceTree, match: Property.Match) IteratorError!?Property {
        var iter = try node.propertyIterator(dt, match);
        return try iter.next();
    }

    test firstMatchingProperty {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const virtio_node = (try dt.firstMatchingNode(.{ .name = "virtio_mmio@10007000" })).?.node;

        const interrupts_property = (try virtio_node.firstMatchingProperty(dt, .{
            .name = "interrupts",
        })).?;

        try shared.customExpectEqual(interrupts_property.value.toU32(), 0x07);
    }

    /// Iterate over the properties of a node that match `match`.
    ///
    /// See `Property.Match` for more information on the different matching types.
    pub fn propertyIterator(node: Node, dt: DeviceTree, match: Property.Match) IteratorError!Property.Iterator {
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
        const compatible_property = (try node.firstMatchingProperty(
            dt,
            .{ .name = "compatible" },
        )) orelse return false;

        var string_list_iter = compatible_property.value.stringListIterator();
        while (try string_list_iter.next()) |string| {
            if (std.mem.eql(u8, string, compatible)) return true;
        }

        return false;
    }

    test checkCompatible {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const platform_bus_node = (try dt.firstMatchingNode(.{ .name = "platform-bus" })).?.node;

        // match
        try std.testing.expect(try platform_bus_node.checkCompatible(dt, "simple-bus"));

        // no match
        try std.testing.expect(!(try platform_bus_node.checkCompatible(dt, "ns16550a")));
    }

    /// Find a subnode of `parent_node` with a compatible property which lists the given `compatible` string.
    ///
    /// See `SubnodeIterationType` for more information on the different iteration types.
    pub fn firstSubnodeWithCompatible(
        parent_node: Node,
        dt: DeviceTree,
        iteration_type: SubnodeIterationType,
        compatible: []const u8,
    ) CheckCompatibleError!?Node.WithName {
        var node_iter = try parent_node.iterateSubnodes(
            dt,
            iteration_type,
            .{ .property_name = "compatible" },
        );

        while (try node_iter.next()) |node_with_name| {
            if (try node_with_name.node.checkCompatible(dt, compatible)) {
                return node_with_name;
            }
        }

        return null;
    }

    test firstSubnodeWithCompatible {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);
        const node = try Node.root.firstSubnodeWithCompatible(
            dt,
            .direct_children,
            "syscon-poweroff",
        );
        try std.testing.expectEqualStrings("poweroff", node.?.name);
    }

    /// Fetch the `#address-cells` property of a node if it exists.
    pub fn addressCells(node: Node, dt: DeviceTree) IteratorError!?u32 {
        if (try node.firstMatchingProperty(dt, .{ .name = "#address-cells" })) |prop| {
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
        if (try node.firstMatchingProperty(dt, .{ .name = "#size-cells" })) |prop| {
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
        if (try node.firstMatchingProperty(dt, .{ .name = "phandle" })) |prop| {
            return prop.value.toPHandle();
        }

        if (try node.firstMatchingProperty(dt, .{ .name = "linux,phandle" })) |prop| {
            return prop.value.toPHandle();
        }

        return null;
    }

    test pHandle {
        const dt: DeviceTree = try .fromSlice(shared.test_dtb);

        const plic_node = (try dt.firstMatchingNode(.{ .name = "plic@c000000" })).?;
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

        const pci_node = (try dt.firstMatchingNode(.{ .name = "pci" })).?.node;
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

        const soc_node = (try dt.firstMatchingNode(.{ .name = "soc" })).?.node;

        const soc_parent = try soc_node.parent(dt);
        try shared.customExpectEqual(soc_parent.?.node, Node.root);

        const clint_node = (try soc_node.firstMatchingSubnode(
            dt,
            .direct_children,
            .{ .name = "clint" },
        )).?.node;

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
            full_path.fixedWriter().writeByte('/') catch {
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
                    const writer = full_path.fixedWriter();
                    writer.writeByte('/') catch {
                        @branchHint(.cold);
                        return error.NoSpace;
                    };
                    writer.writeAll(value.begin_node) catch {
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

        const virtio_mmio_node = (try dt.firstMatchingNode(.{ .name = "virtio_mmio@10001000" })).?.node;

        const virtio_mmio_path = try virtio_mmio_node.path(dt, buf[0..]);
        try std.testing.expectEqualStrings("/soc/virtio_mmio@10001000", virtio_mmio_path);
    }

    pub const WithName = struct {
        name: [:0]const u8,
        node: Node,
    };

    /// An iterator over nodes in a device tree.
    pub const Iterator = struct {
        dt: DeviceTree,

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
        pub fn next(self: *Iterator) IteratorError!?Node.WithName {
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

                        if (try self.match.isMatch(self.dt, node_with_name, self.match_name_no_address)) {
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
                .property_name => |property_name| if (try node_with_name.node.firstMatchingProperty(
                    dt,
                    .{ .name = property_name },
                )) |_| return true,
                .property_value => |property_value| if (try node_with_name.node.firstMatchingProperty(
                    dt,
                    .{ .value = property_value },
                )) |_| return true,
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

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
