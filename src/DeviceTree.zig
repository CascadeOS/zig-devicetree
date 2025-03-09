// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

//! A read-only Flattened Device Tree (DTB) API.
//!
//! Supports versions 2 through 17.
//!
//! Compatible with [Devicetree Specification v0.4](https://github.com/devicetree-org/devicetree-specification/releases/tag/v0.4).

const DeviceTree = @This();

fdt: []align(8) const u8,

version: Version,

/// The offset of the structure block from the beginning of `fdt`.
structure_block_offset: u32,

/// The limit of any offsets into the structure block.
///
/// This may or may not be the actual size of the structure block.
structure_block_offset_limit: u32,

/// The offset of the strings block from the beginning of `fdt`.
strings_block_offset: u32,

/// The limit of any offsets into the strings block.
///
/// This may or may not be the actual size of the strings block.
strings_block_offset_limit: u32,

/// The offset of the memory reservation block.
memory_reservation_block_offset: u32,

pub const FromSliceError = error{
    /// The slice is too small to contain a valid DeviceTree of any version.
    BufferTooSmall,

    /// The length of the provided slice does not match the length of the DeviceTree.
    BufferSizeMismatch,
} || FromPtrError;

/// Initialize a DeviceTree from a slice of an FDT in memory.
pub fn fromSlice(slice: []align(8) const u8) FromSliceError!DeviceTree {
    if (slice.len < Version.v1.headerSize()) {
        @branchHint(.cold);
        return FromSliceError.BufferTooSmall;
    }

    const dt = try fromPtr(slice.ptr);
    if (dt.fdt.len != slice.len) {
        @branchHint(.cold);
        return FromSliceError.BufferSizeMismatch;
    }

    return dt;
}

test fromSlice {
    const fdt = try fromSlice(test_dtb);
    try customExpectEqual(fdt.version, .v17);
    try customExpectEqual(fdt.structure_block_offset, 88);
    try customExpectEqual(fdt.structure_block_offset_limit, 5076);
    try customExpectEqual(fdt.strings_block_offset, 5164);
    try customExpectEqual(fdt.strings_block_offset_limit, 503);
    try customExpectEqual(fdt.memory_reservation_block_offset, 40);
}

pub const FromPtrError = error{
    /// The header's magic word is incorrect.
    BadMagic,

    /// The value of the header's `totalsize` field is too small to contain the header.
    TotalSizeTooSmall,

    /// This version of DeviceTree is not supported.
    VersionNotSupported,

    /// The `version` and/or `last_compatible_version` fields in the header are invalid.
    InvalidVersion,

    /// The `off_dt_struct` field in the header is invalid.
    StructureBlockOffsetInvalid,

    /// The `off_dt_struct` field in the header is not aligned to 4 bytes.
    StructureBlockNotAligned,

    /// The `off_dt_strings` field in the header is invalid.
    StringsBlockOffsetInvalid,

    /// The `off_mem_rsvmap` field in the header is invalid.
    MemoryReservationBlockOffsetInvalid,

    /// The `off_mem_rsvmap` field in the header is not aligned to 8 bytes.
    MemoryReservationBlockNotAligned,

    /// The value of the `size_dt_strings` field in the header causes the strings block to overflow.
    StringsBlockOverflow,

    /// The value of the `size_dt_struct` field in the header causes the structure block to overflow.
    StructureBlockOverflow,
};

/// Initialize a DeviceTree from a pointer to an FDT in memory.
pub fn fromPtr(ptr: [*]align(8) const u8) FromPtrError!DeviceTree {
    const raw_header: *const RawHeader = @ptrCast(ptr);

    if (bigToNative(u32, raw_header.magic) != RawHeader.MAGIC) {
        @branchHint(.cold);
        return FromPtrError.BadMagic;
    }

    const total_size = bigToNative(u32, raw_header.totalsize);
    if (total_size < Version.v1.headerSize()) {
        @branchHint(.cold);
        return FromPtrError.TotalSizeTooSmall;
    }

    const version: Version = @enumFromInt(bigToNative(u32, raw_header.version));
    const last_compatible_version: Version = @enumFromInt(bigToNative(u32, raw_header.last_comp_version));

    if (version.lessThan(last_compatible_version)) {
        @branchHint(.cold);
        return FromPtrError.InvalidVersion;
    }

    if (version.lessThan(.oldest_supported) or last_compatible_version.greaterThan(.newest_supported)) {
        @branchHint(.cold);
        return FromPtrError.VersionNotSupported;
    }

    const header_size = version.headerSize();
    if (total_size < header_size) {
        @branchHint(.cold);
        return FromPtrError.TotalSizeTooSmall;
    }

    const structure_block_offset = bigToNative(u32, raw_header.off_dt_struct);
    if (structure_block_offset < header_size or structure_block_offset > total_size) {
        @branchHint(.cold);
        return FromPtrError.StructureBlockOffsetInvalid;
    }
    if (!std.mem.isAligned(structure_block_offset, 4)) {
        @branchHint(.cold);
        return FromPtrError.StructureBlockNotAligned;
    }

    const strings_block_offset = bigToNative(u32, raw_header.off_dt_strings);
    if (strings_block_offset < header_size or strings_block_offset > total_size) {
        @branchHint(.cold);
        return FromPtrError.StringsBlockOffsetInvalid;
    }

    const memory_reservation_block_offset = bigToNative(u32, raw_header.off_mem_rsvmap);
    if (memory_reservation_block_offset < header_size or memory_reservation_block_offset > total_size) {
        @branchHint(.cold);
        return FromPtrError.MemoryReservationBlockOffsetInvalid;
    }
    if (!std.mem.isAligned(memory_reservation_block_offset, 8)) {
        @branchHint(.cold);
        return FromPtrError.MemoryReservationBlockNotAligned;
    }

    const strings_block_size = if (version.greaterThanOrEqual(.v3)) blk: {
        const strings_block_size = bigToNative(u32, raw_header.size_dt_strings);
        if (strings_block_offset + strings_block_size > total_size) {
            @branchHint(.cold);
            return FromPtrError.StringsBlockOverflow;
        }
        break :blk strings_block_size;
    } else null;

    const structure_block_size = if (version.greaterThanOrEqual(.v17)) blk: {
        const structure_block_size = bigToNative(u32, raw_header.size_dt_struct);
        if (structure_block_offset + structure_block_size > total_size) {
            @branchHint(.cold);
            return FromPtrError.StructureBlockOverflow;
        }
        break :blk structure_block_size;
    } else null;

    return .{
        .fdt = ptr[0..total_size],
        .version = version,
        .structure_block_offset = structure_block_offset,
        .structure_block_offset_limit = if (structure_block_size) |size|
            size
        else
            @intCast(total_size - structure_block_offset),
        .strings_block_offset = strings_block_offset,
        .strings_block_offset_limit = if (strings_block_size) |size|
            size
        else
            @intCast(total_size - strings_block_offset),
        .memory_reservation_block_offset = memory_reservation_block_offset,
    };
}

test fromPtr {
    const dt = try fromPtr(test_dtb.ptr);
    try customExpectEqual(dt.version, .v17);
    try customExpectEqual(dt.structure_block_offset, 88);
    try customExpectEqual(dt.structure_block_offset_limit, 5076);
    try customExpectEqual(dt.strings_block_offset, 5164);
    try customExpectEqual(dt.strings_block_offset_limit, 503);
    try customExpectEqual(dt.memory_reservation_block_offset, 40);
}

/// Get the physical CPU ID of the system's boot CPU.
///
/// It shall be identical to the physical ID given in the `reg` property of the boot CPU's node within the devicetree.
///
/// Available from version 2, returns null if the version is less than 2.
pub fn bootCpuid(dt: DeviceTree) ?u32 {
    if (dt.version.lessThan(.v2)) return null;

    const raw_header: *const RawHeader = @ptrCast(dt.fdt.ptr);
    return bigToNative(u32, raw_header.boot_cpuid_phys);
}

test bootCpuid {
    const dt = try fromPtr(test_dtb.ptr);
    try customExpectEqual(bootCpuid(dt), 0);
}

/// Iterate over all memory reservations in a device tree.
pub fn memoryReservationIterator(dt: DeviceTree) MemoryReservation.Iterator {
    return .{
        .fdt_ptr = dt.fdt.ptr,
        .offset_limit = @intCast(dt.fdt.len - dt.memory_reservation_block_offset),
        .offset = dt.memory_reservation_block_offset,
    };
}

test memoryReservationIterator {
    const dt: DeviceTree = try .fromSlice(test_dtb);
    var reservations = dt.memoryReservationIterator();
    try customExpectEqual(
        try reservations.next(),
        .{ .address = 0x10000000, .size = 0x4000 },
    );
    try customExpectEqual(
        try reservations.next(),
        .{ .address = 0x12345678, .size = 0x1234 },
    );
    try customExpectEqual(try reservations.next(), null);
}

/// A physical memory area that is reserved.
pub const MemoryReservation = struct {
    /// The physical address of the reservation.
    address: u64,

    /// The size of the reservation in bytes.
    size: u64,

    /// An iterator over memory reservations in a device tree.
    pub const Iterator = struct {
        fdt_ptr: [*]align(8) const u8,
        offset_limit: u32,
        offset: u32,

        /// Get the next memory reservation.
        pub fn next(self: *Iterator) IteratorError!?MemoryReservation {
            if (self.offset + @sizeOf(MemoryReservation.Raw) > self.offset_limit) {
                @branchHint(.cold);
                return IteratorError.Truncated;
            }
            const reservation: MemoryReservation = .read(@alignCast(self.fdt_ptr[self.offset..]));
            if (reservation.address == 0 and reservation.size == 0) {
                @branchHint(.unlikely);
                return null;
            }
            self.offset += @sizeOf(MemoryReservation.Raw);
            return reservation;
        }
    };

    fn read(ptr: [*]align(8) const u8) MemoryReservation {
        const raw: *const Raw = @ptrCast(ptr);
        return .{
            .address = bigToNative(u64, raw.address),
            .size = bigToNative(u64, raw.size),
        };
    }

    const Raw = extern struct {
        address: u64,
        size: u64,
    };
};

/// Retrieve the path referenced by a given alias.
///
/// That is, the value of the property named `alias` in the node '/aliases'.
pub fn getAlias(dt: DeviceTree, alias: []const u8) IteratorError!?[]const u8 {
    const alias_node = (try Node.root.firstMatchingSubnode(
        dt,
        .direct_children,
        .{ .name = "aliases" },
    )) orelse return null;

    if (try alias_node.node.firstMatchingProperty(dt, .{ .name = alias })) |prop| {
        return prop.value.toString();
    }

    return null;
}

test getAlias {
    const dt: DeviceTree = try .fromSlice(test_dtb);
    const path = try getAlias(dt, "off");
    try std.testing.expectEqualStrings("/poweroff", path.?);
}

/// Get the first node in a device tree that matches `match`.
///
/// See `Node.Match` for more information on the different matching types.
pub fn firstMatchingNode(dt: DeviceTree, match: Node.Match) IteratorError!?Node.WithName {
    var iter = nodeIterator(dt, match);
    return try iter.next();
}

test firstMatchingNode {
    const dt: DeviceTree = try .fromSlice(test_dtb);

    const node = try firstMatchingNode(dt, .{ .property_name = "device_type" });
    try std.testing.expectEqualStrings("memory@80000000", node.?.name);
}

/// Find a node with a compatible property which lists the given `compatible` string.
pub fn firstNodeWithCompatible(
    dt: DeviceTree,
    compatible: []const u8,
) Node.CheckCompatibleError!?Node.WithName {
    var node_iter = dt.nodeIterator(.{ .property_name = "compatible" });

    while (try node_iter.next()) |node_with_name| {
        if (try node_with_name.node.checkCompatible(dt, compatible)) {
            return node_with_name;
        }
    }

    return null;
}

test firstNodeWithCompatible {
    const dt: DeviceTree = try .fromSlice(test_dtb);
    const node = try dt.firstNodeWithCompatible("pci-host-ecam-generic");
    try std.testing.expectEqualStrings("pci@30000000", node.?.name);
}

/// Iterate over all nodes in a device tree that match `match`.
///
/// See `Node.Match` for more information on the different matching types.
pub fn nodeIterator(dt: DeviceTree, match: Node.Match) Node.Iterator {
    return .{
        .dt = dt,
        .tag_iterator = dt.tagIterator(@intFromEnum(Node.root)),
        .iteration_type = .all,
        .match = match,
        .match_name_no_address = switch (match) {
            .name => |name| std.mem.indexOfScalar(u8, name, '@') == null,
            else => false,
        },
    };
}

test nodeIterator {
    const dt: DeviceTree = try .fromSlice(test_dtb);

    // any
    {
        var iter = dt.nodeIterator(.any);

        var number_of_nodes: usize = 0;

        while (try iter.next()) |_| {
            number_of_nodes += 1;
        }

        try customExpectEqual(number_of_nodes, 31);
    }

    // by name - with address
    {
        var iter = dt.nodeIterator(.{ .name = "plic@c000000" });

        const plic_node = try iter.next();
        try std.testing.expectEqualStrings("plic@c000000", plic_node.?.name);
    }

    // by name - without address
    {
        var iter = dt.nodeIterator(.{ .name = "pci" });

        const pci_node = try iter.next();
        try std.testing.expectEqualStrings("pci@30000000", pci_node.?.name);
    }

    // by property name
    {
        var iter = dt.nodeIterator(.{ .property_name = "clock-frequency" });

        const serial_node = try iter.next();
        try std.testing.expectEqualStrings("serial@10000000", serial_node.?.name);
    }

    // by property value
    {
        var iter = dt.nodeIterator(.{
            .property_value = .{
                .name = "mmu-type",
                .value = .fromString("riscv,sv57"),
            },
        });

        const cpu0_node = try iter.next();
        try std.testing.expectEqualStrings("cpu@0", cpu0_node.?.name);
    }
}

pub const NodeFromPathError = error{
    /// The path is invalid.
    BadPath,
} || IteratorError;

/// Find a node by its full path.
///
/// Each path component may omit the unit address portion.
///
/// If the path is not absolute (i.e. does not begin with '/'), the first component is treated as an alias.
/// That is, the property by that name is looked up in the /aliases node, and the value of that property used in
/// place of that first component.
///
/// For example, for this small fragment:
/// ```dts
/// / {
///     aliases {
///         i2c2 = &foo; // RHS compiles to "/soc@0/i2c@30a40000"
///     };
///     soc@0 {
///         foo: i2c@30a40000 {
///             eeprom@52 {
///             };
///         };
///     };
///};
/// ```
/// these would be equivalent:
///  - `/soc@0/i2c@30a40000/eeprom@52`
///  - `i2c2/eeprom@52`
pub fn nodeFromPath(dt: DeviceTree, path: []const u8) NodeFromPathError!?Node.WithName {
    if (path.len == 0) {
        @branchHint(.cold);
        return error.BadPath;
    }

    const root_with_name: Node.WithName = .{
        .name = "",
        .node = Node.root,
    };

    if (std.mem.eql(u8, path, "/")) return root_with_name;

    var component_iter = std.mem.splitScalar(u8, path, '/');

    var current_node: Node.WithName = if (path[0] != '/') blk: {
        // starts with an alias
        const alias = component_iter.next() orelse {
            @branchHint(.cold);
            return error.BadPath;
        };
        const resolved_alias = (try dt.getAlias(alias)) orelse {
            @branchHint(.cold);
            // non-existent alias
            return null;
        };
        break :blk (try dt.nodeFromPath(resolved_alias)) orelse root_with_name;
    } else blk: {
        std.debug.assert(component_iter.next().?.len == 0);
        break :blk root_with_name;
    };

    while (component_iter.next()) |component| {
        if (component.len == 0) {
            @branchHint(.cold);
            return error.BadPath;
        }

        current_node = (try current_node.node.firstMatchingSubnode(
            dt,
            .direct_children,
            .{ .name = component },
        )) orelse return null;
    }

    return current_node;
}

test nodeFromPath {
    const dt: DeviceTree = try .fromSlice(test_dtb);

    const root_node = (try dt.nodeFromPath("/")).?.node;
    try customExpectEqual(root_node, Node.root);

    // with address
    const serial_node = (try dt.nodeFromPath("/soc/serial@10000000")).?;
    try std.testing.expectEqualStrings("serial@10000000", serial_node.name);

    // without address
    const rtc_node = (try dt.nodeFromPath("/soc/rtc")).?;
    try std.testing.expectEqualStrings("rtc@101000", rtc_node.name);

    // alias
    const memory_node = (try dt.nodeFromPath("memory")).?;
    try std.testing.expectEqualStrings("memory@80000000", memory_node.name);

    // non-existent alias
    try customExpectEqual(try dt.nodeFromPath("no_alias"), null);

    try std.testing.expectError(error.BadPath, dt.nodeFromPath(""));
    try std.testing.expectError(error.BadPath, dt.nodeFromPath("//"));
    try std.testing.expectError(error.BadPath, dt.nodeFromPath("/soc/serial@10000000/"));
}

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
        const dt: DeviceTree = try .fromSlice(test_dtb);

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
        var tag_iterator = dt.tagIterator(@intFromEnum(parent_node));

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
        const dt: DeviceTree = try .fromSlice(test_dtb);

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

            try customExpectEqual(number_of_nodes, 11);
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

            try customExpectEqual(number_of_nodes, 30);
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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        const virtio_node = (try dt.firstMatchingNode(.{ .name = "virtio_mmio@10007000" })).?.node;

        const interrupts_property = (try virtio_node.firstMatchingProperty(dt, .{
            .name = "interrupts",
        })).?;

        try customExpectEqual(interrupts_property.value.toU32(), 0x07);
    }

    /// Iterate over the properties of a node that match `match`.
    ///
    /// See `Property.Match` for more information on the different matching types.
    pub fn propertyIterator(node: Node, dt: DeviceTree, match: Property.Match) IteratorError!Property.Iterator {
        var tag_iterator = dt.tagIterator(@intFromEnum(node));

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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        // any
        {
            var iter = try Node.root.propertyIterator(dt, .any);

            try std.testing.expectEqualStrings("#address-cells", (try iter.next()).?.name);
            try std.testing.expectEqualStrings("#size-cells", (try iter.next()).?.name);
            try std.testing.expectEqualStrings("compatible", (try iter.next()).?.name);
            try std.testing.expectEqualStrings("model", (try iter.next()).?.name);
            try customExpectEqual(try iter.next(), null);
        }

        // by name - found
        {
            var iter = try Node.root.propertyIterator(dt, .{ .name = "#address-cells" });
            const address_cells_property = try iter.next();
            try customExpectEqual(address_cells_property.?.value.toU32(), 0x02);
        }

        // by name - not found
        {
            var iter = try Node.root.propertyIterator(dt, .{ .name = "not-found" });
            const not_found_name_property = try iter.next();
            try customExpectEqual(not_found_name_property, null);
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
            try customExpectEqual(compatible_not_match_property, null);
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
        const dt: DeviceTree = try .fromSlice(test_dtb);

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
        const dt: DeviceTree = try .fromSlice(test_dtb);
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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        try customExpectEqual(
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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        try customExpectEqual(
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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        const plic_node = (try dt.firstMatchingNode(.{ .name = "plic@c000000" })).?;
        try std.testing.expectEqualStrings("plic@c000000", plic_node.name);

        try customExpectEqual(plic_node.node.pHandle(dt), @enumFromInt(0x03));
    }

    /// Retrieve the depth of a node.
    ///
    /// The root node has depth 0, its immediate subnodes depth 1 and so forth.
    ///
    /// NOTE: This function is expensive, as it must scan the devicetree from the start to the `node`.
    pub fn depth(node: Node, dt: DeviceTree) IteratorError!usize {
        var tag_iterator = dt.tagIterator(@intFromEnum(Node.root));

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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        try customExpectEqual(try Node.root.depth(dt), 0);

        const pci_node = (try dt.firstMatchingNode(.{ .name = "pci" })).?.node;
        try customExpectEqual(try pci_node.depth(dt), 2);
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

        var tag_iterator = dt.tagIterator(@intFromEnum(Node.root));

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
        const dt: DeviceTree = try .fromSlice(test_dtb);

        try customExpectEqual(try Node.root.parent(dt), null);

        const soc_node = (try dt.firstMatchingNode(.{ .name = "soc" })).?.node;

        const soc_parent = try soc_node.parent(dt);
        try customExpectEqual(soc_parent.?.node, Node.root);

        const clint_node = (try soc_node.firstMatchingSubnode(
            dt,
            .direct_children,
            .{ .name = "clint" },
        )).?.node;

        const clint_parent = try clint_node.parent(dt);
        try customExpectEqual(clint_parent.?.node, soc_node);
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

        var tag_iterator = dt.tagIterator(@intFromEnum(Node.root));

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
        const dt: DeviceTree = try .fromSlice(test_dtb);

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

pub const PHandle = enum(u32) {
    _,

    /// Find a node by its phandle.
    pub fn node(phandle: PHandle, dt: DeviceTree) IteratorError!?Node.WithName {
        // can't use `.property_name` here because it could be either 'phandle' or 'linux,phandle'
        var node_iter = dt.nodeIterator(.any);

        while (try node_iter.next()) |node_with_name| {
            if (try node_with_name.node.pHandle(dt)) |p_handle| {
                if (p_handle == phandle) return node_with_name;
            }
        }
        return null;
    }

    test node {
        const dt: DeviceTree = try .fromSlice(test_dtb);

        const handle: PHandle = @enumFromInt(0x1);

        const cpu0_node = try handle.node(dt);
        try std.testing.expectEqualStrings("cpu@0", cpu0_node.?.name);
    }
};

/// A property of a node.
pub const Property = struct {
    name: [:0]const u8,
    value: Value,

    pub const Value = struct {
        /// The raw value of the property in big-endian format.
        _raw: []const u8,

        /// Create a `Value` from a string.
        ///
        /// The `Value` references the provided string, not a copy of the string.
        pub fn fromString(str: [:0]const u8) Value {
            return .{ ._raw = str[0 .. str.len + 1] };
        }

        /// Converts a `Value` to a string.
        ///
        /// The returned string refers to the raw property value in the devicetree, not a copy.
        pub fn toString(self: Value) [:0]const u8 {
            return self._raw[0 .. self._raw.len - 1 :0];
        }

        test "Property.Value.fromString/toString" {
            try std.testing.expectEqualStrings("foo", Value.fromString("foo").toString());
            try std.testing.expectEqualStrings("", Value.fromString("").toString());
            try std.testing.expectEqualStrings("✓", Value.fromString("✓").toString());
        }

        /// Converts a `PHandle` to a `Value`.
        ///
        /// The `buf` parameter is used to store the raw value of the `Value`.
        pub fn fromPHandle(phandle: PHandle, buf: *[4]u8) Value {
            return .fromU32(@intFromEnum(phandle), buf);
        }

        /// Converts a `Value` to a `PHandle`.
        pub fn toPHandle(self: Value) PHandle {
            return @enumFromInt(self.toU32());
        }

        test "Property.Value.fromPHandle/toPHandle" {
            const handles: []const PHandle = &.{
                @enumFromInt(0x1),
                @enumFromInt(0x12345678),
            };

            var buf: [4]u8 = undefined;

            for (handles) |handle| {
                try customExpectEqual(handle, fromPHandle(handle, &buf).toPHandle());
            }
        }

        /// Converts a `Value` to a `u32`.
        pub fn toU32(self: Value) u32 {
            const ptr: *align(1) const u32 = @ptrCast(self._raw.ptr);
            return bigToNative(u32, ptr.*);
        }

        /// Converts a `u32` to a `Value`.
        ///
        /// The `buf` parameter is used to store the raw value of the `Value`.
        pub fn fromU32(val: u32, buf: *[4]u8) Value {
            const ptr: *align(1) u32 = @ptrCast(buf);
            ptr.* = nativeToBig(u32, val);
            return Value{ ._raw = buf[0..] };
        }

        test "Property.Value.toU32/fromU32" {
            const values: []const u32 = &.{
                0x1,
                0x12345678,
            };

            var buf: [4]u8 = undefined;

            for (values) |val| {
                try customExpectEqual(val, fromU32(val, &buf).toU32());
            }
        }

        /// Converts a `Value` to a `u64`.
        pub fn toU64(self: Value) u64 {
            const ptr: *align(1) const u64 = @ptrCast(self._raw.ptr);
            return bigToNative(u64, ptr.*);
        }

        /// Converts a `u64` to a `Value`.
        ///
        /// The `buf` parameter is used to store the raw value of the `Value`.
        pub fn fromU64(val: u64, buf: *[8]u8) Value {
            const ptr: *align(1) u64 = @ptrCast(buf);
            ptr.* = nativeToBig(u64, val);
            return Value{ ._raw = buf[0..] };
        }

        test "Property.Value.toU64/fromU64" {
            const values: []const u64 = &.{
                0x1,
                0x12345678,
                0x123456789abcdef0,
            };

            var buf: [8]u8 = undefined;

            for (values) |val| {
                try customExpectEqual(val, fromU64(val, &buf).toU64());
            }
        }

        /// Iterate over the strings in a `Value` which is a string list.
        pub fn stringListIterator(self: Value) StringListIterator {
            return .{
                .string_list = self._raw,
            };
        }

        /// Create a `Value` from a comptime string list.
        pub fn fromComptimeStringList(comptime strings: []const []const u8) Value {
            return .{
                ._raw = comptime blk: {
                    var buf: []const u8 = &.{};

                    for (strings) |str| {
                        buf = buf ++ str ++ &[_]u8{0x00};
                    }

                    break :blk buf;
                },
            };
        }

        test fromComptimeStringList {
            const string_list = fromComptimeStringList(&.{
                "foo",
                "bar",
                "baz",
            });

            var iter = string_list.stringListIterator();
            try std.testing.expectEqualStrings("foo", (try iter.next()).?);
            try std.testing.expectEqualStrings("bar", (try iter.next()).?);
            try std.testing.expectEqualStrings("baz", (try iter.next()).?);
            try std.testing.expectEqual(null, try iter.next());
        }

        pub const StringListBuilder = struct {
            buf: std.ArrayListUnmanaged(u8) = .empty,

            /// Append a string to the string list.
            pub fn append(self: *StringListBuilder, allocator: std.mem.Allocator, str: []const u8) !void {
                try self.buf.appendSlice(allocator, str);
                try self.buf.append(allocator, 0x00);
            }

            /// Deinitialize the string list.
            pub fn deinit(self: *StringListBuilder, allocator: std.mem.Allocator) void {
                self.buf.deinit(allocator);
            }

            /// Get the string list as a `Value`.
            ///
            /// The `Value` references the internal buffer of the `StringListBuilder`, not a copy.
            ///
            /// Any modifications to the `StringListBuilder` after calling this function will not be reflected in the
            /// `Value`.
            pub fn toValue(self: StringListBuilder) Value {
                return .{ ._raw = self.buf.items };
            }
        };

        test StringListBuilder {
            var builder: StringListBuilder = .{};
            defer builder.deinit(std.testing.allocator);

            try builder.append(std.testing.allocator, "foo");
            try builder.append(std.testing.allocator, "bar");
            try builder.append(std.testing.allocator, "baz");
            try builder.append(std.testing.allocator, "qux");
            try builder.append(std.testing.allocator, "quux");

            const string_list = builder.toValue();

            var iter = string_list.stringListIterator();
            try std.testing.expectEqualStrings("foo", (try iter.next()).?);
            try std.testing.expectEqualStrings("bar", (try iter.next()).?);
            try std.testing.expectEqualStrings("baz", (try iter.next()).?);
            try std.testing.expectEqualStrings("qux", (try iter.next()).?);
            try std.testing.expectEqualStrings("quux", (try iter.next()).?);
            try std.testing.expectEqual(null, try iter.next());
        }

        pub const StringListIterator = struct {
            string_list: []const u8,

            pub const Error = error{
                /// The string list does not end with a null terminator.
                Truncated,
            };

            pub fn next(self: *StringListIterator) Error!?[:0]const u8 {
                if (self.string_list.len == 0) return null;

                const len = std.mem.indexOfScalar(u8, self.string_list, 0) orelse {
                    @branchHint(.cold);
                    return Error.Truncated;
                };

                const str: [:0]const u8 = self.string_list[0..len :0];
                self.string_list = self.string_list[len + 1 ..];
                return str;
            }
        };
    };

    pub const Match = union(enum) {
        /// Match any property.
        any,

        /// Match a property by name.
        name: []const u8,

        /// Match a property by name and value.
        value: Property,

        inline fn isMatch(match: Match, match_prop_name: []const u8, actual_prop: Property) bool {
            switch (match) {
                .any => return true,
                .name => return std.mem.eql(u8, actual_prop.name, match_prop_name),
                .value => |match_prop| {
                    if (!std.mem.eql(u8, actual_prop.name, match_prop_name)) return false;

                    return std.mem.eql(
                        u8,
                        actual_prop.value._raw,
                        match_prop.value._raw,
                    );
                },
            }
        }
    };

    /// An iterator over properties of a node.
    pub const Iterator = struct {
        tag_iterator: Tag.Iterator,
        strings_block: [*]const u8,
        offset_limit: u32,

        match: Match,

        /// Used only when `match` is `.name` or `.value`.
        match_prop_name: []const u8,

        /// Get the next property.
        pub fn next(self: *Iterator) IteratorError!?Property {
            const offset_limit = self.offset_limit;

            var value: Tag.Value = undefined;
            while (try self.tag_iterator.next(&value)) |tuple| {
                switch (tuple.tag) {
                    .prop => {
                        const name_start_offset = value.prop.name_offset;

                        var offset = name_start_offset;
                        while (true) : (offset += 1) {
                            if (offset >= offset_limit) {
                                @branchHint(.cold);
                                return error.Truncated;
                            }

                            if (self.strings_block[offset] == 0) break;
                        }

                        const actual_prop: Property = .{
                            .name = self.strings_block[name_start_offset..][0 .. offset - name_start_offset :0],
                            .value = .{ ._raw = value.prop.value },
                        };

                        if (self.match.isMatch(self.match_prop_name, actual_prop)) {
                            return actual_prop;
                        }
                    },
                    .nop => continue,
                    else => self.tag_iterator.offset = null,
                }
            }
            return null;
        }
    };

    const Raw = extern struct {
        len: u32,
        name_offset: u32,

        fn read(structure_block_ptr: [*]align(4) const u8) Raw {
            const ptr: *const Raw = @ptrCast(structure_block_ptr);
            var prop = ptr.*;
            prop.len = bigToNative(u32, prop.len);
            prop.name_offset = bigToNative(u32, prop.name_offset);
            return prop;
        }
    };
};

pub const Version = enum(u32) {
    v1 = 1,
    v2 = 2,
    v3 = 3,
    v16 = 16,
    v17 = 17,

    _,

    fn headerSize(version: Version) u32 {
        const val = @intFromEnum(version);

        return if (val <= 1)
            7 * @sizeOf(u32)
        else if (val <= 2)
            8 * @sizeOf(u32)
        else if (val <= 16)
            9 * @sizeOf(u32)
        else
            10 * @sizeOf(u32);
    }

    pub const oldest_supported: Version = .v2;
    pub const newest_supported: Version = .v17;

    fn lessThan(self: Version, other: Version) bool {
        return @intFromEnum(self) < @intFromEnum(other);
    }

    fn greaterThan(self: Version, other: Version) bool {
        return @intFromEnum(self) > @intFromEnum(other);
    }

    fn greaterThanOrEqual(self: Version, other: Version) bool {
        return @intFromEnum(self) >= @intFromEnum(other);
    }
};

pub const IteratorError = error{
    /// Provided offset is out of bounds or is not an offset of a valid tag.
    BadOffset,

    /// The devicetree is improperly terminated.
    Truncated,
};

/// Iterate over the tags of a device tree.
fn tagIterator(dt: DeviceTree, start_offset: u32) Tag.Iterator {
    return .{
        .structure_block = @alignCast(dt.fdt.ptr + dt.structure_block_offset),
        .offset_limit = dt.structure_block_offset_limit,
        .offset = start_offset,
    };
}

const Tag = enum(u32) {
    begin_node = nativeToBig(u32, 0x1),
    end_node = nativeToBig(u32, 0x2),
    prop = nativeToBig(u32, 0x3),
    nop = nativeToBig(u32, 0x4),
    end = nativeToBig(u32, 0x9),

    fn read(structure_block_ptr: [*]const u8) Tag {
        const ptr: *const Tag = @ptrCast(@alignCast(structure_block_ptr));
        return ptr.*;
    }

    /// An iterator over the tags of a device tree.
    const Iterator = struct {
        structure_block: [*]align(4) const u8,
        offset_limit: u32,

        offset: ?u32,

        /// Get the next tag.
        pub fn next(self: *Iterator, value: *Tag.Value) error{Truncated}!?Tag.Tuple {
            const offset_limit = self.offset_limit;
            var offset = self.offset orelse return null;

            while (true) {
                if (offset + @sizeOf(Tag) > offset_limit) {
                    @branchHint(.cold);
                    return error.Truncated;
                }

                const tag: Tag = .read(self.structure_block[offset..]);
                const tag_offset = offset;
                offset += @sizeOf(Tag);

                switch (tag) {
                    .begin_node => {
                        const name_start_offset = offset;
                        while (true) {
                            if (offset >= offset_limit) {
                                @branchHint(.cold);
                                return error.Truncated;
                            }

                            if (self.structure_block[offset] == 0) {
                                value.* = .{
                                    .begin_node = self.structure_block[name_start_offset..][0 .. offset - name_start_offset :0],
                                };
                                offset += 1; // to account for the null terminator
                                break;
                            }
                            offset += 1;
                        }

                        self.offset = std.mem.alignForward( // account for any alignment padding
                            u32,
                            offset,
                            @sizeOf(Tag),
                        );
                    },
                    .end_node => self.offset = offset,
                    .prop => {
                        if (offset + @sizeOf(Property.Raw) > offset_limit) {
                            @branchHint(.cold);
                            return error.Truncated;
                        }

                        const prop: Property.Raw = .read(@alignCast(self.structure_block[offset..]));
                        offset += @sizeOf(Property.Raw);

                        if (offset + prop.len > offset_limit) {
                            @branchHint(.cold);
                            return error.Truncated;
                        }

                        value.* = .{
                            .prop = .{
                                .value = self.structure_block[offset..][0..prop.len],
                                .name_offset = prop.name_offset,
                            },
                        };

                        self.offset = std.mem.alignForward( // account for any alignment padding
                            u32,
                            offset + prop.len,
                            @sizeOf(Tag),
                        );
                    },
                    .nop => continue,
                    .end => self.offset = null,
                }

                return .{ .tag = tag, .offset = tag_offset };
            }
        }
    };

    const Tuple = struct {
        tag: Tag,
        offset: u32,
    };

    const Value = union {
        begin_node: [:0]const u8,
        prop: struct {
            value: []const u8,
            name_offset: u32,
        },
    };
};

const RawHeader = extern struct {
    /// magic word
    magic: u32,

    /// total size of DT block
    totalsize: u32,

    /// offset to structure
    off_dt_struct: u32,

    /// offset to strings
    off_dt_strings: u32,

    /// offset to memory reserve map
    off_mem_rsvmap: u32,

    /// format version
    version: u32,

    /// last compatible version
    last_comp_version: u32,

    /// Which physical CPU id we're booting on
    ///
    /// Available from version 2
    boot_cpuid_phys: u32,

    /// size of the strings block
    ///
    /// Available from version 3
    size_dt_strings: u32,

    /// size of the structure block
    ///
    /// Available from version 17
    size_dt_struct: u32,

    const MAGIC: u32 = 0xd00dfeed;

    comptime {
        std.debug.assert(@sizeOf(@This()) == @sizeOf(u32) * 10);
    }
};

const std = @import("std");
const builtin = @import("builtin");
const bigToNative = std.mem.bigToNative;
const nativeToBig = std.mem.nativeToBig;

const test_dtb: if (builtin.is_test)
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
inline fn customExpectEqual(actual: anytype, expected: @TypeOf(actual)) !void {
    try std.testing.expectEqual(expected, actual);
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
