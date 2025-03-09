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
    const fdt = try fromSlice(shared.test_dtb);
    try shared.customExpectEqual(fdt.version, .v17);
    try shared.customExpectEqual(fdt.structure_block_offset, 88);
    try shared.customExpectEqual(fdt.structure_block_offset_limit, 5076);
    try shared.customExpectEqual(fdt.strings_block_offset, 5164);
    try shared.customExpectEqual(fdt.strings_block_offset_limit, 503);
    try shared.customExpectEqual(fdt.memory_reservation_block_offset, 40);
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

    if (shared.bigToNative(u32, raw_header.magic) != RawHeader.MAGIC) {
        @branchHint(.cold);
        return FromPtrError.BadMagic;
    }

    const total_size = shared.bigToNative(u32, raw_header.totalsize);
    if (total_size < Version.v1.headerSize()) {
        @branchHint(.cold);
        return FromPtrError.TotalSizeTooSmall;
    }

    const version: Version = @enumFromInt(shared.bigToNative(u32, raw_header.version));
    const last_compatible_version: Version = @enumFromInt(shared.bigToNative(u32, raw_header.last_comp_version));

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

    const structure_block_offset = shared.bigToNative(u32, raw_header.off_dt_struct);
    if (structure_block_offset < header_size or structure_block_offset > total_size) {
        @branchHint(.cold);
        return FromPtrError.StructureBlockOffsetInvalid;
    }
    if (!std.mem.isAligned(structure_block_offset, 4)) {
        @branchHint(.cold);
        return FromPtrError.StructureBlockNotAligned;
    }

    const strings_block_offset = shared.bigToNative(u32, raw_header.off_dt_strings);
    if (strings_block_offset < header_size or strings_block_offset > total_size) {
        @branchHint(.cold);
        return FromPtrError.StringsBlockOffsetInvalid;
    }

    const memory_reservation_block_offset = shared.bigToNative(u32, raw_header.off_mem_rsvmap);
    if (memory_reservation_block_offset < header_size or memory_reservation_block_offset > total_size) {
        @branchHint(.cold);
        return FromPtrError.MemoryReservationBlockOffsetInvalid;
    }
    if (!std.mem.isAligned(memory_reservation_block_offset, 8)) {
        @branchHint(.cold);
        return FromPtrError.MemoryReservationBlockNotAligned;
    }

    const strings_block_size = if (version.greaterThanOrEqual(.v3)) blk: {
        const strings_block_size = shared.bigToNative(u32, raw_header.size_dt_strings);
        if (strings_block_offset + strings_block_size > total_size) {
            @branchHint(.cold);
            return FromPtrError.StringsBlockOverflow;
        }
        break :blk strings_block_size;
    } else null;

    const structure_block_size = if (version.greaterThanOrEqual(.v17)) blk: {
        const structure_block_size = shared.bigToNative(u32, raw_header.size_dt_struct);
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
    const dt = try fromPtr(shared.test_dtb.ptr);
    try shared.customExpectEqual(dt.version, .v17);
    try shared.customExpectEqual(dt.structure_block_offset, 88);
    try shared.customExpectEqual(dt.structure_block_offset_limit, 5076);
    try shared.customExpectEqual(dt.strings_block_offset, 5164);
    try shared.customExpectEqual(dt.strings_block_offset_limit, 503);
    try shared.customExpectEqual(dt.memory_reservation_block_offset, 40);
}

/// Get the physical CPU ID of the system's boot CPU.
///
/// It shall be identical to the physical ID given in the `reg` property of the boot CPU's node within the devicetree.
///
/// Available from version 2, returns null if the version is less than 2.
pub fn bootCpuid(dt: DeviceTree) ?u32 {
    if (dt.version.lessThan(.v2)) return null;

    const raw_header: *const RawHeader = @ptrCast(dt.fdt.ptr);
    return shared.bigToNative(u32, raw_header.boot_cpuid_phys);
}

test bootCpuid {
    const dt = try fromPtr(shared.test_dtb.ptr);
    try shared.customExpectEqual(bootCpuid(dt), 0);
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
    const dt: DeviceTree = try .fromSlice(shared.test_dtb);
    var reservations = dt.memoryReservationIterator();
    try shared.customExpectEqual(
        try reservations.next(),
        .{ .address = 0x10000000, .size = 0x4000 },
    );
    try shared.customExpectEqual(
        try reservations.next(),
        .{ .address = 0x12345678, .size = 0x1234 },
    );
    try shared.customExpectEqual(try reservations.next(), null);
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
            .address = shared.bigToNative(u64, raw.address),
            .size = shared.bigToNative(u64, raw.size),
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
    const dt: DeviceTree = try .fromSlice(shared.test_dtb);
    const path = try getAlias(dt, "off");
    try std.testing.expectEqualStrings("/poweroff", path.?);
}

/// Get the first node in a device tree that matches `match`.
///
/// See `Node.Match` for more information on the different matching types.
pub fn firstMatchingNode(dt: DeviceTree, match: Node.Match) IteratorError!?Node.WithName {
    var iter = nodeIterator(dt, match);
    return try iter.next(dt);
}

test firstMatchingNode {
    const dt: DeviceTree = try .fromSlice(shared.test_dtb);

    const node = try firstMatchingNode(dt, .{ .property_name = "device_type" });
    try std.testing.expectEqualStrings("memory@80000000", node.?.name);
}

/// Find a node with a compatible property which lists the given `compatible` string.
pub fn firstNodeWithCompatible(
    dt: DeviceTree,
    compatible: []const u8,
) Node.CheckCompatibleError!?Node.WithName {
    var node_iter = dt.nodeIterator(.{ .property_name = "compatible" });

    while (try node_iter.next(dt)) |node_with_name| {
        if (try node_with_name.node.checkCompatible(dt, compatible)) {
            return node_with_name;
        }
    }

    return null;
}

test firstNodeWithCompatible {
    const dt: DeviceTree = try .fromSlice(shared.test_dtb);
    const node = try dt.firstNodeWithCompatible("pci-host-ecam-generic");
    try std.testing.expectEqualStrings("pci@30000000", node.?.name);
}

/// Iterate over all nodes in a device tree that match `match`.
///
/// See `Node.Match` for more information on the different matching types.
pub fn nodeIterator(dt: DeviceTree, match: Node.Match) Node.Iterator {
    return .{
        .tag_iterator = Tag.iterator(dt, @intFromEnum(Node.root)),
        .iteration_type = .all,
        .match = match,
        .match_name_no_address = switch (match) {
            .name => |name| std.mem.indexOfScalar(u8, name, '@') == null,
            else => false,
        },
    };
}

test nodeIterator {
    const dt: DeviceTree = try .fromSlice(shared.test_dtb);

    // any
    {
        var iter = dt.nodeIterator(.any);

        var number_of_nodes: usize = 0;

        while (try iter.next(dt)) |_| {
            number_of_nodes += 1;
        }

        try shared.customExpectEqual(number_of_nodes, 31);
    }

    // by name - with address
    {
        var iter = dt.nodeIterator(.{ .name = "plic@c000000" });

        const plic_node = try iter.next(dt);
        try std.testing.expectEqualStrings("plic@c000000", plic_node.?.name);
    }

    // by name - without address
    {
        var iter = dt.nodeIterator(.{ .name = "pci" });

        const pci_node = try iter.next(dt);
        try std.testing.expectEqualStrings("pci@30000000", pci_node.?.name);
    }

    // by property name
    {
        var iter = dt.nodeIterator(.{ .property_name = "clock-frequency" });

        const serial_node = try iter.next(dt);
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

        const cpu0_node = try iter.next(dt);
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
    const dt: DeviceTree = try .fromSlice(shared.test_dtb);

    const root_node = (try dt.nodeFromPath("/")).?.node;
    try shared.customExpectEqual(root_node, Node.root);

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
    try shared.customExpectEqual(try dt.nodeFromPath("no_alias"), null);

    try std.testing.expectError(error.BadPath, dt.nodeFromPath(""));
    try std.testing.expectError(error.BadPath, dt.nodeFromPath("//"));
    try std.testing.expectError(error.BadPath, dt.nodeFromPath("/soc/serial@10000000/"));
}

pub const Node = @import("Node.zig").Node;
pub const Property = @import("Property.zig");
pub const PHandle = @import("PHandle.zig").PHandle;

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

const Tag = @import("Tag.zig").Tag;

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
const shared = @import("shared.zig");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
