// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

pub const FDT = opaque {
    pub const MAGIC: u32 = c.FDT_MAGIC;

    /// Reinterprets a pointer as an FDT.
    ///
    /// No validation is performed.
    pub fn fromPtr(ptr: [*]align(8) const u8) *const FDT {
        return @ptrCast(ptr);
    }

    /// Reinterprets a slice as an FDT, no validation is performed.
    ///
    /// No validation is performed.
    pub fn fromSlice(slice: []align(8) const u8) *const FDT {
        return .fromPtr(slice.ptr);
    }

    /// Moves an FDT to a new buffer.
    ///
    /// The buffer may overlap with the existing device tree blob `fdt`.
    pub fn move(fdt: *const FDT, buf: []align(8) u8) !*const FDT {
        const ret: ReturnValue = @enumFromInt(c.fdt_move(
            @ptrCast(fdt),
            buf.ptr,
            @intCast(buf.len),
        ));
        try ret.toError();
        return .fromSlice(buf);
    }

    /// Sanity check a device tree header.
    ///
    /// Checks that the given FDT contains what appears to be a flattened device tree, and that the header contains
    /// valid information (to the extent that can be determined from the header alone).
    pub fn checkHeader(fdt: *const FDT) !void {
        const ret: ReturnValue = @enumFromInt(c.fdt_check_header(
            @ptrCast(fdt),
        ));
        try ret.toError();
    }

    test checkHeader {
        const fdt = FDT.fromSlice(test_dtb);
        try fdt.checkHeader();
    }

    /// Sanity check a device tree.
    pub fn checkFull(fdt: *const FDT, buf_size: usize) !void {
        const ret: ReturnValue = @enumFromInt(c.fdt_check_full(
            @ptrCast(fdt),
            buf_size,
        ));
        try ret.toError();
    }

    test checkFull {
        const fdt = FDT.fromSlice(test_dtb);
        try fdt.checkFull(test_dtb.len);
    }

    /// Extracts the header from an FDT.
    ///
    /// Validates the header.
    pub fn header(fdt: *const FDT) !Header {
        const bigToNative = std.mem.bigToNative;

        const full_ptr: *const c.struct_fdt_header = @ptrCast(@alignCast(fdt));

        var h: Header = std.mem.zeroes(Header);

        h.magic = bigToNative(u32, full_ptr.magic);

        if (h.magic != MAGIC)
            return Error.BadMagic;

        h.totalsize = bigToNative(u32, full_ptr.totalsize);

        if (h.totalsize < c.FDT_V1_SIZE)
            return Error.Truncated;

        h.off_dt_struct = bigToNative(u32, full_ptr.off_dt_struct);
        h.off_dt_strings = bigToNative(u32, full_ptr.off_dt_strings);
        h.off_mem_rsvmap = bigToNative(u32, full_ptr.off_mem_rsvmap);
        h.version = bigToNative(u32, full_ptr.version);
        h.last_comp_version = bigToNative(u32, full_ptr.last_comp_version);

        if (h.version < c.FDT_FIRST_SUPPORTED_VERSION or h.version > c.FDT_LAST_SUPPORTED_VERSION)
            return Error.BadVersion;

        const header_size = headerSize(h.version);

        if (h.totalsize < header_size)
            return Error.Truncated;

        if (h.off_dt_struct < header_size or h.off_dt_struct > h.totalsize)
            return Error.Truncated;

        if (h.off_dt_strings < header_size or h.off_dt_strings > h.totalsize)
            return Error.Truncated;

        if (h.off_mem_rsvmap < header_size or h.off_mem_rsvmap > h.totalsize)
            return Error.Truncated;

        if (h.version >= 2) {
            h.boot_cpuid_phys = bigToNative(u32, full_ptr.boot_cpuid_phys);
        }

        if (h.version >= 3) {
            h.size_dt_strings = bigToNative(u32, full_ptr.size_dt_strings);

            if (h.off_dt_strings + h.size_dt_strings > h.totalsize)
                return Error.Truncated;
        }

        if (h.version >= 17) {
            h.size_dt_struct = bigToNative(u32, full_ptr.size_dt_struct);

            if (h.off_dt_struct + h.size_dt_struct > h.totalsize)
                return Error.Truncated;
        }

        return h;
    }

    test header {
        const fdt = FDT.fromSlice(test_dtb);
        const h = try fdt.header();

        try customExpectEqual(h.magic, c.FDT_MAGIC);
        try customExpectEqual(h.totalsize, test_dtb.len);
        try customExpectEqual(h.off_dt_struct, 88);
        try customExpectEqual(h.off_dt_strings, 5164);
        try customExpectEqual(h.off_mem_rsvmap, 40);
        try customExpectEqual(h.version, 17);
        try customExpectEqual(h.last_comp_version, 16);
        try customExpectEqual(h.boot_cpuid_phys, 0);
        try customExpectEqual(h.size_dt_strings, 503);
        try customExpectEqual(h.size_dt_struct, 5076);
    }

    /// Find a node by its full path.
    ///
    /// `findNodeWithPath` finds a node of a given path in the device tree.
    ///
    /// Each path component may omit the unit address portion, but the results of this are undefined if any such path
    /// component is ambiguous (that is if there are multiple nodes at the relevant level matching the given component,
    /// differentiated only by unit address).
    ///
    /// If the path is not absolute (i.e. does not begin with '/'), the first component is treated as an alias.
    /// That is, the property by that name is looked up in the /aliases node, and the value of that property used in
    /// place of that first component.
    ///
    /// For example, for this small fragment:
    /// ```dts
    /// / {
    ///     aliases {
    ///         i2c2 = &foo; // RHS compiles to "/soc@0/i2c@30a40000/eeprom@52"
    ///     };
    ///     soc@0 {
    ///         foo: i2c@30a40000 {
    ///             bar: eeprom@52 {
    ///             };
    ///         };
    ///     };
    ///};
    /// ```
    /// these would be equivalent:
    ///  - `/soc@0/i2c@30a40000/eeprom@52`
    ///  - `i2c2/eeprom@52`
    pub fn findWithPath(fdt: *const FDT, path: []const u8) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_path_offset_namelen(
            @ptrCast(fdt),
            path.ptr,
            @intCast(path.len),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Node);
    }

    test findWithPath {
        const fdt = FDT.fromSlice(test_dtb);

        // with address
        const serial_node = try fdt.findWithPath("/soc/serial@10000000");
        try std.testing.expectEqualStrings(
            "serial@10000000",
            try serial_node.?.getName(fdt),
        );

        // without address
        const rtc_node = try fdt.findWithPath("/soc/rtc");
        try std.testing.expectEqualStrings(
            "rtc@101000",
            try rtc_node.?.getName(fdt),
        );

        // alias
        const memory_node = try fdt.findWithPath("memory");
        try std.testing.expectEqualStrings(
            "memory@80000000",
            try memory_node.?.getName(fdt),
        );
    }

    /// The number of memory reserve map entries.
    ///
    /// Returns the number of entries in the device tree blob's memory reservation map.
    /// This does not include the terminating 0,0 entry or any other (0,0) entries reserved for expansion.
    pub fn numberOfMemoryReservations(fdt: *const FDT) !usize {
        const ret: ReturnValue = @enumFromInt(c.fdt_num_mem_rsv(
            @ptrCast(fdt),
        ));
        return try ret.toValue();
    }

    test numberOfMemoryReservations {
        const fdt = FDT.fromSlice(test_dtb);
        try customExpectEqual(
            try fdt.numberOfMemoryReservations(),
            2,
        );
    }

    /// Retrieves the memory reservation at `index`.
    pub fn getMemoryReservation(fdt: *const FDT, index: usize) !MemoryReservation {
        var reservation: MemoryReservation = undefined;
        const ret: ReturnValue = @enumFromInt(c.fdt_get_mem_rsv(
            @ptrCast(fdt),
            @intCast(index),
            @ptrCast(&reservation.address),
            @ptrCast(&reservation.size),
        ));
        try ret.toError();
        return reservation;
    }

    test getMemoryReservation {
        const fdt = FDT.fromSlice(test_dtb);
        const reservation = try fdt.getMemoryReservation(1);
        try customExpectEqual(reservation.address, 0x12345678);
        try customExpectEqual(reservation.size, 0x1234);
    }

    /// Iterates over the memory reservations in an FDT.
    pub fn memoryReservationIterator(fdt: *const FDT) !MemoryReservationIterator {
        return .{
            .fdt = fdt,
            .index = 0,
            .count = try fdt.numberOfMemoryReservations(),
        };
    }

    pub const MemoryReservationIterator = struct {
        fdt: *const FDT,
        index: usize,
        count: usize,

        pub fn next(self: *MemoryReservationIterator) !?MemoryReservation {
            if (self.index >= self.count) return null;
            const reservation = try self.fdt.getMemoryReservation(self.index);
            self.index += 1;
            return reservation;
        }
    };

    test memoryReservationIterator {
        const fdt = FDT.fromSlice(test_dtb);
        var reservations = try fdt.memoryReservationIterator();
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

    /// Retrieve the path referenced by a given alias. That is, the value of the property named @name in the node /
    /// aliases.
    pub fn getAlias(fdt: *const FDT, alias: []const u8) !?[]const u8 {
        const opt_c_str: ?[*:0]const u8 = c.fdt_get_alias_namelen(
            @ptrCast(fdt),
            alias.ptr,
            @intCast(alias.len),
        );
        const c_str = opt_c_str orelse return null;
        return std.mem.sliceTo(c_str, 0);
    }

    test getAlias {
        const fdt = FDT.fromSlice(test_dtb);
        const path = try fdt.getAlias("off");
        try std.testing.expectEqualStrings("/poweroff", path.?);
    }

    /// Find a node with a compatible property matching `compatible`.
    ///
    /// Returns the first node after `after`, which has a compatible property which lists the given `compatible`
    /// string.
    pub fn findWithCompatible(fdt: *const FDT, after: Node, compatible: [:0]const u8) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_node_offset_by_compatible(
            @ptrCast(fdt),
            @intFromEnum(after),
            compatible.ptr,
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Node);
    }

    test findWithCompatible {
        const fdt = FDT.fromSlice(test_dtb);
        try std.testing.expect(try fdt.findWithCompatible(
            .all,
            "syscon-poweroff",
        ) != null);
    }

    /// Iterate over all nodes after `after`.
    pub fn nodeIterator(fdt: *const FDT, after: Node) NodeIterator {
        return .{
            .fdt = fdt,
            .previous_node = after,
        };
    }

    pub const NodeIterator = struct {
        fdt: *const FDT,
        previous_node: ?Node,

        pub fn next(self: *NodeIterator) !?Node {
            const ret: ReturnValue = @enumFromInt(c.fdt_next_node(
                @ptrCast(self.fdt),
                @intFromEnum(self.previous_node orelse return null),
                null,
            ));

            const node = try ret.toOffset(Node);
            self.previous_node = node;
            return node;
        }
    };

    test nodeIterator {
        const fdt = FDT.fromSlice(test_dtb);

        var count: usize = 0;

        var node_iter = fdt.nodeIterator(.all);
        while (try node_iter.next()) |node| {
            switch (count) {
                0 => try std.testing.expectEqualStrings("", try node.getName(fdt)),
                1 => try std.testing.expectEqualStrings("aliases", try node.getName(fdt)),
                2 => try std.testing.expectEqualStrings("poweroff", try node.getName(fdt)),
                3 => try std.testing.expectEqualStrings("reboot", try node.getName(fdt)),
                4 => try std.testing.expectEqualStrings("platform-bus@4000000", try node.getName(fdt)),
                5 => try std.testing.expectEqualStrings("memory@80000000", try node.getName(fdt)),
                6 => try std.testing.expectEqualStrings("cpus", try node.getName(fdt)),
                7 => try std.testing.expectEqualStrings("cpu@0", try node.getName(fdt)),
                8 => try std.testing.expectEqualStrings("interrupt-controller", try node.getName(fdt)),
                9 => try std.testing.expectEqualStrings("cpu-map", try node.getName(fdt)),
                10 => try std.testing.expectEqualStrings("cluster0", try node.getName(fdt)),
                11 => try std.testing.expectEqualStrings("core0", try node.getName(fdt)),
                12 => try std.testing.expectEqualStrings("pmu", try node.getName(fdt)),
                13 => try std.testing.expectEqualStrings("fw-cfg@10100000", try node.getName(fdt)),
                14 => try std.testing.expectEqualStrings("flash@20000000", try node.getName(fdt)),
                15 => try std.testing.expectEqualStrings("chosen", try node.getName(fdt)),
                16 => try std.testing.expectEqualStrings("soc", try node.getName(fdt)),
                17 => try std.testing.expectEqualStrings("rtc@101000", try node.getName(fdt)),
                18 => try std.testing.expectEqualStrings("serial@10000000", try node.getName(fdt)),
                19 => try std.testing.expectEqualStrings("test@100000", try node.getName(fdt)),
                20 => try std.testing.expectEqualStrings("virtio_mmio@10008000", try node.getName(fdt)),
                21 => try std.testing.expectEqualStrings("virtio_mmio@10007000", try node.getName(fdt)),
                22 => try std.testing.expectEqualStrings("virtio_mmio@10006000", try node.getName(fdt)),
                23 => try std.testing.expectEqualStrings("virtio_mmio@10005000", try node.getName(fdt)),
                24 => try std.testing.expectEqualStrings("virtio_mmio@10004000", try node.getName(fdt)),
                25 => try std.testing.expectEqualStrings("virtio_mmio@10003000", try node.getName(fdt)),
                26 => try std.testing.expectEqualStrings("virtio_mmio@10002000", try node.getName(fdt)),
                27 => try std.testing.expectEqualStrings("virtio_mmio@10001000", try node.getName(fdt)),
                28 => try std.testing.expectEqualStrings("plic@c000000", try node.getName(fdt)),
                29 => try std.testing.expectEqualStrings("clint@2000000", try node.getName(fdt)),
                30 => try std.testing.expectEqualStrings("pci@30000000", try node.getName(fdt)),
                else => {},
            }

            count += 1;
        }

        try customExpectEqual(count, 31);
    }

    /// Find a node with a given name.
    ///
    /// Returns the first node after `after`, which has a property named `name`.
    ///
    /// If `after` is `.all` all nodes will be considered even root.
    pub fn findWithName(fdt: *const FDT, after: Node, name: [:0]const u8) !?Node {
        var node_iter = fdt.nodeIterator(after);
        while (try node_iter.next()) |node| {
            const node_name = try node.getName(fdt);

            if (node_name.len < name.len) {
                continue;
            }

            if (!std.mem.eql(u8, node_name[0..name.len], name)) {
                continue;
            }

            if (node_name[name.len] == 0) {
                return node;
            } else if (node_name[name.len] == '@' and std.mem.indexOfScalar(u8, name, '@') == null) {
                return node;
            }
        }
        return null;
    }

    test findWithName {
        const fdt = FDT.fromSlice(test_dtb);

        // with address
        const plic_node = try fdt.findWithName(.all, "plic@c000000");
        try std.testing.expectEqualStrings("plic@c000000", try plic_node.?.getName(fdt));

        // without address
        const pci_node = try fdt.findWithName(.all, "pci");
        try std.testing.expectEqualStrings("pci@30000000", try pci_node.?.getName(fdt));
    }

    /// Find a node with a given property.
    ///
    /// Returns the first node after `after`, which has a property named `name`.
    ///
    /// If `after` is `.all` all nodes will be considered even root.
    pub fn findWithProperty(fdt: *const FDT, after: Node, property_name: [:0]const u8) !?Node {
        var node_iter = fdt.nodeIterator(after);
        while (try node_iter.next()) |node| {
            if (try node.getPropertyValueWithName(fdt, property_name)) |_| {
                return node;
            }
        }
        return null;
    }

    test findWithProperty {
        const fdt = FDT.fromSlice(test_dtb);

        const serial_node = try fdt.findWithProperty(.all, "clock-frequency");
        try std.testing.expectEqualStrings("serial@10000000", try serial_node.?.getName(fdt));
    }

    /// Find a node with a given property value.
    ///
    /// Returns the first node after `after`, which has a property named `name` whose value is `value`.
    ///
    /// If `after` is `.all` all nodes will be considered even root.
    pub fn findWithPropertyValue(fdt: *const FDT, after: Node, name: [:0]const u8, value: Property.Value) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_node_offset_by_prop_value(
            @ptrCast(fdt),
            @intFromEnum(after),
            name.ptr,
            value._value.ptr,
            @intCast(value._value.len),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Node);
    }

    test findWithPropertyValue {
        const fdt = FDT.fromSlice(test_dtb);

        const pci_node = try fdt.findWithPropertyValue(
            .all,
            "device_type",
            .fromString("pci"),
        );
        try std.testing.expectEqualStrings("pci@30000000", try pci_node.?.getName(fdt));
    }
};

pub const Node = enum(c_int) {
    root = 0,

    /// Some node search functions need to use this value to include the root node in the search.
    ///
    /// Affected functions:
    ///  - `nodeIterator`
    ///  - `findWithProperty`
    ///  - `findWithPropertyValue`
    ///  - `findWithCompatible`
    all = -1,

    _,

    /// Retrieve the name of a given node.
    pub fn getName(self: Node, fdt: *const FDT) ![:0]const u8 {
        var ret: ReturnValue = undefined;

        const c_str = c.fdt_get_name(
            @ptrCast(fdt),
            @intFromEnum(self),
            @ptrCast(&ret),
        );

        const len = try ret.toValue();
        return c_str[0..len :0];
    }

    test getName {
        const fdt = FDT.fromSlice(test_dtb);
        const node = try Node.root.firstSubnode(fdt);
        try std.testing.expectEqualStrings("aliases", try node.?.getName(fdt));
    }

    /// Get the first direct subnode of `parent`.
    ///
    /// See `subnodeIterator` for an alternative.
    pub fn firstSubnode(parent: Node, fdt: *const FDT) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_first_subnode(
            @ptrCast(fdt),
            @intFromEnum(parent),
        ));
        return try ret.toOffset(Node);
    }

    test firstSubnode {
        const fdt = FDT.fromSlice(test_dtb);
        const cpus_node = try fdt.findWithName(.all, "cpus");

        const node = try cpus_node.?.firstSubnode(fdt);
        try std.testing.expectEqualStrings("cpu@0", try node.?.getName(fdt));
    }

    /// Get the next direct subnode after `node`.
    ///
    /// After first calling `firstSubnode`, call this function repeatedly to get direct subnodes of a parent node.
    ///
    /// See `subnodeIterator` for an alternative.
    pub fn nextSubnode(node: Node, fdt: *const FDT) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_next_subnode(
            @ptrCast(fdt),
            @intFromEnum(node),
        ));
        return try ret.toOffset(Node);
    }

    test nextSubnode {
        const fdt = FDT.fromSlice(test_dtb);

        const cpus_node = try fdt.findWithName(.all, "cpus");

        const cpu0_node = try cpus_node.?.firstSubnode(fdt);
        try std.testing.expectEqualStrings("cpu@0", try cpu0_node.?.getName(fdt));

        const cpu_map_node = try cpu0_node.?.nextSubnode(fdt);
        try std.testing.expectEqualStrings("cpu-map", try cpu_map_node.?.getName(fdt));

        try customExpectEqual(try cpu_map_node.?.nextSubnode(fdt), null);
    }

    /// Find a subnode of `parent` with the given `name`.
    ///
    /// `name` may include a unit address, in which case `subnodeWithName` will find the subnode with that unit
    /// address, or the unit address may be omitted, in which case `subnodeWithName` will find an arbitrary subnode
    /// whose name excluding unit address matches the given `name`.
    pub fn findSubnodeWithName(parent: Node, name: []const u8, fdt: *const FDT) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_subnode_offset_namelen(
            @ptrCast(fdt),
            @intFromEnum(parent),
            name.ptr,
            @intCast(name.len),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Node);
    }

    test findSubnodeWithName {
        const fdt = FDT.fromSlice(test_dtb);

        // with address
        const plaform_bus_node = try Node.root.findSubnodeWithName("platform-bus@4000000", fdt);
        try std.testing.expectEqualStrings("platform-bus@4000000", try plaform_bus_node.?.getName(fdt));

        // no address
        const memory_node = try Node.root.findSubnodeWithName("memory", fdt);
        try std.testing.expectEqualStrings("memory@80000000", try memory_node.?.getName(fdt));
    }

    /// Iterate over all subnodes of `parent`.
    pub fn subnodeIterator(parent: Node, fdt: *const FDT) SubnodeIterator {
        return .{
            .fdt = fdt,
            .parent = parent,
        };
    }

    pub const SubnodeIterator = struct {
        fdt: *const FDT,
        parent: ?Node,
        previous_subnode: ?Node = null,

        pub fn next(self: *SubnodeIterator) !?Node {
            const ret: ReturnValue = if (self.parent) |parent| blk: {
                @branchHint(.unlikely);

                self.parent = null;

                break :blk @enumFromInt(c.fdt_first_subnode(
                    @ptrCast(self.fdt),
                    @intFromEnum(parent),
                ));
            } else @enumFromInt(c.fdt_next_subnode(
                @ptrCast(self.fdt),
                @intFromEnum(self.previous_subnode orelse return null),
            ));

            const subnode = try ret.toOffset(Node);
            self.previous_subnode = subnode;
            return subnode;
        }
    };

    test subnodeIterator {
        const fdt = FDT.fromSlice(test_dtb);
        const cpus_node = try fdt.findWithName(.all, "cpus");

        var iter = cpus_node.?.subnodeIterator(fdt);
        try std.testing.expectEqualStrings("cpu@0", try (try iter.next()).?.getName(fdt));
        try std.testing.expectEqualStrings("cpu-map", try (try iter.next()).?.getName(fdt));
        try customExpectEqual(try iter.next(), null);
    }

    /// Get the next node after `node`.
    pub fn nextNode(node: Node, fdt: *const FDT) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_next_node(
            @ptrCast(fdt),
            @intFromEnum(node),
            null,
        ));
        return try ret.toOffset(Node);
    }

    test nextNode {
        const fdt = FDT.fromSlice(test_dtb);

        const pmu_node = try fdt.findWithName(.all, "pmu");

        const fw_cfg_node = try pmu_node.?.nextNode(fdt);
        try std.testing.expectEqualStrings("fw-cfg@10100000", try fw_cfg_node.?.getName(fdt));
    }

    /// Get the first property of `node`.
    pub fn firstProperty(node: Node, fdt: *const FDT) !?Property {
        const ret: ReturnValue = @enumFromInt(c.fdt_first_property_offset(
            @ptrCast(fdt),
            @intFromEnum(node),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Property);
    }

    test firstProperty {
        const fdt = FDT.fromSlice(test_dtb);

        const chosen_node = try fdt.findWithName(.all, "chosen");

        const stdout_path_property = try chosen_node.?.firstProperty(fdt);

        const name, const value = try stdout_path_property.?.getValue(fdt);
        try std.testing.expectEqualStrings("stdout-path", name);
        try std.testing.expectEqualStrings("/soc/serial@10000000", value.toString());
    }

    /// Iterate over all properties of `node`.
    pub fn propertyIterator(node: Node, fdt: *const FDT) PropertyIterator {
        return .{
            .fdt = fdt,
            .node = node,
        };
    }

    pub const PropertyIterator = struct {
        fdt: *const FDT,
        node: ?Node,
        previous_property: ?Property = null,

        pub fn next(self: *PropertyIterator) !?Property {
            const ret: ReturnValue = if (self.node) |node| blk: {
                @branchHint(.unlikely);

                self.node = null;

                break :blk @enumFromInt(c.fdt_first_property_offset(
                    @ptrCast(self.fdt),
                    @intFromEnum(node),
                ));
            } else @enumFromInt(c.fdt_next_property_offset(
                @ptrCast(self.fdt),
                @intFromEnum(self.previous_property orelse return null),
            ));

            const property = try ret.toOffset(Property);
            self.previous_property = property;
            return property;
        }
    };

    test propertyIterator {
        const fdt = FDT.fromSlice(test_dtb);

        const aliased_node = try fdt.findWithName(.all, "aliases");

        var iter = aliased_node.?.propertyIterator(fdt);

        const memory_prop_name, const memory_prop_value = try (try iter.next()).?.getValue(fdt);
        try std.testing.expectEqualStrings("memory", memory_prop_name);
        try std.testing.expectEqualStrings("/memory@80000000", memory_prop_value.toString());

        const off_prop_name, const off_prop_value = try (try iter.next()).?.getValue(fdt);
        try std.testing.expectEqualStrings("off", off_prop_name);
        try std.testing.expectEqualStrings("/poweroff", off_prop_value.toString());
    }

    /// Get the value of a property with a given name.
    pub fn getPropertyValueWithName(node: Node, fdt: *const FDT, property_name: []const u8) !?Property.Value {
        var ret: ReturnValue = undefined;

        const value_ptr: ?[*]const u8 = @ptrCast(c.fdt_getprop_namelen(
            @ptrCast(fdt),
            @intFromEnum(node),
            property_name.ptr,
            @intCast(property_name.len),
            @ptrCast(&ret),
        ));
        if (ret == .NotFound) return null;

        const value_len = try ret.toValue();
        return .{ ._value = value_ptr.?[0..value_len] };
    }

    test getPropertyValueWithName {
        const fdt = FDT.fromSlice(test_dtb);
        const serial_node = try fdt.findWithName(.all, "serial");

        const value = try serial_node.?.getPropertyValueWithName(fdt, "compatible");
        try std.testing.expectEqualStrings("ns16550a", value.?.toString());
    }

    /// Retrieve the phandle of `node`.
    pub fn getPhandle(node: Node, fdt: *const FDT) ?PHandle {
        const phandle = c.fdt_get_phandle(
            @ptrCast(fdt),
            @intFromEnum(node),
        );
        if (phandle == 0 or phandle == std.math.maxInt(u32)) {
            @branchHint(.unlikely);
            return null;
        }
        return @enumFromInt(phandle);
    }

    test getPhandle {
        const fdt = FDT.fromSlice(test_dtb);

        const test_node = try fdt.findWithName(.all, "test");

        try customExpectEqual(test_node.?.getPhandle(fdt), @enumFromInt(0x4));
    }

    /// Retrieve the depth of a node.
    ///
    /// The root node has depth 0, its immediate subnodes depth 1 and so forth.
    ///
    /// NOTE: This function is expensive, as it must scan the device tree structure from the start to the `node`.
    pub fn depth(node: Node, fdt: *const FDT) !usize {
        const ret: ReturnValue = @enumFromInt(c.fdt_node_depth(
            @ptrCast(fdt),
            @intFromEnum(node),
        ));
        return try ret.toValue();
    }

    test depth {
        const fdt = FDT.fromSlice(test_dtb);

        const pci_node = try fdt.findWithName(.all, "pci");

        try customExpectEqual(pci_node.?.depth(fdt), 2);
    }

    /// Retrieve the parent of a node (that is, it finds the the node which contains `node` as a
    /// subnode).
    ///
    /// NOTE: This function is expensive, as it must scan the device tree structure from the start to
    /// `node`, *twice*.
    pub fn getParent(node: Node, fdt: *const FDT) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_parent_offset(
            @ptrCast(fdt),
            @intFromEnum(node),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Node);
    }

    test getParent {
        const fdt = FDT.fromSlice(test_dtb);

        const cpu0_node = try fdt.findWithName(.all, "cpus");
        try customExpectEqual(cpu0_node.?.getParent(fdt), Node.root);
    }

    /// Retrieve the full path of `node`.
    ///
    /// The returned path is stored in the provided buffer `buf`.
    ///
    /// NOTE: This function is expensive, as it must scan the device tree structure from the start to
    /// `node`.
    pub fn path(node: Node, fdt: *const FDT, buf: []u8) ![:0]const u8 {
        const ret: ReturnValue = @enumFromInt(c.fdt_get_path(
            @ptrCast(fdt),
            @intFromEnum(node),
            buf.ptr,
            @intCast(buf.len),
        ));
        try ret.toError();
        const c_str: [*:0]const u8 = @ptrCast(buf.ptr);
        return std.mem.sliceTo(c_str, 0);
    }

    test path {
        const fdt = FDT.fromSlice(test_dtb);

        const virtio_mmio_node = try fdt.findWithName(.all, "virtio_mmio@10001000");

        var buf: [256]u8 = undefined;
        const p = try virtio_mmio_node.?.path(fdt, buf[0..]);
        try std.testing.expectEqualStrings("/soc/virtio_mmio@10001000", p);
    }

    pub const CheckCompatibleResult = enum {
        /// The node has a compatible property and it matches.
        match,

        /// The node has a compatible property but it does not match.
        no_match,

        /// The node has no compatible property.
        no_compatible_property,
    };

    /// Check the compatible property of `node` against `compatible`.
    pub fn checkCompatible(self: Node, fdt: *const FDT, compatible: [:0]const u8) !CheckCompatibleResult {
        const ret: ReturnValue = @enumFromInt(c.fdt_node_check_compatible(
            @ptrCast(fdt),
            @intFromEnum(self),
            compatible.ptr,
        ));
        if (ret == .NotFound) return .no_compatible_property;
        return switch (try ret.toValue()) {
            0 => .match,
            1 => .no_match,
            else => unreachable,
        };
    }

    test checkCompatible {
        const fdt = FDT.fromSlice(test_dtb);

        const platform_bus_node = try fdt.findWithName(.all, "platform-bus");

        // match
        try customExpectEqual(
            platform_bus_node.?.checkCompatible(fdt, "simple-bus"),
            .match,
        );

        // no match
        try customExpectEqual(
            platform_bus_node.?.checkCompatible(fdt, "ns16550a"),
            .no_match,
        );

        const memory_node = try fdt.findWithName(.all, "memory");

        // no compatible property
        try customExpectEqual(
            memory_node.?.checkCompatible(fdt, "syscon-reboot"),
            .no_compatible_property,
        );
    }

    /// Retrieve address size for a bus represented in the tree.
    pub fn addressCells(self: Node, fdt: *const FDT) !usize {
        const ret: ReturnValue = @enumFromInt(c.fdt_address_cells(
            @ptrCast(fdt),
            @intFromEnum(self),
        ));
        return try ret.toValue();
    }

    test addressCells {
        const fdt = FDT.fromSlice(test_dtb);

        const platform_bus_node = try fdt.findWithName(.all, "platform-bus");
        try customExpectEqual(platform_bus_node.?.addressCells(fdt), 1);
    }

    /// Retrieve address range size for a bus represented in the tree.
    pub fn sizeCells(self: Node, fdt: *const FDT) !usize {
        const ret: ReturnValue = @enumFromInt(c.fdt_size_cells(
            @ptrCast(fdt),
            @intFromEnum(self),
        ));
        return try ret.toValue();
    }

    test sizeCells {
        const fdt = FDT.fromSlice(test_dtb);

        const cpus_node = try fdt.findWithName(.all, "cpus");
        try customExpectEqual(cpus_node.?.sizeCells(fdt), 0);
    }
};

pub const Property = enum(c_int) {
    _,

    /// Get the name and value of the property.
    pub fn getValue(self: Property, fdt: *const FDT) !struct {
        [:0]const u8,
        Value,
    } {
        var name_ptr: [*:0]const u8 = undefined;
        var ret: ReturnValue = undefined;

        const value_ptr: ?[*]const u8 = @ptrCast(c.fdt_getprop_by_offset(
            @ptrCast(fdt),
            @intFromEnum(self),
            @ptrCast(&name_ptr),
            @ptrCast(&ret),
        ));

        const value_len = try ret.toValue();
        return .{
            std.mem.sliceTo(name_ptr, 0),
            .{ ._value = value_ptr.?[0..value_len] },
        };
    }

    test getValue {
        const fdt = FDT.fromSlice(test_dtb);

        const memory_node = try fdt.findWithName(.all, "memory");

        const device_type_property = try memory_node.?.firstProperty(fdt);
        const name, const value = try device_type_property.?.getValue(fdt);

        try std.testing.expectEqualStrings("device_type", name);
        try std.testing.expectEqualStrings("memory", value.toString());
    }

    pub const Value = struct {
        /// The raw value of the property.
        ///
        /// NOTE: This is a pointer directly into the device tree so the data is big-endian.
        _value: []const u8,

        pub fn fromString(str: [:0]const u8) Value {
            return .{ ._value = str[0 .. str.len + 1] };
        }

        pub fn toString(self: Value) [:0]const u8 {
            return self._value[0 .. self._value.len - 1 :0];
        }

        test "Property.Value.fromString/toString" {
            try std.testing.expectEqualStrings("foo", Value.fromString("foo").toString());
            try std.testing.expectEqualStrings("", Value.fromString("").toString());
            try std.testing.expectEqualStrings("✓", Value.fromString("✓").toString());
        }

        pub fn stringListIterator(self: Value) StringListIterator { // TODO: add test for stringListIterator
            return .{
                .string_list = self._value,
            };
        }

        pub const StringListIterator = struct {
            string_list: []const u8,

            pub fn next(self: *StringListIterator) ?[:0]const u8 {
                if (self.string_list.len == 0) return null;

                const len = std.mem.indexOfScalar(u8, self.string_list, 0) orelse unreachable;

                const str: [:0]const u8 = self.string_list[0..len :0];
                self.string_list = self.string_list[len + 1 ..];
                return str;
            }
        };
    };

    /// Get the next property after `property`.
    ///
    /// This will be a property of the same node as the given property.
    pub fn nextProperty(self: Property, fdt: *const FDT) !?Property {
        const ret: ReturnValue = @enumFromInt(c.fdt_next_property_offset(
            @ptrCast(fdt),
            @intFromEnum(self),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Property);
    }

    test nextProperty {
        const fdt = FDT.fromSlice(test_dtb);

        const aliases_node = try fdt.findWithName(.all, "aliases");

        const memory_property = try aliases_node.?.firstProperty(fdt);
        const memory_name, const memory_value = try memory_property.?.getValue(fdt);
        try std.testing.expectEqualStrings("memory", memory_name);
        try std.testing.expectEqualStrings("/memory@80000000", memory_value.toString());

        const off_property = try memory_property.?.nextProperty(fdt);
        const off_name, const off_value = try off_property.?.getValue(fdt);
        try std.testing.expectEqualStrings("off", off_name);
        try std.testing.expectEqualStrings("/poweroff", off_value.toString());
    }
};

pub const PHandle = enum(u32) {
    _,

    /// Find a node by its phandle.
    pub fn find(handle: PHandle, fdt: *const FDT) !?Node {
        const ret: ReturnValue = @enumFromInt(c.fdt_node_offset_by_phandle(
            @ptrCast(fdt),
            @intFromEnum(handle),
        ));
        if (ret == .NotFound) return null;
        return try ret.toOffset(Node);
    }

    test find {
        const fdt = FDT.fromSlice(test_dtb);

        const handle: PHandle = @enumFromInt(0x1);

        const cpu0_node = try handle.find(fdt);
        try std.testing.expectEqualStrings("cpu@0", try cpu0_node.?.getName(fdt));
    }
};

pub const Error = error{
    /// The requested node or property does not exist
    NotFound,
    /// Attempted to create a node or property which already exists
    Exists,
    /// Operation needed to expand the device tree, but its buffer did not
    /// have sufficient space to contain the expanded tree.
    ///
    /// Use `FDT.move` to move the device tree to a buffer with more space.
    NoSpace,
    /// Function was passed a structure block offset which is out-of-bounds, or which points to an
    /// unsuitable part of the structure for the operation.
    BadOffset,
    /// Function was passed a badly formatted path (e.g. missing a leading / for a function which
    /// requires an absolute path)
    BadPath,
    /// Function was passed an invalid phandle.
    ///
    /// This can be caused either by an invalid phandle property length, or the phandle value was
    /// either 0 or -1, which are not permitted.
    BadPHandler,
    /// Function was passed an incomplete device tree created by the sequential-write functions,
    /// which is not sufficiently complete for the requested operation.
    BadState,
    /// FDT or a sub-block is improperly terminated (overflows, goes outside allowed bounds, or
    /// isn't properly terminated).
    Truncated,
    /// Given "device tree" appears not to be a device tree at all - it is missing the flattened device
    /// tree magic number.
    BadMagic,
    /// Given device tree has a version which can't be handled by the requested operation.
    ///
    /// For read-write functions, this may mean that `FDT.move` is required to convert the tree
    /// to the expected version.
    BadVersion,
    /// Given device tree has a corrupt structure block or other serious error (e.g. misnested
    /// nodes, or subnodes preceding properties).
    BadStructure,
    /// For read-write functions, the given device tree has it's sub-blocks in an order that the
    /// function can't handle (memory reserve map, then structure, then strings).
    ///
    /// Use `FDT.move` to reorganize the tree into a form suitable for the read-write operations.
    BadLayout,
    /// libfdt has failed an internal assertion.
    ///
    /// Should never be returned, if it is, it indicates a bug in libfdt itself.
    Internal,
    /// Device tree has a #address-cells, #size-cells or similar property with a bad format or value.
    BadNCells,
    /// Device tree has a property with an unexpected value.
    ///
    /// For example: a property expected to contain a string list is not NUL-terminated within the
    /// length of its value.
    BadValue,
    /// The device tree overlay, while correctly structured, cannot be applied due to some unexpected
    /// or missing value, property or node.
    BadOverlay,
    /// The device tree doesn't have any phandle available anymore without causing an overflow.
    NoPHandles,
    /// The function was passed a flags field that contains invalid flags or an invalid combination
    /// of flags.
    BadFlags,
    /// The device tree base address is not 8-byte aligned.
    Alignment,
};

pub const MemoryReservation = struct {
    address: u64,
    size: u64,
};

pub const Header = struct {
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
};

const ReturnValue = enum(c_int) {
    Success = 0,

    NotFound = -c.FDT_ERR_NOTFOUND,
    Exists = -c.FDT_ERR_EXISTS,
    NoSpace = -c.FDT_ERR_NOSPACE,
    BadOffset = -c.FDT_ERR_BADOFFSET,
    BadPath = -c.FDT_ERR_BADPATH,
    BadPHandler = -c.FDT_ERR_BADPHANDLE,
    BadState = -c.FDT_ERR_BADSTATE,
    Truncated = -c.FDT_ERR_TRUNCATED,
    BadMagic = -c.FDT_ERR_BADMAGIC,
    BadVersion = -c.FDT_ERR_BADVERSION,
    BadStructure = -c.FDT_ERR_BADSTRUCTURE,
    BadLayout = -c.FDT_ERR_BADLAYOUT,
    Internal = -c.FDT_ERR_INTERNAL,
    BadNCells = -c.FDT_ERR_BADNCELLS,
    BadValue = -c.FDT_ERR_BADVALUE,
    BadOverlay = -c.FDT_ERR_BADOVERLAY,
    NoPHandles = -c.FDT_ERR_NOPHANDLES,
    BadFlags = -c.FDT_ERR_BADFLAGS,
    Alignment = -c.FDT_ERR_ALIGNMENT,

    _,

    fn toValue(self: ReturnValue) !usize {
        @setRuntimeSafety(false);

        const len = @intFromEnum(self);
        if (len < 0) {
            @branchHint(.unlikely);
            try self.toErrorInternal();
            unreachable;
        }

        return @intCast(len);
    }

    fn toOffset(self: ReturnValue, comptime OffsetT: type) !?OffsetT {
        if (self == .NotFound) return null;

        const value = @intFromEnum(self);
        if (value < 0) {
            @branchHint(.unlikely);
            try self.toErrorInternal();
            unreachable;
        }

        return @enumFromInt(value);
    }

    inline fn toError(self: ReturnValue) Error!void {
        if (self == .Success) {
            @branchHint(.likely);
            return;
        }
        try self.toErrorInternal();
    }

    fn toErrorInternal(self: ReturnValue) Error!void {
        return switch (self) {
            .Success => unreachable, // handled in `toError`
            .NotFound => Error.NotFound,
            .Exists => Error.Exists,
            .NoSpace => Error.NoSpace,
            .BadOffset => Error.BadOffset,
            .BadPath => Error.BadPath,
            .BadPHandler => Error.BadPHandler,
            .BadState => Error.BadState,
            .Truncated => Error.Truncated,
            .BadMagic => Error.BadMagic,
            .BadVersion => Error.BadVersion,
            .BadStructure => Error.BadStructure,
            .BadLayout => Error.BadLayout,
            .Internal => blk: {
                @branchHint(.cold);
                break :blk Error.Internal;
            },
            .BadNCells => Error.BadNCells,
            .BadValue => Error.BadValue,
            .BadOverlay => Error.BadOverlay,
            .NoPHandles => Error.NoPHandles,
            .BadFlags => Error.BadFlags,
            .Alignment => Error.Alignment,
            _ => unreachable,
        };
    }
};

fn headerSize(version: u32) u32 {
    return if (version <= 1)
        c.FDT_V1_SIZE
    else if (version <= 2)
        c.FDT_V2_SIZE
    else if (version <= 3)
        c.FDT_V3_SIZE
    else if (version <= 16)
        c.FDT_V16_SIZE
    else if (version <= 17)
        c.FDT_V17_SIZE
    else
        unreachable;
}

const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("libfdt.h");
});

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
pub inline fn customExpectEqual(actual: anytype, expected: @TypeOf(actual)) !void {
    try std.testing.expectEqual(expected, actual);
}

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
