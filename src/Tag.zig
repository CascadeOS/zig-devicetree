// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

pub const Tag = enum(u32) {
    begin_node = shared.nativeToBig(u32, 0x1),
    end_node = shared.nativeToBig(u32, 0x2),
    prop = shared.nativeToBig(u32, 0x3),
    nop = shared.nativeToBig(u32, 0x4),
    end = shared.nativeToBig(u32, 0x9),

    fn read(structure_block_ptr: [*]const u8) Tag {
        const ptr: *const Tag = @ptrCast(@alignCast(structure_block_ptr));
        return ptr.*;
    }

    /// Iterate over the tags of a device tree.
    pub fn iterator(dt: DeviceTree, start_offset: u32) Tag.Iterator {
        return .{
            .structure_block = @alignCast(dt.fdt.ptr + dt.structure_block_offset),
            .offset_limit = dt.structure_block_offset_limit,
            .offset = start_offset,
        };
    }

    /// An iterator over the tags of a device tree.
    pub const Iterator = struct {
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
                        if (offset + @sizeOf(RawProperty) > offset_limit) {
                            @branchHint(.cold);
                            return error.Truncated;
                        }

                        const prop: RawProperty = .read(@alignCast(self.structure_block[offset..]));
                        offset += @sizeOf(RawProperty);

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

    pub const Value = union {
        begin_node: [:0]const u8,
        prop: struct {
            value: []const u8,
            name_offset: u32,
        },
    };
};

const RawProperty = extern struct {
    len: u32,
    name_offset: u32,

    fn read(structure_block_ptr: [*]align(4) const u8) RawProperty {
        const ptr: *const RawProperty = @ptrCast(structure_block_ptr);
        var prop = ptr.*;
        prop.len = shared.bigToNative(u32, prop.len);
        prop.name_offset = shared.bigToNative(u32, prop.name_offset);
        return prop;
    }
};

const std = @import("std");
const shared = @import("shared.zig");

const DeviceTree = @import("DeviceTree.zig");

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
