// SPDX-License-Identifier: BSD-2-Clause
// SPDX-FileCopyrightText: 2025 Lee Cannon <leecannon@leecannon.xyz>
// SPDX-FileCopyrightText: 2006 David Gibson, IBM Corporation.

//! A property of a node.

const Property = @This();

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

    /// Iterate over the strings in a `Value` which is a string list.
    pub fn stringListIterator(self: Value) StringListIterator {
        return .{
            .string_list = self._raw,
        };
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
            try shared.customExpectEqual(handle, fromPHandle(handle, &buf).toPHandle());
        }
    }

    pub const ListIteratorError = error{
        /// The size of the `Value` is not a multiple of type the iterator is for.
        SizeNotMultiple,
    };

    /// Iterate over the value as a list of `PHandle`s.
    pub fn pHandleListIterator(self: Value) ListIteratorError!PHandleListIterator {
        if (!std.mem.isAligned(self._raw.len, @sizeOf(PHandle))) {
            @branchHint(.cold);
            return error.SizeNotMultiple;
        }

        const phandle_list: []align(1) const u32 = std.mem.bytesAsSlice(u32, self._raw);

        return .{
            .phandle_list = phandle_list,
        };
    }

    pub const PHandleListIterator = struct {
        phandle_list: []align(1) const u32,

        pub fn next(self: *PHandleListIterator) ?PHandle {
            if (self.phandle_list.len == 0) return null;

            const phandle_value = shared.bigToNative(u32, self.phandle_list[0]);

            self.phandle_list = self.phandle_list[1..];

            return @enumFromInt(phandle_value);
        }
    };

    pub const PHandleListBuilder = struct {
        buf: std.ArrayListUnmanaged(u32) = .empty,

        /// Append a PHandle to the phandle list.
        pub fn append(self: *PHandleListBuilder, allocator: std.mem.Allocator, phandle: PHandle) !void {
            try self.buf.append(allocator, shared.nativeToBig(u32, @intFromEnum(phandle)));
        }

        /// Deinitialize the phandle list.
        pub fn deinit(self: *PHandleListBuilder, allocator: std.mem.Allocator) void {
            self.buf.deinit(allocator);
        }

        /// Get the handle list as a `Value`.
        ///
        /// The `Value` references the internal buffer of the `PHandleListBuilder`, not a copy.
        ///
        /// Any modifications to the `PHandleListBuilder` after calling this function will not be reflected in the
        /// `Value`.
        pub fn toValue(self: PHandleListBuilder) Value {
            return .{ ._raw = std.mem.sliceAsBytes(self.buf.items) };
        }
    };

    test PHandleListBuilder {
        var builder: PHandleListBuilder = .{};
        defer builder.deinit(std.testing.allocator);

        try builder.append(std.testing.allocator, @enumFromInt(0x01));
        try builder.append(std.testing.allocator, @enumFromInt(0x02));
        try builder.append(std.testing.allocator, @enumFromInt(0x03));
        try builder.append(std.testing.allocator, @enumFromInt(0x04));
        try builder.append(std.testing.allocator, @enumFromInt(0x05));

        const phandle_list = builder.toValue();

        var iter = try phandle_list.pHandleListIterator();
        try shared.customExpectEqual(iter.next(), @enumFromInt(0x01));
        try shared.customExpectEqual(iter.next(), @enumFromInt(0x02));
        try shared.customExpectEqual(iter.next(), @enumFromInt(0x03));
        try shared.customExpectEqual(iter.next(), @enumFromInt(0x04));
        try shared.customExpectEqual(iter.next(), @enumFromInt(0x05));
        try shared.customExpectEqual(iter.next(), null);
    }

    /// Converts a `Value` to a `u32`.
    pub fn toU32(self: Value) u32 {
        const ptr: *align(1) const u32 = @ptrCast(self._raw.ptr);
        return shared.bigToNative(u32, ptr.*);
    }

    /// Converts a `u32` to a `Value`.
    ///
    /// The `buf` parameter is used to store the raw value of the `Value`.
    pub fn fromU32(val: u32, buf: *[4]u8) Value {
        const ptr: *align(1) u32 = @ptrCast(buf);
        ptr.* = shared.nativeToBig(u32, val);
        return Value{ ._raw = buf[0..] };
    }

    test "Property.Value.toU32/fromU32" {
        const values: []const u32 = &.{
            0x1,
            0x12345678,
        };

        var buf: [4]u8 = undefined;

        for (values) |val| {
            try shared.customExpectEqual(val, fromU32(val, &buf).toU32());
        }
    }

    /// Iterate over the value as a list of `u32`s.
    pub fn u32ListIterator(self: Value) ListIteratorError!U32ListIterator {
        if (!std.mem.isAligned(self._raw.len, @sizeOf(u32))) {
            @branchHint(.cold);
            return error.SizeNotMultiple;
        }

        const u32_list: []align(1) const u32 = std.mem.bytesAsSlice(u32, self._raw);

        return .{
            .u32_list = u32_list,
        };
    }

    pub const U32ListIterator = struct {
        u32_list: []align(1) const u32,

        pub fn next(self: *U32ListIterator) ?u32 {
            if (self.u32_list.len == 0) return null;

            const value = shared.bigToNative(u32, self.u32_list[0]);

            self.u32_list = self.u32_list[1..];

            return value;
        }
    };

    pub const U32ListBuilder = struct {
        buf: std.ArrayListUnmanaged(u32) = .empty,

        /// Append a u32 to the u32 list.
        pub fn append(self: *U32ListBuilder, allocator: std.mem.Allocator, value: u32) !void {
            try self.buf.append(allocator, shared.nativeToBig(u32, value));
        }

        /// Deinitialize the u32 list.
        pub fn deinit(self: *U32ListBuilder, allocator: std.mem.Allocator) void {
            self.buf.deinit(allocator);
        }

        /// Get the u32 list as a `Value`.
        ///
        /// The `Value` references the internal buffer of the `U32ListBuilder`, not a copy.
        ///
        /// Any modifications to the `U32ListBuilder` after calling this function will not be reflected in the
        /// `Value`.
        pub fn toValue(self: U32ListBuilder) Value {
            return .{ ._raw = std.mem.sliceAsBytes(self.buf.items) };
        }
    };

    test U32ListBuilder {
        var builder: U32ListBuilder = .{};
        defer builder.deinit(std.testing.allocator);

        try builder.append(std.testing.allocator, 0x01);
        try builder.append(std.testing.allocator, 0x02);
        try builder.append(std.testing.allocator, 0x03);
        try builder.append(std.testing.allocator, 0x04);
        try builder.append(std.testing.allocator, 0x05);

        const u32_list = builder.toValue();

        var iter = try u32_list.u32ListIterator();
        try shared.customExpectEqual(iter.next(), 0x01);
        try shared.customExpectEqual(iter.next(), 0x02);
        try shared.customExpectEqual(iter.next(), 0x03);
        try shared.customExpectEqual(iter.next(), 0x04);
        try shared.customExpectEqual(iter.next(), 0x05);
        try shared.customExpectEqual(iter.next(), null);
    }

    /// Converts a `Value` to a `u64`.
    pub fn toU64(self: Value) u64 {
        const ptr: *align(1) const u64 = @ptrCast(self._raw.ptr);
        return shared.bigToNative(u64, ptr.*);
    }

    /// Converts a `u64` to a `Value`.
    ///
    /// The `buf` parameter is used to store the raw value of the `Value`.
    pub fn fromU64(val: u64, buf: *[8]u8) Value {
        const ptr: *align(1) u64 = @ptrCast(buf);
        ptr.* = shared.nativeToBig(u64, val);
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
            try shared.customExpectEqual(val, fromU64(val, &buf).toU64());
        }
    }

    /// Iterate over the value as a list of `u64`s.
    pub fn u64ListIterator(self: Value) ListIteratorError!U64ListIterator {
        if (!std.mem.isAligned(self._raw.len, @sizeOf(u64))) {
            @branchHint(.cold);
            return error.SizeNotMultiple;
        }

        const u64_list: []align(1) const u64 = std.mem.bytesAsSlice(u64, self._raw);

        return .{
            .u64_list = u64_list,
        };
    }

    pub const U64ListIterator = struct {
        u64_list: []align(1) const u64,

        pub fn next(self: *U64ListIterator) ?u64 {
            if (self.u64_list.len == 0) return null;

            const value = shared.bigToNative(u64, self.u64_list[0]);

            self.u64_list = self.u64_list[1..];

            return value;
        }
    };

    pub const U64ListBuilder = struct {
        buf: std.ArrayListUnmanaged(u64) = .empty,

        /// Append a u64 to the u64 list.
        pub fn append(self: *U64ListBuilder, allocator: std.mem.Allocator, value: u64) !void {
            try self.buf.append(allocator, shared.nativeToBig(u64, value));
        }

        /// Deinitialize the u64 list.
        pub fn deinit(self: *U64ListBuilder, allocator: std.mem.Allocator) void {
            self.buf.deinit(allocator);
        }

        /// Get the u64 list as a `Value`.
        ///
        /// The `Value` references the internal buffer of the `U64ListBuilder`, not a copy.
        ///
        /// Any modifications to the `U32ListBuU64ListBuilderilder` after calling this function will not be reflected in the
        /// `Value`.
        pub fn toValue(self: U64ListBuilder) Value {
            return .{ ._raw = std.mem.sliceAsBytes(self.buf.items) };
        }
    };

    test U64ListBuilder {
        var builder: U64ListBuilder = .{};
        defer builder.deinit(std.testing.allocator);

        try builder.append(std.testing.allocator, 0x01);
        try builder.append(std.testing.allocator, 0x02);
        try builder.append(std.testing.allocator, 0x03);
        try builder.append(std.testing.allocator, 0x04);
        try builder.append(std.testing.allocator, 0x05);
        try builder.append(std.testing.allocator, 0xFFFFFFFFFFFFFFFF);

        const u64_list = builder.toValue();

        var iter = try u64_list.u64ListIterator();
        try shared.customExpectEqual(iter.next(), 0x01);
        try shared.customExpectEqual(iter.next(), 0x02);
        try shared.customExpectEqual(iter.next(), 0x03);
        try shared.customExpectEqual(iter.next(), 0x04);
        try shared.customExpectEqual(iter.next(), 0x05);
        try shared.customExpectEqual(iter.next(), 0xFFFFFFFFFFFFFFFF);
        try shared.customExpectEqual(iter.next(), null);
    }
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

const std = @import("std");
const shared = @import("shared.zig");

const DeviceTree = @import("DeviceTree.zig");
const PHandle = @import("PHandle.zig").PHandle;
const Tag = @import("Tag.zig").Tag;
const IteratorError = DeviceTree.IteratorError;

comptime {
    std.testing.refAllDeclsRecursive(@This());
}
