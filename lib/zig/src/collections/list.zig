// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

const std = @import("std");
const utils = @import("utils.zig");
const protocol = @import("../protocol/protocol.zig");

const Allocator = std.mem.Allocator;

pub fn List(comptime T: type) type {
    return struct {
        // Internal const to identify struct as a thrift List
        pub const __IsList = true;

        allocator: Allocator,
        backList: std.ArrayList(T),

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{
                .allocator = allocator,
                .backList = .empty,
            };
        }

        pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !@This() {
            return .{
                .allocator = allocator,
                .backList = try .initCapacity(allocator, capacity),
            };
        }

        pub fn deinit(self: *@This()) void {
            const shouldDeinitVal = comptime utils.containsDeinit(T);
            if (shouldDeinitVal) {
                for (self.backList.items) |*item| {
                    item.deinit();
                }
            }
            self.backList.deinit(self.allocator);
        }

        pub fn append(self: *@This(), value: T) !void {
            try self.backList.append(self.allocator, value);
        }

        pub fn count(self: *const @This()) usize {
            return self.backList.items.len;
        }

        pub fn iterator(self: *@This()) ListIterator(T) {
            return ListIterator(T).init(&self.backList);
        }

        pub fn items(self: *const @This()) []T {
            return self.backList.items;
        }

        pub fn clone(self: @This(), allocator: Allocator) !@This() {
            var c: @This() = try initCapacity(allocator, self.backList.items.len);
            errdefer c.deinit();

            for (self.backList.items) |item| {
                c.append(try utils.cloneVar(T, allocator, item)) catch unreachable;
            }

            return c;
        }

        pub fn eql(self: @This(), other: @This()) bool {
            if (self.count() != other.count()) {
                return false;
            }

            for (self.backList.items, 0..) |item, i| {
                if (!utils.equals(T, item, other.items()[i])) {
                    return false;
                }
            }

            return true;
        }

        pub fn format(self: *const @This(), writer: *std.Io.Writer) !void {
            try writer.print("List({s}){{", .{@typeName(T)});
            for (self.items(), 1..) |item, i| {
                try utils.format(T, writer, item);
                if (i < self.items().len) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print("}}", .{});
        }

        pub fn writeToProtocol(self: *const @This(), prot: *protocol.TProtocol) !void {
            const valTType = protocol.typeToTType(T);
            try prot.writeListBegin(.{ .eType = valTType, .size = @intCast(self.backList.items.len) });
            for (self.backList.items) |*item| {
                try protocol.writeToProtocol(T, item.*, prot);
            }
            try prot.writeListEnd();
        }

        pub fn readFromProtocol(allocator: Allocator, prot: *protocol.TProtocol) !@This() {
            const listIdent = try prot.readListBegin();
            try protocol.validateWireType(protocol.typeToTType(T), listIdent.eType);
            var val: @This() = try .initCapacity(allocator, @intCast(listIdent.size));
            errdefer val.deinit();
            for (0..@intCast(listIdent.size)) |_| {
                try val.append(try protocol.readFromProtocol(T, allocator, prot));
            }
            try prot.readListEnd();
            return val;
        }
    };
}

pub fn ListIterator(comptime T: type) type {
    return struct {
        items: []T,
        index: usize = 0,

        pub fn init(list: *std.ArrayList(T)) @This() {
            return .{
                .items = list.items,
                .index = 0,
            };
        }

        pub fn next(self: *@This()) ?*T {
            if (self.index >= self.items.len) return null;
            const item = &self.items[self.index];
            self.index += 1;
            return item;
        }
    };
}
