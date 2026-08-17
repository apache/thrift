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
const protocol = @import("../protocol/protocol.zig");
const utils = @import("utils.zig");

const Allocator = std.mem.Allocator;
const containsDeinit = utils.containsDeinit;

pub fn Set(comptime T: type) type {
    return struct {
        // Internal const to identify struct as a thrift Set
        pub const __IsSet = true;

        const Context = utils.selectContext(T);
        const BackingMap = std.hash_map.HashMap(T, void, Context, std.hash_map.default_max_load_percentage);
        const Iterator = struct {
            inner: BackingMap.Iterator,

            pub fn init(map: *const BackingMap) @This() {
                return .{
                    .inner = map.iterator(),
                };
            }

            pub fn next(self: *@This()) ?*T {
                if (self.inner.next()) |n| {
                    return n.key_ptr;
                }
                return null;
            }
        };

        allocator: std.mem.Allocator,
        map: BackingMap,

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{
                .allocator = allocator,
                .map = .init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            if (comptime containsDeinit(T)) {
                var iter = self.map.iterator();
                while (iter.next()) |entry| {
                    entry.key_ptr.deinit();
                }
            }
            self.map.deinit();
        }

        pub fn put(self: *@This(), value: T) !void {
            const shouldDeinit = comptime containsDeinit(T);

            const gop = try self.map.getOrPut(value);
            if (gop.found_existing) {
                if (shouldDeinit) {
                    value.deinit();
                }
            }
        }

        pub fn iterator(self: *const @This()) Iterator {
            return .init(&self.map);
        }

        pub fn count(self: *const @This()) usize {
            return self.map.count();
        }

        pub fn contains(self: *const @This(), key: T) bool {
            return self.map.contains(key);
        }

        pub fn clone(self: @This(), allocator: Allocator) !@This() {
            var c: @This() = .init(allocator);
            errdefer c.deinit();

            var iter = self.map.iterator();
            while (iter.next()) |pair| {
                try c.put(try utils.cloneVar(T, allocator, pair.key_ptr.*));
            }

            return c;
        }

        pub fn eql(self: @This(), other: @This()) bool {
            if (self.count() != other.count()) {
                return false;
            }

            var it = self.iterator();
            while (it.next()) |entry| {
                if (!other.contains(entry.*)) {
                    return false;
                }
            }

            return true;
        }

        pub fn format(self: *const @This(), writer: *std.Io.Writer) !void {
            try writer.print("Set({s}){{", .{@typeName(T)});
            var it = self.iterator();
            const c = self.count();
            var i: usize = 1;
            while (it.next()) |item| : (i += 1) {
                try utils.format(T, writer, item.*);
                if (i < c) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print("}}", .{});
        }

        pub fn writeToProtocol(self: *const @This(), oProt: *protocol.TProtocol) !void {
            const valTType = protocol.typeToTType(T);
            try oProt.writeSetBegin(.{ .eType = valTType, .size = @intCast(self.map.count()) });
            var iter = self.map.iterator();
            while (iter.next()) |pair| {
                try protocol.writeToProtocol(T, pair.key_ptr.*, oProt);
            }
            try oProt.writeSetEnd();
        }

        pub fn readFromProtocol(allocator: Allocator, prot: *protocol.TProtocol) !@This() {
            const setIdent = try prot.readSetBegin();
            try protocol.validateWireType(protocol.typeToTType(T), setIdent.eType);
            var val: @This() = .init(allocator);
            errdefer val.deinit();
            for (0..@intCast(setIdent.size)) |_| {
                try val.put(try protocol.readFromProtocol(T, allocator, prot));
            }
            try prot.readSetEnd();
            return val;
        }
    };
}
