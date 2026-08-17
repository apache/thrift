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

pub fn Map(comptime K: type, comptime V: type) type {
    return struct {
        // Internal const to identify struct as a thrift Map
        pub const __IsMap = true;

        const Context = utils.selectContext(K);
        const BackingMap = std.hash_map.HashMap(K, V, Context, std.hash_map.default_max_load_percentage);
        const Iterator = struct {
            inner: BackingMap.Iterator,

            pub fn init(map: *const BackingMap) @This() {
                return .{
                    .inner = map.iterator(),
                };
            }

            pub fn next(self: *@This()) ?BackingMap.Entry {
                return self.inner.next();
            }
        };

        allocator: Allocator,
        map: BackingMap,

        pub fn init(allocator: std.mem.Allocator) @This() {
            return .{
                .allocator = allocator,
                .map = .init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            const shouldDeinitKey = comptime containsDeinit(K);
            const shouldDeinitVal = comptime containsDeinit(V);
            if (shouldDeinitKey or shouldDeinitVal) {
                var iter = self.map.iterator();
                while (iter.next()) |entry| {
                    if (shouldDeinitKey) {
                        entry.key_ptr.deinit();
                    }
                    if (shouldDeinitVal) {
                        entry.value_ptr.deinit();
                    }
                }
            }
            self.map.deinit();
        }

        pub fn put(self: *@This(), key: K, value: V) !void {
            const shouldDeinitKey = comptime containsDeinit(K);
            const shouldDeinitVal = comptime containsDeinit(V);

            const previous = try self.map.fetchPut(key, value);
            if (previous) |old| {
                if (shouldDeinitVal) {
                    var old_value = old.value;
                    old_value.deinit();
                }
                if (shouldDeinitKey) {
                    key.deinit();
                }
            }
        }

        pub fn iterator(self: *const @This()) Iterator {
            return .init(&self.map);
        }

        pub fn count(self: *const @This()) usize {
            return self.map.count();
        }

        pub fn get(self: *const @This(), key: K) ?V {
            return self.map.get(key);
        }

        pub fn clone(self: @This(), allocator: Allocator) !@This() {
            var c: @This() = .init(allocator);
            errdefer c.deinit();

            var iter = self.map.iterator();
            while (iter.next()) |pair| {
                try c.put(try utils.cloneVar(K, allocator, pair.key_ptr.*), try utils.cloneVar(V, allocator, pair.value_ptr.*));
            }

            return c;
        }

        pub fn eql(self: @This(), other: @This()) bool {
            if (self.count() != other.count()) {
                return false;
            }

            var it = self.iterator();
            while (it.next()) |entry| {
                const res = other.get(entry.key_ptr.*);

                if (res) |otherVal| {
                    if (!utils.equals(V, entry.value_ptr.*, otherVal)) {
                        return false;
                    }
                } else {
                    return false;
                }
            }

            return true;
        }

        pub fn format(self: *const @This(), writer: *std.Io.Writer) !void {
            try writer.print("Map({s}, {s}){{", .{ @typeName(K), @typeName(V) });
            var it = self.iterator();
            const c = self.count();
            var i: usize = 1;
            while (it.next()) |item| : (i += 1) {
                try utils.format(K, writer, item.key_ptr.*);
                try writer.print(": ", .{});
                try utils.format(K, writer, item.key_ptr.*);
                if (i < c) {
                    try writer.print(", ", .{});
                }
            }
            try writer.print("}}", .{});
        }

        pub fn writeToProtocol(self: *const @This(), oProt: *protocol.TProtocol) !void {
            const keyTType = protocol.typeToTType(K);
            const valTType = protocol.typeToTType(V);
            try oProt.writeMapBegin(.{ .kType = keyTType, .vType = valTType, .size = @intCast(self.map.count()) });
            var iter = self.map.iterator();
            while (iter.next()) |pair| {
                try protocol.writeToProtocol(K, pair.key_ptr.*, oProt);
                try protocol.writeToProtocol(V, pair.value_ptr.*, oProt);
            }
            try oProt.writeMapEnd();
        }

        pub fn readFromProtocol(allocator: Allocator, prot: *protocol.TProtocol) !@This() {
            const mapIdent = try prot.readMapBegin();
            // Compact protocol encodes empty maps as a single zero byte with no type info.
            if (mapIdent.size > 0) {
                try protocol.validateWireType(protocol.typeToTType(K), mapIdent.kType);
                try protocol.validateWireType(protocol.typeToTType(V), mapIdent.vType);
            }
            var val: @This() = .init(allocator);
            errdefer val.deinit();
            for (0..@intCast(mapIdent.size)) |_| {
                try val.put(try protocol.readFromProtocol(K, allocator, prot), try protocol.readFromProtocol(V, allocator, prot));
            }
            try prot.readMapEnd();
            return val;
        }
    };
}
