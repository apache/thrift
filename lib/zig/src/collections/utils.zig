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

const Allocator = std.mem.Allocator;

pub fn containsDeinit(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .@"struct", .@"union", .@"enum" => {},
        else => return false,
    }

    return @hasDecl(T, "deinit");
}

pub fn cloneVar(comptime T: type, allocator: Allocator, val: T) !T {
    switch (@typeInfo(T)) {
        .@"struct", .@"union" => return try val.clone(allocator),
        else => return val,
    }
}

pub fn equals(comptime T: type, v1: T, v2: T) bool {
    switch (@typeInfo(T)) {
        .@"struct", .@"union" => return v1.eql(v2),
        else => return v1 == v2,
    }
}

pub fn format(comptime T: type, writer: *std.Io.Writer, val: T) !void {
    switch (@typeInfo(T)) {
        .@"struct", .@"union" => try writer.print("{f}", .{val}),
        .@"enum" => try writer.print("{s}", .{@tagName(val)}),
        else => try writer.print("{}", .{val}),
    }
}

pub fn selectContext(comptime T: type) type {
    switch (@typeInfo(T)) {
        .@"enum" => return PrimitiveContext(T),
        .@"struct", .@"union" => return StructContext(T),
        else => {},
    }

    const ttype = comptime protocol.typeToTType(T);
    switch (ttype) {
        .List, .Map, .Set, .String, .Uuid, .Struct => return StructContext(T),
        .Bool, .Byte, .Double, .I16, .I32, .I64, .I8 => return PrimitiveContext(T),
        else => @compileError("Unsupported type as key"),
    }
}

pub fn PrimitiveContext(T: type) type {
    return struct {
        pub fn hash(_: @This(), v: T) u64 {
            var hasher = std.hash.Wyhash.init(0);
            switch (@typeInfo(T)) {
                .float => std.hash.autoHash(&hasher, @as(u64, @bitCast(v))),
                else => std.hash.autoHash(&hasher, v),
            }
            return hasher.final();
        }
        pub fn eql(_: @This(), a: T, b: T) bool {
            return a == b;
        }
    };
}

pub fn StructContext(S: type) type {
    switch (@typeInfo(S)) {
        .@"struct", .@"union" => {},
        else => @compileError("Provided type is not a struct or union"),
    }

    return struct {
        pub fn hash(_: @This(), v: S) u64 {
            var hasher = std.hash.Wyhash.init(0);
            v.hash(&hasher);
            return hasher.final();
        }
        pub fn eql(_: @This(), a: S, b: S) bool {
            return a.eql(b);
        }
    };
}
