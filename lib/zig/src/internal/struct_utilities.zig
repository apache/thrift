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

pub inline fn hash(hasher: anytype, v: anytype) void {
    switch (@typeInfo(@TypeOf(v))) {
        .int, .@"enum", .bool => std.hash.autoHash(hasher, v),
        .float => std.hash.autoHash(hasher, @as(u64, @bitCast(v))),
        .@"struct", .@"union" => v.hash(hasher),
        .pointer => |ptr| {
            switch (ptr.size) {
                .one => hash(hasher, v.*),
                else => @compileError("unsupported pointer kind: " ++ @typeName(@TypeOf(v))),
            }
        },
        .optional => {
            if (v) |val| {
                hash(hasher, val);
            }
        },
        else => @compileError("Hashing for type " ++ @typeName(@TypeOf(v)) ++ " not supported."),
    }
}

pub inline fn eql(a: anytype, b: anytype) bool {
    comptime if (@TypeOf(a) != @TypeOf(b)) {
        @compileError("types up for comparison cannot be different");
    };

    switch (@typeInfo(@TypeOf(a))) {
        .int, .float, .@"enum", .bool => return a == b,
        .@"struct", .@"union" => return a.eql(b),
        .pointer => |ptr| {
            switch (ptr.size) {
                .one => return eql(a.*, b.*),
                else => @compileError("unsupported pointer kind: " ++ @typeName(@TypeOf(a))),
            }
        },
        .optional => {
            if (a) |val| {
                if (b) |b_val| return eql(val, b_val);
                return false;
            }
            return b == null;
        },
        else => @compileError("Equality for type " ++ @typeName(@TypeOf(a)) ++ " not supported."),
    }
}

pub inline fn unionHash(hasher: anytype, v: anytype) void {
    switch (v) {
        inline else => |val, tag| {
            std.hash.autoHash(hasher, @backingInt(tag));
            switch (@typeInfo(@TypeOf(val))) {
                .void => {},
                else => hash(hasher, val),
            }
        },
    }
}

pub inline fn unionEql(a: anytype, b: anytype) bool {
    comptime if (@TypeOf(a) != @TypeOf(b)) {
        @compileError("types up for comparison cannot be different");
    };

    switch (a) {
        inline else => |a_val, a_tag| {
            switch (b) {
                inline else => |b_val, b_tag| {
                    if (a_tag != b_tag) return false;
                    switch (@typeInfo(@TypeOf(a_val))) {
                        .void => return true,
                        else => return eql(a_val, b_val),
                    }
                },
            }
        },
    }
}
