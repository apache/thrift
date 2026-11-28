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
const thrift = @import("thrift");
const cd = @import("constants_demo");
const fuzz = @import("fuzz_test");

const t = std.testing;
const utils = thrift.collections.utils;

fn expectContext(comptime T: type, a: T, b: T, expect_equal: bool) !void {
    const Context = utils.selectContext(T);
    const ctx = Context{};

    try t.expectEqual(expect_equal, ctx.eql(a, b));
    try t.expectEqual(expect_equal, ctx.hash(a) == ctx.hash(b));
}

fn testString(allocator: std.mem.Allocator, value: []const u8) !thrift.String {
    return try thrift.String.initFromSlice(allocator, value);
}

fn testBinary(allocator: std.mem.Allocator, value: []const u8) !thrift.BinaryBytes {
    return try thrift.BinaryBytes.initFromSlice(allocator, value);
}

test "PrimitiveContext hashes and compares primitives" {
    try expectContext(i32, 42, 42, true);
    try expectContext(i32, 42, 43, false);
    try expectContext(f64, 1.5, 1.5, true);
    try expectContext(f64, 1.5, 2.5, false);
}

test "PrimitiveContext hashes and compares enums" {
    try expectContext(fuzz.TestEnum, .ONE, .ONE, true);
    try expectContext(fuzz.TestEnum, .ONE, .TWO, false);
}

test "StructContext hashes and compares string and uuid values" {
    var hello = try testString(t.allocator, "hello");
    defer hello.deinit();
    var world = try testString(t.allocator, "world");
    defer world.deinit();
    var hello_copy = try testString(t.allocator, "hello");
    defer hello_copy.deinit();

    try expectContext(thrift.String, hello, hello_copy, true);
    try expectContext(thrift.String, hello, world, false);
    try expectContext(thrift.UUID, cd.GEN_UUID, cd.MY_UUID, true);
    try expectContext(thrift.UUID, cd.GEN_UUID, cd.GEN_GUID, false);
}

test "StructContext hashes and compares generated structs" {
    const str_a = try testString(t.allocator, "alpha");
    const bin_a = try testBinary(t.allocator, &[_]u8{ 1, 2, 3 });
    var a = try fuzz.BasicTypes.init(t.allocator, .{
        .bool_field = true,
        .byte_field = 7,
        .i16_field = 16,
        .i32_field = 32,
        .i64_field = 64,
        .double_field = 3.14,
        .string_field = str_a,
        .binary_field = bin_a,
    });
    defer a.deinit();

    const str_b = try testString(t.allocator, "alpha");
    const bin_b = try testBinary(t.allocator, &[_]u8{ 1, 2, 3 });
    var b = try fuzz.BasicTypes.init(t.allocator, .{
        .bool_field = true,
        .byte_field = 7,
        .i16_field = 16,
        .i32_field = 32,
        .i64_field = 64,
        .double_field = 3.14,
        .string_field = str_b,
        .binary_field = bin_b,
    });
    defer b.deinit();

    const str_c = try testString(t.allocator, "beta");
    const bin_c = try testBinary(t.allocator, &[_]u8{9});
    var c = try fuzz.BasicTypes.init(t.allocator, .{
        .bool_field = false,
        .byte_field = 7,
        .i16_field = 16,
        .i32_field = 32,
        .i64_field = 64,
        .double_field = 3.14,
        .string_field = str_c,
        .binary_field = bin_c,
    });
    defer c.deinit();

    try expectContext(fuzz.BasicTypes, a, b, true);
    try expectContext(fuzz.BasicTypes, a, c, false);
}

test "StructContext hashes and compares union variants" {
    const int_a: fuzz.TestUnion = .{ .int_field = 5 };
    const int_b: fuzz.TestUnion = .{ .int_field = 5 };
    const int_c: fuzz.TestUnion = .{ .int_field = 6 };

    const str_u: fuzz.TestUnion = .{ .string_field = thrift.String.initFromBorrowed("5") };

    try expectContext(fuzz.TestUnion, int_a, int_b, true);
    try expectContext(fuzz.TestUnion, int_a, int_c, false);
    try expectContext(fuzz.TestUnion, int_a, str_u, false);
}
