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
const thrift_test = @import("thrift_test");
const fuzz_test = @import("fuzz_test");

const t = std.testing;
const TProtocolFactory = thrift.TProtocolFactory;
const protocol_suite = thrift.testing.protocol_suite;

fn testString(allocator: std.mem.Allocator, value: []const u8) !thrift.String {
    return try thrift.String.initFromSlice(allocator, value);
}

fn testBinary(allocator: std.mem.Allocator, value: []const u8) !thrift.BinaryBytes {
    return try thrift.BinaryBytes.initFromSlice(allocator, value);
}

fn roundTrip(
    allocator: std.mem.Allocator,
    io: std.Io,
    factory: *TProtocolFactory,
    value: anytype,
) !void {
    try protocol_suite.roundTripStruct(allocator, io, factory, value);
}

fn makeBonk(allocator: std.mem.Allocator) !thrift_test.Bonk {
    return try thrift_test.Bonk.init(allocator, .{
        .message = try testString(allocator, "hello bonk"),
        .type = 42,
    });
}

fn makeBools(allocator: std.mem.Allocator) !thrift_test.Bools {
    return try thrift_test.Bools.init(allocator, .{
        .im_true = true,
        .im_false = false,
    });
}

fn makeXtruct(allocator: std.mem.Allocator) !thrift_test.Xtruct {
    return try thrift_test.Xtruct.init(allocator, .{
        .string_thing = try testString(allocator, "xtruct"),
        .byte_thing = 7,
        .i32_thing = 12345,
        .i64_thing = 9876543210,
    });
}

fn makeXtruct2(allocator: std.mem.Allocator) !thrift_test.Xtruct2 {
    return try thrift_test.Xtruct2.init(allocator, .{
        .byte_thing = 3,
        .struct_thing = try makeXtruct(allocator),
        .i32_thing = 99,
    });
}

fn makeVersioningTestV2(allocator: std.mem.Allocator) !thrift_test.VersioningTestV2 {
    var list = thrift.List(i32).init(allocator);
    try list.append(1);
    try list.append(2);

    var set = thrift.Set(i32).init(allocator);
    try set.put(10);

    var map = thrift.Map(i32, i32).init(allocator);
    try map.put(1, 100);

    return try thrift_test.VersioningTestV2.init(allocator, .{
        .begin_in_both = 1,
        .newint = 2,
        .newbyte = 3,
        .newshort = 4,
        .newlong = 5,
        .newdouble = 6.0,
        .newstruct = try thrift_test.Bonk.init(allocator, .{
            .message = try testString(allocator, "nested"),
            .type = 7,
        }),
        .newlist = list,
        .newset = set,
        .newmap = map,
        .newstring = try testString(allocator, "versioned"),
        .end_in_both = 8,
    });
}

fn makeLargeDeltas(allocator: std.mem.Allocator) !thrift_test.LargeDeltas {
    const bools = try makeBools(allocator);
    const bools_flipped = try thrift_test.Bools.init(allocator, .{
        .im_true = false,
        .im_false = true,
    });

    var a_set2500 = thrift.Set(thrift.String).init(allocator);
    try a_set2500.put(try testString(allocator, "lazy"));
    try a_set2500.put(try testString(allocator, "brown"));
    try a_set2500.put(try testString(allocator, "cow"));

    var vertwo3000_set = thrift.Set(i32).init(allocator);
    try vertwo3000_set.put(2);
    try vertwo3000_set.put(3);
    try vertwo3000_set.put(5);
    try vertwo3000_set.put(7);
    try vertwo3000_set.put(11);

    var big_numbers = thrift.List(i32).init(allocator);
    try big_numbers.append(1 << 8);
    try big_numbers.append(1 << 16);
    try big_numbers.append(std.math.maxInt(i32));
    try big_numbers.append(-std.math.maxInt(i32));

    return try thrift_test.LargeDeltas.init(allocator, .{
        .b1 = bools,
        .b10 = bools_flipped,
        .b100 = bools,
        .check_true = true,
        .b1000 = bools_flipped,
        .check_false = false,
        .vertwo2000 = try thrift_test.VersioningTestV2.init(allocator, .{
            .begin_in_both = 1,
            .newint = 2,
            .newbyte = 3,
            .newshort = 4,
            .newlong = 5,
            .newdouble = 6.0,
            .newstruct = try thrift_test.Bonk.init(allocator, .{
                .message = try testString(allocator, "World!"),
                .type = 314,
            }),
            .newlist = thrift.List(i32).init(allocator),
            .newset = thrift.Set(i32).init(allocator),
            .newmap = thrift.Map(i32, i32).init(allocator),
            .newstring = try testString(allocator, ""),
            .end_in_both = 8,
        }),
        .a_set2500 = a_set2500,
        .vertwo3000 = try thrift_test.VersioningTestV2.init(allocator, .{
            .begin_in_both = 1,
            .newint = 2,
            .newbyte = 3,
            .newshort = 4,
            .newlong = 5,
            .newdouble = 6.0,
            .newstruct = try thrift_test.Bonk.init(allocator, .{ .message = try testString(allocator, ""), .type = 0 }),
            .newlist = thrift.List(i32).init(allocator),
            .newset = vertwo3000_set,
            .newmap = thrift.Map(i32, i32).init(allocator),
            .newstring = try testString(allocator, ""),
            .end_in_both = 8,
        }),
        .big_numbers = big_numbers,
    });
}

fn makeInsanity(allocator: std.mem.Allocator) !thrift_test.Insanity {
    var user_map = thrift.Map(thrift_test.Numberz, i64).init(allocator);
    try user_map.put(.TWO, 2000);

    var xtructs = thrift.List(thrift_test.Xtruct).init(allocator);
    try xtructs.append(try makeXtruct(allocator));

    return try thrift_test.Insanity.init(allocator, .{
        .userMap = user_map,
        .xtructs = xtructs,
    });
}

fn makeBasicTypes(allocator: std.mem.Allocator) !fuzz_test.BasicTypes {
    return try fuzz_test.BasicTypes.init(allocator, .{
        .bool_field = true,
        .byte_field = -8,
        .i16_field = 16000,
        .i32_field = 1_000_000,
        .i64_field = 9_000_000_000,
        .double_field = 3.14159,
        .string_field = try testString(allocator, "basic"),
        .binary_field = try testBinary(allocator, "deadbeef"),
    });
}

fn makeRequiredness(allocator: std.mem.Allocator) !fuzz_test.Requiredness {
    return try fuzz_test.Requiredness.init(allocator, .{
        .req_field = 1,
        .default_field = 2,
    });
}

fn makeFieldIDTest(allocator: std.mem.Allocator) !fuzz_test.FieldIDTest {
    return try fuzz_test.FieldIDTest.init(allocator, .{
        .first = 1,
        .gap = 100,
        .medium_id = 255,
        .large_id = 32767,
    });
}

fn runThriftTestFixtures(factory: *TProtocolFactory) !void {
    {
        var bonk = try makeBonk(t.allocator);
        defer bonk.deinit();
        try roundTrip(t.allocator, t.io, factory, bonk);
    }
    {
        var bools = try makeBools(t.allocator);
        defer bools.deinit();
        try roundTrip(t.allocator, t.io, factory, bools);
    }
    {
        var xtruct = try makeXtruct(t.allocator);
        defer xtruct.deinit();
        try roundTrip(t.allocator, t.io, factory, xtruct);
    }
    {
        var xtruct2 = try makeXtruct2(t.allocator);
        defer xtruct2.deinit();
        try roundTrip(t.allocator, t.io, factory, xtruct2);
    }
    {
        var versioning = try makeVersioningTestV2(t.allocator);
        defer versioning.deinit();
        try roundTrip(t.allocator, t.io, factory, versioning);
    }
    {
        var insanity = try makeInsanity(t.allocator);
        defer insanity.deinit();
        try roundTrip(t.allocator, t.io, factory, insanity);
    }
    {
        var large_deltas = try makeLargeDeltas(t.allocator);
        defer large_deltas.deinit();
        try roundTrip(t.allocator, t.io, factory, large_deltas);
    }
}

fn runFuzzTestFixtures(factory: *TProtocolFactory) !void {
    {
        var basic = try makeBasicTypes(t.allocator);
        defer basic.deinit();
        try roundTrip(t.allocator, t.io, factory, basic);
    }
    {
        var requiredness = try makeRequiredness(t.allocator);
        defer requiredness.deinit();
        try roundTrip(t.allocator, t.io, factory, requiredness);
    }
    {
        var field_ids = try makeFieldIDTest(t.allocator);
        defer field_ids.deinit();
        try roundTrip(t.allocator, t.io, factory, field_ids);
    }
}

test "ThriftTest structs round-trip (binary)" {
    var config = thrift.TConfiguration.default;
    var factory = thrift.TBinaryProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    try runThriftTestFixtures(&factory_itf);
}

test "ThriftTest structs round-trip (compact)" {
    var config = thrift.TConfiguration.default;
    var factory = thrift.TCompactProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    try runThriftTestFixtures(&factory_itf);
}

test "FuzzTestNoUuid structs round-trip (binary)" {
    var config = thrift.TConfiguration.default;
    var factory = thrift.TBinaryProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    try runFuzzTestFixtures(&factory_itf);
}

test "FuzzTestNoUuid structs round-trip (compact)" {
    var config = thrift.TConfiguration.default;
    var factory = thrift.TCompactProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    try runFuzzTestFixtures(&factory_itf);
}
