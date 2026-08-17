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
const ort = @import("optional_required_test");
const fuzz = @import("fuzz_test");

const t = std.testing;

fn testString(allocator: std.mem.Allocator, value: []const u8) !thrift.String {
    return try thrift.String.initFromSlice(allocator, value);
}

fn testBinary(allocator: std.mem.Allocator, value: []const u8) !thrift.BinaryBytes {
    return try thrift.BinaryBytes.initFromSlice(allocator, value);
}

fn expectAllIsset(value: anytype) !void {
    inline for (@typeInfo(@TypeOf(value.__isset)).@"struct".field_names) |fieldName| {
        try t.expect(@field(value.__isset, fieldName));
    }
}

test "Simple init requires default and required fields" {
    var value = try ort.Simple.init(t.allocator, .{
        .im_default = 7,
        .im_required = 99,
    });
    defer value.deinit();

    try t.expectEqual(@as(i16, 7), value.im_default);
    try t.expectEqual(@as(i16, 99), value.im_required);
    try t.expect(value.im_optional == null);
    try t.expect(value.__isset.im_default);
    try t.expect(value.__isset.im_required);
    try t.expect(!value.__isset.im_optional);
}

test "Simple eql distinguishes unset optional from set optional" {
    var with_optional = try ort.Simple.init(t.allocator, .{
        .im_default = 1,
        .im_required = 2,
        .im_optional = 3,
    });
    defer with_optional.deinit();

    var without_optional = try ort.Simple.init(t.allocator, .{
        .im_default = 1,
        .im_required = 2,
    });
    defer without_optional.deinit();

    try t.expect(!with_optional.eql(without_optional));
    try t.expect(!without_optional.eql(with_optional));
    try t.expect(without_optional.eql(without_optional));
}

test "Simple init applies optional override" {
    var value = try ort.Simple.init(t.allocator, .{
        .im_default = 1,
        .im_required = 2,
        .im_optional = 3,
    });
    defer value.deinit();

    try t.expectEqual(@as(?i16, 3), value.im_optional);
    try t.expect(value.__isset.im_optional);
}

test "Tricky2 init with no args keeps optional unset" {
    var value = try ort.Tricky2.init(t.allocator, .{});
    defer value.deinit();

    try t.expect(value.im_optional == null);
    try t.expect(!value.__isset.im_optional);
}

test "OptionalDefault readFromProtocol empty struct does not leak IDL default string" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();

    try prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "",
    });
    try prot.writeFieldStop();
    try prot.writeStructEnd();
    try prot.flush();

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    var decoded = try ort.OptionalDefault.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expect(decoded.__isset.opt_int);
    try t.expect(decoded.__isset.opt_str);
    try t.expectEqual(@as(?i16, 1234), decoded.opt_int);
    try t.expectEqualStrings("default", decoded.opt_str.?.contents);
}

test "OptionalDefault init keeps IDL defaults when args omitted" {
    var value = try ort.OptionalDefault.init(t.allocator, .{});
    defer value.deinit();

    try t.expectEqual(@as(?i16, 1234), value.opt_int);
    const opt_str = value.opt_str.?;
    try t.expectEqualStrings("default", opt_str.contents);
    try expectAllIsset(value);
}

test "OptionalDefault init overrides provided fields only" {
    const custom_str = try testString(t.allocator, "custom");
    var value = try ort.OptionalDefault.init(t.allocator, .{
        .opt_int = 42,
        .opt_str = custom_str,
    });
    defer value.deinit();

    try t.expectEqual(@as(?i16, 42), value.opt_int);
    try t.expectEqualStrings("custom", value.opt_str.?.contents);
}

test "ManyOpt init requires only default-requiredness field" {
    var value = try ort.ManyOpt.init(t.allocator, .{
        .def4 = 77,
    });
    defer value.deinit();

    try t.expect(value.opt1 == null);
    try t.expect(value.opt2 == null);
    try t.expect(value.opt3 == null);
    try t.expectEqual(@as(i32, 77), value.def4);
    try t.expect(value.opt5 == null);
    try t.expect(value.opt6 == null);
    try t.expect(!value.__isset.opt1);
    try t.expect(!value.__isset.opt2);
    try t.expect(!value.__isset.opt3);
    try t.expect(value.__isset.def4);
    try t.expect(!value.__isset.opt5);
    try t.expect(!value.__isset.opt6);
}

test "Requiredness init keeps IDL defaults for omitted optional and defaulted fields" {
    var value = try fuzz.Requiredness.init(t.allocator, .{
        .req_field = 10,
        .default_field = 20,
    });
    defer value.deinit();

    try t.expectEqual(@as(i32, 10), value.req_field);
    try t.expect(value.opt_field == null);
    try t.expectEqual(@as(i32, 20), value.default_field);
    try t.expectEqualStrings("test", value.opt_with_default.?.contents);
    try t.expect(value.req_with_default);
    try t.expect(value.__isset.req_field);
    try t.expect(!value.__isset.opt_field);
    try t.expect(value.__isset.default_field);
    try t.expect(value.__isset.opt_with_default);
    try t.expect(value.__isset.req_with_default);
}

test "Requiredness init overrides fields with IDL defaults" {
    const custom = try testString(t.allocator, "override");
    var value = try fuzz.Requiredness.init(t.allocator, .{
        .req_field = 1,
        .default_field = 2,
        .opt_with_default = custom,
        .req_with_default = false,
    });
    defer value.deinit();

    try t.expectEqualStrings("override", value.opt_with_default.?.contents);
    try t.expect(!value.req_with_default);
}

test "JavaTestHelper init with heap fields does not leak" {
    const req_obj = try testString(t.allocator, "required");
    const req_bin = try testBinary(t.allocator, "bytes");
    const opt_obj = try testString(t.allocator, "optional");

    var value = try ort.JavaTestHelper.init(t.allocator, .{
        .req_int = 5,
        .opt_int = 6,
        .req_obj = req_obj,
        .opt_obj = opt_obj,
        .req_bin = req_bin,
    });
    defer value.deinit();

    try t.expectEqual(@as(i32, 5), value.req_int);
    try t.expectEqual(@as(?i32, 6), value.opt_int);
    try t.expectEqualStrings("required", value.req_obj.contents);
    try t.expectEqualStrings("optional", value.opt_obj.?.contents);
    try t.expectEqualStrings("bytes", value.req_bin.contents);
    try t.expect(value.opt_bin == null);
    try t.expect(value.__isset.req_int);
    try t.expect(value.__isset.opt_int);
    try t.expect(value.__isset.req_obj);
    try t.expect(value.__isset.opt_obj);
    try t.expect(value.__isset.req_bin);
    try t.expect(!value.__isset.opt_bin);
}

test "Complex init owns nested struct and container" {
    const req_simp = try ort.Simple.init(t.allocator, .{
        .im_default = 10,
        .im_required = 20,
    });
    const the_map = thrift.Map(i16, ort.Simple).init(t.allocator);

    var value = try ort.Complex.init(t.allocator, .{
        .cp_default = 1,
        .cp_required = 2,
        .the_map = the_map,
        .req_simp = req_simp,
    });
    defer value.deinit();

    try t.expect(value.cp_optional == null);
    try t.expect(value.opt_simp == null);
    try t.expectEqual(@as(i16, 20), value.req_simp.im_required);
    try t.expectEqual(@as(usize, 0), value.the_map.count());
    try t.expect(value.__isset.cp_default);
    try t.expect(value.__isset.cp_required);
    try t.expect(!value.__isset.cp_optional);
    try t.expect(value.__isset.the_map);
    try t.expect(value.__isset.req_simp);
    try t.expect(!value.__isset.opt_simp);
}

test "OldSchool init with heap containers does not leak" {
    const im_str = try testString(t.allocator, "hello");
    const im_big = thrift.List(thrift.Map(i32, thrift.String)).init(t.allocator);

    var value = try ort.OldSchool.init(t.allocator, .{
        .im_int = 42,
        .im_str = im_str,
        .im_big = im_big,
    });
    defer value.deinit();

    try t.expectEqual(@as(i16, 42), value.im_int);
    try t.expectEqualStrings("hello", value.im_str.contents);
    try t.expectEqual(@as(usize, 0), value.im_big.count());
    try expectAllIsset(value);
}

test "Requiredness readFromProtocol empty struct rejects missing required fields" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();

    try prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "",
    });
    try prot.writeFieldStop();
    try prot.writeStructEnd();
    try prot.flush();

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    try t.expectError(thrift.protocol.ProtocolError.MissingField, fuzz.Requiredness.readFromProtocol(t.allocator, &read_prot));
}

test "initDefault failure returns OutOfMemory without leaking" {
    var fail_alloc = std.testing.FailingAllocator.init(t.allocator, .{ .fail_index = 0 });
    const result = ort.OptionalDefault.init(fail_alloc.allocator(), .{});
    try t.expectError(error.OutOfMemory, result);
}
