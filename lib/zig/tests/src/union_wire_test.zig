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
const fuzz_test = @import("fuzz_test");

const t = std.testing;
const TProtocolFactory = thrift.TProtocolFactory;
const ProtocolSession = thrift.testing.protocol_suite.ProtocolSession;

fn roundTripUnion(
    allocator: std.mem.Allocator,
    io: std.Io,
    factory: *TProtocolFactory,
    original: anytype,
) !@TypeOf(original) {
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try original.writeToProtocol(session.outProt());
    try session.finishWrite(factory);

    const decoded = try @TypeOf(original).readFromProtocol(allocator, session.inProt());
    try t.expect((&original).eql(decoded));
    try session.expectFullyConsumed();
    return decoded;
}

test "TestUnion i32 arm round trips (binary)" {
    var config = thrift.TConfiguration.default;
    var factory = thrift.TBinaryProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    _ = try roundTripUnion(t.allocator, t.io, &factory_itf, fuzz_test.TestUnion{ .int_field = 42 });
}

test "TestUnion _unknown round trips (binary)" {
    var config = thrift.TConfiguration.default;
    var factory = thrift.TBinaryProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    _ = try roundTripUnion(t.allocator, t.io, &factory_itf, fuzz_test.TestUnion{ ._unknown = {} });
}

test "TestUnion deinit and clone for allocating arms" {
    var original = fuzz_test.TestUnion{
        .string_field = try thrift.String.initFromSlice(t.allocator, "hello"),
    };
    defer original.deinit();

    var cloned = try original.clone(t.allocator);
    defer cloned.deinit();

    try t.expect(original.eql(cloned));
    try t.expectEqualStrings("hello", cloned.string_field.contents);
}

test "TestUnion _unknown deinit and clone are no-ops" {
    var original = fuzz_test.TestUnion{ ._unknown = {} };
    original.deinit();

    var cloned = try original.clone(t.allocator);
    defer cloned.deinit();
    try t.expect(original.eql(cloned));
}

test "TestUnion empty struct deserializes to _unknown" {
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
    const decoded = try fuzz_test.TestUnion.readFromProtocol(t.allocator, &read_prot);
    try t.expect((&decoded).eql(fuzz_test.TestUnion{ ._unknown = {} }));
}

test "TestUnion multiple known fields skip extra payload and leave stream aligned" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();

    try prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "TestUnion",
    });
    try prot.writeFieldBegin(.{
        .allocator = undefined,
        .name = &"int_field".*,
        .fieldType = thrift.protocol.TType.I32,
        .id = 1,
    });
    try prot.writeI32(42);
    try prot.writeFieldEnd();
    try prot.writeFieldBegin(.{
        .allocator = undefined,
        .name = &"string_field".*,
        .fieldType = thrift.protocol.TType.String,
        .id = 2,
    });
    var extra = try thrift.String.initFromSlice(t.allocator, "extra");
    defer extra.deinit();
    try prot.writeString(extra);
    try prot.writeFieldEnd();
    try prot.writeFieldStop();
    try prot.writeStructEnd();
    try prot.flush();

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    try t.expectError(thrift.protocol.ProtocolError.UnionHasMultipleFields, fuzz_test.TestUnion.readFromProtocol(t.allocator, &read_prot));
    try transport.expectFullyConsumed();
}

test "TestUnion known field id with mismatched type skips payload and deserializes to _unknown" {
    var transport: thrift.transport.TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var config = thrift.TConfiguration.default;
    var prot: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();

    try prot.writeStructBegin(.{
        .allocator = undefined,
        .name = "TestUnion",
    });
    try prot.writeFieldBegin(.{
        .allocator = undefined,
        .name = &"int_field".*,
        .fieldType = thrift.protocol.TType.String,
        .id = 1,
    });
    var wrong_type_payload = try thrift.String.initFromSlice(t.allocator, "not-an-int");
    defer wrong_type_payload.deinit();
    try prot.writeString(wrong_type_payload);
    try prot.writeFieldEnd();
    try prot.writeFieldStop();
    try prot.writeStructEnd();
    try prot.flush();

    var read_prot_inst: thrift.TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer read_prot_inst.deinit();
    var read_prot = read_prot_inst.interface();
    const decoded = try fuzz_test.TestUnion.readFromProtocol(t.allocator, &read_prot);
    try t.expect((&decoded).eql(fuzz_test.TestUnion{ ._unknown = {} }));
    try transport.expectFullyConsumed();
}
