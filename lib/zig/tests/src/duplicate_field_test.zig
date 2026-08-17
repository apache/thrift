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

const t = std.testing;
const TTestingMemoryTransport = thrift.transport.TTestingMemoryTransport;
const TBinaryProtocol = thrift.TBinaryProtocol;
const ProtocolError = thrift.protocol.ProtocolError;
const TFieldIdentifier = thrift.protocol.TFieldIdentifier;
const TStructIdentifier = thrift.protocol.TStructIdentifier;

var config = thrift.TConfiguration.default;

fn testString(allocator: std.mem.Allocator, value: []const u8) !thrift.String {
    return try thrift.String.initFromSlice(allocator, value);
}

fn field(id: i16, field_type: thrift.protocol.TType) TFieldIdentifier {
    return .{
        .allocator = undefined,
        .name = null,
        .fieldType = field_type,
        .id = id,
    };
}

fn writeStructBegin(prot: *TBinaryProtocol) !void {
    try prot.writeStructBegin(TStructIdentifier{
        .allocator = undefined,
        .name = "",
    });
}

fn writeStructEnd(prot: *TBinaryProtocol) !void {
    try prot.writeFieldStop();
    try prot.writeStructEnd();
}

fn writeI32Field(prot: *TBinaryProtocol, id: i16, value: i32) !void {
    try prot.writeFieldBegin(field(id, .I32));
    try prot.writeI32(value);
    try prot.writeFieldEnd();
}

fn writeStringField(allocator: std.mem.Allocator, prot: *TBinaryProtocol, id: i16, value: []const u8) !void {
    try prot.writeFieldBegin(field(id, .String));
    var s = try testString(allocator, value);
    defer s.deinit();
    try prot.writeString(s);
    try prot.writeFieldEnd();
}

fn writeI32ListField(prot: *TBinaryProtocol, id: i16, values: []const i32) !void {
    try prot.writeFieldBegin(field(id, .List));
    try prot.writeListBegin(.{ .eType = .I32, .size = @intCast(values.len) });
    for (values) |v| {
        try prot.writeI32(v);
    }
    try prot.writeListEnd();
    try prot.writeFieldEnd();
}

fn writeBadElementTypeListField(allocator: std.mem.Allocator, prot: *TBinaryProtocol, id: i16, e_type: thrift.protocol.TType, size: i32) !void {
    try prot.writeFieldBegin(field(id, .List));
    try prot.writeListBegin(.{ .eType = e_type, .size = size });
    var s = try testString(allocator, "bad");
    defer s.deinit();
    try prot.writeString(s);
    try prot.writeListEnd();
    try prot.writeFieldEnd();
}

fn writeI32MapField(prot: *TBinaryProtocol, id: i16, entries: []const struct { i32, i32 }) !void {
    try prot.writeFieldBegin(field(id, .Map));
    try prot.writeMapBegin(.{ .kType = .I32, .vType = .I32, .size = @intCast(entries.len) });
    for (entries) |entry| {
        try prot.writeI32(entry[0]);
        try prot.writeI32(entry[1]);
    }
    try prot.writeMapEnd();
    try prot.writeFieldEnd();
}

fn buildTransport(
    allocator: std.mem.Allocator,
    io: std.Io,
    write_payload: *const fn (std.mem.Allocator, *TBinaryProtocol) anyerror!void,
) !TTestingMemoryTransport {
    var transport: TTestingMemoryTransport = try .init(allocator, io);
    var transport_itf = transport.interface();

    var prot: TBinaryProtocol = try .init(allocator, &transport_itf, &config);
    defer prot.deinit();

    try writeStructBegin(&prot);
    try write_payload(allocator, &prot);
    try writeStructEnd(&prot);
    try prot.flush();

    return transport;
}

fn readStructA(allocator: std.mem.Allocator, transport: *TTestingMemoryTransport) !thrift_test.StructA {
    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();
    return try thrift_test.StructA.readFromProtocol(allocator, &read_prot);
}

test "duplicate string field keeps last value on the wire" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(allocator: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeStringField(allocator, prot, 1, "first");
            try writeStringField(allocator, prot, 1, "last");
        }
    }.payload);
    defer transport.deinit();

    var decoded = try readStructA(t.allocator, &transport);
    defer decoded.deinit();

    try t.expect(decoded.__isset.s);
    try t.expectEqualStrings("last", decoded.s.contents);
}

test "duplicate primitive field keeps last value on the wire" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(_: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeI32Field(prot, 2, 10);
            try writeI32Field(prot, 2, 20);
        }
    }.payload);
    defer transport.deinit();

    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();

    var decoded = try thrift_test.VersioningTestV2.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expect(decoded.__isset.newint);
    try t.expectEqual(@as(i32, 20), decoded.newint);
}

test "duplicate list field keeps last value on the wire" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(_: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeI32ListField(prot, 1, &.{ 1, 2 });
            try writeI32ListField(prot, 1, &.{ 3, 4, 5 });
        }
    }.payload);
    defer transport.deinit();

    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();

    var decoded = try thrift_test.ListTypeVersioningV1.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expect(decoded.__isset.myints);
    try t.expectEqual(@as(usize, 3), decoded.myints.count());
    try t.expectEqual(@as(i32, 3), decoded.myints.items()[0]);
    try t.expectEqual(@as(i32, 4), decoded.myints.items()[1]);
    try t.expectEqual(@as(i32, 5), decoded.myints.items()[2]);
}

test "duplicate map field keeps last value on the wire" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(_: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeI32MapField(prot, 10, &.{.{ 1, 100 }});
            try writeI32MapField(prot, 10, &.{ .{ 2, 200 }, .{ 3, 300 } });
        }
    }.payload);
    defer transport.deinit();

    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();

    var decoded = try thrift_test.VersioningTestV2.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expect(decoded.__isset.newmap);
    try t.expectEqual(@as(usize, 2), decoded.newmap.count());
    try t.expectEqual(@as(i32, 200), decoded.newmap.get(2).?);
    try t.expectEqual(@as(i32, 300), decoded.newmap.get(3).?);
}

test "duplicate optional string field keeps last value on the wire" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(allocator: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeStringField(allocator, prot, 2, "first");
            try writeStringField(allocator, prot, 2, "last");
        }
    }.payload);
    defer transport.deinit();

    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();

    var decoded = try thrift_test.BoolTest.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expect(decoded.__isset.s);
    try t.expect(decoded.s != null);
    try t.expectEqualStrings("last", decoded.s.?.contents);
}

test "duplicate string field does not leak on successful last-wins read" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(allocator: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeStringField(allocator, prot, 1, "first");
            try writeStringField(allocator, prot, 1, "last");
        }
    }.payload);
    defer transport.deinit();

    var decoded = try readStructA(t.allocator, &transport);
    defer decoded.deinit();
}

test "duplicate list field does not leak on successful last-wins read" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(_: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeI32ListField(prot, 1, &.{ 1, 2, 3 });
            try writeI32ListField(prot, 1, &.{ 4, 5 });
        }
    }.payload);
    defer transport.deinit();

    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();

    var decoded = try thrift_test.ListTypeVersioningV1.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();
}

test "duplicate alloc field read failure does not double-free on errdefer cleanup" {
    var transport = try buildTransport(t.allocator, t.io, struct {
        fn payload(allocator: std.mem.Allocator, prot: *TBinaryProtocol) !void {
            try writeI32ListField(prot, 1, &.{ 1, 2, 3 });
            try writeBadElementTypeListField(allocator, prot, 1, .String, 1);
        }
    }.payload);
    defer transport.deinit();

    var transport_itf = transport.interface();
    var prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer prot.deinit();
    var read_prot = prot.interface();

    const result = thrift_test.ListTypeVersioningV1.readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
}
