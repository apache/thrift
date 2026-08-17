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
const TCompactProtocol = thrift.TCompactProtocol;
const ProtocolError = thrift.protocol.ProtocolError;

var config = thrift.TConfiguration.default;

fn testString(allocator: std.mem.Allocator, value: []const u8) !thrift.String {
    return try thrift.String.initFromSlice(allocator, value);
}

test "list read rejects wrong element wire type without consuming elements (binary)" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeListBegin(.{ .eType = .String, .size = 1 });
    var bad_str = try testString(t.allocator, "bad");
    defer bad_str.deinit();
    try out_prot.writeString(bad_str);
    try out_prot.writeListEnd();
    try out_prot.flush();

    const header_size: usize = 5; // TType byte + i32 size

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    const result = thrift.List(i32).readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
    try t.expectEqual(header_size, in_prot.reader.seek);
}

test "set read rejects wrong element wire type without consuming elements (binary)" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeSetBegin(.{ .eType = .String, .size = 1 });
    var bad_str = try testString(t.allocator, "bad");
    defer bad_str.deinit();
    try out_prot.writeString(bad_str);
    try out_prot.writeSetEnd();
    try out_prot.flush();

    const header_size: usize = 5;

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    const result = thrift.Set(i32).readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
    try t.expectEqual(header_size, in_prot.reader.seek);
}

test "map read rejects wrong key wire type without consuming entries (binary)" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeMapBegin(.{ .kType = .String, .vType = .I32, .size = 1 });
    var key_str = try testString(t.allocator, "key");
    defer key_str.deinit();
    try out_prot.writeString(key_str);
    try out_prot.writeI32(42);
    try out_prot.writeMapEnd();
    try out_prot.flush();

    const header_size: usize = 6; // kType + vType + i32 size

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    const result = thrift.Map(i32, i32).readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
    try t.expectEqual(header_size, in_prot.reader.seek);
}

test "map read rejects wrong value wire type without consuming entries (binary)" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeMapBegin(.{ .kType = .I32, .vType = .String, .size = 1 });
    try out_prot.writeI32(1);
    var bad_str = try testString(t.allocator, "bad");
    defer bad_str.deinit();
    try out_prot.writeString(bad_str);
    try out_prot.writeMapEnd();
    try out_prot.flush();

    const header_size: usize = 6;

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    const result = thrift.Map(i32, i32).readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
    try t.expectEqual(header_size, in_prot.reader.seek);
}

test "list read rejects wrong element wire type (compact)" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TCompactProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeListBegin(.{ .eType = .String, .size = 1 });
    var bad_str = try testString(t.allocator, "bad");
    defer bad_str.deinit();
    try out_prot.writeString(bad_str);
    try out_prot.writeListEnd();
    try out_prot.flush();

    var in_prot: TCompactProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    const result = thrift.List(i32).readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
    try t.expect(in_prot.reader.seek < in_prot.reader.end);
}

test "generated struct list field rejects wrong container element type" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeStructBegin(.{ .allocator = undefined, .name = "" });
    try out_prot.writeFieldBegin(.{
        .allocator = undefined,
        .name = null,
        .fieldType = .List,
        .id = 1,
    });
    try out_prot.writeListBegin(.{ .eType = .String, .size = 1 });
    var bad_str = try testString(t.allocator, "bad");
    defer bad_str.deinit();
    try out_prot.writeString(bad_str);
    try out_prot.writeListEnd();
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldStop();
    try out_prot.writeStructEnd();
    try out_prot.flush();

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    const result = thrift_test.ListTypeVersioningV1.readFromProtocol(t.allocator, &read_prot);
    try t.expectError(ProtocolError.InvalidTypeWireValue, result);
}

test "map read keeps last value for duplicate keys on the wire" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeMapBegin(.{ .kType = .I32, .vType = .I32, .size = 3 });
    try out_prot.writeI32(1);
    try out_prot.writeI32(100);
    try out_prot.writeI32(2);
    try out_prot.writeI32(200);
    try out_prot.writeI32(1);
    try out_prot.writeI32(300);
    try out_prot.writeMapEnd();
    try out_prot.flush();

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    var decoded = try thrift.Map(i32, i32).readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expectEqual(@as(usize, 2), decoded.count());
    try t.expectEqual(@as(i32, 300), decoded.get(1).?);
    try t.expectEqual(@as(i32, 200), decoded.get(2).?);
}

test "map read with duplicate string values does not leak" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeMapBegin(.{ .kType = .I32, .vType = .String, .size = 2 });
    try out_prot.writeI32(1);
    var first = try testString(t.allocator, "first");
    defer first.deinit();
    try out_prot.writeString(first);
    try out_prot.writeI32(1);
    var last = try testString(t.allocator, "last");
    defer last.deinit();
    try out_prot.writeString(last);
    try out_prot.writeMapEnd();
    try out_prot.flush();

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    var decoded = try thrift.Map(i32, thrift.String).readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expectEqual(@as(usize, 1), decoded.count());
    try t.expectEqualStrings("last", decoded.get(1).?.contents);
}

test "set read keeps first value for duplicate elements on the wire" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeSetBegin(.{ .eType = .I32, .size = 3 });
    try out_prot.writeI32(10);
    try out_prot.writeI32(20);
    try out_prot.writeI32(10);
    try out_prot.writeSetEnd();
    try out_prot.flush();

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    var decoded = try thrift.Set(i32).readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expectEqual(@as(usize, 2), decoded.count());
    try t.expect(decoded.contains(10));
    try t.expect(decoded.contains(20));
}

test "set read with duplicate string elements does not leak" {
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeSetBegin(.{ .eType = .String, .size = 2 });
    var first = try testString(t.allocator, "first");
    defer first.deinit();
    try out_prot.writeString(first);
    var duplicate = try testString(t.allocator, "first");
    defer duplicate.deinit();
    try out_prot.writeString(duplicate);
    try out_prot.writeSetEnd();
    try out_prot.flush();

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    var decoded = try thrift.Set(thrift.String).readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expectEqual(@as(usize, 1), decoded.count());
    var it = decoded.iterator();
    const item = it.next().?;
    try t.expectEqualStrings("first", item.contents);
}

test "generated struct round-trips list/set/map fields" {
    var list = thrift.List(i32).init(t.allocator);
    try list.append(1);
    try list.append(2);
    try list.append(3);

    var set = thrift.Set(i32).init(t.allocator);
    try set.put(10);
    try set.put(20);

    var map = thrift.Map(i32, i32).init(t.allocator);
    try map.put(1, 100);
    try map.put(2, 200);

    var original = try thrift_test.VersioningTestV2.init(t.allocator, .{
        .begin_in_both = 1,
        .newint = 2,
        .newbyte = 3,
        .newshort = 4,
        .newlong = 5,
        .newdouble = 6.0,
        .newstruct = try thrift_test.Bonk.init(t.allocator, .{
            .message = try thrift.String.initFromSlice(t.allocator, "hi"),
            .type = 7,
        }),
        .newlist = list,
        .newset = set,
        .newmap = map,
        .newstring = try thrift.String.initFromSlice(t.allocator, "test"),
        .end_in_both = 8,
    });
    defer original.deinit();

    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    var write_prot = out_prot.interface();
    try original.writeToProtocol(&write_prot);
    try out_prot.flush();

    var in_prot: TBinaryProtocol = try .init(t.allocator, &transport_itf, &config);
    defer in_prot.deinit();

    var read_prot = in_prot.interface();
    var decoded = try thrift_test.VersioningTestV2.readFromProtocol(t.allocator, &read_prot);
    defer decoded.deinit();

    try t.expect(original.eql(decoded));
}
