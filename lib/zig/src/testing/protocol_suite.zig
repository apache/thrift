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
const mem = std.mem;

const protocol = @import("../protocol/protocol.zig");
const types = @import("../lib/types.zig");
const memory_transport = @import("memory_transport.zig");

const Allocator = mem.Allocator;
const TProtocol = protocol.TProtocol;
const TProtocolFactory = protocol.TProtocolFactory;
const TTransport = @import("../transport/interface.zig").TTransport;
const TFieldIdentifier = protocol.TFieldIdentifier;
const TStructIdentifier = protocol.TStructIdentifier;
const TMessageIdentifier = protocol.TMessageIdentifier;
const TTestingMemoryTransport = memory_transport.TTestingMemoryTransport;
const t = std.testing;

pub const ProtocolSession = struct {
    transport: TTestingMemoryTransport,
    transport_itf: TTransport,
    out: TProtocol,
    inp: ?TProtocol = null,
    allocator: Allocator,

    pub fn beginWrite(
        session: *ProtocolSession,
        allocator: Allocator,
        io: std.Io,
        factory: *TProtocolFactory,
    ) !void {
        session.* = .{
            .transport = try TTestingMemoryTransport.init(allocator, io),
            .transport_itf = undefined,
            .out = undefined,
            .allocator = allocator,
        };
        session.transport_itf = session.transport.interface();
        session.out = try factory.getProtocol(allocator, &session.transport_itf);
    }

    pub fn outProt(self: *ProtocolSession) *TProtocol {
        return &self.out;
    }

    pub fn finishWrite(self: *ProtocolSession, factory: *TProtocolFactory) !void {
        self.inp = try factory.getProtocol(self.allocator, &self.transport_itf);
    }

    pub fn inProt(self: *ProtocolSession) *TProtocol {
        return &self.inp.?;
    }

    pub fn deinit(self: *ProtocolSession) void {
        self.out.deinit();
        self.out.destroy(self.allocator);
        if (self.inp) |*in_prot| {
            in_prot.deinit();
            in_prot.destroy(self.allocator);
        }
        self.transport.deinit();
    }

    pub fn expectFullyConsumed(self: *ProtocolSession) !void {
        try self.transport.expectFullyConsumed();
    }
};

fn field(id: i16, field_type: protocol.TType) TFieldIdentifier {
    return .{
        .allocator = undefined,
        .name = null,
        .fieldType = field_type,
        .id = id,
    };
}

fn testNakedValues(
    allocator: Allocator,
    io: std.Io,
    factory: *TProtocolFactory,
    comptime T: type,
    values: []const T,
) !void {
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    for (values) |val| {
        try protocol.writeToProtocol(T, val, session.outProt());
    }
    try session.finishWrite(factory);

    for (values) |expected| {
        const actual = try protocol.readFromProtocol(T, allocator, session.inProt());
        if (@typeInfo(T) == .@"struct" and (T == types.String or T == types.BinaryBytes)) {
            defer actual.deinit();
        }
        if (@typeInfo(T) == .@"struct" and T == types.String) {
            try t.expectEqualStrings(expected.contents, actual.contents);
        } else if (@typeInfo(T) == .@"struct" and T == types.BinaryBytes) {
            try t.expectEqualSlices(u8, expected.contents, actual.contents);
        } else {
            try t.expectEqual(expected, actual);
        }
    }
    try session.expectFullyConsumed();
}

pub fn testI8(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, i8, &.{ -42, std.math.maxInt(i8), std.math.minInt(i8), 0 });
}

pub fn testByte(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, u8, &.{ 0xFF, 0x00, 0x42 });
}

pub fn testBool(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, bool, &.{ true, false });
}

pub fn testI16(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, i16, &.{
        -42, 0, 42, std.math.maxInt(i16), std.math.minInt(i16),
    });
}

pub fn testI32(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, i32, &.{
        1,                    0,                    -1,     190000013, -190000013, 42,    -42,
        std.math.maxInt(i32), std.math.minInt(i32), 128,    256,       1024,       65536, 1 << 20,
        -128,                 -256,                 -65536,
    });
}

pub fn testI64(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, i64, &.{
        1,                    0,                    -1,      42,         -42,     190000013,  -190000013,
        std.math.maxInt(i64), std.math.minInt(i64), 1 << 32, -(1 << 32), 1 << 48, -(1 << 48), 1_000_000_000_000,
        -1_000_000_000_000,
    });
}

pub fn testDouble(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    try testNakedValues(allocator, io, factory, f64, &.{
        0.0, 1.5, -3.14159, std.math.inf(f64), -std.math.inf(f64),
    });

    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();
    try session.outProt().writeDouble(std.math.nan(f64));
    try session.outProt().writeDouble(-0.0);
    try session.finishWrite(factory);

    const nan_val = try session.inProt().readDouble();
    try t.expect(std.math.isNan(nan_val));
    try t.expectEqual(@as(f64, -0.0), try session.inProt().readDouble());
    try session.expectFullyConsumed();
}

pub fn testString(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const values = [_][]const u8{ "hello", "", "world! 123" };
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    for (values) |s| {
        var str = try types.String.initFromSlice(allocator, s);
        defer str.deinit();
        try session.outProt().writeString(str);
    }
    try session.finishWrite(factory);

    for (values) |expected| {
        var actual = try session.inProt().readString(allocator);
        defer actual.deinit();
        try t.expectEqualStrings(expected, actual.contents);
    }
    try session.expectFullyConsumed();
}

pub fn testBytes(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const value1 = [_]u8{ 0xDE, 0xAD, 0xBE, 0xEF };
    const value2 = [_]u8{};
    const value3 = [_]u8{0xFF};

    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    var bytes1 = try types.BinaryBytes.initFromSlice(allocator, &value1);
    defer bytes1.deinit();
    var bytes2 = try types.BinaryBytes.initFromSlice(allocator, &value2);
    defer bytes2.deinit();
    var bytes3 = try types.BinaryBytes.initFromSlice(allocator, &value3);
    defer bytes3.deinit();

    try session.outProt().writeBytes(bytes1);
    try session.outProt().writeBytes(bytes2);
    try session.outProt().writeBytes(bytes3);
    try session.finishWrite(factory);

    var read1 = try session.inProt().readBytes(allocator);
    defer read1.deinit();
    try t.expectEqual(@as(usize, 4), read1.contents.len);
    try t.expectEqual(@as(u8, 0xDE), read1.contents[0]);
    try t.expectEqual(@as(u8, 0xEF), read1.contents[3]);

    var read2 = try session.inProt().readBytes(allocator);
    defer read2.deinit();
    try t.expectEqual(@as(usize, 0), read2.contents.len);

    var read3 = try session.inProt().readBytes(allocator);
    defer read3.deinit();
    try t.expectEqual(@as(usize, 1), read3.contents.len);
    try t.expectEqual(@as(u8, 0xFF), read3.contents[0]);
    try session.expectFullyConsumed();
}

pub fn testUuid(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const uuid = types.UUID{ .bytes = [_]u8{ 0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF, 0x01, 0x23, 0x45, 0x67, 0x89, 0xAB, 0xCD, 0xEF } };
    try testNakedValues(allocator, io, factory, types.UUID, &.{uuid});
}

pub fn testBoolInField(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try session.outProt().writeFieldBegin(field(1, .Bool));
    try session.outProt().writeBool(true);
    try session.outProt().writeFieldEnd();
    try session.finishWrite(factory);

    var read_field = try session.inProt().readFieldBegin(allocator);
    defer read_field.deinit();
    try t.expectEqual(.Bool, read_field.fieldType);
    try t.expectEqual(@as(i16, 1), read_field.id.?);
    try t.expectEqual(true, try session.inProt().readBool());
    try session.inProt().readFieldEnd();
    try session.expectFullyConsumed();
}

pub fn testFieldRoundTrip(
    allocator: Allocator,
    io: std.Io,
    factory: *TProtocolFactory,
    comptime T: type,
    val: T,
) !void {
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try session.outProt().writeFieldBegin(field(1, protocol.typeToTType(T)));
    try protocol.writeToProtocol(T, val, session.outProt());
    try session.outProt().writeFieldEnd();
    try session.finishWrite(factory);

    var read_field = try session.inProt().readFieldBegin(allocator);
    defer read_field.deinit();
    try t.expectEqual(@as(i16, 1), read_field.id.?);
    try t.expectEqual(protocol.typeToTType(T), read_field.fieldType);

    const actual = try protocol.readFromProtocol(T, allocator, session.inProt());
    if (@typeInfo(T) == .@"struct" and (T == types.String or T == types.BinaryBytes)) {
        defer actual.deinit();
    }
    if (@typeInfo(T) == .@"struct" and T == types.String) {
        try t.expectEqualStrings(val.contents, actual.contents);
    } else if (@typeInfo(T) == .@"struct" and T == types.BinaryBytes) {
        try t.expectEqualSlices(u8, val.contents, actual.contents);
    } else {
        try t.expectEqual(val, actual);
    }
    try session.inProt().readFieldEnd();
    try session.expectFullyConsumed();
}

pub fn testStructBeginEnd(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try session.outProt().writeStructBegin(.{ .allocator = allocator, .name = "TestStruct" });
    try session.outProt().writeFieldBegin(field(1, .I32));
    try session.outProt().writeI32(42);
    try session.outProt().writeFieldEnd();
    try session.outProt().writeFieldStop();
    try session.outProt().writeStructEnd();
    try session.finishWrite(factory);

    var struct_id = try session.inProt().readStructBegin(allocator);
    defer struct_id.deinit();

    var field_id = try session.inProt().readFieldBegin(allocator);
    defer field_id.deinit();
    try t.expectEqual(@as(i16, 1), field_id.id.?);
    try t.expectEqual(.I32, field_id.fieldType);
    try t.expectEqual(@as(i32, 42), try session.inProt().readI32());
    try session.inProt().readFieldEnd();

    var stop_field = try session.inProt().readFieldBegin(allocator);
    defer stop_field.deinit();
    try t.expectEqual(.Stop, stop_field.fieldType);
    try session.inProt().readStructEnd();
    try session.expectFullyConsumed();
}

pub fn testMessageBeginEnd(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const messages = [_]struct {
        name: []const u8,
        msg_type: protocol.TMessageType,
        seq: i32,
    }{
        .{ .name = "a", .msg_type = .Call, .seq = 1 },
        .{ .name = "short message name", .msg_type = .Call, .seq = 0 },
        .{ .name = "1", .msg_type = .Reply, .seq = 12345 },
        .{ .name = "loooooooooooooooooooooooooooooooooong", .msg_type = .Exception, .seq = 1 << 16 },
        .{ .name = "one way push", .msg_type = .OneWay, .seq = 12 },
    };

    for (messages) |msg| {
        var session: ProtocolSession = undefined;
        try ProtocolSession.beginWrite(&session, allocator, io, factory);
        defer session.deinit();

        try session.outProt().writeMessageBegin(.{
            .allocator = allocator,
            .name = msg.name,
            .msgType = msg.msg_type,
            .sequenceNumber = msg.seq,
        });
        try session.outProt().writeMessageEnd();
        try session.finishWrite(factory);

        var read_msg = try session.inProt().readMessageBegin(allocator);
        defer read_msg.deinit();
        try t.expectEqualStrings(msg.name, read_msg.name);
        try t.expectEqual(msg.msg_type, read_msg.msgType);
        try t.expectEqual(msg.seq, read_msg.sequenceNumber);
        try session.inProt().readMessageEnd();
        try session.expectFullyConsumed();
    }
}

pub fn testList(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const values = [_]i32{ 1, 2, 3, -42, 0 };
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try session.outProt().writeListBegin(.{ .eType = .I32, .size = @intCast(values.len) });
    for (values) |v| try session.outProt().writeI32(v);
    try session.outProt().writeListEnd();
    try session.finishWrite(factory);

    const list_id = try session.inProt().readListBegin();
    try t.expectEqual(.I32, list_id.eType);
    try t.expectEqual(@as(i32, @intCast(values.len)), list_id.size);
    for (values) |expected| {
        try t.expectEqual(expected, try session.inProt().readI32());
    }
    try session.inProt().readListEnd();
    try session.expectFullyConsumed();
}

pub fn testSet(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const values = [_]i32{ 10, 20, 30 };
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try session.outProt().writeSetBegin(.{ .eType = .I32, .size = @intCast(values.len) });
    for (values) |v| try session.outProt().writeI32(v);
    try session.outProt().writeSetEnd();
    try session.finishWrite(factory);

    const set_id = try session.inProt().readSetBegin();
    try t.expectEqual(.I32, set_id.eType);
    try t.expectEqual(@as(i32, @intCast(values.len)), set_id.size);
    for (values) |expected| {
        try t.expectEqual(expected, try session.inProt().readI32());
    }
    try session.inProt().readSetEnd();
    try session.expectFullyConsumed();
}

pub fn testMap(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    const entries = [_]struct { i32, i32 }{ .{ 1, 100 }, .{ 2, 200 } };
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try session.outProt().writeMapBegin(.{ .kType = .I32, .vType = .I32, .size = @intCast(entries.len) });
    for (entries) |entry| {
        try session.outProt().writeI32(entry[0]);
        try session.outProt().writeI32(entry[1]);
    }
    try session.outProt().writeMapEnd();
    try session.finishWrite(factory);

    const map_id = try session.inProt().readMapBegin();
    try t.expectEqual(.I32, map_id.kType);
    try t.expectEqual(.I32, map_id.vType);
    try t.expectEqual(@as(i32, @intCast(entries.len)), map_id.size);
    for (entries) |entry| {
        try t.expectEqual(entry[0], try session.inProt().readI32());
        try t.expectEqual(entry[1], try session.inProt().readI32());
    }
    try session.inProt().readMapEnd();
    try session.expectFullyConsumed();
}

pub fn roundTripStruct(
    allocator: Allocator,
    io: std.Io,
    factory: *TProtocolFactory,
    original: anytype,
) !void {
    const T = @TypeOf(original);
    var session: ProtocolSession = undefined;
    try ProtocolSession.beginWrite(&session, allocator, io, factory);
    defer session.deinit();

    try original.writeToProtocol(session.outProt());
    try session.finishWrite(factory);

    var decoded = try T.readFromProtocol(allocator, session.inProt());
    defer decoded.deinit();
    try t.expect(original.eql(decoded));
    try session.expectFullyConsumed();
}

const TestFn = *const fn (Allocator, std.Io, *TProtocolFactory) anyerror!void;

const test_groups = [_]struct { name: []const u8, run: TestFn }{
    .{ .name = "testI8", .run = testI8 },
    .{ .name = "testByte", .run = testByte },
    .{ .name = "testBool", .run = testBool },
    .{ .name = "testI16", .run = testI16 },
    .{ .name = "testI32", .run = testI32 },
    .{ .name = "testI64", .run = testI64 },
    .{ .name = "testDouble", .run = testDouble },
    .{ .name = "testString", .run = testString },
    .{ .name = "testBytes", .run = testBytes },
    .{ .name = "testUuid", .run = testUuid },
    .{ .name = "testBoolInField", .run = testBoolInField },
    .{ .name = "testStructBeginEnd", .run = testStructBeginEnd },
    .{ .name = "testMessageBeginEnd", .run = testMessageBeginEnd },
    .{ .name = "testList", .run = testList },
    .{ .name = "testSet", .run = testSet },
    .{ .name = "testMap", .run = testMap },
};

pub fn runAll(allocator: Allocator, io: std.Io, factory: *TProtocolFactory) !void {
    for (test_groups) |group| {
        group.run(allocator, io, factory) catch |err| {
            std.debug.print("protocol suite failed in {s}: {}\n", .{ group.name, err });
            return err;
        };
    }
}
