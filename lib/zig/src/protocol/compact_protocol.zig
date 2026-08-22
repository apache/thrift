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
const protocol = @import("protocol.zig");
const root = @import("../root.zig");
const utils = @import("utils.zig");
const TConfiguration = @import("../lib/configuration.zig").TConfiguration;

const Allocator = std.mem.Allocator;

const TMessageIdentifier = protocol.TMessageIdentifier;
const TFieldIdentifier = protocol.TFieldIdentifier;
const TMapIdentifier = protocol.TMapIdentifier;
const TListIdentifier = protocol.TListIdentifier;
const TSetIdentifier = protocol.TSetIdentifier;
const TStructIdentifier = protocol.TStructIdentifier;
const TProtocol = protocol.TProtocol;
const ProtocolError = protocol.ProtocolError;
const TType = protocol.TType;

const COMPACT_PROTOCOL_ID: u8 = 0b1000_0010;
const MESSAGE_TYPE_VERSION: u8 = 1;

const BOOL_TRUE_VALUE: u8 = 1;
const BOOL_FALSE_VALUE: u8 = 2;

fn tTypeToByte(tType: TType) !u8 {
    return switch (tType) {
        .I8, .Byte => 3,
        .I16 => 4,
        .I32 => 5,
        .I64 => 6,
        .Double => 7,
        .String => 8,
        .List => 9,
        .Set => 10,
        .Map => 11,
        .Struct => 12,
        .Uuid => 13,
        .Bool, .Stop, .Void => ProtocolError.NoTypeValueForType,
    };
}

fn byteToTType(b: u8) !TType {
    return switch (b) {
        0 => .Stop,
        1, 2 => .Bool,
        3 => .I8,
        4 => .I16,
        5 => .I32,
        6 => .I64,
        7 => .Double,
        8 => .String,
        9 => .List,
        10 => .Set,
        11 => .Map,
        12 => .Struct,
        13 => .Uuid,
        else => ProtocolError.InvalidTypeWireValue,
    };
}

fn toZigZag(n: i64) u64 {
    const bits: u64 = @bitCast(n);
    return (bits << 1) ^ @as(u64, @bitCast(n >> 63));
}

fn fromZigZag(i: u64) i64 {
    return @as(i64, @bitCast(i >> 1)) ^ -@as(i64, @bitCast(i & 1));
}

pub const TCompactProtocol = struct {
    const Self = @This();

    allocator: Allocator,
    transport: *root.TTransport,
    config: *const TConfiguration,
    inputRecursion: utils.RecursionTracker,
    outputRecursion: utils.RecursionTracker,
    writer: *std.Io.Writer,
    reader: *std.Io.Reader,

    boolValue: ?bool,
    boolField: ?TFieldIdentifier,

    lastFieldId: i16,
    lastFieldStack: std.ArrayList(i16),

    pub fn init(allocator: Allocator, transport: *root.TTransport, config: *const TConfiguration) !Self {
        return .{
            .allocator = allocator,
            .transport = transport,
            .config = config,
            .inputRecursion = utils.RecursionTracker.init(config),
            .outputRecursion = utils.RecursionTracker.init(config),
            .writer = try transport.writer(),
            .reader = try transport.reader(),
            .boolValue = null,
            .boolField = null,
            .lastFieldId = 0,
            .lastFieldStack = try .initCapacity(allocator, 2),
        };
    }

    pub fn deinit(self: *Self) void {
        self.lastFieldStack.deinit(self.allocator);
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        a.destroy(self);
    }

    pub fn interface(self: *Self) TProtocol {
        return TProtocol.init(self);
    }

    // --- Util Methods ---
    fn writeVarint32(self: *Self, value: i32) !void {
        try self.writeVarint(@intCast(value));
    }

    fn readVarint32(self: *Self) !i32 {
        return @intCast(try self.readVarint(i32));
    }

    fn writeVarint(self: *Self, i: u64) !void {
        if (i == 0) {
            try self.writer.writeInt(u8, 0, .big);
            return;
        }

        var zigzag: u64 = @bitCast(i);
        while (zigzag != 0) {
            var b: u8 = @as(u8, @truncate(zigzag)) & 0x7f;
            zigzag >>= 7;
            if (zigzag != 0) {
                b |= 0x80;
            }
            try self.writer.writeInt(u8, b, .big);
        }
    }

    fn readVarint(self: *Self, comptime T: type) !u64 {
        if (@typeInfo(T) != .int) {
            @compileError("can only read ints");
        }

        const maxBits = @typeInfo(T).int.bits;
        var res: u64 = 0;

        var shift: u6 = 0;
        while (true) {
            const b = try self.reader.takeInt(u8, .big);
            res |= @as(u64, b & 0x7f) << shift;
            if ((b & 0x80) != 0x80) {
                break;
            }
            shift += 7;
            if (shift >= maxBits) {
                return ProtocolError.InvalidIntData;
            }
        }

        return res;
    }

    fn writeRawString(self: *Self, value: []const u8) !void {
        const len = try utils.checkWriteLength(value.len);
        try self.writeVarint32(len);
        try self.writer.writeAll(value);
    }

    fn readRawString(self: *Self, allocator: Allocator) ![]const u8 {
        const len = try utils.checkReadLength(try self.readVarint32());
        return try self.reader.readAlloc(allocator, len);
    }

    // --- Out Methods ---
    pub fn writeMessageBegin(self: *Self, id: TMessageIdentifier) !void {
        try self.writeByte(COMPACT_PROTOCOL_ID);
        try self.writeByte((@intFromEnum(id.msgType) << 5) | MESSAGE_TYPE_VERSION);
        try self.writeVarint32(id.sequenceNumber);
        try self.writeRawString(id.name);
    }

    pub fn writeMessageEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeStructBegin(self: *Self, id: TStructIdentifier) !void {
        _ = id;
        try self.outputRecursion.increment();
        try self.lastFieldStack.append(self.allocator, self.lastFieldId);
        self.lastFieldId = 0;
    }

    pub fn writeStructEnd(self: *Self) !void {
        self.lastFieldId = self.lastFieldStack.pop() orelse return ProtocolError.InvalidStructNesting;
        try self.outputRecursion.decrement();
    }

    fn writeFieldBeginInternal(self: *Self, id: TFieldIdentifier, typeOverride: ?u8) !void {
        const fieldType: u8 = if (typeOverride) |override| override else try tTypeToByte(id.fieldType);
        const fieldId = id.id orelse return error.MissingFieldId;
        defer self.lastFieldId = fieldId;

        if (fieldId > self.lastFieldId) {
            const delta = (fieldId - self.lastFieldId);
            if (delta <= 15) {
                try self.writeByte(fieldType | (@as(u8, @intCast(delta)) << 4));
                return;
            }
        }

        try self.writeByte(fieldType);
        try self.writeI16(fieldId);
    }

    pub fn writeFieldBegin(self: *Self, id: TFieldIdentifier) !void {
        if (id.fieldType == .Bool) {
            self.boolField = .{
                .allocator = undefined,
                .id = id.id,
                .fieldType = id.fieldType,
                .name = null,
            };
            return;
        }

        try self.writeFieldBeginInternal(id, null);
    }

    pub fn writeFieldEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeFieldStop(self: *Self) !void {
        try self.writeByte(0);
    }

    pub fn writeMapBegin(self: *Self, id: TMapIdentifier) !void {
        try utils.checkContainerSize(id.size);
        if (id.size == 0) {
            try self.writeByte(0);
        } else {
            try self.writeVarint32(id.size);
            const kTypeByte = try tTypeToByte(id.kType);
            const vTypeByte = try tTypeToByte(id.vType);
            try self.writeByte((kTypeByte << 4) | vTypeByte);
        }
    }

    pub fn writeMapEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeListBegin(self: *Self, id: TListIdentifier) !void {
        try utils.checkContainerSize(id.size);
        const eTypeByte = try tTypeToByte(id.eType);
        if (id.size <= 14) {
            try self.writeByte(@as(u8, @intCast(id.size)) << 4 | eTypeByte);
        } else {
            try self.writeByte(0xf0 | eTypeByte);
            try self.writeVarint32(id.size);
        }
    }

    pub fn writeListEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeSetBegin(self: *Self, id: TSetIdentifier) !void {
        try utils.checkContainerSize(id.size);
        const eTypeByte = try tTypeToByte(id.eType);
        if (id.size <= 14) {
            try self.writeByte(@as(u8, @intCast(id.size)) << 4 | eTypeByte);
        } else {
            try self.writeByte(0xf0 | eTypeByte);
            try self.writeVarint32(id.size);
        }
    }

    pub fn writeSetEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeBool(self: *Self, value: bool) !void {
        if (self.boolField) |f| {
            try self.writeFieldBeginInternal(f, if (value) BOOL_TRUE_VALUE else BOOL_FALSE_VALUE);
            self.boolField = null;
        } else {
            try self.writeByte(if (value) BOOL_TRUE_VALUE else BOOL_FALSE_VALUE);
        }
    }

    pub fn writeByte(self: *Self, value: u8) !void {
        try self.writer.writeInt(u8, value, .big);
    }

    pub fn writeI8(self: *Self, value: i8) !void {
        try self.writer.writeInt(i8, value, .big);
    }

    pub fn writeI16(self: *Self, value: i16) !void {
        try self.writeVarint(toZigZag(@intCast(value)));
    }

    pub fn writeI32(self: *Self, value: i32) !void {
        try self.writeVarint(toZigZag(@intCast(value)));
    }

    pub fn writeI64(self: *Self, value: i64) !void {
        try self.writeVarint(toZigZag(value));
    }

    pub fn writeDouble(self: *Self, value: f64) !void {
        try self.writer.writeInt(i64, @bitCast(value), .little);
    }

    pub fn writeBytes(self: *Self, value: root.BinaryBytes) !void {
        try self.writeRawString(value.contents);
    }

    pub fn writeString(self: *Self, value: root.String) !void {
        try self.writeRawString(value.contents);
    }

    pub fn writeUUID(self: *Self, value: root.UUID) !void {
        try self.writer.writeAll(&value.bytes);
    }

    // --- In Methods ---
    pub fn readMessageBegin(self: *Self, allocator: Allocator) !TMessageIdentifier {
        const protocolId = try self.readByte();
        if (protocolId != COMPACT_PROTOCOL_ID) {
            return ProtocolError.InvalidProtocolId;
        }

        const msgTypeVal = try self.readByte();
        const msgType = try utils.getTMessageType((msgTypeVal & 0xe0) >> 5);
        const msgVersion = msgTypeVal & 0x1f;
        if (msgVersion != 1) {
            return ProtocolError.InvalidMessageVersion;
        }

        const seqNumber = try self.readVarint32();
        const name = try self.readRawString(allocator);

        return .{
            .allocator = allocator,
            .sequenceNumber = seqNumber,
            .name = name,
            .msgType = msgType,
        };
    }

    pub fn readMessageEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readStructBegin(self: *Self, allocator: Allocator) !TStructIdentifier {
        try self.inputRecursion.increment();
        try self.lastFieldStack.append(allocator, self.lastFieldId);
        self.lastFieldId = 0;

        return .{
            .allocator = allocator,
            .name = try allocator.alloc(u8, 0),
        };
    }

    pub fn readStructEnd(self: *Self) !void {
        self.lastFieldId = self.lastFieldStack.pop() orelse return ProtocolError.InvalidStructNesting;
        try self.inputRecursion.decrement();
    }

    pub fn readFieldBegin(self: *Self, allocator: Allocator) !TFieldIdentifier {
        const fielType = try self.reader.takeByte();
        if (fielType == 0) {
            return .{
                .allocator = allocator,
                .id = 0,
                .fieldType = .Stop,
                .name = null,
            };
        }

        const idDiff = (fielType & 0xf0) >> 4;
        const fieldId: i16 = if (idDiff == 0) try self.readI16() else self.lastFieldId + idDiff;
        defer self.lastFieldId = fieldId;

        const fieldTypeByte = fielType & 0xf;
        const fieldTType: TType = ttype: {
            if (fieldTypeByte == BOOL_FALSE_VALUE or fieldTypeByte == BOOL_TRUE_VALUE) {
                self.boolValue = if (fieldTypeByte == BOOL_FALSE_VALUE) false else true;
                break :ttype .Bool;
            }
            break :ttype try byteToTType(fieldTypeByte);
        };

        return .{
            .allocator = allocator,
            .id = fieldId,
            .fieldType = fieldTType,
            .name = null,
        };
    }

    pub fn readFieldEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readMapBegin(self: *Self) !TMapIdentifier {
        const size = try self.readVarint32();
        if (size == 0) {
            return .{
                .size = 0,
                .kType = .Stop,
                .vType = .Stop,
            };
        }
        try utils.checkContainerSize(size);

        const types = try self.readByte();

        const kType = (types & 0xf0) >> 4;
        const vType = (types & 0xf);

        return .{
            .size = size,
            .kType = try byteToTType(kType),
            .vType = try byteToTType(vType),
        };
    }

    pub fn readMapEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readListBegin(self: *Self) !TListIdentifier {
        const typeLen = try self.readByte();
        const size: i32 = size: {
            if (((typeLen & 0xf0) >> 4) < 15) {
                break :size @intCast((typeLen & 0xf0) >> 4);
            } else {
                break :size try self.readVarint32();
            }
        };
        try utils.checkContainerSize(size);

        return .{
            .eType = try byteToTType(typeLen & 0xf),
            .size = size,
        };
    }

    pub fn readListEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readSetBegin(self: *Self) !TSetIdentifier {
        const typeLen = try self.readByte();
        const size: i32 = size: {
            if (((typeLen & 0xf0) >> 4) < 15) {
                break :size @intCast((typeLen & 0xf0) >> 4);
            } else {
                break :size try self.readVarint32();
            }
        };
        try utils.checkContainerSize(size);

        return .{
            .eType = try byteToTType(typeLen & 0xf),
            .size = size,
        };
    }

    pub fn readSetEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readBool(self: *Self) !bool {
        if (self.boolValue) |b| {
            self.boolValue = null;
            return b;
        } else {
            const tmp = try self.readByte();
            if (tmp == BOOL_TRUE_VALUE) {
                return true;
            } else if (tmp == BOOL_FALSE_VALUE) {
                return false;
            } else {
                return ProtocolError.InvalidBooleanWireValue;
            }
        }
    }

    pub fn readByte(self: *Self) !u8 {
        return try self.reader.takeInt(u8, .big);
    }

    pub fn readI8(self: *Self) !i8 {
        return try self.reader.takeInt(i8, .big);
    }

    pub fn readI16(self: *Self) !i16 {
        return @intCast(fromZigZag(try self.readVarint(i16)));
    }

    pub fn readI32(self: *Self) !i32 {
        return @intCast(fromZigZag(try self.readVarint(i32)));
    }

    pub fn readI64(self: *Self) !i64 {
        return @intCast(fromZigZag(try self.readVarint(i64)));
    }

    pub fn readDouble(self: *Self) !f64 {
        return @bitCast(try self.reader.takeInt(i64, .little));
    }

    pub fn readBytes(self: *Self, allocator: Allocator) !root.BinaryBytes {
        return .{
            .allocator = allocator,
            .contents = try self.readRawString(allocator),
        };
    }

    pub fn readString(self: *Self, allocator: Allocator) !root.String {
        return .{
            .allocator = allocator,
            .contents = try self.readRawString(allocator),
        };
    }

    pub fn readUUID(self: *Self) !root.UUID {
        var temp: root.UUID = .empty;
        try self.reader.readSliceAll(&temp.bytes);
        return temp;
    }

    pub fn flush(self: *Self) !void {
        try self.transport.flush();
    }

    pub fn skipRawString(self: *Self) !void {
        const len = try utils.checkReadLength(try self.readVarint32());
        try self.reader.discardAll(len);
    }

    pub fn getRecursionLimit(self: *Self) i32 {
        return self.config.recursionLimit;
    }
};

pub const TCompactProtocolFactory = struct {
    const Self = @This();

    config: *const TConfiguration,

    pub fn init(config: *const TConfiguration) Self {
        return .{ .config = config };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        a.destroy(self);
    }

    pub fn getProtocol(self: *Self, allocator: Allocator, transport: *root.TTransport) anyerror!TProtocol {
        const protocol_impl = try allocator.create(TCompactProtocol);
        protocol_impl.* = try TCompactProtocol.init(allocator, transport, self.config);
        return protocol_impl.interface();
    }

    pub fn interface(self: *Self) protocol.TProtocolFactory {
        return protocol.TProtocolFactory.init(self);
    }
};

const t = std.testing;
const TTestingMemoryTransport = @import("../testing/memory_transport.zig").TTestingMemoryTransport;
const protocol_suite = @import("../testing/protocol_suite.zig");

test "protocol conformance suite" {
    var config = TConfiguration.default;
    var factory = TCompactProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    try protocol_suite.runAll(t.allocator, t.io, &factory_itf);
}

test "writeRawString rejects oversized payload" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var protocol_impl: TCompactProtocol = try .init(t.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();

    const oversize_len = @as(usize, @intCast(std.math.maxInt(i32))) + 1;
    const oversize = @as([*]const u8, @ptrFromInt(1))[0..oversize_len];
    try t.expectError(ProtocolError.InvalidLength, protocol_impl.writeRawString(oversize));
}

test "readMessageBegin rejects invalid message type" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    const writer = try transport.writer();
    try writer.writeByte(COMPACT_PROTOCOL_ID);
    try writer.writeByte(MESSAGE_TYPE_VERSION); // message type 0 in high bits
    try writer.writeByte(0); // sequence number varint
    try writer.writeByte(0); // name length varint
    try transport.flush();

    var protocol_impl: TCompactProtocol = try .init(t.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();
    try t.expectError(ProtocolError.InvalidMessageType, protocol_impl.readMessageBegin(t.allocator));
}

test "writeListBegin rejects negative size" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var protocol_impl: TCompactProtocol = try .init(t.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();

    try t.expectError(
        ProtocolError.InvalidLength,
        protocol_impl.writeListBegin(.{ .eType = .I32, .size = -1 }),
    );
}

test "writeStructEnd rejects unmatched nesting" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var protocol_impl: TCompactProtocol = try .init(t.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();

    try t.expectError(ProtocolError.InvalidStructNesting, protocol_impl.writeStructEnd());
}

test "int16 zigzag edge cases" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    var itf = transport.interface();
    defer transport.deinit();

    var outProtocol: TCompactProtocol = try .init(t.allocator, &itf, &config);
    defer outProtocol.deinit();

    // zigzag 1-byte boundary: values -64..63
    try outProtocol.writeI16(-1);
    try outProtocol.writeI16(1);
    try outProtocol.writeI16(-64);
    try outProtocol.writeI16(63);
    // zigzag 2-byte boundary: values -8192..8191
    try outProtocol.writeI16(-65);
    try outProtocol.writeI16(64);
    try outProtocol.writeI16(-8192);
    try outProtocol.writeI16(8191);
    // zigzag 3-byte boundary: values beyond 8191/-8192
    try outProtocol.writeI16(-8193);
    try outProtocol.writeI16(8192);

    var inProtocol: TCompactProtocol = try .init(t.allocator, &itf, &config);
    defer inProtocol.deinit();
    try t.expectEqual(@as(i16, -1), try inProtocol.readI16());
    try t.expectEqual(@as(i16, 1), try inProtocol.readI16());
    try t.expectEqual(@as(i16, -64), try inProtocol.readI16());
    try t.expectEqual(@as(i16, 63), try inProtocol.readI16());
    try t.expectEqual(@as(i16, -65), try inProtocol.readI16());
    try t.expectEqual(@as(i16, 64), try inProtocol.readI16());
    try t.expectEqual(@as(i16, -8192), try inProtocol.readI16());
    try t.expectEqual(@as(i16, 8191), try inProtocol.readI16());
    try t.expectEqual(@as(i16, -8193), try inProtocol.readI16());
    try t.expectEqual(@as(i16, 8192), try inProtocol.readI16());

    try t.expectEqual(inProtocol.reader.end, inProtocol.reader.seek);
}

test "int32 zigzag edge cases" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    var itf = transport.interface();
    defer transport.deinit();

    var outProtocol: TCompactProtocol = try .init(t.allocator, &itf, &config);
    defer outProtocol.deinit();

    // 1-byte boundary: zigzag values 0..127 → signed -64..63
    try outProtocol.writeI32(-1);
    try outProtocol.writeI32(1);
    try outProtocol.writeI32(-64);
    try outProtocol.writeI32(63);
    // 2-byte boundary: zigzag values 128..16383 → signed -8192..8191
    try outProtocol.writeI32(-65);
    try outProtocol.writeI32(64);
    try outProtocol.writeI32(-8192);
    try outProtocol.writeI32(8191);
    // 3-byte boundary: zigzag values 16384..2097151 → signed -1048576..1048575
    try outProtocol.writeI32(-8193);
    try outProtocol.writeI32(8192);
    try outProtocol.writeI32(-1048576);
    try outProtocol.writeI32(1048575);
    // 4-byte boundary: zigzag values 2097152..268435455 → signed -134217728..134217727
    try outProtocol.writeI32(-1048577);
    try outProtocol.writeI32(1048576);
    try outProtocol.writeI32(-134217728);
    try outProtocol.writeI32(134217727);
    // 5-byte territory
    try outProtocol.writeI32(-134217729);
    try outProtocol.writeI32(134217728);

    var inProtocol: TCompactProtocol = try .init(t.allocator, &itf, &config);
    defer inProtocol.deinit();
    // 1-byte boundary
    try t.expectEqual(@as(i32, -1), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 1), try inProtocol.readI32());
    try t.expectEqual(@as(i32, -64), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 63), try inProtocol.readI32());
    // 2-byte boundary
    try t.expectEqual(@as(i32, -65), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 64), try inProtocol.readI32());
    try t.expectEqual(@as(i32, -8192), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 8191), try inProtocol.readI32());
    // 3-byte boundary
    try t.expectEqual(@as(i32, -8193), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 8192), try inProtocol.readI32());
    try t.expectEqual(@as(i32, -1048576), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 1048575), try inProtocol.readI32());
    // 4-byte boundary
    try t.expectEqual(@as(i32, -1048577), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 1048576), try inProtocol.readI32());
    try t.expectEqual(@as(i32, -134217728), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 134217727), try inProtocol.readI32());
    // 5-byte territory
    try t.expectEqual(@as(i32, -134217729), try inProtocol.readI32());
    try t.expectEqual(@as(i32, 134217728), try inProtocol.readI32());

    try t.expectEqual(inProtocol.reader.end, inProtocol.reader.seek);
}

test "field with delta encoding" {
    var config = TConfiguration.default;
    var transport: TTestingMemoryTransport = try .init(t.allocator, t.io);
    var itf = transport.interface();
    defer transport.deinit();

    var outProtocol: TCompactProtocol = try .init(t.allocator, &itf, &config);
    defer outProtocol.deinit();

    try outProtocol.writeFieldBegin(.{ .allocator = t.allocator, .id = 1, .fieldType = .I32, .name = null });
    try outProtocol.writeI32(1);
    try outProtocol.writeFieldEnd();

    try outProtocol.writeFieldBegin(.{ .allocator = t.allocator, .id = 2, .fieldType = .I32, .name = null });
    try outProtocol.writeI32(2);
    try outProtocol.writeFieldEnd();

    try outProtocol.writeFieldBegin(.{ .allocator = t.allocator, .id = 3, .fieldType = .I32, .name = null });
    try outProtocol.writeI32(3);
    try outProtocol.writeFieldEnd();

    var inProtocol: TCompactProtocol = try .init(t.allocator, &itf, &config);
    defer inProtocol.deinit();

    var f1 = try inProtocol.readFieldBegin(t.allocator);
    defer f1.deinit();
    try t.expectEqual(@as(i16, 1), f1.id);
    try t.expectEqual(@as(i32, 1), try inProtocol.readI32());
    try inProtocol.readFieldEnd();

    var f2 = try inProtocol.readFieldBegin(t.allocator);
    defer f2.deinit();
    try t.expectEqual(@as(i16, 2), f2.id);
    try t.expectEqual(@as(i32, 2), try inProtocol.readI32());
    try inProtocol.readFieldEnd();

    var f3 = try inProtocol.readFieldBegin(t.allocator);
    defer f3.deinit();
    try t.expectEqual(@as(i16, 3), f3.id);
    try t.expectEqual(@as(i32, 3), try inProtocol.readI32());
    try inProtocol.readFieldEnd();

    try t.expectEqual(inProtocol.reader.end, inProtocol.reader.seek);
}
