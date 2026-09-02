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

const STRICT_MODE_MESSAGE_VERSION: u16 = (1 << 15) | 1; // 0b1000000000000001;
const MESSAGE_TYPE_MASK = 0b00000111;

fn tTypeToByte(t: TType) i8 {
    return switch (t) {
        .Stop => 0,
        .Void => 1,
        .Bool => 2,
        .I8, .Byte => 3,
        .Double => 4,
        .I16 => 6,
        .I32 => 8,
        .I64 => 10,
        .String => 11,
        .Struct => 12,
        .Map => 13,
        .Set => 14,
        .List => 15,
        .Uuid => 16,
    };
}

fn byteToTType(b: i8) !TType {
    return switch (b) {
        0 => .Stop,
        1 => .Void,
        2 => .Bool,
        3 => .I8,
        4 => .Double,
        6 => .I16,
        8 => .I32,
        10 => .I64,
        11 => .String,
        12 => .Struct,
        13 => .Map,
        14 => .Set,
        15 => .List,
        16 => .Uuid,
        else => ProtocolError.InvalidTypeWireValue,
    };
}

pub const TBinaryProtocol = struct {
    const Self = @This();

    allocator: Allocator,
    transport: *root.TTransport,
    config: *const TConfiguration,
    inputRecursion: utils.RecursionTracker,
    outputRecursion: utils.RecursionTracker,
    writer: *std.Io.Writer,
    reader: *std.Io.Reader,

    pub fn init(allocator: Allocator, transport: *root.TTransport, config: *const TConfiguration) !Self {
        return .{
            .allocator = allocator,
            .transport = transport,
            .config = config,
            .inputRecursion = utils.RecursionTracker.init(config),
            .outputRecursion = utils.RecursionTracker.init(config),
            .writer = try transport.writer(),
            .reader = try transport.reader(),
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        a.destroy(self);
    }

    pub fn interface(self: *Self) TProtocol {
        return TProtocol.init(self);
    }

    // --- Util Methods ---

    fn readRawString(self: *Self, allocator: Allocator) ![]const u8 {
        const len = try utils.checkReadLength(try self.readI32());
        return try self.reader.readAlloc(allocator, len);
    }

    fn writeRawString(self: *Self, value: []const u8) !void {
        const len = try utils.checkWriteLength(value.len);
        try self.writer.writeInt(i32, len, .big);
        try self.writer.writeAll(value);
    }

    // --- Out Methods ---
    pub fn writeMessageBegin(self: *Self, id: TMessageIdentifier) !void {
        // TODO: non-strict mode
        try self.writeI16(@bitCast(STRICT_MODE_MESSAGE_VERSION));
        try self.writeByte(0);
        try self.writeByte(@intFromEnum(id.msgType) & MESSAGE_TYPE_MASK);
        try self.writeRawString(id.name);
        try self.writeI32(id.sequenceNumber);
    }

    pub fn writeMessageEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeStructBegin(self: *Self, id: TStructIdentifier) !void {
        _ = id;
        try self.outputRecursion.increment();
    }

    pub fn writeStructEnd(self: *Self) !void {
        try self.outputRecursion.decrement();
    }

    pub fn writeFieldBegin(self: *Self, id: TFieldIdentifier) !void {
        try self.writeI8(tTypeToByte(id.fieldType));
        if (id.id) |i| {
            try self.writeI16(i);
        } else {
            return ProtocolError.MissingFieldId;
        }
    }

    pub fn writeFieldEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeFieldStop(self: *Self) !void {
        try self.writeByte(0);
    }

    pub fn writeMapBegin(self: *Self, id: TMapIdentifier) !void {
        try utils.checkContainerSize(id.size);
        try self.writeI8(tTypeToByte(id.kType));
        try self.writeI8(tTypeToByte(id.vType));
        try self.writeI32(id.size);
    }

    pub fn writeMapEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeListBegin(self: *Self, id: TListIdentifier) !void {
        try utils.checkContainerSize(id.size);
        try self.writeI8(tTypeToByte(id.eType));
        try self.writeI32(id.size);
    }

    pub fn writeListEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeSetBegin(self: *Self, id: TSetIdentifier) !void {
        try utils.checkContainerSize(id.size);
        try self.writeI8(tTypeToByte(id.eType));
        try self.writeI32(id.size);
    }

    pub fn writeSetEnd(self: *Self) !void {
        _ = self;
    }

    pub fn writeBool(self: *Self, value: bool) !void {
        try self.writer.writeByte(if (value) 1 else 0);
    }

    pub fn writeByte(self: *Self, value: u8) !void {
        try self.writer.writeByte(value);
    }

    pub fn writeI8(self: *Self, value: i8) !void {
        try self.writer.writeInt(i8, value, .big);
    }

    pub fn writeI16(self: *Self, value: i16) !void {
        try self.writer.writeInt(i16, value, .big);
    }

    pub fn writeI32(self: *Self, value: i32) !void {
        try self.writer.writeInt(i32, value, .big);
    }

    pub fn writeI64(self: *Self, value: i64) !void {
        try self.writer.writeInt(i64, value, .big);
    }

    pub fn writeDouble(self: *Self, value: f64) !void {
        try self.writer.writeInt(i64, @bitCast(value), .big);
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
        // TODO: non-strict mode
        const version: u16 = @bitCast(try self.readI16());
        if (version != STRICT_MODE_MESSAGE_VERSION) {
            std.debug.print("Unknown message version {b}, should be {b}\n", .{ version, STRICT_MODE_MESSAGE_VERSION });
            return ProtocolError.InvalidMessageVersion;
        }
        try self.reader.discardAll(1);
        const messageType = try utils.getTMessageType(try self.readByte() & MESSAGE_TYPE_MASK);
        const name = try self.readRawString(allocator);
        const seqId = try self.readI32();
        return .{ .allocator = allocator, .msgType = messageType, .name = name, .sequenceNumber = seqId };
    }

    pub fn readMessageEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readStructBegin(self: *Self, allocator: Allocator) !TStructIdentifier {
        try self.inputRecursion.increment();
        return .{
            .allocator = allocator,
            .name = try allocator.alloc(u8, 0),
        };
    }

    pub fn readStructEnd(self: *Self) !void {
        try self.inputRecursion.decrement();
    }

    pub fn readFieldBegin(self: *Self, allocator: Allocator) !TFieldIdentifier {
        const fieldType = try byteToTType(try self.readI8());
        if (fieldType == .Stop) {
            return .{
                .allocator = allocator,
                .fieldType = .Stop,
                .id = null,
                .name = null,
            };
        }

        const fieldId = try self.readI16();
        return .{ .allocator = allocator, .id = fieldId, .fieldType = fieldType, .name = null };
    }

    pub fn readFieldEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readMapBegin(self: *Self) !TMapIdentifier {
        const keyType = try byteToTType(try self.readI8());
        const valueType = try byteToTType(try self.readI8());
        const size = try self.readI32();
        try utils.checkContainerSize(size);
        return .{ .kType = keyType, .vType = valueType, .size = size };
    }

    pub fn readMapEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readListBegin(self: *Self) !TListIdentifier {
        const elementType = try byteToTType(try self.readI8());
        const size = try self.readI32();
        try utils.checkContainerSize(size);
        return .{ .eType = elementType, .size = size };
    }

    pub fn readListEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readSetBegin(self: *Self) !TSetIdentifier {
        const elementType = try byteToTType(try self.readI8());
        const size = try self.readI32();
        try utils.checkContainerSize(size);
        return .{ .eType = elementType, .size = size };
    }

    pub fn readSetEnd(self: *Self) !void {
        _ = self;
    }

    pub fn readBool(self: *Self) !bool {
        return switch (try self.reader.takeByte()) {
            0 => false,
            1 => true,
            else => ProtocolError.InvalidBooleanWireValue,
        };
    }

    pub fn readByte(self: *Self) !u8 {
        return try self.reader.takeByte();
    }

    pub fn readI8(self: *Self) !i8 {
        return try self.reader.takeInt(i8, .big);
    }

    pub fn readI16(self: *Self) !i16 {
        return try self.reader.takeInt(i16, .big);
    }

    pub fn readI32(self: *Self) !i32 {
        return try self.reader.takeInt(i32, .big);
    }

    pub fn readI64(self: *Self) !i64 {
        return try self.reader.takeInt(i64, .big);
    }

    pub fn readDouble(self: *Self) !f64 {
        return @bitCast(try self.reader.takeInt(i64, .big));
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
        var uuid: root.UUID = .{ .bytes = undefined };
        try self.reader.readSliceAll(&uuid.bytes);
        return uuid;
    }

    pub fn flush(self: *Self) !void {
        try self.transport.flush();
    }

    pub fn skipRawString(self: *Self) !void {
        const len = try utils.checkReadLength(try self.readI32());
        try self.reader.discardAll(len);
    }

    pub fn getRecursionLimit(self: *Self) i32 {
        return self.config.recursionLimit;
    }
};

pub const TBinaryProtocolFactory = struct {
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
        const protocol_impl = try allocator.create(TBinaryProtocol);
        protocol_impl.* = try TBinaryProtocol.init(allocator, transport, self.config);
        return protocol_impl.interface();
    }

    pub fn interface(self: *Self) protocol.TProtocolFactory {
        return protocol.TProtocolFactory.init(self);
    }
};

const testing = std.testing;
const memory_transport = @import("../testing/memory_transport.zig");
const protocol_suite = @import("../testing/protocol_suite.zig");

test "protocol conformance suite" {
    var config = TConfiguration.default;
    var factory = TBinaryProtocolFactory.init(&config);
    var factory_itf = factory.interface();
    try protocol_suite.runAll(testing.allocator, testing.io, &factory_itf);
}

test "writeRawString rejects oversized payload" {
    var config = TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var protocol_impl = try TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();

    const oversize_len = @as(usize, @intCast(std.math.maxInt(i32))) + 1;
    const oversize = @as([*]const u8, @ptrFromInt(1))[0..oversize_len];
    try testing.expectError(ProtocolError.InvalidLength, protocol_impl.writeRawString(oversize));
}

test "readMessageBegin rejects invalid message type" {
    var config = TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    const writer = try transport.writer();
    try writer.writeInt(i16, @bitCast(STRICT_MODE_MESSAGE_VERSION), .big);
    try writer.writeByte(0);
    try writer.writeByte(0); // invalid message type
    try writer.writeInt(i32, 0, .big); // empty name
    try writer.writeInt(i32, 1, .big); // sequence number
    try transport.flush();

    var protocol_impl = try TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();
    try testing.expectError(ProtocolError.InvalidMessageType, protocol_impl.readMessageBegin(testing.allocator));
}

test "writeListBegin rejects negative size" {
    var config = TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var protocol_impl = try TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();

    try testing.expectError(
        ProtocolError.InvalidLength,
        protocol_impl.writeListBegin(.{ .eType = .I32, .size = -1 }),
    );
}

test "readStructBegin enforces recursion limit" {
    var config = TConfiguration.default;
    config.recursionLimit = 1;

    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var protocol_impl = try TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer protocol_impl.deinit();

    _ = try protocol_impl.readStructBegin(testing.allocator);
    try testing.expectError(ProtocolError.RecursionLimitExceeded, protocol_impl.readStructBegin(testing.allocator));
}

fn writeNestedStruct(prot: *TBinaryProtocol, depth: usize) !void {
    try prot.writeStructBegin(.{ .allocator = undefined, .name = "" });
    if (depth > 0) {
        try prot.writeFieldBegin(.{ .allocator = undefined, .name = null, .fieldType = .Struct, .id = 1 });
        try writeNestedStruct(prot, depth - 1);
        try prot.writeFieldEnd();
    }
    try prot.writeFieldStop();
    try prot.writeStructEnd();
}

test "skip enforces recursion limit for nested structs" {
    var write_config = TConfiguration.default;
    var read_config = TConfiguration.default;
    read_config.recursionLimit = 5;

    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try TBinaryProtocol.init(testing.allocator, &transport_itf, &write_config);
    defer out_prot.deinit();
    try writeNestedStruct(&out_prot, 6);
    try out_prot.flush();

    var in_prot = try TBinaryProtocol.init(testing.allocator, &transport_itf, &read_config);
    defer in_prot.deinit();
    var in_itf = in_prot.interface();

    try testing.expectError(ProtocolError.RecursionLimitExceeded, in_itf.skip(testing.allocator, .Struct));
}

test "skip enforces recursion limit through nested lists" {
    var write_config = TConfiguration.default;
    var read_config = TConfiguration.default;
    read_config.recursionLimit = 5;

    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try TBinaryProtocol.init(testing.allocator, &transport_itf, &write_config);
    defer out_prot.deinit();

    // 6 levels of list nesting wrapping a single i32
    try out_prot.writeListBegin(.{ .eType = .List, .size = 1 });
    try out_prot.writeListBegin(.{ .eType = .List, .size = 1 });
    try out_prot.writeListBegin(.{ .eType = .List, .size = 1 });
    try out_prot.writeListBegin(.{ .eType = .List, .size = 1 });
    try out_prot.writeListBegin(.{ .eType = .List, .size = 1 });
    try out_prot.writeListBegin(.{ .eType = .List, .size = 1 });
    try out_prot.writeI32(42);
    try out_prot.writeListEnd();
    try out_prot.writeListEnd();
    try out_prot.writeListEnd();
    try out_prot.writeListEnd();
    try out_prot.writeListEnd();
    try out_prot.writeListEnd();
    try out_prot.flush();

    var in_prot = try TBinaryProtocol.init(testing.allocator, &transport_itf, &read_config);
    defer in_prot.deinit();
    var in_itf = in_prot.interface();

    try testing.expectError(ProtocolError.RecursionLimitExceeded, in_itf.skip(testing.allocator, .List));
}
