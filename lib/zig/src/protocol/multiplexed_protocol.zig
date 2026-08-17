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
const TConfiguration = @import("../lib/configuration.zig").TConfiguration;

const Allocator = std.mem.Allocator;
const testing = std.testing;

const TMessageIdentifier = protocol.TMessageIdentifier;
const TMessageType = protocol.TMessageType;
const TFieldIdentifier = protocol.TFieldIdentifier;
const TMapIdentifier = protocol.TMapIdentifier;
const TListIdentifier = protocol.TListIdentifier;
const TSetIdentifier = protocol.TSetIdentifier;
const TStructIdentifier = protocol.TStructIdentifier;
const TProtocol = protocol.TProtocol;
const TType = protocol.TType;

pub const TMultiplexedProtocol = struct {
    const Self = @This();

    allocator: Allocator,
    serviceName: []const u8,
    wrapped: TProtocol,

    pub fn init(allocator: Allocator, wrapped: TProtocol, serviceName: []const u8) !Self {
        return .{
            .allocator = allocator,
            .serviceName = try allocator.dupe(u8, serviceName),
            .wrapped = wrapped,
        };
    }

    pub fn deinit(self: *Self) void {
        self.wrapped.deinit();
        self.allocator.free(self.serviceName);
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        self.deinit();
        self.wrapped.destroy(a);
        a.destroy(self);
    }

    pub fn interface(self: *Self) TProtocol {
        return TProtocol.init(self);
    }

    pub fn writeMessageBegin(self: *Self, id: TMessageIdentifier) !void {
        switch (id.msgType) {
            .Call, .OneWay => {
                const prefixed = try std.fmt.allocPrint(
                    self.allocator,
                    "{s}:{s}",
                    .{ self.serviceName, id.name },
                );
                defer self.allocator.free(prefixed);
                const outId = TMessageIdentifier{
                    .allocator = id.allocator,
                    .name = prefixed,
                    .msgType = id.msgType,
                    .sequenceNumber = id.sequenceNumber,
                };
                return self.wrapped.writeMessageBegin(outId);
            },
            else => return self.wrapped.writeMessageBegin(id),
        }
    }

    pub fn writeMessageEnd(self: *Self) !void {
        return self.wrapped.writeMessageEnd();
    }

    pub fn writeStructBegin(self: *Self, id: TStructIdentifier) !void {
        return self.wrapped.writeStructBegin(id);
    }

    pub fn writeStructEnd(self: *Self) !void {
        return self.wrapped.writeStructEnd();
    }

    pub fn writeFieldBegin(self: *Self, id: TFieldIdentifier) !void {
        return self.wrapped.writeFieldBegin(id);
    }

    pub fn writeFieldEnd(self: *Self) !void {
        return self.wrapped.writeFieldEnd();
    }

    pub fn writeFieldStop(self: *Self) !void {
        return self.wrapped.writeFieldStop();
    }

    pub fn writeMapBegin(self: *Self, id: TMapIdentifier) !void {
        return self.wrapped.writeMapBegin(id);
    }

    pub fn writeMapEnd(self: *Self) !void {
        return self.wrapped.writeMapEnd();
    }

    pub fn writeListBegin(self: *Self, id: TListIdentifier) !void {
        return self.wrapped.writeListBegin(id);
    }

    pub fn writeListEnd(self: *Self) !void {
        return self.wrapped.writeListEnd();
    }

    pub fn writeSetBegin(self: *Self, id: TSetIdentifier) !void {
        return self.wrapped.writeSetBegin(id);
    }

    pub fn writeSetEnd(self: *Self) !void {
        return self.wrapped.writeSetEnd();
    }

    pub fn writeBool(self: *Self, value: bool) !void {
        return self.wrapped.writeBool(value);
    }

    pub fn writeByte(self: *Self, value: u8) !void {
        return self.wrapped.writeByte(value);
    }

    pub fn writeI8(self: *Self, value: i8) !void {
        return self.wrapped.writeI8(value);
    }

    pub fn writeI16(self: *Self, value: i16) !void {
        return self.wrapped.writeI16(value);
    }

    pub fn writeI32(self: *Self, value: i32) !void {
        return self.wrapped.writeI32(value);
    }

    pub fn writeI64(self: *Self, value: i64) !void {
        return self.wrapped.writeI64(value);
    }

    pub fn writeDouble(self: *Self, value: f64) !void {
        return self.wrapped.writeDouble(value);
    }

    pub fn writeBytes(self: *Self, value: root.BinaryBytes) !void {
        return self.wrapped.writeBytes(value);
    }

    pub fn writeString(self: *Self, value: root.String) !void {
        return self.wrapped.writeString(value);
    }

    pub fn writeUUID(self: *Self, value: root.UUID) !void {
        return self.wrapped.writeUUID(value);
    }

    pub fn readMessageBegin(self: *Self, allocator: Allocator) !TMessageIdentifier {
        return self.wrapped.readMessageBegin(allocator);
    }

    pub fn readMessageEnd(self: *Self) !void {
        return self.wrapped.readMessageEnd();
    }

    pub fn readStructBegin(self: *Self, allocator: Allocator) !TStructIdentifier {
        return self.wrapped.readStructBegin(allocator);
    }

    pub fn readStructEnd(self: *Self) !void {
        return self.wrapped.readStructEnd();
    }

    pub fn readFieldBegin(self: *Self, allocator: Allocator) !TFieldIdentifier {
        return self.wrapped.readFieldBegin(allocator);
    }

    pub fn readFieldEnd(self: *Self) !void {
        return self.wrapped.readFieldEnd();
    }

    pub fn readMapBegin(self: *Self) !TMapIdentifier {
        return self.wrapped.readMapBegin();
    }

    pub fn readMapEnd(self: *Self) !void {
        return self.wrapped.readMapEnd();
    }

    pub fn readListBegin(self: *Self) !TListIdentifier {
        return self.wrapped.readListBegin();
    }

    pub fn readListEnd(self: *Self) !void {
        return self.wrapped.readListEnd();
    }

    pub fn readSetBegin(self: *Self) !TSetIdentifier {
        return self.wrapped.readSetBegin();
    }

    pub fn readSetEnd(self: *Self) !void {
        return self.wrapped.readSetEnd();
    }

    pub fn readBool(self: *Self) !bool {
        return self.wrapped.readBool();
    }

    pub fn readByte(self: *Self) !u8 {
        return self.wrapped.readByte();
    }

    pub fn readI8(self: *Self) !i8 {
        return self.wrapped.readI8();
    }

    pub fn readI16(self: *Self) !i16 {
        return self.wrapped.readI16();
    }

    pub fn readI32(self: *Self) !i32 {
        return self.wrapped.readI32();
    }

    pub fn readI64(self: *Self) !i64 {
        return self.wrapped.readI64();
    }

    pub fn readDouble(self: *Self) !f64 {
        return self.wrapped.readDouble();
    }

    pub fn readBytes(self: *Self, allocator: Allocator) !root.BinaryBytes {
        return self.wrapped.readBytes(allocator);
    }

    pub fn readString(self: *Self, allocator: Allocator) !root.String {
        return self.wrapped.readString(allocator);
    }

    pub fn readUUID(self: *Self) !root.UUID {
        return self.wrapped.readUUID();
    }

    pub fn flush(self: *Self) !void {
        return self.wrapped.flush();
    }

    pub fn skipRawString(self: *Self) !void {
        return self.wrapped.skipRawString();
    }

    pub fn getRecursionLimit(self: *Self) i32 {
        return self.wrapped.getRecursionLimit();
    }
};

const TTestingMemoryTransport = @import("../testing/memory_transport.zig").TTestingMemoryTransport;
const TBinaryProtocol = @import("binary_protocol.zig").TBinaryProtocol;

test "writeMessageBegin prefixes service name on Call" {
    const allocator = testing.allocator;
    var config = TConfiguration.default;

    var memory = try allocator.create(TTestingMemoryTransport);
    memory.* = try TTestingMemoryTransport.init(allocator, testing.io_instance.io());
    defer {
        memory.deinit();
        allocator.destroy(memory);
    }

    var base = try allocator.create(TBinaryProtocol);
    var transportIntf = memory.interface();
    base.* = try TBinaryProtocol.init(allocator, &transportIntf, &config);

    var mux = try allocator.create(TMultiplexedProtocol);
    mux.* = try TMultiplexedProtocol.init(allocator, base.interface(), "foo");
    defer mux.destroy(allocator);

    const ident = TMessageIdentifier{
        .allocator = allocator,
        .name = "bar",
        .msgType = .Call,
        .sequenceNumber = 2,
    };
    try mux.writeMessageBegin(ident);
    try mux.writeMessageEnd();
    try mux.flush();

    const expected = [_]u8{
        0x80, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x07,
        0x66, 0x6F, 0x6F, 0x3A, 0x62, 0x61, 0x72, 0x00,
        0x00, 0x00, 0x02,
    };
    try testing.expectEqualSlices(u8, &expected, memory.buffer.written());
}
