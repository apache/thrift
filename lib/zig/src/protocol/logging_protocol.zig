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

const Allocator = std.mem.Allocator;

const TMessageIdentifier = protocol.TMessageIdentifier;
const TFieldIdentifier = protocol.TFieldIdentifier;
const TMapIdentifier = protocol.TMapIdentifier;
const TListIdentifier = protocol.TListIdentifier;
const TSetIdentifier = protocol.TSetIdentifier;
const TStructIdentifier = protocol.TStructIdentifier;
const TProtocol = protocol.TProtocol;
const TType = protocol.TType;

pub const TLoggingProtocol = struct {
    const Self = @This();

    allocator: Allocator,
    wrapped: TProtocol,
    name: []const u8,

    pub fn init(allocator: Allocator, wrapped: TProtocol, name: []const u8) !Self {
        return .{
            .allocator = allocator,
            .wrapped = wrapped,
            .name = try allocator.dupe(u8, name),
        };
    }

    pub fn deinit(self: *Self) void {
        self.wrapped.deinit();
        self.allocator.free(self.name);
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        self.wrapped.destroy(a);
        a.destroy(self);
    }

    pub fn interface(self: *Self) TProtocol {
        return TProtocol.init(self);
    }

    // --- Out Methods ---
    pub fn writeMessageBegin(self: *Self, id: TMessageIdentifier) !void {
        std.debug.print("[{s}] writeMessageBegin(name={s}, type={}, seq={})\n", .{ self.name, id.name, id.msgType, id.sequenceNumber });
        return self.wrapped.writeMessageBegin(id);
    }

    pub fn writeMessageEnd(self: *Self) !void {
        std.debug.print("[{s}] writeMessageEnd\n", .{self.name});
        return self.wrapped.writeMessageEnd();
    }

    pub fn writeStructBegin(self: *Self, id: TStructIdentifier) !void {
        std.debug.print("[{s}] writeStructBegin(name={s})\n", .{ self.name, id.name });
        return self.wrapped.writeStructBegin(id);
    }

    pub fn writeStructEnd(self: *Self) !void {
        std.debug.print("[{s}] writeStructEnd\n", .{self.name});
        return self.wrapped.writeStructEnd();
    }

    pub fn writeFieldBegin(self: *Self, id: TFieldIdentifier) !void {
        std.debug.print("[{s}] writeFieldBegin(type={}, id={?}, name={?s})\n", .{ self.name, id.fieldType, id.id, id.name });
        return self.wrapped.writeFieldBegin(id);
    }

    pub fn writeFieldEnd(self: *Self) !void {
        std.debug.print("[{s}] writeFieldEnd\n", .{self.name});
        return self.wrapped.writeFieldEnd();
    }

    pub fn writeFieldStop(self: *Self) !void {
        std.debug.print("[{s}] writeFieldStop\n", .{self.name});
        return self.wrapped.writeFieldStop();
    }

    pub fn writeMapBegin(self: *Self, id: TMapIdentifier) !void {
        std.debug.print("[{s}] writeMapBegin(kType={}, vType={}, size={})\n", .{ self.name, id.kType, id.vType, id.size });
        return self.wrapped.writeMapBegin(id);
    }

    pub fn writeMapEnd(self: *Self) !void {
        std.debug.print("[{s}] writeMapEnd\n", .{self.name});
        return self.wrapped.writeMapEnd();
    }

    pub fn writeListBegin(self: *Self, id: TListIdentifier) !void {
        std.debug.print("[{s}] writeListBegin(eType={}, size={})\n", .{ self.name, id.eType, id.size });
        return self.wrapped.writeListBegin(id);
    }

    pub fn writeListEnd(self: *Self) !void {
        std.debug.print("[{s}] writeListEnd\n", .{self.name});
        return self.wrapped.writeListEnd();
    }

    pub fn writeSetBegin(self: *Self, id: TSetIdentifier) !void {
        std.debug.print("[{s}] writeSetBegin(eType={}, size={})\n", .{ self.name, id.eType, id.size });
        return self.wrapped.writeSetBegin(id);
    }

    pub fn writeSetEnd(self: *Self) !void {
        std.debug.print("[{s}] writeSetEnd\n", .{self.name});
        return self.wrapped.writeSetEnd();
    }

    pub fn writeBool(self: *Self, value: bool) !void {
        std.debug.print("[{s}] writeBool({})\n", .{ self.name, value });
        return self.wrapped.writeBool(value);
    }

    pub fn writeByte(self: *Self, value: u8) !void {
        std.debug.print("[{s}] writeByte({})\n", .{ self.name, value });
        return self.wrapped.writeByte(value);
    }

    pub fn writeI8(self: *Self, value: i8) !void {
        std.debug.print("[{s}] writeI8({})\n", .{ self.name, value });
        return self.wrapped.writeI8(value);
    }

    pub fn writeI16(self: *Self, value: i16) !void {
        std.debug.print("[{s}] writeI16({})\n", .{ self.name, value });
        return self.wrapped.writeI16(value);
    }

    pub fn writeI32(self: *Self, value: i32) !void {
        std.debug.print("[{s}] writeI32({})\n", .{ self.name, value });
        return self.wrapped.writeI32(value);
    }

    pub fn writeI64(self: *Self, value: i64) !void {
        std.debug.print("[{s}] writeI64({})\n", .{ self.name, value });
        return self.wrapped.writeI64(value);
    }

    pub fn writeDouble(self: *Self, value: f64) !void {
        std.debug.print("[{s}] writeDouble({})\n", .{ self.name, value });
        return self.wrapped.writeDouble(value);
    }

    pub fn writeBytes(self: *Self, value: root.BinaryBytes) !void {
        std.debug.print("[{s}] writeBytes(len={})\n", .{ self.name, value.contents.len });
        return self.wrapped.writeBytes(value);
    }

    pub fn writeString(self: *Self, value: root.String) !void {
        std.debug.print("[{s}] writeString(len={})\n", .{ self.name, value.contents.len });
        return self.wrapped.writeString(value);
    }

    pub fn writeUUID(self: *Self, value: root.UUID) !void {
        std.debug.print("[{s}] writeUUID\n", .{self.name});
        return self.wrapped.writeUUID(value);
    }

    // --- In Methods ---
    pub fn readMessageBegin(self: *Self, allocator: Allocator) !TMessageIdentifier {
        std.debug.print("[{s}] readMessageBegin\n", .{self.name});
        return self.wrapped.readMessageBegin(allocator);
    }

    pub fn readMessageEnd(self: *Self) !void {
        std.debug.print("[{s}] readMessageEnd\n", .{self.name});
        return self.wrapped.readMessageEnd();
    }

    pub fn readStructBegin(self: *Self, allocator: Allocator) !TStructIdentifier {
        std.debug.print("[{s}] readStructBegin\n", .{self.name});
        return self.wrapped.readStructBegin(allocator);
    }

    pub fn readStructEnd(self: *Self) !void {
        std.debug.print("[{s}] readStructEnd\n", .{self.name});
        return self.wrapped.readStructEnd();
    }

    pub fn readFieldBegin(self: *Self, allocator: Allocator) !TFieldIdentifier {
        std.debug.print("[{s}] readFieldBegin\n", .{self.name});
        return self.wrapped.readFieldBegin(allocator);
    }

    pub fn readFieldEnd(self: *Self) !void {
        std.debug.print("[{s}] readFieldEnd\n", .{self.name});
        return self.wrapped.readFieldEnd();
    }

    pub fn readMapBegin(self: *Self) !TMapIdentifier {
        std.debug.print("[{s}] readMapBegin\n", .{self.name});
        return self.wrapped.readMapBegin();
    }

    pub fn readMapEnd(self: *Self) !void {
        std.debug.print("[{s}] readMapEnd\n", .{self.name});
        return self.wrapped.readMapEnd();
    }

    pub fn readListBegin(self: *Self) !TListIdentifier {
        std.debug.print("[{s}] readListBegin\n", .{self.name});
        return self.wrapped.readListBegin();
    }

    pub fn readListEnd(self: *Self) !void {
        std.debug.print("[{s}] readListEnd\n", .{self.name});
        return self.wrapped.readListEnd();
    }

    pub fn readSetBegin(self: *Self) !TSetIdentifier {
        std.debug.print("[{s}] readSetBegin\n", .{self.name});
        return self.wrapped.readSetBegin();
    }

    pub fn readSetEnd(self: *Self) !void {
        std.debug.print("[{s}] readSetEnd\n", .{self.name});
        return self.wrapped.readSetEnd();
    }

    pub fn readBool(self: *Self) !bool {
        std.debug.print("[{s}] readBool\n", .{self.name});
        return self.wrapped.readBool();
    }

    pub fn readByte(self: *Self) !u8 {
        std.debug.print("[{s}] readByte\n", .{self.name});
        return self.wrapped.readByte();
    }

    pub fn readI8(self: *Self) !i8 {
        std.debug.print("[{s}] readI8\n", .{self.name});
        return self.wrapped.readI8();
    }

    pub fn readI16(self: *Self) !i16 {
        std.debug.print("[{s}] readI16\n", .{self.name});
        return self.wrapped.readI16();
    }

    pub fn readI32(self: *Self) !i32 {
        std.debug.print("[{s}] readI32\n", .{self.name});
        return self.wrapped.readI32();
    }

    pub fn readI64(self: *Self) !i64 {
        std.debug.print("[{s}] readI64\n", .{self.name});
        return self.wrapped.readI64();
    }

    pub fn readDouble(self: *Self) !f64 {
        std.debug.print("[{s}] readDouble\n", .{self.name});
        return self.wrapped.readDouble();
    }

    pub fn readBytes(self: *Self, allocator: Allocator) !root.BinaryBytes {
        std.debug.print("[{s}] readBytes\n", .{self.name});
        return self.wrapped.readBytes(allocator);
    }

    pub fn readString(self: *Self, allocator: Allocator) !root.String {
        std.debug.print("[{s}] readString\n", .{self.name});
        return self.wrapped.readString(allocator);
    }

    pub fn readUUID(self: *Self) !root.UUID {
        std.debug.print("[{s}] readUUID\n", .{self.name});
        return self.wrapped.readUUID();
    }

    pub fn flush(self: *Self) !void {
        std.debug.print("[{s}] flush\n", .{self.name});
        return self.wrapped.flush();
    }

    pub fn skipRawString(self: *Self) !void {
        return self.wrapped.skipRawString();
    }

    pub fn getRecursionLimit(self: *Self) i32 {
        return self.wrapped.getRecursionLimit();
    }
};

pub const TLoggingProtocolFactory = struct {
    const Self = @This();

    wrappedFactory: protocol.TProtocolFactory,

    pub fn init(wrappedFactory: protocol.TProtocolFactory) Self {
        return .{
            .wrappedFactory = wrappedFactory,
        };
    }

    pub fn deinit(self: *Self) void {
        self.wrappedFactory.deinit();
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        a.destroy(self);
    }

    pub fn getProtocol(self: *Self, allocator: Allocator, transport: *root.TTransport) anyerror!TProtocol {
        const wrapped = try self.wrappedFactory.getProtocol(allocator, transport);
        const name = try allocator.dupe(u8, "logging");
        const logging_prot = try allocator.create(TLoggingProtocol);
        logging_prot.* = try TLoggingProtocol.init(allocator, wrapped, name);
        return logging_prot.interface();
    }

    pub fn interface(self: *Self) protocol.TProtocolFactory {
        return protocol.TProtocolFactory.init(self);
    }
};
