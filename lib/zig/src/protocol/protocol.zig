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

const types = @import("../lib/types.zig");
const transportLib = @import("../transport/interface.zig");
const utils = @import("utils.zig");

const Allocator = mem.Allocator;
const TTransport = transportLib.TTransport;

pub const ProtocolError = error{
    InvalidMessageType,
    InvalidProtocolId,
    InvalidBooleanWireValue,
    InvalidMessageVersion,
    InvalidIntData,
    MissingField,
    MissingFieldType,
    MissingFieldId,
    InvalidLength,
    RecursionLimitExceeded,
    InvalidTypeWireValue,
    InvalidStructNesting,
    UnknownField,
    NoTypeValueForType,
    InvalidTypeToSkip,
};

pub const TType = enum { Stop, Void, Bool, Byte, I8, I16, I32, I64, Double, String, Struct, Map, Set, List, Uuid };

// Maps thrift analogous zig types into protocol wire TType
pub fn typeToTType(comptime T: type) TType {
    switch (@typeInfo(T)) {
        .void => return .Void,
        .bool => return .Bool,
        .int => |i| {
            if (i.signedness != .signed) {
                if (i.bits == 8) {
                    return .Byte;
                }
                @compileError("only signed ints or u8's are supported");
            }
            switch (i.bits) {
                8 => return .I8,
                16 => return .I16,
                32 => return .I32,
                64 => return .I64,
                else => @compileError("unsupported bit length " ++ i.bits),
            }
        },
        .float => |f| {
            if (f.bits != 64) {
                @compileError("only 64 bit floats are supported");
            }
            return .Double;
        },
        .@"struct" => {
            if (T == types.String) {
                return .String;
            }
            if (T == types.BinaryBytes) {
                return .String;
            }
            if (T == types.UUID) {
                return .Uuid;
            }
            if (@hasDecl(T, "__IsMap")) {
                return .Map;
            }
            if (@hasDecl(T, "__IsSet")) {
                return .Set;
            }
            if (@hasDecl(T, "__IsList")) {
                return .List;
            }
            return .Struct;
        },
        .@"enum" => |e| {
            if (e.tag_type != i32) {
                @compileError("only enums backed by i32 are supported");
            }
            return .I32;
        },
        else => {},
    }

    @compileError("unsupported type: " ++ @typeName(T));
}

pub fn validateWireType(expected: TType, actual: TType) !void {
    if (expected != actual) {
        return ProtocolError.InvalidTypeWireValue;
    }
}

pub fn readFromProtocol(comptime T: type, allocator: Allocator, prot: *TProtocol) !T {
    switch (@typeInfo(T)) {
        .void => return,
        .bool => return try prot.readBool(),
        .int => |i| {
            if (i.signedness != .signed) {
                if (i.bits == 8) {
                    return try prot.readByte();
                }
                @compileError("only signed ints or u8's are supported");
            }
            switch (i.bits) {
                8 => return try prot.readI8(),
                16 => return try prot.readI16(),
                32 => return try prot.readI32(),
                64 => return try prot.readI64(),
                else => @compileError("unsupported bit length " ++ i.bits),
            }
        },
        .float => |f| {
            if (f.bits != 64) {
                @compileError("only 64 bit floats are supported");
            }
            return try prot.readDouble();
        },
        .@"struct", .@"enum" => {
            if (T == types.UUID) {
                return try prot.readUUID();
            }
            if (T == types.String) {
                return try prot.readString(allocator);
            }
            if (T == types.BinaryBytes) {
                return try prot.readBytes(allocator);
            }
            return try T.readFromProtocol(allocator, prot);
        },
        else => {},
    }
    if (T == types.String) {
        return try prot.readString(allocator);
    }
    if (T == types.BinaryBytes) {
        return try prot.readBytes(allocator);
    }
    if (T == types.UUID) {
        return try prot.readUUID();
    }

    @compileError("unsupported type: " ++ @typeName(T));
}

pub fn writeToProtocol(comptime T: type, val: T, prot: *TProtocol) !void {
    switch (@typeInfo(T)) {
        .void => return,
        .bool => return try prot.writeBool(val),
        .int => |i| {
            if (i.signedness != .signed) {
                if (i.bits == 8) {
                    return try prot.writeByte(val);
                }
                @compileError("only signed ints or u8's are supported");
            }
            switch (i.bits) {
                8 => return try prot.writeI8(val),
                16 => return try prot.writeI16(val),
                32 => return try prot.writeI32(val),
                64 => return try prot.writeI64(val),
                else => @compileError("unsupported bit length " ++ i.bits),
            }
        },
        .float => |f| {
            if (f.bits != 64) {
                @compileError("only 64 bit floats are supported");
            }
            return try prot.writeDouble(val);
        },
        .@"struct", .@"enum" => {
            if (T == types.UUID) {
                return try prot.writeUUID(val);
            }
            if (T == types.String) {
                return try prot.writeString(val);
            }
            if (T == types.BinaryBytes) {
                return try prot.writeBytes(val);
            }
            return try val.writeToProtocol(prot);
        },
        else => {},
    }
    if (T == types.String) {
        return try prot.writeString(val);
    }
    if (T == types.BinaryBytes) {
        return try prot.writeBytes(val);
    }
    if (T == types.UUID) {
        return try prot.writeUUID(val);
    }

    @compileError("unsupported type: " ++ @typeName(T));
}

pub const TMessageType = enum(u8) {
    Call = 1,
    Reply = 2,
    Exception = 3,
    OneWay = 4,
};

pub const TMessageIdentifier = struct {
    const Self = @This();

    allocator: Allocator,
    name: []const u8,
    msgType: TMessageType,
    sequenceNumber: i32,

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
    }
};

pub const TFieldIdentifier = struct {
    const Self = @This();

    allocator: Allocator,
    name: ?[]const u8,
    fieldType: TType,
    id: ?i16,

    pub fn deinit(self: *Self) void {
        if (self.name) |name| {
            self.allocator.free(name);
        }
    }
};

pub const TMapIdentifier = struct { kType: TType, vType: TType, size: i32 };
pub const TListIdentifier = struct { eType: TType, size: i32 };
pub const TSetIdentifier = struct { eType: TType, size: i32 };
pub const TStructIdentifier = struct {
    const Self = @This();

    allocator: Allocator,
    name: []const u8,

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.name);
    }
};

pub const TProtocol = struct {
    const Self = @This();

    ptr: *anyopaque,

    deinitFn: *const fn (ptr: *anyopaque) void,
    destroyFn: *const fn (ptr: *anyopaque, a: Allocator) void,

    writeMessageBeginFn: *const fn (ptr: *anyopaque, id: TMessageIdentifier) anyerror!void,
    writeMessageEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeStructBeginFn: *const fn (ptr: *anyopaque, id: TStructIdentifier) anyerror!void,
    writeStructEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeFieldBeginFn: *const fn (ptr: *anyopaque, id: TFieldIdentifier) anyerror!void,
    writeFieldEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeFieldStopFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeMapBeginFn: *const fn (ptr: *anyopaque, id: TMapIdentifier) anyerror!void,
    writeMapEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeListBeginFn: *const fn (ptr: *anyopaque, id: TListIdentifier) anyerror!void,
    writeListEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeSetBeginFn: *const fn (ptr: *anyopaque, id: TSetIdentifier) anyerror!void,
    writeSetEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    writeBoolFn: *const fn (ptr: *anyopaque, value: bool) anyerror!void,
    writeByteFn: *const fn (ptr: *anyopaque, value: u8) anyerror!void,
    writeI8Fn: *const fn (ptr: *anyopaque, value: i8) anyerror!void,
    writeI16Fn: *const fn (ptr: *anyopaque, value: i16) anyerror!void,
    writeI32Fn: *const fn (ptr: *anyopaque, value: i32) anyerror!void,
    writeI64Fn: *const fn (ptr: *anyopaque, value: i64) anyerror!void,
    writeDoubleFn: *const fn (ptr: *anyopaque, value: f64) anyerror!void,
    writeBytesFn: *const fn (ptr: *anyopaque, value: types.BinaryBytes) anyerror!void,
    writeStringFn: *const fn (ptr: *anyopaque, value: types.String) anyerror!void,
    writeUUIDFn: *const fn (ptr: *anyopaque, value: types.UUID) anyerror!void,

    readMessageBeginFn: *const fn (ptr: *anyopaque, allocator: Allocator) anyerror!TMessageIdentifier,
    readMessageEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    readStructBeginFn: *const fn (ptr: *anyopaque, allocator: Allocator) anyerror!TStructIdentifier,
    readStructEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    readFieldBeginFn: *const fn (ptr: *anyopaque, allocator: Allocator) anyerror!TFieldIdentifier,
    readFieldEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    readMapBeginFn: *const fn (ptr: *anyopaque) anyerror!TMapIdentifier,
    readMapEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    readListBeginFn: *const fn (ptr: *anyopaque) anyerror!TListIdentifier,
    readListEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    readSetBeginFn: *const fn (ptr: *anyopaque) anyerror!TSetIdentifier,
    readSetEndFn: *const fn (ptr: *anyopaque) anyerror!void,
    readBoolFn: *const fn (ptr: *anyopaque) anyerror!bool,
    readByteFn: *const fn (ptr: *anyopaque) anyerror!u8,
    readI8Fn: *const fn (ptr: *anyopaque) anyerror!i8,
    readI16Fn: *const fn (ptr: *anyopaque) anyerror!i16,
    readI32Fn: *const fn (ptr: *anyopaque) anyerror!i32,
    readI64Fn: *const fn (ptr: *anyopaque) anyerror!i64,
    readDoubleFn: *const fn (ptr: *anyopaque) anyerror!f64,
    readBytesFn: *const fn (ptr: *anyopaque, allocator: Allocator) anyerror!types.BinaryBytes,
    readStringFn: *const fn (ptr: *anyopaque, allocator: Allocator) anyerror!types.String,
    readUUIDFn: *const fn (ptr: *anyopaque) anyerror!types.UUID,

    flushFn: *const fn (ptr: *anyopaque) anyerror!void,

    skipRawStringFn: *const fn (ptr: *anyopaque) anyerror!void,
    getRecursionLimitFn: *const fn (ptr: *anyopaque) i32,

    pub fn init(ptr: anytype) Self {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.deinit(self);
            }
            pub fn destroy(pointer: *anyopaque, a: Allocator) void {
                const self: T = @ptrCast(@alignCast(pointer));
                a.destroy(self);
            }
            pub fn writeMessageBegin(pointer: *anyopaque, id: TMessageIdentifier) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeMessageBegin(self, id);
            }
            pub fn writeMessageEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeMessageEnd(self);
            }
            pub fn writeStructBegin(pointer: *anyopaque, id: TStructIdentifier) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeStructBegin(self, id);
            }
            pub fn writeStructEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeStructEnd(self);
            }
            pub fn writeFieldBegin(pointer: *anyopaque, id: TFieldIdentifier) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeFieldBegin(self, id);
            }
            pub fn writeFieldEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeFieldEnd(self);
            }
            pub fn writeFieldStop(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeFieldStop(self);
            }
            pub fn writeMapBegin(pointer: *anyopaque, id: TMapIdentifier) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeMapBegin(self, id);
            }
            pub fn writeMapEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeMapEnd(self);
            }
            pub fn writeListBegin(pointer: *anyopaque, id: TListIdentifier) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeListBegin(self, id);
            }
            pub fn writeListEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeListEnd(self);
            }
            pub fn writeSetBegin(pointer: *anyopaque, id: TSetIdentifier) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeSetBegin(self, id);
            }
            pub fn writeSetEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeSetEnd(self);
            }
            pub fn writeBool(pointer: *anyopaque, value: bool) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeBool(self, value);
            }
            pub fn writeByte(pointer: *anyopaque, value: u8) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeByte(self, value);
            }
            pub fn writeI8(pointer: *anyopaque, value: i8) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeI8(self, value);
            }
            pub fn writeI16(pointer: *anyopaque, value: i16) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeI16(self, value);
            }
            pub fn writeI32(pointer: *anyopaque, value: i32) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeI32(self, value);
            }
            pub fn writeI64(pointer: *anyopaque, value: i64) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeI64(self, value);
            }
            pub fn writeDouble(pointer: *anyopaque, value: f64) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeDouble(self, value);
            }
            pub fn writeBytes(pointer: *anyopaque, value: types.BinaryBytes) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeBytes(self, value);
            }
            pub fn writeString(pointer: *anyopaque, value: types.String) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeString(self, value);
            }
            pub fn writeUUID(pointer: *anyopaque, value: types.UUID) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writeUUID(self, value);
            }
            pub fn readMessageBegin(pointer: *anyopaque, a: Allocator) anyerror!TMessageIdentifier {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readMessageBegin(self, a);
            }
            pub fn readMessageEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readMessageEnd(self);
            }
            pub fn readStructBegin(pointer: *anyopaque, a: Allocator) anyerror!TStructIdentifier {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readStructBegin(self, a);
            }
            pub fn readStructEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readStructEnd(self);
            }
            pub fn readFieldBegin(pointer: *anyopaque, a: Allocator) anyerror!TFieldIdentifier {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readFieldBegin(self, a);
            }
            pub fn readFieldEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readFieldEnd(self);
            }
            pub fn readMapBegin(pointer: *anyopaque) anyerror!TMapIdentifier {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readMapBegin(self);
            }
            pub fn readMapEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readMapEnd(self);
            }
            pub fn readListBegin(pointer: *anyopaque) anyerror!TListIdentifier {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readListBegin(self);
            }
            pub fn readListEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readListEnd(self);
            }
            pub fn readSetBegin(pointer: *anyopaque) anyerror!TSetIdentifier {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readSetBegin(self);
            }
            pub fn readSetEnd(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readSetEnd(self);
            }
            pub fn readBool(pointer: *anyopaque) anyerror!bool {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readBool(self);
            }
            pub fn readByte(pointer: *anyopaque) anyerror!u8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readByte(self);
            }
            pub fn readI8(pointer: *anyopaque) anyerror!i8 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readI8(self);
            }
            pub fn readI16(pointer: *anyopaque) anyerror!i16 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readI16(self);
            }
            pub fn readI32(pointer: *anyopaque) anyerror!i32 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readI32(self);
            }
            pub fn readI64(pointer: *anyopaque) anyerror!i64 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readI64(self);
            }
            pub fn readDouble(pointer: *anyopaque) anyerror!f64 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readDouble(self);
            }
            pub fn readBytes(pointer: *anyopaque, a: Allocator) anyerror!types.BinaryBytes {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readBytes(self, a);
            }
            pub fn readString(pointer: *anyopaque, a: Allocator) anyerror!types.String {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readString(self, a);
            }
            pub fn readUUID(pointer: *anyopaque) anyerror!types.UUID {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.readUUID(self);
            }
            pub fn flush(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.flush(self);
            }
            pub fn skipRawString(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.skipRawString(self);
            }
            pub fn getRecursionLimit(pointer: *anyopaque) i32 {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.getRecursionLimit(self);
            }
        };

        return .{
            .ptr = ptr,
            .deinitFn = gen.deinit,
            .destroyFn = gen.destroy,
            .writeMessageBeginFn = gen.writeMessageBegin,
            .writeMessageEndFn = gen.writeMessageEnd,
            .writeStructBeginFn = gen.writeStructBegin,
            .writeStructEndFn = gen.writeStructEnd,
            .writeFieldBeginFn = gen.writeFieldBegin,
            .writeFieldEndFn = gen.writeFieldEnd,
            .writeFieldStopFn = gen.writeFieldStop,
            .writeMapBeginFn = gen.writeMapBegin,
            .writeMapEndFn = gen.writeMapEnd,
            .writeListBeginFn = gen.writeListBegin,
            .writeListEndFn = gen.writeListEnd,
            .writeSetBeginFn = gen.writeSetBegin,
            .writeSetEndFn = gen.writeSetEnd,
            .writeBoolFn = gen.writeBool,
            .writeByteFn = gen.writeByte,
            .writeI8Fn = gen.writeI8,
            .writeI16Fn = gen.writeI16,
            .writeI32Fn = gen.writeI32,
            .writeI64Fn = gen.writeI64,
            .writeDoubleFn = gen.writeDouble,
            .writeBytesFn = gen.writeBytes,
            .writeStringFn = gen.writeString,
            .writeUUIDFn = gen.writeUUID,
            .readMessageBeginFn = gen.readMessageBegin,
            .readMessageEndFn = gen.readMessageEnd,
            .readStructBeginFn = gen.readStructBegin,
            .readStructEndFn = gen.readStructEnd,
            .readFieldBeginFn = gen.readFieldBegin,
            .readFieldEndFn = gen.readFieldEnd,
            .readMapBeginFn = gen.readMapBegin,
            .readMapEndFn = gen.readMapEnd,
            .readListBeginFn = gen.readListBegin,
            .readListEndFn = gen.readListEnd,
            .readSetBeginFn = gen.readSetBegin,
            .readSetEndFn = gen.readSetEnd,
            .readBoolFn = gen.readBool,
            .readByteFn = gen.readByte,
            .readI8Fn = gen.readI8,
            .readI16Fn = gen.readI16,
            .readI32Fn = gen.readI32,
            .readI64Fn = gen.readI64,
            .readDoubleFn = gen.readDouble,
            .readBytesFn = gen.readBytes,
            .readStringFn = gen.readString,
            .readUUIDFn = gen.readUUID,
            .flushFn = gen.flush,
            .skipRawStringFn = gen.skipRawString,
            .getRecursionLimitFn = gen.getRecursionLimit,
        };
    }

    pub fn deinit(self: *Self) void {
        return self.deinitFn(self.ptr);
    }
    pub fn destroy(self: *Self, a: Allocator) void {
        return self.destroyFn(self.ptr, a);
    }

    pub fn writeMessageBegin(self: *Self, id: TMessageIdentifier) anyerror!void {
        return self.writeMessageBeginFn(self.ptr, id);
    }
    pub fn writeMessageEnd(self: *Self) anyerror!void {
        return self.writeMessageEndFn(self.ptr);
    }
    pub fn writeStructBegin(self: *Self, id: TStructIdentifier) anyerror!void {
        return self.writeStructBeginFn(self.ptr, id);
    }
    pub fn writeStructEnd(self: *Self) anyerror!void {
        return self.writeStructEndFn(self.ptr);
    }
    pub fn writeFieldBegin(self: *Self, id: TFieldIdentifier) anyerror!void {
        return self.writeFieldBeginFn(self.ptr, id);
    }
    pub fn writeFieldEnd(self: *Self) anyerror!void {
        return self.writeFieldEndFn(self.ptr);
    }
    pub fn writeFieldStop(self: *Self) anyerror!void {
        return self.writeFieldStopFn(self.ptr);
    }
    pub fn writeMapBegin(self: *Self, id: TMapIdentifier) anyerror!void {
        return self.writeMapBeginFn(self.ptr, id);
    }
    pub fn writeMapEnd(self: *Self) anyerror!void {
        return self.writeMapEndFn(self.ptr);
    }
    pub fn writeListBegin(self: *Self, id: TListIdentifier) anyerror!void {
        return self.writeListBeginFn(self.ptr, id);
    }
    pub fn writeListEnd(self: *Self) anyerror!void {
        return self.writeListEndFn(self.ptr);
    }
    pub fn writeSetBegin(self: *Self, id: TSetIdentifier) anyerror!void {
        return self.writeSetBeginFn(self.ptr, id);
    }
    pub fn writeSetEnd(self: *Self) anyerror!void {
        return self.writeSetEndFn(self.ptr);
    }
    pub fn writeBool(self: *Self, value: bool) anyerror!void {
        return self.writeBoolFn(self.ptr, value);
    }
    pub fn writeByte(self: *Self, value: u8) anyerror!void {
        return self.writeByteFn(self.ptr, value);
    }
    pub fn writeI8(self: *Self, value: i8) anyerror!void {
        return self.writeI8Fn(self.ptr, value);
    }
    pub fn writeI16(self: *Self, value: i16) anyerror!void {
        return self.writeI16Fn(self.ptr, value);
    }
    pub fn writeI32(self: *Self, value: i32) anyerror!void {
        return self.writeI32Fn(self.ptr, value);
    }
    pub fn writeI64(self: *Self, value: i64) anyerror!void {
        return self.writeI64Fn(self.ptr, value);
    }
    pub fn writeDouble(self: *Self, value: f64) anyerror!void {
        return self.writeDoubleFn(self.ptr, value);
    }
    pub fn writeBytes(self: *Self, value: types.BinaryBytes) anyerror!void {
        return self.writeBytesFn(self.ptr, value);
    }
    pub fn writeString(self: *Self, value: types.String) anyerror!void {
        return self.writeStringFn(self.ptr, value);
    }
    pub fn writeUUID(self: *Self, value: types.UUID) anyerror!void {
        return self.writeUUIDFn(self.ptr, value);
    }
    pub fn readMessageBegin(self: *Self, allocator: Allocator) anyerror!TMessageIdentifier {
        return self.readMessageBeginFn(self.ptr, allocator);
    }
    pub fn readMessageEnd(self: *Self) anyerror!void {
        return self.readMessageEndFn(self.ptr);
    }
    pub fn readStructBegin(self: *Self, allocator: Allocator) anyerror!TStructIdentifier {
        return self.readStructBeginFn(self.ptr, allocator);
    }
    pub fn readStructEnd(self: *Self) anyerror!void {
        return self.readStructEndFn(self.ptr);
    }
    pub fn readFieldBegin(self: *Self, allocator: Allocator) anyerror!TFieldIdentifier {
        return self.readFieldBeginFn(self.ptr, allocator);
    }
    pub fn readFieldEnd(self: *Self) anyerror!void {
        return self.readFieldEndFn(self.ptr);
    }
    pub fn readMapBegin(self: *Self) anyerror!TMapIdentifier {
        return self.readMapBeginFn(self.ptr);
    }
    pub fn readMapEnd(self: *Self) anyerror!void {
        return self.readMapEndFn(self.ptr);
    }
    pub fn readListBegin(self: *Self) anyerror!TListIdentifier {
        return self.readListBeginFn(self.ptr);
    }
    pub fn readListEnd(self: *Self) anyerror!void {
        return self.readListEndFn(self.ptr);
    }
    pub fn readSetBegin(self: *Self) anyerror!TSetIdentifier {
        return self.readSetBeginFn(self.ptr);
    }
    pub fn readSetEnd(self: *Self) anyerror!void {
        return self.readSetEndFn(self.ptr);
    }
    pub fn readBool(self: *Self) anyerror!bool {
        return self.readBoolFn(self.ptr);
    }
    pub fn readByte(self: *Self) anyerror!u8 {
        return self.readByteFn(self.ptr);
    }
    pub fn readI8(self: *Self) anyerror!i8 {
        return self.readI8Fn(self.ptr);
    }
    pub fn readI16(self: *Self) anyerror!i16 {
        return self.readI16Fn(self.ptr);
    }
    pub fn readI32(self: *Self) anyerror!i32 {
        return self.readI32Fn(self.ptr);
    }
    pub fn readI64(self: *Self) anyerror!i64 {
        return self.readI64Fn(self.ptr);
    }
    pub fn readDouble(self: *Self) anyerror!f64 {
        return self.readDoubleFn(self.ptr);
    }
    pub fn readBytes(self: *Self, allocator: Allocator) anyerror!types.BinaryBytes {
        return self.readBytesFn(self.ptr, allocator);
    }
    pub fn readString(self: *Self, allocator: Allocator) anyerror!types.String {
        return self.readStringFn(self.ptr, allocator);
    }
    pub fn readUUID(self: *Self) anyerror!types.UUID {
        return self.readUUIDFn(self.ptr);
    }
    pub fn flush(self: *Self) anyerror!void {
        return self.flushFn(self.ptr);
    }
    pub fn skipRawString(self: *Self) anyerror!void {
        return self.skipRawStringFn(self.ptr);
    }
    pub fn getRecursionLimit(self: *Self) i32 {
        return self.getRecursionLimitFn(self.ptr);
    }

    pub fn skipWithDepth(self: *Self, allocator: Allocator, fieldType: TType, max_depth: i32) anyerror!void {
        try utils.checkSkipDepth(max_depth);
        const child_depth = max_depth - 1;

        switch (fieldType) {
            .Void => return,
            .Bool => _ = try self.readBool(),
            .Byte => _ = try self.readByte(),
            .I8 => _ = try self.readI8(),
            .I16 => _ = try self.readI16(),
            .I32 => _ = try self.readI32(),
            .I64 => _ = try self.readI64(),
            .Double => _ = try self.readDouble(),
            .Uuid => _ = try self.readUUID(),
            .String => try self.skipRawString(),
            .Struct => {
                var sIdent = try self.readStructBegin(allocator);
                defer sIdent.deinit();
                while (true) {
                    var field_ident = try self.readFieldBegin(allocator);
                    defer field_ident.deinit();
                    if (field_ident.fieldType == .Stop) {
                        break;
                    }
                    try self.skipWithDepth(allocator, field_ident.fieldType, child_depth);
                    try self.readFieldEnd();
                }
                try self.readStructEnd();
            },
            .List => {
                const list_ident = try self.readListBegin();
                for (0..@intCast(list_ident.size)) |_| {
                    try self.skipWithDepth(allocator, list_ident.eType, child_depth);
                }
                try self.readListEnd();
            },
            .Set => {
                const set_ident = try self.readSetBegin();
                for (0..@intCast(set_ident.size)) |_| {
                    try self.skipWithDepth(allocator, set_ident.eType, child_depth);
                }
                try self.readSetEnd();
            },
            .Map => {
                const map_ident = try self.readMapBegin();
                for (0..@intCast(map_ident.size)) |_| {
                    try self.skipWithDepth(allocator, map_ident.kType, child_depth);
                    try self.skipWithDepth(allocator, map_ident.vType, child_depth);
                }
                try self.readMapEnd();
            },
            else => return ProtocolError.InvalidTypeToSkip,
        }
    }

    pub fn skip(self: *Self, allocator: Allocator, fieldType: TType) anyerror!void {
        try self.skipWithDepth(allocator, fieldType, self.getRecursionLimit());
    }
};

pub const TProtocolFactory = struct {
    const Self = @This();

    ptr: *anyopaque,

    deinitFn: *const fn (ptr: *anyopaque) void,
    destroyFn: *const fn (ptr: *anyopaque, a: Allocator) void,
    getProtocolFn: *const fn (ptr: *anyopaque, allocator: Allocator, transport: *TTransport) anyerror!TProtocol,

    pub fn init(ptr: anytype) Self {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.deinit(self);
            }
            pub fn destroy(pointer: *anyopaque, a: Allocator) void {
                const self: T = @ptrCast(@alignCast(pointer));
                a.destroy(self);
            }
            pub fn getProtocol(pointer: *anyopaque, allocator: Allocator, transport: *TTransport) anyerror!TProtocol {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.getProtocol(self, allocator, transport);
            }
        };

        return .{
            .ptr = ptr,
            .deinitFn = gen.deinit,
            .destroyFn = gen.destroy,
            .getProtocolFn = gen.getProtocol,
        };
    }

    pub fn deinit(self: *Self) void {
        return self.deinitFn(self.ptr);
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        return self.destroyFn(self.ptr, a);
    }

    pub fn getProtocol(self: *Self, allocator: Allocator, transport: *TTransport) anyerror!TProtocol {
        return self.getProtocolFn(self.ptr, allocator, transport);
    }
};

const t = std.testing;
const collections = @import("../collections/mod.zig");

test "validateWireType accepts matching types" {
    try validateWireType(.I32, .I32);
    try validateWireType(.List, .List);
}

test "validateWireType rejects mismatched types" {
    try t.expectError(ProtocolError.InvalidTypeWireValue, validateWireType(.I32, .String));
    try t.expectError(ProtocolError.InvalidTypeWireValue, validateWireType(.Map, .List));
}

test "test typeToTType" {
    try t.expect(typeToTType(void) == .Void);
    try t.expect(typeToTType(bool) == .Bool);
    try t.expect(typeToTType(u8) == .Byte);
    try t.expect(typeToTType(i8) == .I8);
    try t.expect(typeToTType(i16) == .I16);
    try t.expect(typeToTType(i32) == .I32);
    try t.expect(typeToTType(i64) == .I64);
    try t.expect(typeToTType(f64) == .Double);
    try t.expect(typeToTType(collections.list.List(i32)) == .List);
    try t.expect(typeToTType(collections.map.Map(i32, i32)) == .Map);
    try t.expect(typeToTType(collections.set.Set(i32)) == .Set);
    try t.expect(typeToTType(types.String) == .String);
    try t.expect(typeToTType(types.BinaryBytes) == .String);
    try t.expect(typeToTType(types.UUID) == .Uuid);
    try t.expect(typeToTType(struct {}) == .Struct);
    try t.expect(typeToTType(enum(i32) {}) == .I32);
}
