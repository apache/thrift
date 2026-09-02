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
const protocol = @import("../protocol/mod.zig");
const types = @import("../lib/types.zig");
const utils = @import("../internal/struct_utilities.zig");

const TProtocol = protocol.TProtocol;
const TFieldIdentifier = protocol.TFieldIdentifier;
const TStructIdentifier = protocol.TStructIdentifier;
const TType = protocol.TType;
const ProtocolError = protocol.ProtocolError;

const Allocator = std.mem.Allocator;

pub const TApplicationExcpetionType = enum(i32) {
    Unknown = 0,
    UnknownMethod = 1, // used in case the method requested by the client is unknown by the server.
    InvalidMessageType = 2,
    WrongMethodName = 3,
    BadSequenceId = 4, // bad sequence id: 4, used internally by the client to indicate a wrong sequence id in the response.
    MissingResult = 5, // missing result: 5, used internally by the client to indicate a response without any field (result nor exception).
    InternalError = 6, // internal error: 6, used when the server throws an exception that is not declared in the Thrift IDL file.
    ProtocolError = 7, // protocol error: 7, used when something goes wrong during decoding. For example when a list is too long or a required field is missing.
    InvalidTransform = 8,
    InvalidProtocol = 9,
    UnsupportedClientType = 10,
};

// TODO: find way to auto generate this automatically
pub const TApplicationException = struct {
    const IsSet = struct {
        message: bool = false,
        type_: bool = false,
    };
    __isset: IsSet = .{},

    allocator: Allocator,
    message: types.String,
    type_: i32,

    pub fn initDefault(allocator: Allocator) !@This() {
        return .{
            .allocator = allocator,
            .message = try .initDefault(allocator),
            .type_ = 0,
        };
    }

    pub fn deinit(self: *@This()) void {
        self.message.deinit();
    }

    pub fn hash(self: *@This(), hasher: anytype) void {
        utils.hash(hasher, self.message);
        utils.hash(hasher, self.type_);
    }

    pub fn eql(self: *@This(), other: *@This()) u64 {
        if (!(std.mem.eql(u8, self.message, other.message))) {
            return false;
        }
        if (!(self.type_ == other.type_)) {
            return false;
        }
    }

    pub fn clone(self: *const @This(), allocator: Allocator) !@This() {
        const ret: @This() = .{
            .allocator = allocator,
            .message = try self.message.clone(allocator),
            .type_ = self.type_,
        };
        return ret;
    }

    pub fn readFromProtocol(allocator: Allocator, i_prot: *TProtocol) !TApplicationException {
        var structIdent = try i_prot.readStructBegin(allocator);
        defer structIdent.deinit();
        var temp_struct: TApplicationException = try .initDefault(allocator);
        errdefer temp_struct.deinit();

        while (true) {
            var field_ident = try i_prot.readFieldBegin(allocator);
            defer field_ident.deinit();
            if (field_ident.fieldType == .Stop) {
                break;
            }
            const field_id = field_ident.id orelse return ProtocolError.MissingFieldId;
            switch (field_id) {
                1 => {
                    temp_struct.message = try i_prot.readString(allocator);
                    temp_struct.__isset.message = true;
                },
                2 => {
                    temp_struct.type_ = try i_prot.readI32();
                    temp_struct.__isset.type_ = true;
                },
                else => {
                    try i_prot.skip(allocator, field_ident.fieldType);
                },
            }
            try i_prot.readFieldEnd();
        }
        try i_prot.readStructEnd();
        return temp_struct;
    }

    pub fn writeToProtocol(self: *const @This(), o_prot: *TProtocol) !void {
        try o_prot.writeStructBegin(TStructIdentifier{
            .allocator = undefined,
            .name = &"TApplicationException".*,
        });
        {
            try o_prot.writeFieldBegin(TFieldIdentifier{ .allocator = undefined, .name = &"message".*, .fieldType = TType.String, .id = 1 });
            try o_prot.writeString(self.message);
            try o_prot.writeFieldEnd();
        }
        {
            try o_prot.writeFieldBegin(TFieldIdentifier{ .allocator = undefined, .name = &"type".*, .fieldType = TType.I32, .id = 2 });
            try o_prot.writeI32(self.type_);
            try o_prot.writeFieldEnd();
        }
        try o_prot.writeFieldStop();
        try o_prot.writeStructEnd();
    }
};
