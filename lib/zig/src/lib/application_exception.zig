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

pub const TApplicationException = @import("generated/t_application_exception.zig").TApplicationException;

pub const ApplicationError = error{
    UnknownMethod,
    MissingResult,
};

// Wire-level exception type codes shared across Thrift implementations.
// See doc/specs/thrift-rpc.md.
pub const TApplicationExcpetionType = enum(i32) {
    Unknown = 0,
    UnknownMethod = 1,
    InvalidMessageType = 2,
    WrongMethodName = 3,
    BadSequenceId = 4,
    MissingResult = 5,
    InternalError = 6,
    ProtocolError = 7,
    InvalidTransform = 8,
    InvalidProtocol = 9,
    UnsupportedClientType = 10,
};

const protocol = @import("../protocol/mod.zig");
const ProtocolError = protocol.ProtocolError;

pub fn exceptionTypeFromError(err: anyerror) TApplicationExcpetionType {
    if (err == ApplicationError.UnknownMethod) return .UnknownMethod;
    if (err == ApplicationError.MissingResult) return .MissingResult;
    if (err == ProtocolError.InvalidMessageType) return .InvalidMessageType;

    inline for (@typeInfo(ProtocolError).error_set.error_names.?) |errorName| {
        if (err == @field(ProtocolError, errorName)) return .ProtocolError;
    }

    return .Unknown;
}

pub fn defaultMessageForType(exc_type: TApplicationExcpetionType) []const u8 {
    return switch (exc_type) {
        .Unknown => "unknown application exception",
        .UnknownMethod => "unknown method",
        .InvalidMessageType => "invalid message type",
        .WrongMethodName => "wrong method name",
        .BadSequenceId => "bad sequence ID",
        .MissingResult => "missing result",
        .InternalError => "unknown internal error",
        .ProtocolError => "unknown protocol error",
        .InvalidTransform => "invalid transform",
        .InvalidProtocol => "invalid protocol",
        .UnsupportedClientType => "unsupported client type",
    };
}

const testing = std.testing;

test "exceptionTypeFromError maps dispatch errors to wire codes" {
    try testing.expectEqual(TApplicationExcpetionType.UnknownMethod, exceptionTypeFromError(ApplicationError.UnknownMethod));
    try testing.expectEqual(TApplicationExcpetionType.MissingResult, exceptionTypeFromError(ApplicationError.MissingResult));
    try testing.expectEqual(TApplicationExcpetionType.InvalidMessageType, exceptionTypeFromError(ProtocolError.InvalidMessageType));
    try testing.expectEqual(TApplicationExcpetionType.ProtocolError, exceptionTypeFromError(ProtocolError.InvalidLength));
    try testing.expectEqual(TApplicationExcpetionType.Unknown, exceptionTypeFromError(error.OutOfMemory));
}
