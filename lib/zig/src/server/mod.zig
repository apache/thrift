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

const Allocator = std.mem.Allocator;

const simple_server = @import("simple_server.zig");
const protocol = @import("../protocol/mod.zig");
const lib = @import("../lib/mod.zig");
const application_exception = lib.application_exception;
const types = @import("../lib/types.zig");

pub const TSimpleServer = simple_server.TSimpleServer;

fn formatExceptionMessage(
    allocator: Allocator,
    exc_type: application_exception.TApplicationExcpetionType,
    err: anyerror,
    method_name: []const u8,
) ![]const u8 {
    return switch (exc_type) {
        .UnknownMethod => std.fmt.allocPrint(allocator, "Invalid method name: '{s}'", .{method_name}),
        else => std.fmt.allocPrint(allocator, "{s}: {}", .{ application_exception.defaultMessageForType(exc_type), err }),
    };
}

pub fn handleProcessResult(allocator: Allocator, msgIdentifier: protocol.TMessageIdentifier, res: anyerror!void, oProt: *protocol.TProtocol) !void {
    res catch |err| {
        const exc_type = application_exception.exceptionTypeFromError(err);
        const message_text = try formatExceptionMessage(allocator, exc_type, err, msgIdentifier.name);
        defer allocator.free(message_text);

        var exception = try lib.TApplicationException.init(allocator, .{
            .message = try types.String.initFromSlice(allocator, message_text),
            .type = @backingInt(exc_type),
        });
        defer exception.deinit();

        const ident = protocol.TMessageIdentifier{
            .allocator = undefined,
            .name = msgIdentifier.name,
            .msgType = protocol.TMessageType.Exception,
            .sequenceNumber = msgIdentifier.sequenceNumber,
        };

        try oProt.writeMessageBegin(ident);
        try exception.writeToProtocol(oProt);
        try oProt.writeMessageEnd();
        try oProt.flush();
    };
}
