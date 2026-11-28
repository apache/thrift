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

/// Inbound args/message body could not be read. Servers should close the connection.
pub const InboundReadFailed = error{
    InboundReadFailed,
};

const inbound_read_failure_message = "Error reading message";

fn writeInboundReadException(
    allocator: Allocator,
    msgIdentifier: protocol.TMessageIdentifier,
    oProt: *protocol.TProtocol,
) !void {
    var exception = try lib.TApplicationException.init(allocator, .{
        .message = try types.String.initFromSlice(allocator, inbound_read_failure_message),
        .type = @backingInt(application_exception.TApplicationExcpetionType.ProtocolError),
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
}

fn formatExceptionMessage(
    allocator: Allocator,
    exc_type: application_exception.TApplicationExcpetionType,
    err: anyerror,
    method_name: []const u8,
) ![]const u8 {
    return switch (exc_type) {
        .UnknownMethod => std.fmt.allocPrint(allocator, "Invalid method name: '{s}'", .{method_name}),
        .InternalError => blk: {
            if (err == application_exception.ApplicationError.HandlerError) {
                break :blk try std.fmt.allocPrint(allocator, "Internal error processing '{s}'", .{method_name});
            }
            break :blk try std.fmt.allocPrint(allocator, "{s}", .{application_exception.defaultMessageForType(exc_type)});
        },
        else => std.fmt.allocPrint(allocator, "{s}: {}", .{ application_exception.defaultMessageForType(exc_type), err }),
    };
}

/// Handles handler-time failures by replying with `TApplicationException`.
/// On inbound read failure, sends a protocol error reply then propagates
/// `InboundReadFailed` so the server can drop the connection.
pub fn finishProcess(
    allocator: Allocator,
    msgIdentifier: protocol.TMessageIdentifier,
    res: anyerror!void,
    oProt: *protocol.TProtocol,
) !void {
    res catch |err| {
        if (err == InboundReadFailed.InboundReadFailed) {
            try writeInboundReadException(allocator, msgIdentifier, oProt);
            return err;
        }
        try handleProcessResult(allocator, msgIdentifier, res, oProt);
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

const testing = std.testing;

test "finishProcess sends protocol error then propagates inbound read failure" {
    var transport: @import("../testing/memory_transport.zig").TTestingMemoryTransport = try .init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var config = lib.TConfiguration.default;
    var prot = try protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer prot.deinit();
    var out = prot.interface();

    const msg = protocol.TMessageIdentifier{
        .allocator = undefined,
        .name = "ping",
        .msgType = .Call,
        .sequenceNumber = 1,
    };
    const readFail = struct {
        fn call() !void {
            return InboundReadFailed.InboundReadFailed;
        }
    }.call;
    try testing.expectError(
        InboundReadFailed.InboundReadFailed,
        finishProcess(testing.allocator, msg, readFail(), &out),
    );
    try testing.expect(transport.buffer.written().len > 0);
}

test "finishProcess forwards handler failures to exception reply" {
    var transport: @import("../testing/memory_transport.zig").TTestingMemoryTransport = try .init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();
    var config = lib.TConfiguration.default;
    var prot = try protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer prot.deinit();
    var out = prot.interface();

    const msg = protocol.TMessageIdentifier{
        .allocator = undefined,
        .name = "calculate",
        .msgType = .Call,
        .sequenceNumber = 7,
    };
    const handlerFail = struct {
        fn call() !void {
            return application_exception.ApplicationError.HandlerError;
        }
    }.call;
    try finishProcess(testing.allocator, msg, handlerFail(), &out);
    try testing.expect(transport.buffer.written().len > 0);

    const response_reader = try transport.reader();
    response_reader.seek = 0;
    var response_prot = prot.interface();
    var response_ident = try response_prot.readMessageBegin(testing.allocator);
    defer response_ident.deinit();
    try testing.expect(response_ident.msgType == protocol.TMessageType.Exception);
    var exception = try lib.TApplicationException.readFromProtocol(testing.allocator, &response_prot);
    defer exception.deinit();
    try testing.expectEqual(@as(i32, @intFromEnum(application_exception.TApplicationExcpetionType.InternalError)), exception.type);
    try testing.expectEqualStrings("Internal error processing 'calculate'", exception.message.contents);
}
