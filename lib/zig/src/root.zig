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

pub const collections = @import("collections/mod.zig");
pub const map = collections.map;
pub const list = collections.list;
pub const set = collections.set;

pub const Map = map.Map;
pub const List = list.List;
pub const Set = set.Set;

pub const processor = @import("processor/mod.zig");
pub const protocol = @import("protocol/mod.zig");
pub const server = @import("server/mod.zig");
pub const transport = @import("transport/mod.zig");
pub const lib = @import("lib/mod.zig");
pub const internal = @import("internal/mod.zig");

pub const TTransport = transport.TTransport;
pub const TServerTransport = transport.TServerTransport;
pub const TTransportFactory = transport.TTransportFactory;
pub const TFramedTransport = transport.TFramedTransport;
pub const TFramedTransportFactory = transport.TFramedTransportFactory;
pub const FramedTransportError = transport.FramedTransportError;
pub const TSocket = transport.TSocket;
pub const TServerSocket = transport.TServerSocket;

pub const TMessageType = protocol.TMessageType;
pub const TProtocol = protocol.TProtocol;
pub const TProtocolFactory = protocol.TProtocolFactory;

pub const TBinaryProtocol = protocol.TBinaryProtocol;
pub const TBinaryProtocolFactory = protocol.TBinaryProtocolFactory;

pub const TCompactProtocol = protocol.TCompactProtocol;
pub const TCompactProtocolFactory = protocol.TCompactProtocolFactory;

pub const testing = @import("testing/mod.zig");

pub const TLoggingProtocol = protocol.TLoggingProtocol;
pub const TLoggingProtocolFactory = protocol.TLoggingProtocolFactory;

pub const TStoredProtocol = protocol.TStoredProtocol;
pub const TMultiplexedProtocol = protocol.TMultiplexedProtocol;

pub const TProcessor = processor.TProcessor;
pub const TMultiplexedProcessor = processor.TMultiplexedProcessor;

pub const TSimpleServer = server.TSimpleServer;

pub const application_exception = @import("lib/application_exception.zig");
pub const TApplicationException = application_exception.TApplicationException;
pub const result = @import("lib/result.zig");

pub const ServiceCallResult = result.ServiceCallResult;
pub const ClientResult = result.ClientResult;

pub const ApplicationError = error{
    UnknownMethod,
    MissingResult,
};

pub const types = lib.types;

pub const TConfiguration = lib.TConfiguration;
pub const DEFAULT_MAX_MESSAGE_SIZE = lib.DEFAULT_MAX_MESSAGE_SIZE;
pub const DEFAULT_MAX_FRAME_SIZE = lib.DEFAULT_MAX_FRAME_SIZE;
pub const DEFAULT_RECURSION_DEPTH = lib.DEFAULT_RECURSION_DEPTH;

pub const BinaryBytes = types.BinaryBytes;
pub const String = types.String;
pub const UUID = types.UUID;

// --- Tests ---
const mem = std.mem;
const net = std.Io.net;
const Allocator = mem.Allocator;
const t = std.testing;

const ServerThreadCtx = struct {
    allocator: Allocator,
    itf: *TServerTransport,
    readBuffer: []u8,
};

fn serverThreadFn(ctx: ServerThreadCtx) void {
    var connection = ctx.itf.accept(ctx.allocator) catch unreachable;
    defer {
        connection.deinit();
        connection.destroy(ctx.allocator);
    }
    var reader = connection.reader() catch unreachable;
    reader.readSliceAll(ctx.readBuffer) catch return;
}

fn clientThreadFn() void {
    const address = net.IpAddress.parse("0.0.0.0", 6009) catch unreachable;
    var conn = TSocket.initFromAddress(t.allocator, t.io_instance.io(), address) catch unreachable;
    defer conn.deinit();

    var writer = conn.writer() catch unreachable;
    writer.writeAll(&[_]u8{0} ** 100) catch unreachable;
    writer.flush() catch unreachable;
}

test "Socket stuff" {
    const address = try net.IpAddress.parse("127.0.0.1", 6009);
    var serverSocket = try TServerSocket.init(t.io_instance.io(), address);
    defer serverSocket.deinit();

    var serverSocketInterface = serverSocket.interface();

    var serverReadBuffer = [_]u8{0x69} ** 100;

    {
        try serverSocketInterface.open();
        try serverSocketInterface.listen();
        const serverThread = try std.Thread.spawn(
            .{},
            serverThreadFn,
            .{
                ServerThreadCtx{
                    .allocator = t.allocator,
                    .itf = &serverSocketInterface,
                    .readBuffer = &serverReadBuffer,
                },
            },
        );

        const clientThread = try std.Thread.spawn(.{}, clientThreadFn, .{});

        serverThread.join();
        clientThread.join();
    }

    try t.expectEqualSlices(u8, &[_]u8{0} ** 100, &serverReadBuffer);
}

test {
    _ = collections;
    _ = processor;
    _ = protocol;
    _ = server;
    _ = transport;
    _ = application_exception;
    _ = result;
}
