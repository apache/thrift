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
const clap = @import("clap");
const thrift = @import("thrift");
const thrift_test = @import("thrift_test");
const transportConfig = @import("transport_config.zig");
const protocolConfig = @import("protocol_config.zig");

const net = std.Io.net;
const Allocator = std.mem.Allocator;

pub const ThriftTestHandler = struct {
    allocator: Allocator,
    io: std.Io,

    pub fn init(allocator: Allocator, io: std.Io) @This() {
        return .{ .allocator = allocator, .io = io };
    }

    pub fn testVoid(self: *@This()) !thrift_test.ThriftTestTestVoidResult {
        _ = self;
        std.debug.print("testVoid()\n", .{});
        return .success({});
    }

    pub fn testString(self: *@This(), thing: thrift.String) !thrift_test.ThriftTestTestStringResult {
        std.debug.print("testString(\"{f}\")\n", .{thing});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testBool(self: *@This(), thing: bool) !thrift_test.ThriftTestTestBoolResult {
        _ = self;
        std.debug.print("testBool(\"{}\")\n", .{thing});
        return .success(thing);
    }

    pub fn testByte(self: *@This(), thing: i8) !thrift_test.ThriftTestTestByteResult {
        _ = self;
        std.debug.print("testByte(\"{}\")\n", .{thing});
        return .success(thing);
    }

    pub fn testI32(self: *@This(), thing: i32) !thrift_test.ThriftTestTestI32Result {
        _ = self;
        std.debug.print("testI32(\"{}\")\n", .{thing});
        return .success(thing);
    }

    pub fn testI64(self: *@This(), thing: i64) !thrift_test.ThriftTestTestI64Result {
        _ = self;
        std.debug.print("testI64(\"{}\")\n", .{thing});
        return .success(thing);
    }

    pub fn testDouble(self: *@This(), thing: f64) !thrift_test.ThriftTestTestDoubleResult {
        _ = self;
        std.debug.print("testDouble(\"{}\")\n", .{thing});
        return .success(thing);
    }

    pub fn testBinary(self: *@This(), thing: thrift.BinaryBytes) !thrift_test.ThriftTestTestBinaryResult {
        std.debug.print("testBinary(\"{f}\")\n", .{thing});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testUuid(self: *@This(), thing: thrift.UUID) !thrift_test.ThriftTestTestUuidResult {
        std.debug.print("testUUID(\"{f}\")\n", .{thing});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testStruct(self: *@This(), thing: thrift_test.Xtruct) !thrift_test.ThriftTestTestStructResult {
        std.debug.print("testStruct()\n", .{});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testNest(self: *@This(), thing: thrift_test.Xtruct2) !thrift_test.ThriftTestTestNestResult {
        std.debug.print("testNest()\n", .{});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testMap(self: *@This(), thing: thrift.Map(i32, i32)) !thrift_test.ThriftTestTestMapResult {
        std.debug.print("testMap()\n", .{});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testStringMap(self: *@This(), thing: thrift.Map(thrift.String, thrift.String)) !thrift_test.ThriftTestTestStringMapResult {
        std.debug.print("testStringMap()\n", .{});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testSet(self: *@This(), thing: thrift.Set(i32)) !thrift_test.ThriftTestTestSetResult {
        std.debug.print("testSet()\n", .{});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testList(self: *@This(), thing: thrift.List(i32)) !thrift_test.ThriftTestTestListResult {
        std.debug.print("testList()\n", .{});
        return .success(try thing.clone(self.allocator));
    }

    pub fn testEnum(self: *@This(), thing: thrift_test.Numberz) !thrift_test.ThriftTestTestEnumResult {
        _ = self;
        std.debug.print("testEnum(\"{}\")\n", .{@intFromEnum(thing)});
        return .success(thing);
    }

    pub fn testTypedef(self: *@This(), thing: thrift_test.UserId) !thrift_test.ThriftTestTestTypedefResult {
        _ = self;
        std.debug.print("testTypedef(\"{}\")\n", .{thing});
        return .success(thing);
    }

    pub fn testMapMap(self: *@This(), hello: i32) !thrift_test.ThriftTestTestMapMapResult {
        std.debug.print("testMapMap(\"{}\")\n", .{hello});
        var positive = thrift.Map(i32, i32).init(self.allocator);
        errdefer positive.deinit();
        var negative = thrift.Map(i32, i32).init(self.allocator);
        errdefer negative.deinit();

        var i: i32 = 1;
        while (i < 5) : (i += 1) {
            try positive.put(@intCast(i), @intCast(i));
            try negative.put(@intCast(-i), @intCast(-i));
        }

        var result = thrift.Map(i32, thrift.Map(i32, i32)).init(self.allocator);
        errdefer result.deinit();
        try result.put(4, positive);
        try result.put(-4, negative);

        return .success(result);
    }

    pub fn testInsanity(self: *@This(), argument: thrift_test.Insanity) !thrift_test.ThriftTestTestInsanityResult {
        std.debug.print("testInsanity()\n", .{});

        const InnerMap = thrift.Map(thrift_test.Numberz, thrift_test.Insanity);

        var result: thrift.Map(thrift_test.UserId, InnerMap) = .init(self.allocator);
        errdefer result.deinit();

        {
            var map: InnerMap = .init(self.allocator);
            errdefer map.deinit();
            try map.put(thrift_test.Numberz.TWO, try argument.clone(self.allocator));
            try map.put(thrift_test.Numberz.THREE, try argument.clone(self.allocator));
            try result.put(1, map);
        }
        {
            var map: InnerMap = .init(self.allocator);
            errdefer map.deinit();
            try map.put(thrift_test.Numberz.SIX, thrift_test.Insanity{
                .allocator = self.allocator,
                .userMap = .init(self.allocator),
                .xtructs = .init(self.allocator),
            });
            try result.put(2, map);
        }

        return .success(result);
    }

    pub fn testMulti(self: *@This(), arg0: i8, arg1: i32, arg2: i64, arg3: thrift.Map(i16, thrift.String), arg4: thrift_test.Numberz, arg5: thrift_test.UserId) !thrift_test.ThriftTestTestMultiResult {
        std.debug.print("testMulit()\n", .{});
        _ = arg3;
        _ = arg4;
        _ = arg5;
        const result = thrift_test.Xtruct{
            .allocator = self.allocator,
            .string_thing = try .initFromSlice(self.allocator, "Hello2"),
            .byte_thing = arg0,
            .i32_thing = arg1,
            .i64_thing = arg2,
        };
        return .success(result);
    }

    pub fn testException(self: *@This(), arg: thrift.String) !thrift_test.ThriftTestTestExceptionResult {
        std.debug.print("testException(\"{f}\")\n", .{arg});
        if (std.mem.eql(u8, arg.contents, "Xception")) {
            return .fail(.{ .err1 = .{
                .allocator = self.allocator,
                .errorCode = 1001,
                .message = try arg.clone(self.allocator),
            } });
        }
        if (std.mem.eql(u8, arg.contents, "TException")) {
            return error.TException;
        }
        return .success({});
    }

    pub fn testMultiException(self: *@This(), arg0: thrift.String, arg1: thrift.String) !thrift_test.ThriftTestTestMultiExceptionResult {
        std.debug.print("testMultiException({f}, {f})\n", .{ arg0, arg1 });
        if (std.mem.eql(u8, arg0.contents, "Xception")) {
            return .fail(.{ .err1 = .{
                .allocator = self.allocator,
                .errorCode = 1001,
                .message = try .initFromSlice(self.allocator, "This is an Xception"),
            } });
        }
        if (std.mem.eql(u8, arg0.contents, "Xception2")) {
            return .fail(.{ .err2 = .{
                .allocator = self.allocator,
                .errorCode = 2002,
                .struct_thing = .{
                    .allocator = self.allocator,
                    .string_thing = try .initFromSlice(self.allocator, "This is an Xception2"),
                    .byte_thing = 0,
                    .i32_thing = 0,
                    .i64_thing = 0,
                },
            } });
        }

        const result = thrift_test.Xtruct{
            .allocator = self.allocator,
            .string_thing = try arg1.clone(self.allocator),
            .byte_thing = 0,
            .i32_thing = 0,
            .i64_thing = 0,
        };
        return .success(result);
    }

    pub fn testOneway(self: *@This(), secondsToSleep: i32) !void {
        std.debug.print("testOneway({}): sleeping...\n", .{secondsToSleep});
        try self.io.sleep(.fromSeconds(secondsToSleep), .boot);
        std.debug.print("testOneway({}): done sleeping\n", .{secondsToSleep});
    }

    pub fn interface(self: *@This()) thrift_test.ThriftTestHandler {
        return thrift_test.ThriftTestHandler.init(self);
    }
};

pub const SecondServiceHandler = struct {
    allocator: Allocator,

    pub fn init(allocator: Allocator) @This() {
        return .{ .allocator = allocator };
    }

    pub fn secondtestString(self: *@This(), thing: thrift.String) !thrift_test.SecondServiceSecondtestStringResult {
        std.debug.print("secondtestString({f})\n", .{thing});
        const msg = try std.fmt.allocPrint(self.allocator, "testString(\"{f}\")", .{thing});
        defer self.allocator.free(msg);
        return .success(try thrift.String.initFromSlice(self.allocator, msg));
    }

    pub fn interface(self: *@This()) thrift_test.SecondServiceHandler {
        return thrift_test.SecondServiceHandler.init(self);
    }
};

pub fn main(init: std.process.Init) !void {
    const allocator = init.gpa;
    var config = thrift.TConfiguration.default;

    const params = comptime clap.parseParamsComptime(
        \\ -h, --help                   produce help message
        \\ --port <u16>                 Port number to listen
        \\ --domain-socket <str>        Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)
        \\ --pipe <str>                 Windows Named Pipe (e.g. MyThriftPipe)
        \\ --server-type <str>          Type of server, "simple", "thread-pool", "threaded", or "nonblocking"
        \\ --transport <str>            transport: buffered, framed, http, anonpipe, zlib
        \\ --protocol <str>             protocol: binary, compact, header, json
        \\ --multiplex                  Add TMultiplexedProtocol service name "ThriftTest"
        \\ --abstract-namespace         Create the domain socket in the Abstract Namespace
        \\                              (no connection with filesystem pathnames)
        \\ --ssl                        Encrypted Transport using SSL
        \\ --zlib                       Wrapped Transport using Zlib
        \\ --processor-events           processor-events
        \\ -n, --workers <usize>        Number of thread pools workers. Only valid for thread-pool server type
    );

    std.debug.print("Parsing args\n", .{});
    var diag = clap.Diagnostic{};
    var result = clap.parse(clap.Help, &params, clap.parsers.default, init.minimal.args, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        try diag.reportToFile(init.io, .stderr(), err);
        return err;
    };
    defer result.deinit();

    std.debug.print("Starting server\n", .{});
    const port = result.args.port orelse return error.PortNoDefined;
    const address = try net.IpAddress.parse("127.0.0.1", port);
    var serverSocket = try thrift.TServerSocket.init(init.io, address);
    defer serverSocket.deinit();

    const protocolMode = try protocolConfig.parseProtocolMode(result.args.protocol);

    var handler = ThriftTestHandler.init(allocator, init.io);
    var handlerItf = handler.interface();
    var thriftProcessor = thrift_test.ThriftTestProcessor{
        .allocator = allocator,
        .handler = &handlerItf,
    };

    var multiplexProcessor: ?*thrift.TMultiplexedProcessor = null;
    var secondHandler = SecondServiceHandler.init(allocator);
    var secondHandlerItf = secondHandler.interface();
    var secondProcessor = thrift_test.SecondServiceProcessor{
        .allocator = allocator,
        .handler = &secondHandlerItf,
    };

    var processorIntf: thrift.TProcessor = if (protocolMode.multiplex) blk: {
        multiplexProcessor = try allocator.create(thrift.TMultiplexedProcessor);
        multiplexProcessor.?.* = .init(allocator);
        multiplexProcessor.?.registerDefault(thriftProcessor.interface());
        try multiplexProcessor.?.register("ThriftTest", thriftProcessor.interface());
        try multiplexProcessor.?.register("SecondService", secondProcessor.interface());
        break :blk multiplexProcessor.?.interface();
    } else thriftProcessor.interface();
    defer if (multiplexProcessor) |mp| mp.destroy(allocator);

    var protocolFactory = try protocolConfig.createProtocolFactory(allocator, &config, result.args.protocol);
    defer protocolConfig.destroyProtocolFactory(&protocolFactory, allocator);

    // logging factory takes ownership of wrapped factory, i.e. calls the deinit method
    var loggingFactory = thrift.TLoggingProtocolFactory.init(protocolFactory);
    var loggingFactoryIntf = loggingFactory.interface();
    defer loggingFactory.deinit();

    const transportFactoryPtr = try transportConfig.createTransportFactory(allocator, &config, result.args.transport);
    defer if (transportFactoryPtr) |factory| transportConfig.destroyTransportFactory(factory, allocator);

    var serverSocketIntf = serverSocket.interface();

    var server = thrift.TSimpleServer.init(allocator, &serverSocketIntf, &processorIntf, &loggingFactoryIntf, transportFactoryPtr);
    std.debug.print("Serving\n", .{});
    try server.serve();
    std.debug.print("Stopping\n", .{});
}
