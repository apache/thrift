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
const thrift = @import("thrift");
const thrift_test = @import("thrift_test");
const clap = @import("clap");
const transportConfig = @import("transport_config.zig");
const protocolConfig = @import("protocol_config.zig");

const net = std.Io.net;
const Allocator = std.mem.Allocator;

fn runTests(init: std.process.Init) !u8 {
    const allocator = init.gpa;
    var config = thrift.TConfiguration.default;

    const params = comptime clap.parseParamsComptime(
        \\ -h, --help                   produce help message
        \\ --host <str>                 Host to connect
        \\ --port <u16>                 Port number to connect
        \\ --domain-socket <str>        Domain Socket (e.g. /tmp/ThriftTest.thrift), instead of host and port
        \\ --pipe <str>                 Windows Named Pipe (e.g. MyThriftPipe)
        //\\ --anon-pipes hRead hWrite    Windows Anonymous Pipes pair (handles)
        \\ --abstract-namespace         Create the domain socket in the Abstract Namespace
        \\                              (no connection with filesystem pathnames)
        \\ --transport <str>            Transport: buffered, framed, http, evhttp, zlib
        \\ --protocol <str>             Protocol: binary, compact, header, json
        \\ --multiplex                  Add TMultiplexedProtocol service name "ThriftTest"
        \\ --ssl                        Encrypted Transport using SSL
        \\ --zlib                       Wrap Transport with Zlib
        \\ -n, --testloops <u32>        Number of Tests
        \\ -t, --threads <u32>          Number of Test threads
    );
    var return_code: u8 = 0;

    std.debug.print("Parsing args\n", .{});
    var diag = clap.Diagnostic{};
    var paramsResult = clap.parse(clap.Help, &params, clap.parsers.default, init.minimal.args, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        try diag.reportToFile(init.io, .stderr(), err);
        return err;
    };
    defer paramsResult.deinit();

    const host = paramsResult.args.host orelse "127.0.0.1";
    const port = paramsResult.args.port orelse return error.NoPort;

    const address = try net.IpAddress.parse(host, port);
    const socket = try allocator.create(thrift.TSocket);
    socket.* = try thrift.TSocket.initFromAddress(allocator, init.io, address);

    var baseTransport = socket.interface();
    var transport = try transportConfig.wrapTransport(allocator, &config, paramsResult.args.transport, &baseTransport);
    defer {
        transport.deinit();
        transport.destroy(allocator);
    }

    const protocolMode = try protocolConfig.parseProtocolMode(paramsResult.args.protocol);

    var protocolFactory = try protocolConfig.createProtocolFactory(allocator, &config, paramsResult.args.protocol);
    defer protocolConfig.destroyProtocolFactory(&protocolFactory, allocator);

    var loggingFactory = thrift.TLoggingProtocolFactory.init(protocolFactory);
    var baseProtocol = try loggingFactory.getProtocol(allocator, &transport);

    var muxThrift: ?*thrift.TMultiplexedProtocol = null;
    var muxSecond: ?*thrift.TMultiplexedProtocol = null;

    var thriftProtocol: thrift.TProtocol = if (protocolMode.multiplex) blk: {
        muxThrift = try allocator.create(thrift.TMultiplexedProtocol);
        muxThrift.?.* = try thrift.TMultiplexedProtocol.init(allocator, baseProtocol, "ThriftTest");

        const secondBaseProtocol = try loggingFactory.getProtocol(allocator, &transport);
        muxSecond = try allocator.create(thrift.TMultiplexedProtocol);
        muxSecond.?.* = try thrift.TMultiplexedProtocol.init(allocator, secondBaseProtocol, "SecondService");
        break :blk muxThrift.?.interface();
    } else baseProtocol;
    defer {
        if (muxThrift) |m| m.destroy(allocator);
        if (muxSecond) |m| m.destroy(allocator);
        if (!protocolMode.multiplex) {
            baseProtocol.deinit();
            baseProtocol.destroy(allocator);
        }
    }

    var client = thrift_test.TThriftTestClient.init(allocator, &thriftProtocol, &thriftProtocol);

    var secondClient: ?thrift_test.TSecondServiceClient = null;
    if (protocolMode.multiplex) {
        var secondProtocol = muxSecond.?.interface();
        secondClient = thrift_test.TSecondServiceClient.init(allocator, &secondProtocol, &secondProtocol);
    }

    const num_tests = paramsResult.args.testloops orelse 1;

    for (0..@as(usize, @intCast(num_tests))) |t| {
        std.debug.print("Test #{d}, connect {s}:{d}\n", .{ t + 1, host, port });

        //const start = std.time.timestamp();

        {
            std.debug.print("testVoid() = ", .{});
            var voidResult = try client.testVoid();
            defer voidResult.deinit();
            if (voidResult.get()) |_| {
                std.debug.print("void\n", .{});
            } else |_| {
                std.debug.print("Error: {}\n", .{voidResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            const strTest: thrift.String = try .initFromSlice(allocator, "Test");
            defer strTest.deinit();
            std.debug.print("testString({f}) = ", .{strTest});
            var strResult = try client.testString(strTest);
            defer strResult.deinit();
            if (strResult.get()) |s| {
                if (!s.eql(strTest)) {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ strTest, s });
                    return_code |= 1;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{strResult.unwrapError()});
                return_code |= 1;
            }
        }

        if (secondClient) |*second| {
            const fooArg: thrift.String = try .initFromSlice(allocator, "foo");
            defer fooArg.deinit();
            std.debug.print("secondtestString({f}) = ", .{fooArg});
            var secondResult = try second.secondtestString(fooArg);
            defer secondResult.deinit();
            const expected: thrift.String = try .initFromSlice(allocator, "testString(\"foo\")");
            defer expected.deinit();
            if (secondResult.get()) |s| {
                if (!s.eql(expected)) {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ expected, s });
                    return_code |= 8;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{secondResult.unwrapError()});
                return_code |= 8;
            }
        }

        {
            std.debug.print("testBool(true) = ", .{});
            var boolResult = try client.testBool(true);
            defer boolResult.deinit();
            if (boolResult.get()) |val| {
                if (val) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected true got false\n", .{});
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{boolResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testBool(false) = ", .{});
            var boolResult = try client.testBool(false);
            defer boolResult.deinit();
            if (boolResult.get()) |val| {
                if (!val) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected false got true\n", .{});
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{boolResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testByte(42) = ", .{});
            var byteResult = try client.testByte(42);
            defer byteResult.deinit();
            if (byteResult.get()) |val| {
                if (val == 42) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 42, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{byteResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testByte(-42) = ", .{});
            var byteResult = try client.testByte(-42);
            defer byteResult.deinit();
            if (byteResult.get()) |val| {
                if (val == -42) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ -42, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{byteResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testI32(0) = ", .{});
            var i32Result = try client.testI32(0);
            defer i32Result.deinit();
            if (i32Result.get()) |val| {
                if (val == 0) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 0, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{i32Result.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testI32(-1) = ", .{});
            var i32Result = try client.testI32(-1);
            defer i32Result.deinit();
            if (i32Result.get()) |val| {
                if (val == -1) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ -1, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{i32Result.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testI32(190000013) = ", .{});
            var i32Result3 = try client.testI32(190000013);
            defer i32Result3.deinit();
            if (i32Result3.get()) |val| {
                if (val == 190000013) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 190000013, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{i32Result3.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testI64(0) = ", .{});
            var i64Result = try client.testI64(0);
            defer i64Result.deinit();
            if (i64Result.get()) |val| {
                if (val == 0) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 0, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{i64Result.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testI64(7000000000000000123) = ", .{});
            var i64Result2 = try client.testI64(7000000000000000123);
            defer i64Result2.deinit();
            if (i64Result2.get()) |val| {
                if (val == 7000000000000000123) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 7000000000000000123, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{i64Result2.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testDouble(0.0) = ", .{});
            var dblResult = try client.testDouble(0.0);
            defer dblResult.deinit();
            if (dblResult.get()) |val| {
                if (val == 0.0) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 0.0, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{dblResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testDouble(-1.0) = ", .{});
            var dblResult2 = try client.testDouble(-1.0);
            defer dblResult2.deinit();
            if (dblResult2.get()) |val| {
                if (val == -1.0) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ -1.0, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{dblResult2.unwrapError()});
                return_code |= 1;
            }
        }

        {
            const arg: thrift.BinaryBytes = try .initFromSlice(allocator, &[_]u8{ 1, 2, 3, 4, 5, 6, 7 });
            defer arg.deinit();
            std.debug.print("testBinary({f}) = ", .{arg});
            var binResult = try client.testBinary(arg);
            defer binResult.deinit();
            if (binResult.get()) |val| {
                if (!val.eql(arg)) {
                    std.debug.print("Failed: expected [{f}] got [{f}]\n", .{ arg, val });
                    return_code |= 1;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{binResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            const arg: thrift.UUID = try .parse("74408f86-8b27-48b6-be24-e5cb804a7f95");
            std.debug.print("testUuid({f}) = ", .{arg});
            var uuidResult = try client.testUuid(arg);
            if (uuidResult.get()) |val| {
                if (!val.eql(arg)) {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ arg, val });
                    return_code |= 1;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{uuidResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            var arg: thrift_test.Xtruct = .{
                .allocator = allocator,
                .string_thing = try .initFromSlice(allocator, "string_thing"),
                .byte_thing = 67,
                .i32_thing = 69,
                .i64_thing = 42,
            };
            defer arg.deinit();
            std.debug.print("testStruct({f}) = ", .{arg});
            var structResult = try client.testStruct(arg);
            defer structResult.deinit();
            if (structResult.get()) |val| {
                if (!val.eql(arg)) {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ arg, val });
                    return_code |= 2;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{structResult.unwrapError()});
                return_code |= 2;
            }
        }

        {
            var arg: thrift_test.Xtruct2 = .{
                .allocator = allocator,
                .byte_thing = 67,
                .i32_thing = 69,
                .struct_thing = .{
                    .allocator = allocator,
                    .string_thing = try .initFromSlice(allocator, "string_thing"),
                    .byte_thing = 67,
                    .i32_thing = 69,
                    .i64_thing = 42,
                },
            };
            defer arg.deinit();
            std.debug.print("testNest({f}) = ", .{arg});
            var nestResult = try client.testNest(arg);
            defer nestResult.deinit();
            if (nestResult.get()) |val| {
                if (!val.eql(arg)) {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ arg, val });
                    return_code |= 2;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{nestResult.unwrapError()});
                return_code |= 2;
            }
        }

        {
            var mapTest = thrift.Map(i32, i32).init(allocator);
            try mapTest.put(0, 0);
            try mapTest.put(1, 1);
            try mapTest.put(2, 2);
            defer mapTest.deinit();
            std.debug.print("testMap({f}) = ", .{mapTest});
            var mapResult = try client.testMap(mapTest);
            defer mapResult.deinit();
            if (mapResult.get()) |val| {
                if (val.eql(mapTest)) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ mapTest, val });
                    return_code |= 4;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{mapResult.unwrapError()});
                return_code |= 4;
            }
        }

        {
            var mapTest = thrift.Map(thrift.String, thrift.String).init(allocator);
            try mapTest.put(try .initFromSlice(allocator, "0"), try .initFromSlice(allocator, "0"));
            try mapTest.put(try .initFromSlice(allocator, "1"), try .initFromSlice(allocator, "1"));
            try mapTest.put(try .initFromSlice(allocator, "2"), try .initFromSlice(allocator, "2"));
            defer mapTest.deinit();
            std.debug.print("testStringMap({f}) = ", .{mapTest});
            var mapResult = try client.testStringMap(mapTest);
            defer mapResult.deinit();
            if (mapResult.get()) |val| {
                if (val.eql(mapTest)) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ mapTest, val });
                    return_code |= 4;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{mapResult.unwrapError()});
                return_code |= 4;
            }
        }

        {
            var setTest = thrift.Set(i32).init(allocator);
            try setTest.put(1);
            try setTest.put(2);
            try setTest.put(3);
            defer setTest.deinit();
            std.debug.print("testSet({f}) = ", .{setTest});
            var setResult = try client.testSet(setTest);
            defer setResult.deinit();
            if (setResult.get()) |val| {
                if (val.eql(setTest)) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ setTest, val });
                    return_code |= 4;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{setResult.unwrapError()});
                return_code |= 4;
            }
        }

        {
            var listTest = thrift.List(i32).init(allocator);
            try listTest.append(1);
            try listTest.append(2);
            try listTest.append(3);
            defer listTest.deinit();
            std.debug.print("testList({f}) = ", .{listTest});
            var listResult = try client.testList(listTest);
            defer listResult.deinit();
            if (listResult.get()) |val| {
                if (val.eql(val)) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ listTest, val });
                    return_code |= 4;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{listResult.unwrapError()});
                return_code |= 4;
            }
        }

        {
            std.debug.print("testEnum(ONE) = ", .{});
            var enumResult = try client.testEnum(thrift_test.Numberz.ONE);
            defer enumResult.deinit();
            if (enumResult.get()) |val| {
                if (val == .ONE) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected ONE got {s}\n", .{@tagName(val)});
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{enumResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            std.debug.print("testTypedef(999) = ", .{});
            var typedefResult = try client.testTypedef(999);
            defer typedefResult.deinit();
            if (typedefResult.get()) |val| {
                if (val == 999) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {d} got {d}\n", .{ 999, val });
                    return_code |= 1;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{typedefResult.unwrapError()});
                return_code |= 1;
            }
        }

        {
            const expected = expected: {
                var positive = thrift.Map(i32, i32).init(allocator);
                errdefer positive.deinit();
                var negative = thrift.Map(i32, i32).init(allocator);
                errdefer negative.deinit();

                var i: i32 = 1;
                while (i < 5) : (i += 1) {
                    try positive.put(@intCast(i), @intCast(i));
                    try negative.put(@intCast(-i), @intCast(-i));
                }

                var result = thrift.Map(i32, thrift.Map(i32, i32)).init(allocator);
                errdefer result.deinit();
                try result.put(4, positive);
                try result.put(-4, negative);
                break :expected result;
            };
            defer @constCast(&expected).deinit();

            std.debug.print("testMapMap(4) = ", .{});
            var mapmapResult = try client.testMapMap(4);
            defer mapmapResult.deinit();
            if (mapmapResult.get()) |val| {
                if (val.eql(expected)) {
                    std.debug.print("Success\n", .{});
                } else {
                    std.debug.print("Failed: expected {f} got {f}\n", .{ expected, val });
                    return_code |= 4;
                }
            } else |_| {
                std.debug.print("Error: {}\n", .{mapmapResult.unwrapError()});
                return_code |= 4;
            }
        }

        {
            std.debug.print("testException(ok) = ", .{});
            var exnArg = try thrift.String.initFromSlice(allocator, "ok");
            defer exnArg.deinit();
            var exnResult = try client.testException(exnArg);
            defer exnResult.deinit();
            if (exnResult.get()) |_| {
                std.debug.print("Success\n", .{});
            } else |_| {
                std.debug.print("Error: {}\n", .{exnResult.unwrapError()});
                return_code |= 8;
            }
        }

        {
            std.debug.print("testException(Xception) = ", .{});
            var exnArg = try thrift.String.initFromSlice(allocator, "Xception");
            defer exnArg.deinit();
            var exnResult = try client.testException(exnArg);
            defer exnResult.deinit();
            if (exnResult.get()) |_| {
                std.debug.print("Failed: expected Xception\n", .{});
                return_code |= 8;
            } else |_| {
                switch (exnResult.unwrapError()) {
                    .err1 => |e| {
                        if (e.errorCode != 1001) {
                            std.debug.print("Failed: expected errorCode = 1001, got {}\n", .{e.errorCode});
                            return_code |= 8;
                        } else if (!e.message.eql(exnArg)) {
                            std.debug.print("Failed: expected meesage = {f}, got {f}\n", .{ exnArg, e.message });
                            return_code |= 8;
                        } else {
                            std.debug.print("Success\n", .{});
                        }
                    },
                    else => {
                        std.debug.print("Failed: expected Xception\n", .{});
                        return_code |= 8;
                    },
                }
            }
        }

        {
            std.debug.print("testException(TException) = ", .{});
            var exnArg = try thrift.String.initFromSlice(allocator, "TException");
            defer exnArg.deinit();
            var exnResult = try client.testException(exnArg);
            defer exnResult.deinit();
            if (exnResult.get()) |_| {
                std.debug.print("Failed: expected TException\n", .{});
                return_code |= 8;
            } else |_| {
                switch (exnResult.unwrapError()) {
                    .TApplicationException => |e| {
                        std.debug.print("Success: {d} => {f}\n", .{ e.type_, e.message });
                    },
                    else => {
                        std.debug.print("Failed: expected TException\n", .{});
                        return_code |= 8;
                    },
                }
            }
        }

        {
            std.debug.print("testMultiException(Xception, ...) = ", .{});
            var exnArg = try thrift.String.initFromSlice(allocator, "Xception");
            defer exnArg.deinit();
            var exnResult = try client.testMultiException(exnArg, exnArg);
            defer exnResult.deinit();
            if (exnResult.get()) |_| {
                std.debug.print("Failed: expected Xception\n", .{});
                return_code |= 8;
            } else |_| {
                switch (exnResult.unwrapError()) {
                    .err1 => |e| {
                        const expectedMessage: thrift.String = try .initFromSlice(allocator, "This is an Xception");
                        defer expectedMessage.deinit();
                        if (e.errorCode != 1001) {
                            std.debug.print("Failed: expected errorCode = 1001, got {}\n", .{e.errorCode});
                            return_code |= 8;
                        } else if (!e.message.eql(expectedMessage)) {
                            std.debug.print("Failed: expected meesage = \"{f}\", got \"{f}\"\n", .{ expectedMessage, e.message });
                            return_code |= 8;
                        } else {
                            std.debug.print("Success\n", .{});
                        }
                    },
                    else => {
                        std.debug.print("Failed: expected Xception\n", .{});
                        return_code |= 8;
                    },
                }
            }
        }
        {
            std.debug.print("testMultiException(Xception2, ...) = ", .{});
            var exnArg = try thrift.String.initFromSlice(allocator, "Xception2");
            defer exnArg.deinit();
            var exnResult = try client.testMultiException(exnArg, exnArg);
            defer exnResult.deinit();
            if (exnResult.get()) |_| {
                std.debug.print("Failed: expected Xception2\n", .{});
                return_code |= 8;
            } else |_| {
                switch (exnResult.unwrapError()) {
                    .err2 => |e| {
                        const expectedMessage: thrift.String = try .initFromSlice(allocator, "This is an Xception2");
                        defer expectedMessage.deinit();
                        if (e.errorCode != 2002) {
                            std.debug.print("Failed: expected errorCode = 2002, got {}\n", .{e.errorCode});
                            return_code |= 8;
                        } else if (!e.struct_thing.string_thing.eql(expectedMessage)) {
                            std.debug.print("Failed: expected struct_thing.string_thing = \"{f}\", got \"{f}\"\n", .{ expectedMessage, e.struct_thing.string_thing });
                            return_code |= 8;
                        } else {
                            std.debug.print("Success\n", .{});
                        }
                    },
                    else => {
                        std.debug.print("Failed: expected Xception2\n", .{});
                        return_code |= 8;
                    },
                }
            }
        }
        {
            std.debug.print("testMultiException(noexception, thing) = ", .{});
            var exnArg = try thrift.String.initFromSlice(allocator, "noexception");
            defer exnArg.deinit();
            var exnArg2 = try thrift.String.initFromSlice(allocator, "thing");
            defer exnArg2.deinit();
            var exnResult = try client.testMultiException(exnArg, exnArg2);
            defer exnResult.deinit();
            if (exnResult.get()) |v| {
                if (!v.string_thing.eql(exnArg2)) {
                    std.debug.print("Failed: expected string_thing = \"{f}\", got \"{f}\"\n", .{ exnArg, v.string_thing });
                    return_code |= 8;
                } else {
                    std.debug.print("Success\n", .{});
                }
            } else |_| {
                std.debug.print("Failed: expected XStruct got error\n", .{});
                return_code |= 8;
            }
        }
        {
            std.debug.print("testOneWay(Xception2, ...) = ", .{});
            const start: std.Io.Timestamp = .now(init.io, .boot);
            try client.testOneway(1);
            const end: std.Io.Timestamp = .now(init.io, .boot);
            const elapsed = end.toMilliseconds() - start.toMilliseconds();
            const target = 200;
            if (elapsed > target) {
                std.debug.print("Failed: call took longer than {} milliseconds: {}\n", .{ target, elapsed });
                return_code |= 128;
            } else {
                std.debug.print("Success\n", .{});
            }
        }
    }

    std.debug.print("Tests completed with errors: {d}\n", .{return_code});
    return return_code;
}

pub fn main(init: std.process.Init) !void {
    std.process.exit(runTests(init) catch {
        std.process.exit(64);
    });
}
