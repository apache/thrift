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
const tutorial = @import("tutorial");

const Allocator = std.mem.Allocator;

pub fn main(init: std.process.Init) !void {
    var debug_allocator: std.heap.DebugAllocator(.{}) = .init;
    defer switch (debug_allocator.deinit()) {
        .leak => std.debug.print("UNFREE'D MEMORY DETECTED!!!! CHECK YOUR ALLOCS!!! >:(\n", .{}),
        .ok => std.debug.print("No memory leaks detected :)\n", .{}),
    };
    const gpa = debug_allocator.allocator();
    var config = thrift.TConfiguration.default;

    var socket = try thrift.TSocket.initFromAddress(
        gpa,
        init.io,
        .{ .ip4 = .{ .bytes = [4]u8{ 127, 0, 0, 1 }, .port = 9069 } },
    );
    defer socket.deinit();
    var socketItf = socket.interface();

    var protocol = try thrift.TBinaryProtocol.init(gpa, &socketItf, &config);
    defer protocol.deinit();
    var protocolItf = protocol.interface();

    var client = tutorial.TCalculatorClient.init(gpa, &protocolItf, &protocolItf);

    std.debug.print("pinging\n", .{});
    var result = try client.ping();
    defer result.deinit();

    _ = try result.get();

    var addResult = try client.add(1, 1);
    std.debug.print("1 + 1 = {}\n", .{try addResult.get()});

    var calcCallResult = try client.calculate(1, .{ .allocator = undefined, .op = .DIVIDE, .num1 = 1, .num2 = 0, .comment = null });
    defer calcCallResult.deinit();
    switch (calcCallResult) {
        .err => |e| switch (e) {
            .ouch => |io| {
                std.debug.print("Caught err: {f}\n", .{io.why});
            },
            inline else => |oe| {
                std.debug.print("Caught err: {}\n", .{@TypeOf(oe)});
            },
        },
        .result => {},
    }

    var calculateResult2 = try client.calculate(
        2,
        .{
            .allocator = undefined,
            .op = .SUBTRACT,
            .num1 = 15,
            .num2 = 10,
            .comment = null,
        },
    );
    defer calculateResult2.deinit();

    const c2 = calculateResult2.get() catch unreachable;
    std.debug.print("{d} - {d} = {d}\n", .{ 15, 10, c2 });

    var getStructResult = try client.getStruct(2);
    defer getStructResult.deinit();

    const ss = try getStructResult.get();
    std.debug.print("Received log: {d} => {f}\n", .{ ss.key, ss.value });
}
