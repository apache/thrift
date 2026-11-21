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

const CalculatorHandler = struct {
    const Self = @This();

    allocator: Allocator,
    io: std.Io,
    sharedMap: std.hash_map.AutoHashMap(i32, []const u8),
    mapMutex: std.Io.Mutex,

    pub fn init(allocator: Allocator, io: std.Io) Self {
        return .{
            .allocator = allocator,
            .io = io,
            .sharedMap = .init(allocator),
            .mapMutex = .init,
        };
    }

    pub fn deinit(self: *Self) void {
        self.mapMutex.lockUncancelable(self.io);
        defer self.mapMutex.unlock(self.io);

        var it = self.sharedMap.valueIterator();
        while (it.next()) |l| {
            self.allocator.free(l.*);
        }
        self.sharedMap.deinit();
    }

    pub fn ping(self: *@This()) !tutorial.CalculatorPingResult {
        _ = self;
        std.debug.print("ping\n", .{});
        return .success({});
    }
    pub fn add(self: *@This(), num1: i32, num2: i32) !tutorial.CalculatorAddResult {
        _ = self;
        std.debug.print("add({} + {}) = {}\n", .{ num1, num2, num1 + num2 });
        return .success(num1 + num2);
    }
    pub fn calculate(self: *@This(), logid: i32, w: tutorial.Work) !tutorial.CalculatorCalculateResult {
        const res = res: switch (w.op) {
            .ADD => {
                std.debug.print("calculate {d} + {d}\n", .{ w.num1, w.num2 });
                break :res w.num1 + w.num2;
            },
            .SUBTRACT => {
                std.debug.print("calculate {d} - {d}\n", .{ w.num1, w.num2 });
                break :res w.num1 - w.num2;
            },
            .MULTIPLY => {
                std.debug.print("calculate {d} * {d}\n", .{ w.num1, w.num2 });
                break :res w.num1 * w.num2;
            },
            .DIVIDE => {
                std.debug.print("calculate {d} / {d}\n", .{ w.num1, w.num2 });
                if (w.num2 == 0) {
                    return .fail(.{ .ouch = .{ .allocator = self.allocator, .whatOp = @intFromEnum(w.op), .why = try .initFromSlice(self.allocator, "Division by 0") } });
                }
                break :res @divTrunc(w.num1, w.num2);
            },
            else => |i| {
                return .fail(.{ .ouch = .{ .allocator = self.allocator, .whatOp = @intFromEnum(i), .why = try .initFromSlice(self.allocator, "Unknown operation value") } });
            },
        };

        {
            self.mapMutex.lockUncancelable(self.io);
            defer self.mapMutex.unlock(self.io);

            if (try self.sharedMap.fetchPut(logid, try std.fmt.allocPrint(self.allocator, "{d}", .{res}))) |previous| {
                self.allocator.free(previous.value);
            }
        }

        return .success(res);
    }
    pub fn zip(self: *@This()) !void {
        _ = self;
        std.debug.print("zip\n", .{});
        return;
    }
    pub fn getStruct(self: *@This(), logId: i32) !tutorial.shared.SharedServiceGetStructResult {
        std.debug.print("get log: {d}", .{logId});
        self.mapMutex.lockUncancelable(self.io);
        defer self.mapMutex.unlock(self.io);

        return .success(try tutorial.shared.SharedStruct.init(
            self.allocator,
            .{ .key = logId, .value = try .initFromSlice(self.allocator, self.sharedMap.get(logId) orelse &[0]u8{}) },
        ));
    }

    pub fn interface(self: *Self) tutorial.CalculatorHandler {
        return .init(self);
    }
};

const Event = struct {
    io: std.Io,
    event: std.Io.Event = .unset,
};

var stopEvent: Event = undefined;

pub fn main(init: std.process.Init) !void {
    const gpa = init.gpa;
    var config = thrift.TConfiguration.default;

    var serverHandler = CalculatorHandler.init(gpa, init.io);
    defer serverHandler.deinit();
    var handlerInterface = serverHandler.interface();
    var processor = tutorial.CalculatorProcessor{
        .allocator = gpa,
        .handler = &handlerInterface,
    };
    var processorInterface = processor.interface();

    var protocolFactory = thrift.TBinaryProtocolFactory.init(&config);
    var protoFactIntf = protocolFactory.interface();
    defer protocolFactory.deinit();

    var socket = try thrift.transport.TServerSocket.init(init.io, .{ .ip4 = .{ .bytes = .{ 0, 0, 0, 0 }, .port = 9069 } });
    var socketIntf = socket.interface();
    defer socket.deinit();

    var server = thrift.TSimpleServer.init(gpa, &socketIntf, &processorInterface, &protoFactIntf, null);

    var serverFuture = try init.io.concurrent(struct {
        fn run(s: *thrift.TSimpleServer) void {
            s.serve() catch |e| {
                std.debug.print("Server error: {}\n", .{e});
            };
        }
    }.run, .{&server});
    defer serverFuture.cancel(init.io);

    stopEvent = .{
        .io = init.io,
        .event = .unset,
    };
    const sigHandler = std.posix.Sigaction{
        .handler = .{ .handler = struct {
            pub fn handler(sig: std.posix.SIG) callconv(.c) void {
                std.log.info("Received signal {}\n", .{sig});
                stopEvent.event.set(stopEvent.io);
            }
        }.handler },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };

    std.posix.sigaction(std.posix.SIG.INT, &sigHandler, null);
    std.posix.sigaction(std.posix.SIG.TERM, &sigHandler, null);
    std.posix.sigaction(std.posix.SIG.ABRT, &sigHandler, null);

    try stopEvent.event.wait(init.io);
}
