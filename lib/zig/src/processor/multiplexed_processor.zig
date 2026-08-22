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
const stored_protocol = @import("../protocol/stored_protocol.zig");
const application_exception = @import("../lib/application_exception.zig");
const types = @import("../lib/types.zig");

const Allocator = std.mem.Allocator;
const testing = std.testing;

const TProtocol = protocol.TProtocol;
const TMessageType = protocol.TMessageType;
const TMessageIdentifier = protocol.TMessageIdentifier;
const TProcessor = @import("processor.zig").TProcessor;
const TStoredProtocol = stored_protocol.TStoredProtocol;
const TApplicationException = application_exception.TApplicationException;
const TApplicationExcpetionType = application_exception.TApplicationExcpetionType;

pub const MultiplexedProcessorError = error{
    DuplicateService,
    InvalidMessageName,
};

pub const SplitName = union(enum) {
    multiplexed: struct { service: []const u8, method: []const u8 },
    plain: []const u8,
};

/// Split a Thrift message name into service + method, or plain method.
/// More than one ':' is invalid
pub fn splitMultiplexName(name: []const u8) MultiplexedProcessorError!SplitName {
    if (std.mem.indexOfScalar(u8, name, ':')) |first| {
        const rest = name[first + 1 ..];
        if (std.mem.indexOfScalar(u8, rest, ':') != null) {
            return MultiplexedProcessorError.InvalidMessageName;
        }
        return .{ .multiplexed = .{
            .service = name[0..first],
            .method = rest,
        } };
    }
    return .{ .plain = name };
}

pub const TMultiplexedProcessor = struct {
    const Self = @This();

    allocator: Allocator,
    processors: std.StringHashMap(TProcessor),
    defaultProcessor: ?TProcessor = null,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
            .processors = std.StringHashMap(TProcessor).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.processors.deinit();
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        self.deinit();
        a.destroy(self);
    }

    pub fn registerDefault(self: *Self, processor: TProcessor) void {
        self.defaultProcessor = processor;
    }

    pub fn register(self: *Self, serviceName: []const u8, processor: TProcessor) !void {
        const result = try self.processors.getOrPut(serviceName);
        if (result.found_existing) return MultiplexedProcessorError.DuplicateService;
        result.value_ptr.* = processor;
    }

    pub fn interface(self: *Self) TProcessor {
        return TProcessor.init(self);
    }

    pub fn process(self: *Self, in: *TProtocol, out: *TProtocol) !void {
        var msg = try in.readMessageBegin(self.allocator);
        defer msg.deinit();

        if (msg.msgType != .Call and msg.msgType != .OneWay) {
            return writeProtocolError(self.allocator, in, out, msg, "Unexpected message type");
        }

        const split = splitMultiplexName(msg.name) catch {
            return writeProtocolError(self.allocator, in, out, msg, "Invalid message name");
        };

        const methodName: []const u8 = switch (split) {
            .multiplexed => |parts| parts.method,
            .plain => |name| name,
        };

        const proc: ?*TProcessor = switch (split) {
            .multiplexed => |parts| self.processors.getPtr(parts.service),
            .plain => if (self.defaultProcessor) |*def| def else null,
        };

        const procPtr = proc orelse {
            switch (split) {
                .multiplexed => |parts| {
                    const errMsg = try std.fmt.allocPrint(
                        self.allocator,
                        "Unknown service: {s}",
                        .{parts.service},
                    );
                    defer self.allocator.free(errMsg);
                    return writeProtocolError(self.allocator, in, out, msg, errMsg);
                },
                .plain => return writeProtocolError(
                    self.allocator,
                    in,
                    out,
                    msg,
                    "No default service setup",
                ),
            }
        };

        const storedMsg = TMessageIdentifier{
            .allocator = self.allocator,
            .name = methodName,
            .msgType = msg.msgType,
            .sequenceNumber = msg.sequenceNumber,
        };

        var stored = try TStoredProtocol.init(self.allocator, in.*, storedMsg);
        defer stored.deinit();

        var storedIntf = stored.interface();
        var localProc = @constCast(procPtr).*;
        return localProc.process(&storedIntf, out);
    }

    fn writeProtocolError(
        allocator: Allocator,
        in: *TProtocol,
        out: *TProtocol,
        msg: TMessageIdentifier,
        message: []const u8,
    ) !void {
        try in.skip(allocator, .Struct);
        try in.readMessageEnd();

        var exception = try TApplicationException.initDefault(allocator);
        defer exception.deinit();
        exception.type_ = @intFromEnum(TApplicationExcpetionType.ProtocolError);
        exception.message = try types.String.initFromSlice(allocator, message);
        exception.__isset.message = true;
        exception.__isset.type_ = true;

        const ident = TMessageIdentifier{
            .allocator = undefined,
            .name = msg.name,
            .msgType = .Exception,
            .sequenceNumber = msg.sequenceNumber,
        };
        try out.writeMessageBegin(ident);
        try exception.writeToProtocol(out);
        try out.writeMessageEnd();
        try out.flush();
    }
};

test "splitMultiplexName" {
    const m = try splitMultiplexName("ThriftTest:testString");
    try testing.expect(m == .multiplexed);
    try testing.expectEqualStrings("ThriftTest", m.multiplexed.service);
    try testing.expectEqualStrings("testString", m.multiplexed.method);

    const p = try splitMultiplexName("testString");
    try testing.expect(p == .plain);
    try testing.expectEqualStrings("testString", p.plain);

    try testing.expectError(MultiplexedProcessorError.InvalidMessageName, splitMultiplexName("a:b:c"));
}
