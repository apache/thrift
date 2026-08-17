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
const protocol = @import("../protocol/protocol.zig");
const app_exception = @import("application_exception.zig");
const types = @import("types.zig");

const TProtocol = protocol.TProtocol;
const Allocator = std.mem.Allocator;
const TApplicationException = app_exception.TApplicationException;

pub fn ServiceCallResult(comptime ResultType: type, comptime ErrorType: type, comptime name: []const u8) type {
    return union(enum) {
        result: ResultType,
        err: ErrorType,

        pub fn success(val: ResultType) @This() {
            return .{ .result = val };
        }

        pub fn fail(val: ErrorType) @This() {
            return .{ .err = val };
        }

        pub fn deinit(self: *@This()) void {
            return switch (self.*) {
                .result => |*res| {
                    switch (@typeInfo(ResultType)) {
                        .void, .@"enum", .int, .float, .bool => return,
                        else => {},
                    }
                    if (ResultType == types.UUID) {
                        return;
                    }
                    res.deinit();
                },
                .err => |*e| {
                    if (ErrorType == void) {
                        return;
                    }
                    e.deinit();
                },
            };
        }

        fn assignSuccess(slot: *?@This(), new_result: ResultType) void {
            const incoming: @This() = .{ .result = new_result };
            if (slot.*) |existing| {
                switch (existing) {
                    .err => {
                        var discard = incoming;
                        discard.deinit();
                        return;
                    },
                    .result => {
                        var old = existing;
                        slot.* = null;
                        old.deinit();
                    },
                }
            }
            slot.* = incoming;
        }

        fn assignError(slot: *?@This(), new_err: ErrorType) void {
            if (slot.*) |*old| {
                old.deinit();
            }
            slot.* = .{ .err = new_err };
        }

        pub fn readFromProtocol(allocator: Allocator, iProt: *TProtocol) !@This() {
            var structIdent = try iProt.readStructBegin(allocator);
            defer structIdent.deinit();

            var res: ?@This() = null;
            errdefer if (res) |*r| r.deinit();
            while (true) {
                var fieldId = try iProt.readFieldBegin(allocator);
                defer fieldId.deinit();

                if (fieldId.fieldType == .Stop) {
                    break;
                }

                switch (fieldId.id.?) {
                    0 => {
                        if ((fieldId.fieldType == protocol.typeToTType(ResultType)) or
                            // String and Binary share the same in wire value,
                            // I've checked multiple implementations and those
                            // check if TType == String for Binary fields. I
                            // guess that's fine since they share the same in
                            // wire layout (at least in binary and compact
                            // protocols)
                            (fieldId.fieldType == .String and ResultType == types.BinaryBytes))
                        {
                            const new_result = try protocol.readFromProtocol(ResultType, allocator, iProt);
                            assignSuccess(&res, new_result);
                        } else {
                            try iProt.skip(allocator, fieldId.fieldType);
                        }
                    },
                    else => {
                        if (ErrorType == void) {
                            try iProt.skip(allocator, fieldId.fieldType);
                        } else {
                            const new_err = try ErrorType.readFromProtocol(allocator, fieldId.id.?, iProt);
                            assignError(&res, new_err);
                        }
                    },
                }
                try iProt.readFieldEnd();
            }
            try iProt.readStructEnd();

            if (res) |r| {
                return r;
            } else {
                if (ResultType == void) {
                    return .success({});
                }
                return error.EmptyResultStruct;
            }
        }

        pub fn writeToProtocol(self: *@This(), oProt: *TProtocol) !void {
            if (self.* == .err and ErrorType == void) {
                return error.UnexpectedErrorResult;
            }

            try oProt.writeStructBegin(.{ .allocator = undefined, .name = name });
            switch (self.*) {
                .result => |res| {
                    if (ResultType != void) {
                        try oProt.writeFieldBegin(.{ .allocator = undefined, .id = 0, .name = "success", .fieldType = protocol.typeToTType(ResultType) });
                        try protocol.writeToProtocol(ResultType, res, oProt);
                        try oProt.writeFieldEnd();
                    }
                },
                .err => |*e| {
                    if (comptime ErrorType != void) {
                        try e.writeToProtocol(oProt);
                    }
                },
            }
            try oProt.writeFieldStop();
            try oProt.writeStructEnd();
        }
    };
}

const testing = std.testing;
const binary_protocol = @import("../protocol/binary_protocol.zig");
const configuration = @import("configuration.zig");
const memory_transport = @import("../testing/memory_transport.zig");

test "ServiceCallResult writeToProtocol rejects err when no exceptions declared" {
    const VoidResult = ServiceCallResult(void, void, "VoidResult");

    var config = configuration.TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer out_prot.deinit();
    var out_prot_itf = out_prot.interface();

    var value = VoidResult.fail({});
    try testing.expectError(error.UnexpectedErrorResult, value.writeToProtocol(&out_prot_itf));
}

test "ServiceCallResult readFromProtocol skips unknown fields when no exceptions declared" {
    const VoidResult = ServiceCallResult(void, void, "VoidResult");

    var config = configuration.TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeStructBegin(.{ .allocator = undefined, .name = "VoidResult" });
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 1, .name = "unknownEx", .fieldType = .String });
    var payload = try types.String.initFromSlice(testing.allocator, "unexpected");
    defer payload.deinit();
    try out_prot.writeString(payload);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldStop();
    try out_prot.writeStructEnd();
    try out_prot.flush();

    var in_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer in_prot.deinit();
    var in_prot_itf = in_prot.interface();

    const decoded = try VoidResult.readFromProtocol(testing.allocator, &in_prot_itf);
    try testing.expect(decoded == .result);
}

test "ServiceCallResult readFromProtocol duplicate success field keeps last value without leaking" {
    const StringResult = ServiceCallResult(types.String, void, "StringResult");

    var config = configuration.TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeStructBegin(.{ .allocator = undefined, .name = "StringResult" });
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 0, .name = "success", .fieldType = .String });
    var first = try types.String.initFromSlice(testing.allocator, "first");
    defer first.deinit();
    try out_prot.writeString(first);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 0, .name = "success", .fieldType = .String });
    var last = try types.String.initFromSlice(testing.allocator, "last");
    defer last.deinit();
    try out_prot.writeString(last);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldStop();
    try out_prot.writeStructEnd();
    try out_prot.flush();

    var in_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer in_prot.deinit();
    var in_prot_itf = in_prot.interface();

    var decoded = try StringResult.readFromProtocol(testing.allocator, &in_prot_itf);
    defer decoded.deinit();
    try testing.expect(decoded == .result);
    try testing.expectEqualStrings("last", decoded.result.contents);
}

const TestException = union(enum) {
    ex: types.String,

    pub fn deinit(self: *@This()) void {
        switch (self.*) {
            inline else => |*v| v.deinit(),
        }
    }

    pub fn readFromProtocol(allocator: Allocator, field_id: i16, i_prot: *TProtocol) !@This() {
        switch (field_id) {
            1 => return .{ .ex = try protocol.readFromProtocol(types.String, allocator, i_prot) },
            else => return error.UnknownField,
        }
    }

    pub fn writeToProtocol(self: *const @This(), o_prot: *TProtocol) !void {
        switch (self.*) {
            .ex => |v| {
                try o_prot.writeFieldBegin(.{ .allocator = undefined, .id = 1, .name = "ex", .fieldType = .String });
                try protocol.writeToProtocol(types.String, v, o_prot);
                try o_prot.writeFieldEnd();
            },
        }
    }
};

test "ServiceCallResult readFromProtocol prefers exception over success" {
    const StringResult = ServiceCallResult(types.String, TestException, "StringResult");

    var config = configuration.TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeStructBegin(.{ .allocator = undefined, .name = "StringResult" });
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 0, .name = "success", .fieldType = .String });
    var success = try types.String.initFromSlice(testing.allocator, "ok");
    defer success.deinit();
    try out_prot.writeString(success);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 1, .name = "ex", .fieldType = .String });
    var ex = try types.String.initFromSlice(testing.allocator, "boom");
    defer ex.deinit();
    try out_prot.writeString(ex);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldStop();
    try out_prot.writeStructEnd();
    try out_prot.flush();

    var in_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer in_prot.deinit();
    var in_prot_itf = in_prot.interface();

    var decoded = try StringResult.readFromProtocol(testing.allocator, &in_prot_itf);
    defer decoded.deinit();
    try testing.expect(decoded == .err);
    try testing.expectEqualStrings("boom", decoded.err.ex.contents);
}

test "ServiceCallResult readFromProtocol keeps exception when success arrives later" {
    const StringResult = ServiceCallResult(types.String, TestException, "StringResult");

    var config = configuration.TConfiguration.default;
    var transport = try memory_transport.TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer transport.deinit();
    var transport_itf = transport.interface();

    var out_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer out_prot.deinit();

    try out_prot.writeStructBegin(.{ .allocator = undefined, .name = "StringResult" });
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 1, .name = "ex", .fieldType = .String });
    var ex = try types.String.initFromSlice(testing.allocator, "boom");
    defer ex.deinit();
    try out_prot.writeString(ex);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldBegin(.{ .allocator = undefined, .id = 0, .name = "success", .fieldType = .String });
    var success = try types.String.initFromSlice(testing.allocator, "ok");
    defer success.deinit();
    try out_prot.writeString(success);
    try out_prot.writeFieldEnd();
    try out_prot.writeFieldStop();
    try out_prot.writeStructEnd();
    try out_prot.flush();

    var in_prot = try binary_protocol.TBinaryProtocol.init(testing.allocator, &transport_itf, &config);
    defer in_prot.deinit();
    var in_prot_itf = in_prot.interface();

    var decoded = try StringResult.readFromProtocol(testing.allocator, &in_prot_itf);
    defer decoded.deinit();
    try testing.expect(decoded == .err);
    try testing.expectEqualStrings("boom", decoded.err.ex.contents);
}
