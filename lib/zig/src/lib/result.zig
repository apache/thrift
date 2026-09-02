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

        pub fn get(self: *@This()) !ResultType {
            switch (self.*) {
                .result => |res| {
                    return res.result_value;
                },
                .err => return error.ErrorResult,
            }
        }

        pub fn unwrapError(self: *@This()) ErrorType {
            switch (self.*) {
                .err => |err| {
                    return err;
                },
                .result => unreachable,
            }
        }

        pub fn readFromProtocol(allocator: Allocator, iProt: *TProtocol) !@This() {
            var structIdent = try iProt.readStructBegin(allocator);
            defer structIdent.deinit();

            var res: ?@This() = null;
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
                            res = .{
                                .result = try protocol.readFromProtocol(ResultType, allocator, iProt),
                            };
                        } else {
                            try iProt.skip(allocator, fieldId.fieldType);
                        }
                    },
                    else => {
                        res = e: {
                            if (ErrorType == void) {
                                try iProt.skip(allocator, fieldId.fieldType);
                                break :e .{ .err = {} };
                            }
                            break :e .{
                                .err = try ErrorType.readFromProtocol(allocator, fieldId.id.?, iProt),
                            };
                        };
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
            try oProt.writeStructBegin(.{ .allocator = undefined, .name = name });
            write: switch (self.*) {
                .result => |res| {
                    if (ResultType == void) {
                        break :write;
                    }
                    try oProt.writeFieldBegin(.{ .allocator = undefined, .id = 0, .name = "success", .fieldType = protocol.typeToTType(ResultType) });
                    try protocol.writeToProtocol(ResultType, res, oProt);
                    try oProt.writeFieldEnd();
                },
                .err => |*e| {
                    if (ErrorType == void) {
                        return;
                    } else {
                        try e.writeToProtocol(oProt);
                    }
                },
            }
            try oProt.writeFieldStop();
            try oProt.writeStructEnd();
        }
    };
}

// DEPRECATED
pub fn ClientResult(comptime ResultType: type) type {
    const allFields = @typeInfo(ResultType).@"union".fields;

    var resultField: ?std.builtin.Type.UnionField = null;
    var errorField: ?std.builtin.Type.UnionField = null;

    inline for (allFields) |field| {
        if (std.mem.eql(u8, field.name, "result")) {
            resultField = field;
        } else if (std.mem.eql(u8, field.name, "err")) {
            errorField = field;
        }
    }

    const NestedResultType = resultField.?.type;
    const NestedErrorType = errorField.?.type;

    const ErrorType = comptime errorDef: {
        switch (@typeInfo(NestedErrorType)) {
            .@"union" => |u| {
                const errorUnionFields = u.fields;
                var fieldNames: [errorUnionFields.len + 1][]const u8 = undefined;
                for (errorUnionFields, 0..) |f, i| {
                    fieldNames[i] = f.name;
                }
                fieldNames[errorUnionFields.len] = "TApplicationException";

                var fieldValues: [errorUnionFields.len + 1]u32 = undefined;
                for (0..errorUnionFields.len) |i| {
                    fieldValues[i] = i;
                }
                fieldValues[errorUnionFields.len] = errorUnionFields.len;

                var fieldTypes: [errorUnionFields.len + 1]type = undefined;
                for (errorUnionFields, 0..) |f, i| {
                    fieldTypes[i] = f.type;
                }
                fieldTypes[errorUnionFields.len] = TApplicationException;

                break :errorDef @Union(
                    .auto,
                    @Enum(
                        u32,
                        .exhaustive,
                        &fieldNames,
                        &fieldValues,
                    ),
                    &fieldNames,
                    &fieldTypes,
                    &@splat(.{}),
                );
            },
            .void => {
                break :errorDef @Union(
                    .auto,
                    @Enum(
                        u32,
                        .exhaustive,
                        &.{"TApplicationException"},
                        &.{0},
                    ),
                    &.{"TApplicationException"},
                    &.{TApplicationException},
                    &@splat(.{}),
                );
            },
            else => @compileError("Error type of ResultType must be a union or void, is " ++ @typeName(NestedResultType)),
        }
    };

    return comptime union(enum) {
        result: NestedResultType,
        err: ErrorType,

        pub fn init(val: ResultType) !@This() {
            switch (val) {
                .result => |res| {
                    return .{ .result = res };
                },
                .err => |e| {
                    if (NestedErrorType == void) {
                        unreachable;
                    }
                    return .{ .err = @unionInit(ErrorType, @tagName(e), @field(e, @tagName(e))) };
                },
            }
        }

        pub fn deinit(self: *@This()) void {
            return switch (self.*) {
                .result => |*res| {
                    switch (@typeInfo(NestedResultType)) {
                        .@"union", .@"struct" => {
                            res.deinit();
                        },
                        else => {},
                    }
                },
                .err => |*e| {
                    switch (e.*) {
                        inline else => |*v| {
                            v.deinit();
                        },
                    }
                },
            };
        }

        pub fn get(self: *const @This()) !NestedResultType {
            return ret: switch (self.*) {
                .result => |res| {
                    if (NestedResultType == void) {
                        break :ret;
                    }
                    break :ret res;
                },
                .err => break :ret error.ErrorResult,
            };
        }

        pub fn unwrapError(self: *const @This()) ErrorType {
            switch (self.*) {
                .err => |err| {
                    return err;
                },
                .result => unreachable,
            }
        }
    };
}
