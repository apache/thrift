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

pub const TSimpleServer = simple_server.TSimpleServer;

pub fn handleProcessResult(allocator: Allocator, msgIdentifier: protocol.TMessageIdentifier, res: anyerror!void, oProt: *protocol.TProtocol) !void {
    res catch |err| {
        var exception = lib.TApplicationException{
            .allocator = allocator,
            .type_ = @intFromEnum(lib.application_exception.TApplicationExcpetionType.Unknown),
            .message = .{ .allocator = allocator, .contents = try std.fmt.allocPrint(allocator, "Unknown Error: {}", .{err}) },
        };
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
