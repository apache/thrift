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

const Allocator = std.mem.Allocator;
const TConfiguration = thrift.TConfiguration;
const TProtocolFactory = thrift.TProtocolFactory;

pub const ProtocolConfigError = error{
    UnrecognisedProtocol,
};

pub const ProtocolMode = struct {
    wireProtocol: []const u8,
    multiplex: bool,
};

pub fn parseProtocolMode(protocolArg: ?[]const u8) ProtocolConfigError!ProtocolMode {
    const arg = protocolArg orelse "binary";
    if (std.mem.eql(u8, arg, "binary")) return .{ .wireProtocol = "binary", .multiplex = false };
    if (std.mem.eql(u8, arg, "compact")) return .{ .wireProtocol = "compact", .multiplex = false };
    if (std.mem.eql(u8, arg, "multi")) return .{ .wireProtocol = "binary", .multiplex = true };
    if (std.mem.eql(u8, arg, "multic")) return .{ .wireProtocol = "compact", .multiplex = true };
    return error.UnrecognisedProtocol;
}

pub fn createProtocolFactory(allocator: Allocator, config: *const TConfiguration, protocolArg: ?[]const u8) (ProtocolConfigError || Allocator.Error)!TProtocolFactory {
    const mode = try parseProtocolMode(protocolArg);
    if (std.mem.eql(u8, mode.wireProtocol, "binary")) {
        const bin = try allocator.create(thrift.TBinaryProtocolFactory);
        bin.* = thrift.TBinaryProtocolFactory.init(config);
        return bin.interface();
    }
    if (std.mem.eql(u8, mode.wireProtocol, "compact")) {
        const compact = try allocator.create(thrift.TCompactProtocolFactory);
        compact.* = thrift.TCompactProtocolFactory.init(config);
        return compact.interface();
    }
    return error.UnrecognisedProtocol;
}

pub fn destroyProtocolFactory(factory: *TProtocolFactory, allocator: Allocator) void {
    factory.destroy(allocator);
}
