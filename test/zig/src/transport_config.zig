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
const TTransport = thrift.TTransport;
const TTransportFactory = thrift.TTransportFactory;

pub const TransportConfigError = error{
    UnrecognisedTransport,
};

fn parseTransportArg(transportArg: ?[]const u8) TransportConfigError![]const u8 {
    const arg = transportArg orelse "buffered";
    if (std.mem.eql(u8, arg, "buffered") or std.mem.eql(u8, arg, "framed")) {
        return arg;
    }
    return error.UnrecognisedTransport;
}

pub fn createTransportFactory(allocator: Allocator, config: *const TConfiguration, transportArg: ?[]const u8) (TransportConfigError || Allocator.Error)!?*TTransportFactory {
    const arg = try parseTransportArg(transportArg);
    if (std.mem.eql(u8, arg, "buffered")) {
        return null;
    }

    const impl = try allocator.create(thrift.TFramedTransportFactory);
    impl.* = thrift.TFramedTransportFactory.init(config);
    errdefer {
        impl.deinit();
        allocator.destroy(impl);
    }

    const intf = try allocator.create(TTransportFactory);
    intf.* = impl.interface();
    return intf;
}

pub fn destroyTransportFactory(factory: *TTransportFactory, allocator: Allocator) void {
    factory.destroy(allocator);
    allocator.destroy(factory);
}

pub fn wrapTransport(allocator: Allocator, config: *const TConfiguration, transportArg: ?[]const u8, base: *TTransport) anyerror!TTransport {
    const arg = try parseTransportArg(transportArg);
    if (std.mem.eql(u8, arg, "buffered")) {
        return base.*;
    }

    var factory = thrift.TFramedTransportFactory.init(config);
    return try factory.getTransport(allocator, base);
}
