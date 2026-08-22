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
const root = @import("../root.zig");
const protocolMod = @import("../protocol/mod.zig");
const processorMod = @import("../processor/mod.zig");

const Allocator = std.mem.Allocator;
const TServerTransport = root.TServerTransport;
const TTransportFactory = root.TTransportFactory;
const TProcessor = processorMod.TProcessor;
const TProtocolFactory = protocolMod.TProtocolFactory;

pub const TSimpleServer = struct {
    const Self = @This();

    allocator: Allocator,

    serverTransport: *TServerTransport,
    processor: *TProcessor,
    protocolFactory: *TProtocolFactory,
    transportFactory: ?*TTransportFactory,

    pub fn init(
        allocator: Allocator,
        serverTransport: *TServerTransport,
        processor: *TProcessor,
        protocolFactory: *TProtocolFactory,
        transportFactory: ?*TTransportFactory,
    ) Self {
        return .{
            .allocator = allocator,
            .serverTransport = serverTransport,
            .processor = processor,
            .protocolFactory = protocolFactory,
            .transportFactory = transportFactory,
        };
    }

    pub fn serve(self: *Self) !void {
        try self.serverTransport.listen();

        while (true) {
            var client = try self.serverTransport.accept(self.allocator);
            defer {
                client.deinit();
                client.destroy(self.allocator);
            }

            if (self.transportFactory) |factory| {
                client = try factory.getTransport(self.allocator, &client);
            }

            var protocol = try self.protocolFactory.getProtocol(self.allocator, &client);
            defer {
                protocol.deinit();
                protocol.destroy(self.allocator);
            }

            while (true) {
                self.processor.process(&protocol, &protocol) catch break;
            }
        }
    }
};
