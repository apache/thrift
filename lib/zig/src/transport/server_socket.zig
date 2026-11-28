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
const mem = std.mem;
const net = std.Io.net;
const transport = @import("interface.zig");
const socketLib = @import("socket.zig");

const Allocator = mem.Allocator;
const TTransport = transport.TTransport;
const TServerTransport = transport.TServerTransport;
const TSocket = socketLib.TSocket;

pub const TServerSocket = struct {
    const Self = @This();

    io: std.Io,

    server: net.Server,

    pub fn init(io: std.Io, address: net.IpAddress) !Self {
        return .{
            .io = io,
            .server = try address.listen(io, .{}),
        };
    }

    pub fn deinit(self: *Self) void {
        self.server.deinit(self.io);
    }

    pub fn open(self: *Self) !void {
        _ = self;
    }

    pub fn close(self: *Self) !void {
        _ = self;
    }

    pub fn listen(self: *Self) !void {
        _ = self;
    }

    pub fn accept(self: *Self, allocator: Allocator) !TTransport {
        const conn = try self.server.accept(self.io);

        const socket = try allocator.create(TSocket);
        socket.* = try .initFromStream(allocator, self.io, conn);
        errdefer {
            socket.deinit();
            allocator.destroy(socket);
        }

        return socket.interface();
    }

    pub fn interface(self: *Self) TServerTransport {
        return TServerTransport.init(self);
    }
};
