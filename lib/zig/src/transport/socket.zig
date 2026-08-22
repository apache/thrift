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

const Allocator = mem.Allocator;
const TTransport = transport.TTransport;

pub const TSocket = struct {
    const Self = @This();

    allocator: Allocator,
    io: std.Io,

    stream: net.Stream,

    readBuffer: []u8,
    _reader: net.Stream.Reader,
    writeBuffer: []u8,
    _writer: net.Stream.Writer,

    pub fn initFromStream(allocator: Allocator, io: std.Io, stream: net.Stream) !Self {
        const readBuffer = try allocator.alloc(u8, 1000);
        errdefer allocator.free(readBuffer);
        const writeBuffer = try allocator.alloc(u8, 1000);
        errdefer allocator.free(writeBuffer);

        return .{
            .allocator = allocator,
            .io = io,
            .stream = stream,
            .readBuffer = readBuffer,
            ._reader = stream.reader(io, readBuffer),
            .writeBuffer = writeBuffer,
            ._writer = stream.writer(io, writeBuffer),
        };
    }

    pub fn initFromAddress(allocator: Allocator, io: std.Io, address: net.IpAddress) !Self {
        const stream = try address.connect(io, .{ .mode = .stream });

        const readBuffer = try allocator.alloc(u8, 1000);
        errdefer allocator.free(readBuffer);
        const writeBuffer = try allocator.alloc(u8, 1000);
        errdefer allocator.free(writeBuffer);

        return .{
            .allocator = allocator,
            .io = io,
            .stream = stream,
            .readBuffer = readBuffer,
            ._reader = stream.reader(io, readBuffer),
            .writeBuffer = writeBuffer,
            ._writer = stream.writer(io, writeBuffer),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stream.shutdown(self.io, .both) catch {
            std.debug.panic("Failed to shutdown socket: {}", .{self.stream.socket.handle});
        };
        self.stream.close(self.io);
        self.allocator.free(self.readBuffer);
        self.allocator.free(self.writeBuffer);
    }

    pub fn open(_: *Self) !void {}

    pub fn close(_: *Self) !void {}

    pub fn isOpen(_: *Self) !bool {
        return true;
    }

    pub fn reader(self: *Self) !*std.Io.Reader {
        return &self._reader.interface;
    }

    pub fn writer(self: *Self) !*std.Io.Writer {
        return &self._writer.interface;
    }

    pub fn flush(self: *Self) !void {
        try self._writer.interface.flush();
    }

    pub fn interface(self: *Self) TTransport {
        return TTransport.init(self);
    }
};
