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
const transport = @import("../transport/interface.zig");

const Allocator = mem.Allocator;
const TTransport = transport.TTransport;

// For testing purposes supports: write once, read once
pub const TTestingMemoryTransport = struct {
    const Self = @This();

    allocator: Allocator,
    io: std.Io,

    buffer: std.Io.Writer.Allocating,
    reader_: std.Io.Reader,

    pub fn init(allocator: Allocator, io: std.Io) !Self {
        return .{
            .allocator = allocator,
            .io = io,
            .buffer = .init(allocator),
            .reader_ = .fixed(&[_]u8{}),
        };
    }

    pub fn deinit(self: *Self) void {
        self.buffer.deinit();
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        a.destroy(self);
    }

    pub fn open(_: *Self) !void {}

    pub fn close(_: *Self) !void {}

    pub fn isOpen(_: *Self) !bool {
        return true;
    }

    pub fn reader(self: *Self) !*std.Io.Reader {
        const prev_seek = self.reader_.seek;
        self.reader_ = .fixed(self.buffer.written());
        self.reader_.seek = @min(prev_seek, self.reader_.end);
        return &self.reader_;
    }

    pub fn writer(self: *Self) !*std.Io.Writer {
        return &self.buffer.writer;
    }

    pub fn flush(self: *Self) !void {
        try self.buffer.writer.flush();
    }

    pub fn remainingReadBytes(self: *Self) !?u64 {
        const written = self.buffer.written();
        if (written.len == 0) return null;
        const seek = @min(self.reader_.seek, written.len);
        return written.len - seek;
    }

    pub fn interface(self: *Self) TTransport {
        return TTransport.init(self);
    }

    pub fn expectFullyConsumed(self: *Self) !void {
        const testing = std.testing;
        try testing.expectEqual(self.buffer.written().len, self.reader_.seek);
    }
};

test "remainingReadBytes tracks unread written payload" {
    const testing = std.testing;
    var testing_transport = try TTestingMemoryTransport.init(testing.allocator, testing.io);
    defer testing_transport.deinit();

    try testing.expectEqual(@as(?u64, null), try testing_transport.remainingReadBytes());

    const writer = try testing_transport.writer();
    try writer.writeAll("abcdef");
    try testing_transport.flush();
    try testing.expectEqual(@as(?u64, 6), try testing_transport.remainingReadBytes());

    _ = try testing_transport.reader();
    _ = try testing_transport.reader_.take(2);
    try testing.expectEqual(@as(?u64, 4), try testing_transport.remainingReadBytes());
}
