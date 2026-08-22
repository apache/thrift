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
const transport = @import("interface.zig");
const TConfiguration = @import("../lib/configuration.zig").TConfiguration;

const Allocator = mem.Allocator;
const TTransport = transport.TTransport;
const TTransportFactory = transport.TTransportFactory;

pub const DEFAULT_READER_BUFFER_SIZE: usize = 512;

/// Maximum frame payload size after applying both limits.
/// `maxFrameSize` counts payload only; `maxMessageSize` counts payload plus the 4-byte header.
fn effectiveMaxFramePayload(config: *const TConfiguration) i32 {
    return @min(config.maxFrameSize, config.maxMessageSize - 4);
}

pub const FramedTransportError = error{
    NegativeFrameSize,
    OversizedFrame,
    PartialFrameHeader,
};

// This transport owns the underlying transport interface, i.e. manages it's
// lifecycle
pub const TFramedTransport = struct {
    const Self = @This();

    allocator: Allocator,
    underlying: TTransport,
    config: *const TConfiguration,

    underlyingReader: *std.Io.Reader,
    underlyingWriter: *std.Io.Writer,

    readBuffer: []u8,
    frameBuffer: []u8,
    framePos: usize,
    frameLen: usize,

    writeBuffer: std.Io.Writer.Allocating,
    _reader: std.Io.Reader,

    const readerVtable: std.Io.Reader.VTable = .{
        .stream = readerStream,
        .discard = readerDiscard,
    };

    pub fn init(allocator: Allocator, underlying: TTransport, config: *const TConfiguration) !Self {
        const readBuffer = try allocator.alloc(u8, DEFAULT_READER_BUFFER_SIZE);
        errdefer allocator.free(readBuffer);

        var self: Self = .{
            .allocator = allocator,
            .underlying = underlying,
            .config = config,
            .underlyingReader = undefined,
            .underlyingWriter = undefined,
            .readBuffer = readBuffer,
            .frameBuffer = &.{},
            .framePos = 0,
            .frameLen = 0,
            .writeBuffer = .init(allocator),
            ._reader = .{
                .vtable = &readerVtable,
                .buffer = readBuffer,
                .seek = 0,
                .end = 0,
            },
        };

        self.underlyingReader = try self.underlying.reader();
        self.underlyingWriter = try self.underlying.writer();
        return self;
    }

    pub fn deinit(self: *Self) void {
        self.writeBuffer.deinit();
        self.allocator.free(self.readBuffer);
        self.allocator.free(self.frameBuffer);
        self.underlying.deinit();
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        self.underlying.destroy(a);
        a.destroy(self);
    }

    pub fn open(self: *Self) !void {
        return self.underlying.open();
    }

    pub fn close(self: *Self) !void {
        try self.flush();
        return self.underlying.close();
    }

    pub fn isOpen(self: *Self) !bool {
        return self.underlying.isOpen();
    }

    pub fn reader(self: *Self) !*std.Io.Reader {
        return &self._reader;
    }

    pub fn writer(self: *Self) !*std.Io.Writer {
        return &self.writeBuffer.writer;
    }

    pub fn flush(self: *Self) !void {
        const payload = self.writeBuffer.written();
        const max_payload = effectiveMaxFramePayload(self.config);
        if (payload.len > @as(usize, @intCast(max_payload))) return FramedTransportError.OversizedFrame;
        const frame_size = std.math.cast(i32, payload.len) orelse return FramedTransportError.OversizedFrame;

        var header: [4]u8 = undefined;
        std.mem.writeInt(i32, &header, frame_size, .big);
        try self.underlyingWriter.writeAll(&header);
        if (payload.len > 0) {
            try self.underlyingWriter.writeAll(payload);
        }
        try self.underlyingWriter.flush();
        self.writeBuffer.clearRetainingCapacity();
    }

    pub fn interface(self: *Self) TTransport {
        return TTransport.init(self);
    }

    fn readFrame(self: *Self) !void {
        self.framePos = 0;
        self.frameLen = 0;

        const frameSize = self.underlyingReader.takeInt(i32, .big) catch |err| switch (err) {
            error.EndOfStream => return err,
            else => return FramedTransportError.PartialFrameHeader,
        };

        if (frameSize < 0) return FramedTransportError.NegativeFrameSize;
        const frameSizeU: usize = @intCast(frameSize);
        if (frameSize > effectiveMaxFramePayload(self.config)) return FramedTransportError.OversizedFrame;

        if (self.frameBuffer.len < frameSizeU) {
            self.frameBuffer = try self.allocator.realloc(self.frameBuffer, frameSizeU);
        }

        if (frameSizeU > 0) {
            try self.underlyingReader.readSliceAll(self.frameBuffer[0..frameSizeU]);
        }
        self.frameLen = frameSizeU;
    }

    fn streamFromFrame(self: *Self, w: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        const avail = self.frameLen - self.framePos;
        if (avail == 0) return 0;

        var frameReader: std.Io.Reader = .fixed(self.frameBuffer[self.framePos..self.frameLen]);
        return frameReader.stream(w, limit);
    }

    fn readerStream(r: *std.Io.Reader, w: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        const self: *Self = @fieldParentPtr("_reader", r);

        if (self.framePos < self.frameLen) {
            const n = try self.streamFromFrame(w, limit);
            self.framePos += n;
            return n;
        }

        self.readFrame() catch |err| switch (err) {
            error.EndOfStream => return error.EndOfStream,
            else => return error.ReadFailed,
        };

        if (self.frameLen == 0) return 0;

        const n = try self.streamFromFrame(w, limit);
        self.framePos += n;
        return n;
    }

    fn readerDiscard(r: *std.Io.Reader, limit: std.Io.Limit) std.Io.Reader.Error!usize {
        const self: *Self = @fieldParentPtr("_reader", r);

        if (self.framePos < self.frameLen) {
            const avail = self.frameLen - self.framePos;
            const n = @min(avail, limit.toInt() orelse avail);
            self.framePos += n;
            return n;
        }

        self.readFrame() catch |err| switch (err) {
            error.EndOfStream => return error.EndOfStream,
            else => return error.ReadFailed,
        };

        if (self.frameLen == 0) return 0;

        const avail = self.frameLen - self.framePos;
        const n = @min(avail, limit.toInt() orelse avail);
        self.framePos += n;
        return n;
    }
};

pub const TFramedTransportFactory = struct {
    const Self = @This();

    inner: ?*TTransportFactory = null,
    config: *const TConfiguration,

    pub fn init(config: *const TConfiguration) Self {
        return .{ .config = config };
    }

    pub fn initChained(inner: *TTransportFactory, config: *const TConfiguration) Self {
        return .{ .inner = inner, .config = config };
    }

    pub fn deinit(_: *Self) void {}

    pub fn getTransport(self: *Self, allocator: Allocator, base: *TTransport) !TTransport {
        const underlying = if (self.inner) |factory|
            try factory.getTransport(allocator, base)
        else
            base.*;

        const framed = try allocator.create(TFramedTransport);
        framed.* = try TFramedTransport.init(allocator, underlying, self.config);
        return framed.interface();
    }

    pub fn interface(self: *Self) TTransportFactory {
        return TTransportFactory.init(self);
    }
};

const t = std.testing;
const TTestingMemoryTransport = @import("../testing/memory_transport.zig").TTestingMemoryTransport;

test "framed transport round trip" {
    var config = TConfiguration.default;

    const memory = try t.allocator.create(TTestingMemoryTransport);
    memory.* = try TTestingMemoryTransport.init(t.allocator, t.io);
    defer {
        memory.deinit();
        t.allocator.destroy(memory);
    }

    const base = memory.interface();

    var factory = TFramedTransportFactory.init(&config);
    var framedHandle = try factory.getTransport(t.allocator, &base);
    defer {
        framedHandle.deinit();
        framedHandle.destroy(t.allocator);
    }

    const payload = "hello framed thrift";
    {
        const writer = try framedHandle.writer();
        try writer.writeAll(payload);
        try framedHandle.flush();
    }

    var out: [64]u8 = undefined;
    const reader = try framedHandle.reader();
    const n = try reader.readSliceShort(&out);
    try t.expectEqual(@as(usize, payload.len), n);
    try t.expectEqualStrings(payload, out[0..n]);
}

test "framed transport rejects oversized write buffer on flush" {
    var config = TConfiguration.default;
    config.maxFrameSize = 8;

    const memory = try t.allocator.create(TTestingMemoryTransport);
    memory.* = try TTestingMemoryTransport.init(t.allocator, t.io);
    defer {
        memory.deinit();
        t.allocator.destroy(memory);
    }

    const base = memory.interface();

    var factory = TFramedTransportFactory.init(&config);
    var framedHandle = try factory.getTransport(t.allocator, &base);
    defer {
        framedHandle.deinit();
        framedHandle.destroy(t.allocator);
    }

    const writer = try framedHandle.writer();
    try writer.writeAll("0123456789");
    try t.expectError(FramedTransportError.OversizedFrame, framedHandle.flush());
}

test "framed transport rejects negative frame size" {
    var config = TConfiguration.default;

    const memory = try t.allocator.create(TTestingMemoryTransport);
    memory.* = try TTestingMemoryTransport.init(t.allocator, t.io);
    defer {
        memory.deinit();
        t.allocator.destroy(memory);
    }

    var header: [4]u8 = undefined;
    std.mem.writeInt(i32, &header, -1, .big);

    const writer = try memory.writer();
    try writer.writeAll(&header);
    try writer.flush();

    const base = memory.interface();
    var factory = TFramedTransportFactory.init(&config);
    var framed = try factory.getTransport(t.allocator, &base);
    defer {
        framed.deinit();
        framed.destroy(t.allocator);
    }

    const reader = try framed.reader();
    var buf: [1]u8 = undefined;
    try t.expectError(error.ReadFailed, reader.readSliceShort(&buf));
}

test "framed transport rejects frame exceeding max message size" {
    var config = TConfiguration.default;
    config.maxMessageSize = 10;
    config.maxFrameSize = 100;

    const memory = try t.allocator.create(TTestingMemoryTransport);
    memory.* = try TTestingMemoryTransport.init(t.allocator, t.io);
    defer {
        memory.deinit();
        t.allocator.destroy(memory);
    }

    // 8-byte payload -> message size 12 exceeds maxMessageSize of 10
    var header: [4]u8 = undefined;
    std.mem.writeInt(i32, &header, 8, .big);
    const writer = try memory.writer();
    try writer.writeAll(&header);
    try writer.writeAll("01234567");
    try writer.flush();

    const base = memory.interface();
    var factory = TFramedTransportFactory.init(&config);
    var framed = try factory.getTransport(t.allocator, &base);
    defer {
        framed.deinit();
        framed.destroy(t.allocator);
    }

    const reader = try framed.reader();
    var buf: [1]u8 = undefined;
    try t.expectError(error.ReadFailed, reader.readSliceShort(&buf));
}

test "effectiveMaxFramePayload uses the tighter limit" {
    var config = TConfiguration.default;
    try t.expectEqual(@min(TConfiguration.DEFAULT_MAX_FRAME_SIZE, TConfiguration.DEFAULT_MAX_MESSAGE_SIZE - 4), effectiveMaxFramePayload(&config));

    config.maxMessageSize = 10;
    config.maxFrameSize = 100;
    try t.expectEqual(@as(i32, 6), effectiveMaxFramePayload(&config));

    config.maxMessageSize = 100 * 1024 * 1024;
    config.maxFrameSize = 8;
    try t.expectEqual(@as(i32, 8), effectiveMaxFramePayload(&config));
}
