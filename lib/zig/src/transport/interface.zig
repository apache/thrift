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

const Allocator = mem.Allocator;

// Transport Interface
pub const TTransport = struct {
    const Self = @This();

    ptr: *anyopaque,

    deinitFn: *const fn (ptr: *anyopaque) void,
    destroyFn: *const fn (ptr: *anyopaque, a: Allocator) void,

    openFn: *const fn (ptr: *anyopaque) anyerror!void,
    closeFn: *const fn (ptr: *anyopaque) anyerror!void,
    isOpenFn: *const fn (ptr: *anyopaque) anyerror!bool,
    writerFn: *const fn (ptr: *anyopaque) anyerror!*std.Io.Writer,
    readerFn: *const fn (ptr: *anyopaque) anyerror!*std.Io.Reader,
    flushFn: *const fn (ptr: *anyopaque) anyerror!void,

    pub fn init(ptr: anytype) Self {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.deinit(self);
            }
            pub fn destroy(pointer: *anyopaque, a: Allocator) void {
                const self: T = @ptrCast(@alignCast(pointer));
                a.destroy(self);
            }
            pub fn open(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.open(self);
            }
            pub fn close(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.close(self);
            }
            pub fn isOpen(pointer: *anyopaque) anyerror!bool {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.isOpen(self);
            }
            pub fn reader(pointer: *anyopaque) !*std.Io.Reader {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.reader(self);
            }
            pub fn writer(pointer: *anyopaque) !*std.Io.Writer {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.writer(self);
            }
            pub fn flush(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.flush(self);
            }
        };

        return .{
            .ptr = ptr,
            .deinitFn = gen.deinit,
            .destroyFn = gen.destroy,
            .openFn = gen.open,
            .closeFn = gen.close,
            .isOpenFn = gen.isOpen,
            .readerFn = gen.reader,
            .writerFn = gen.writer,
            .flushFn = gen.flush,
        };
    }
    pub fn deinit(self: *Self) void {
        return self.deinitFn(self.ptr);
    }
    pub fn destroy(self: *Self, a: Allocator) void {
        return self.destroyFn(self.ptr, a);
    }
    pub fn open(self: *Self) !void {
        return self.openFn(self.ptr);
    }
    pub fn close(self: *Self) !void {
        return self.closeFn(self.ptr);
    }
    pub fn isOpen(self: *Self) !bool {
        return self.isOpenFn(self.ptr);
    }
    pub fn reader(self: *Self) !*std.Io.Reader {
        return self.readerFn(self.ptr);
    }
    pub fn writer(self: *Self) !*std.Io.Writer {
        return self.writerFn(self.ptr);
    }
    pub fn flush(self: *Self) !void {
        return self.flushFn(self.ptr);
    }
};

// Server Transport Interface
pub const TServerTransport = struct {
    const Self = @This();

    ptr: *anyopaque,

    deinitFn: *const fn (ptr: *anyopaque) void,

    openFn: *const fn (ptr: *anyopaque) anyerror!void,
    closeFn: *const fn (ptr: *anyopaque) anyerror!void,
    listenFn: *const fn (ptr: *anyopaque) anyerror!void,
    acceptFn: *const fn (ptr: *anyopaque, alloc: Allocator) anyerror!TTransport,

    pub fn init(ptr: anytype) Self {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.deinit(self);
            }
            pub fn open(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.open(self);
            }
            pub fn close(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.close(self);
            }
            pub fn listen(pointer: *anyopaque) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.listen(self);
            }
            pub fn accept(pointer: *anyopaque, a: Allocator) anyerror!TTransport {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.accept(self, a);
            }
        };

        return .{
            .ptr = ptr,
            .deinitFn = gen.deinit,
            .openFn = gen.open,
            .closeFn = gen.close,
            .listenFn = gen.listen,
            .acceptFn = gen.accept,
        };
    }

    pub fn deinit(self: *Self) void {
        return self.deinitFn(self.ptr);
    }
    pub fn open(self: *Self) anyerror!void {
        return self.openFn(self.ptr);
    }
    pub fn close(self: *Self) anyerror!void {
        return self.closeFn(self.ptr);
    }
    pub fn listen(self: *Self) anyerror!void {
        return self.listenFn(self.ptr);
    }
    pub fn accept(self: *Self, allocator: Allocator) anyerror!TTransport {
        return self.acceptFn(self.ptr, allocator);
    }
};

pub const TTransportFactory = struct {
    const Self = @This();

    ptr: *anyopaque,

    deinitFn: *const fn (ptr: *anyopaque) void,
    destroyFn: *const fn (ptr: *anyopaque, a: Allocator) void,
    getTransportFn: *const fn (ptr: *anyopaque, allocator: Allocator, transport: *TTransport) anyerror!TTransport,

    pub fn init(ptr: anytype) Self {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn deinit(pointer: *anyopaque) void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.deinit(self);
            }
            pub fn destroy(pointer: *anyopaque, a: Allocator) void {
                const self: T = @ptrCast(@alignCast(pointer));
                a.destroy(self);
            }
            pub fn getTransport(pointer: *anyopaque, allocator: Allocator, transport: *TTransport) anyerror!TTransport {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.pointer.child.getTransport(self, allocator, transport);
            }
        };

        return .{
            .ptr = ptr,
            .deinitFn = gen.deinit,
            .destroyFn = gen.destroy,
            .getTransportFn = gen.getTransport,
        };
    }

    pub fn deinit(self: *Self) void {
        return self.deinitFn(self.ptr);
    }

    pub fn destroy(self: *Self, a: Allocator) void {
        return self.destroyFn(self.ptr, a);
    }

    pub fn getTransport(self: *Self, allocator: Allocator, transport: *TTransport) anyerror!TTransport {
        return self.getTransportFn(self.ptr, allocator, transport);
    }
};
