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
const protocol = @import("../protocol/mod.zig");

const mem = std.mem;

const Allocator = mem.Allocator;

pub const TProcessor = struct {
    const Self = @This();

    ptr: *anyopaque,

    processFn: *const fn (ptr: *anyopaque, in: *protocol.TProtocol, out: *protocol.TProtocol) anyerror!void,

    pub fn init(ptr: anytype) Self {
        const T = @TypeOf(ptr);
        const ptr_info = @typeInfo(T);

        const gen = struct {
            pub fn process(pointer: *anyopaque, in: *protocol.TProtocol, out: *protocol.TProtocol) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                try ptr_info.pointer.child.process(self, in, out);
            }
        };

        return .{
            .ptr = ptr,
            .processFn = gen.process,
        };
    }

    pub fn process(self: *Self, in: *protocol.TProtocol, out: *protocol.TProtocol) !void {
        return self.processFn(self.ptr, in, out);
    }
};
