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
const protocol = @import("protocol.zig");
const TConfiguration = @import("../lib/configuration.zig").TConfiguration;

const ProtocolError = protocol.ProtocolError;
const TMessageType = protocol.TMessageType;

pub fn getTMessageType(byte: u8) !TMessageType {
    return switch (byte) {
        1 => .Call,
        2 => .Reply,
        3 => .Exception,
        4 => .OneWay,
        else => ProtocolError.InvalidMessageType,
    };
}

pub fn checkWriteLength(len: usize) !i32 {
    if (len > std.math.maxInt(i32)) {
        return ProtocolError.InvalidLength;
    }
    return @intCast(len);
}

pub fn checkReadLength(len: i32) !usize {
    if (len < 0) {
        return ProtocolError.InvalidLength;
    }
    return @intCast(len);
}

pub fn checkContainerSize(size: i32) !void {
    if (size < 0) {
        return ProtocolError.InvalidLength;
    }
}

pub fn checkSkipDepth(max_depth: i32) !void {
    if (max_depth <= 0) {
        return ProtocolError.RecursionLimitExceeded;
    }
}

pub const RecursionTracker = struct {
    depth: u32 = 0,
    config: *const TConfiguration,

    pub fn init(config: *const TConfiguration) RecursionTracker {
        return .{ .config = config };
    }

    pub fn increment(self: *RecursionTracker) !void {
        self.depth += 1;
        if (@as(i32, @intCast(self.depth)) > self.config.recursionLimit) {
            return ProtocolError.RecursionLimitExceeded;
        }
    }

    pub fn decrement(self: *RecursionTracker) !void {
        if (self.depth == 0) {
            return ProtocolError.InvalidStructNesting;
        }
        self.depth -= 1;
    }
};

const testing = std.testing;

test "checkWriteLength rejects oversized values" {
    try testing.expectError(ProtocolError.InvalidLength, checkWriteLength(@as(usize, @intCast(std.math.maxInt(i32))) + 1));
    try testing.expectEqual(@as(i32, 42), try checkWriteLength(42));
}

test "checkReadLength rejects negative values" {
    try testing.expectError(ProtocolError.InvalidLength, checkReadLength(-1));
    try testing.expectEqual(@as(usize, 0), try checkReadLength(0));
}

test "getTMessageType rejects invalid wire values" {
    try testing.expectError(ProtocolError.InvalidMessageType, getTMessageType(0));
    try testing.expectError(ProtocolError.InvalidMessageType, getTMessageType(5));
    try testing.expectEqual(TMessageType.Call, try getTMessageType(1));
}

test "checkContainerSize rejects negative values" {
    try testing.expectError(ProtocolError.InvalidLength, checkContainerSize(-1));
    try checkContainerSize(0);
    try checkContainerSize(42);
}

test "checkSkipDepth rejects exhausted depth" {
    try testing.expectError(ProtocolError.RecursionLimitExceeded, checkSkipDepth(0));
    try checkSkipDepth(1);
}

test "recursion tracker enforces limit" {
    var config = TConfiguration.default;
    config.recursionLimit = 2;
    var tracker = RecursionTracker.init(&config);
    try tracker.increment();
    try tracker.increment();
    try testing.expectError(ProtocolError.RecursionLimitExceeded, tracker.increment());
}
