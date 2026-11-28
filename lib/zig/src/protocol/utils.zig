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

pub fn checkSizeForProtocol(size: i32, config: *const TConfiguration) !void {
    if (size < 0) {
        return ProtocolError.InvalidLength;
    }
    if (size > config.maxMessageSize) {
        return ProtocolError.InvalidLength;
    }
}

pub fn checkWriteLength(len: usize, config: *const TConfiguration) !i32 {
    if (len > std.math.maxInt(i32)) {
        return ProtocolError.InvalidLength;
    }
    const len_i32: i32 = @intCast(len);
    try checkSizeForProtocol(len_i32, config);
    return len_i32;
}

pub fn checkReadLength(len: i32, remaining: ?u64, config: *const TConfiguration) !usize {
    try checkSizeForProtocol(len, config);
    if (remaining) |rem| {
        if (len > 0 and @as(u64, @intCast(len)) > rem) {
            return ProtocolError.InvalidLength;
        }
    }
    return @intCast(len);
}

/// Validates a container element count before using it for allocation.
/// `min_elem_size` is the minimum on-wire size of one element (per protocol).
/// `remaining` is unset when the transport does not know how many bytes are left.
pub fn checkContainerSizeForProtocol(
    size: i64,
    min_elem_size: i32,
    remaining: ?u64,
    config: *const TConfiguration,
) !void {
    if (size < 0) {
        return ProtocolError.InvalidLength;
    }
    if (size > std.math.maxInt(i32)) {
        return ProtocolError.InvalidLength;
    }
    const limit = config.effectiveMaxContainerSize();
    if (size > limit) {
        return ProtocolError.InvalidLength;
    }
    const total_min_size = size * @as(i64, min_elem_size);
    if (total_min_size > std.math.maxInt(i32)) {
        return ProtocolError.InvalidLength;
    }
    const total_min_u64: u64 = @intCast(total_min_size);
    if (remaining) |rem| {
        if (total_min_u64 > rem) {
            return ProtocolError.InvalidLength;
        }
    }
    try checkSizeForProtocol(@intCast(total_min_size), config);
}

pub fn checkContainerSize(size: i32, config: *const TConfiguration) !void {
    try checkContainerSizeForProtocol(size, 1, null, config);
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
    var config = TConfiguration.default;
    try testing.expectError(ProtocolError.InvalidLength, checkWriteLength(@as(usize, @intCast(std.math.maxInt(i32))) + 1, &config));
    try testing.expectEqual(@as(i32, 42), try checkWriteLength(42, &config));
}

test "checkReadLength rejects negative values" {
    var config = TConfiguration.default;
    try testing.expectError(ProtocolError.InvalidLength, checkReadLength(-1, null, &config));
    try testing.expectEqual(@as(usize, 0), try checkReadLength(0, null, &config));
}

test "checkReadLength enforces maxMessageSize" {
    var config = TConfiguration.default;
    config.maxMessageSize = 100;
    try testing.expectError(ProtocolError.InvalidLength, checkReadLength(101, null, &config));
    try testing.expectEqual(@as(usize, 100), try checkReadLength(100, null, &config));
}

test "checkReadLength enforces remaining bytes" {
    var config = TConfiguration.default;
    try testing.expectError(ProtocolError.InvalidLength, checkReadLength(10, 5, &config));
    try testing.expectEqual(@as(usize, 5), try checkReadLength(5, 5, &config));
}

test "getTMessageType rejects invalid wire values" {
    try testing.expectError(ProtocolError.InvalidMessageType, getTMessageType(0));
    try testing.expectError(ProtocolError.InvalidMessageType, getTMessageType(5));
    try testing.expectEqual(TMessageType.Call, try getTMessageType(1));
}

test "checkContainerSize rejects negative values" {
    var config = TConfiguration.default;
    try testing.expectError(ProtocolError.InvalidLength, checkContainerSize(-1, &config));
    try checkContainerSize(0, &config);
    try checkContainerSize(42, &config);
}

test "checkContainerSizeForProtocol enforces maxMessageSize via min wire size" {
    var config = TConfiguration.default;
    config.maxMessageSize = 100;
    const i32_min_wire_size: i32 = 4;
    try checkContainerSizeForProtocol(10, i32_min_wire_size, null, &config);
    try testing.expectError(
        ProtocolError.InvalidLength,
        checkContainerSizeForProtocol(30, i32_min_wire_size, null, &config),
    );
}

test "checkContainerSizeForProtocol rejects count when payload cannot fit" {
    var config = TConfiguration.default;
    try testing.expectError(
        ProtocolError.InvalidLength,
        checkContainerSizeForProtocol(10, 1, 5, &config),
    );
    try checkContainerSizeForProtocol(5, 1, 5, &config);
}

test "checkContainerSizeForProtocol enforces maxContainerSize" {
    var config = TConfiguration.default;
    config.maxContainerSize = 5;
    try checkContainerSizeForProtocol(5, 1, null, &config);
    try testing.expectError(ProtocolError.InvalidLength, checkContainerSizeForProtocol(6, 1, null, &config));
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
