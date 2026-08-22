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
const testing = std.testing;

pub const DEFAULT_MAX_MESSAGE_SIZE: i32 = 100 * 1024 * 1024;

pub const DEFAULT_MAX_FRAME_SIZE: i32 = 16384000;

pub const DEFAULT_RECURSION_DEPTH: i32 = 64;

pub const TConfiguration = struct {
    maxMessageSize: i32 = DEFAULT_MAX_MESSAGE_SIZE,
    maxFrameSize: i32 = DEFAULT_MAX_FRAME_SIZE,
    recursionLimit: i32 = DEFAULT_RECURSION_DEPTH,

    pub const default: TConfiguration = .{};
};

test "default values match spec" {
    const config: TConfiguration = .default;
    try testing.expectEqual(DEFAULT_MAX_MESSAGE_SIZE, config.maxMessageSize);
    try testing.expectEqual(DEFAULT_MAX_FRAME_SIZE, config.maxFrameSize);
    try testing.expectEqual(DEFAULT_RECURSION_DEPTH, config.recursionLimit);
}

test "fields are mutable on a copy" {
    const config: TConfiguration = .{
        .maxMessageSize = 1024,
        .maxFrameSize = 512,
        .recursionLimit = 10,
    };
    try testing.expectEqual(@as(i32, 1024), config.maxMessageSize);
    try testing.expectEqual(@as(i32, 512), config.maxFrameSize);
    try testing.expectEqual(@as(i32, 10), config.recursionLimit);
}
