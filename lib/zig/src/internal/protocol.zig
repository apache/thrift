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

pub fn verifyExpectedSequenceNumber(expected: i32, actual: i32) !void {
    if (expected != actual) {
        return error.BadSequenceId;
    }
}

pub fn verifyExpectedServiceCall(expected: []const u8, actual: []const u8) !void {
    if (!std.mem.eql(u8, expected, actual)) {
        return error.WrongMethodName;
    }
}

pub fn verifyExpectedMessageType(expected: protocol.TMessageType, actual: protocol.TMessageType) !void {
    if (expected != actual) {
        return error.InvalidMessageType;
    }
}
