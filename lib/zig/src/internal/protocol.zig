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
