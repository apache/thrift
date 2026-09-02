const std = @import("std");

const Allocator = std.mem.Allocator;

pub const BinaryBytes = struct {
    allocator: Allocator,
    contents: []const u8,
    owned: bool = true,

    pub fn initDefault(allocator: Allocator) !@This() {
        return .{
            .allocator = allocator,
            .contents = try allocator.alloc(u8, 0),
        };
    }

    pub fn initFromSlice(allocator: Allocator, s: []const u8) !@This() {
        return .{
            .allocator = allocator,
            .contents = try allocator.dupe(u8, s),
        };
    }

    /// Wraps `s` without copying. `deinit` is a no-op; do not use after `s` becomes invalid.
    pub fn initFromBorrowed(s: []const u8) @This() {
        return .{
            .allocator = undefined,
            .contents = s,
            .owned = false,
        };
    }

    pub fn deinit(self: *const @This()) void {
        if (self.owned) {
            self.allocator.free(self.contents);
        }
    }

    pub fn format(self: *const @This(), writer: *std.Io.Writer) !void {
        try writer.print("{x}", .{self.contents});
    }

    pub fn hash(self: *const @This(), hasher: anytype) void {
        hasher.update(self.contents);
    }

    pub fn eql(self: *const @This(), other: @This()) bool {
        return std.mem.eql(u8, self.contents, other.contents);
    }

    pub fn clone(self: *const @This(), allocator: Allocator) !@This() {
        return .{
            .allocator = allocator,
            .contents = try allocator.dupe(u8, self.contents),
        };
    }
};

pub const String = struct {
    allocator: Allocator,
    contents: []const u8,
    owned: bool = true,

    pub fn initDefault(allocator: Allocator) !@This() {
        return .{
            .allocator = allocator,
            .contents = try allocator.alloc(u8, 0),
        };
    }

    pub fn initFromSlice(allocator: Allocator, s: []const u8) !@This() {
        return .{
            .allocator = allocator,
            .contents = try allocator.dupe(u8, s),
        };
    }

    /// Wraps `s` without copying. `deinit` is a no-op; do not use after `s` becomes invalid.
    pub fn initFromBorrowed(s: []const u8) @This() {
        return .{
            .allocator = undefined,
            .contents = s,
            .owned = false,
        };
    }

    pub fn deinit(self: *const @This()) void {
        if (self.owned) {
            self.allocator.free(self.contents);
        }
    }

    pub fn format(self: *const @This(), writer: *std.Io.Writer) !void {
        try writer.print("{s}", .{self.contents});
    }

    pub fn hash(self: *const @This(), hasher: anytype) void {
        hasher.update(self.contents);
    }

    pub fn eql(self: *const @This(), other: @This()) bool {
        return std.mem.eql(u8, self.contents, other.contents);
    }

    pub fn clone(self: *const @This(), allocator: Allocator) !@This() {
        return .{
            .allocator = allocator,
            .contents = try allocator.dupe(u8, self.contents),
        };
    }
};

// Thanks to dmgk/zig-uuid for this reference implementation
pub const UUID = struct {
    const Self = @This();

    bytes: [16]u8,

    pub const empty = Self{
        .bytes = [16]u8{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
    };

    // Hex to nibble mapping.
    const hex_to_nibble = [256]u8{
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
        0x08, 0x09, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
        0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    };

    // Indices in the UUID string representation for each byte.
    const encoded_pos = [16]u8{ 0, 2, 4, 6, 9, 11, 14, 16, 19, 21, 24, 26, 28, 30, 32, 34 };

    pub fn parse(buf: []const u8) !Self {
        var uuid = UUID{ .bytes = undefined };

        if (buf.len != 36 or buf[8] != '-' or buf[13] != '-' or buf[18] != '-' or buf[23] != '-')
            return error.InvalidUUID;

        inline for (encoded_pos, 0..) |i, j| {
            const hi = hex_to_nibble[buf[i + 0]];
            const lo = hex_to_nibble[buf[i + 1]];
            if (hi == 0xff or lo == 0xff) {
                return error.InvalidUUID;
            }
            uuid.bytes[j] = hi << 4 | lo;
        }

        return uuid;
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) !void {
        const hex = "0123456789abcdef";

        var buf: [36]u8 = undefined;
        buf[8] = '-';
        buf[13] = '-';
        buf[18] = '-';
        buf[23] = '-';
        inline for (encoded_pos, 0..) |i, j| {
            buf[i + 0] = hex[self.bytes[j] >> 4];
            buf[i + 1] = hex[self.bytes[j] & 0x0f];
        }
        try writer.print("{s}", .{buf});
    }

    pub fn hash(self: *const @This(), hasher: anytype) void {
        hasher.update(&self.bytes);
    }

    pub fn eql(self: *const @This(), other: @This()) bool {
        return std.mem.eql(u8, &self.bytes, &other.bytes);
    }

    pub fn clone(self: *const @This(), allocator: Allocator) !@This() {
        _ = allocator;
        var tmp: @This() = .{
            .bytes = undefined,
        };
        std.mem.copyForwards(u8, &tmp.bytes, &self.bytes);
        return tmp;
    }
};
