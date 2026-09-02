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

fn addGeneratedThriftModule(
    b: *std.Build,
    comptime name: []const u8,
    root: []const u8,
    thrift: *std.Build.Module,
) *std.Build.Module {
    const mod = b.addModule(name, .{
        .root_source_file = b.path(root),
    });
    mod.addImport("thrift", thrift);
    return mod;
}

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});

    const thrift = b.addModule("thrift", .{
        .root_source_file = b.path("../src/root.zig"),
        .target = target,
    });

    const gen_thrift = addGeneratedThriftModule(b, "thrift_test", "gen-zig/thrift_test.zig", thrift);
    const gen_optional_required = addGeneratedThriftModule(b, "optional_required_test", "gen-zig/optional_required_test.zig", thrift);
    const gen_fuzz = addGeneratedThriftModule(b, "fuzz_test", "gen-zig/fuzz_test_no_uuid.zig", thrift);
    const gen_constants_demo = addGeneratedThriftModule(b, "constants_demo", "gen-zig/constants_demo.zig", thrift);

    const tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/integration_tests.zig"),
            .target = target,
            .imports = &.{
                .{ .name = "thrift", .module = thrift },
                .{ .name = "thrift_test", .module = gen_thrift },
                .{ .name = "optional_required_test", .module = gen_optional_required },
                .{ .name = "fuzz_test", .module = gen_fuzz },
                .{ .name = "constants_demo", .module = gen_constants_demo },
            },
        }),
    });

    const run_tests = b.addRunArtifact(tests);

    const test_step = b.step("test", "Run generated-code integration tests");
    test_step.dependOn(&run_tests.step);
}
