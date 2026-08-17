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

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const thrift = b.addModule("thrift", .{
        .root_source_file = b.path("../../lib/zig/src/root.zig"),
    });

    const clap = b.dependency("clap", .{});

    const gen_thrift = b.addModule("thrift_test", .{
        .root_source_file = b.path("gen-zig/thrift_test.zig"),
    });
    gen_thrift.addImport("thrift", thrift);

    const server_exe = b.addExecutable(.{
        .name = "test_server",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test_server.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "thrift", .module = thrift },
                .{ .name = "thrift_test", .module = gen_thrift },
                .{ .name = "clap", .module = clap.module("clap")}
            },
        }),
    });
    b.installArtifact(server_exe);
    const run_server_step = b.step("run_server", "Run the server");
    const run_server_cmd = b.addRunArtifact(server_exe);
    run_server_step.dependOn(&run_server_cmd.step);

    run_server_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_server_cmd.addArgs(args);
    }

    const client_exe = b.addExecutable(.{
        .name = "test_client",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/test_client.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "thrift", .module = thrift },
                .{ .name = "thrift_test", .module = gen_thrift },
                .{ .name = "clap", .module = clap.module("clap")}
            },
        }),
    });
    b.installArtifact(client_exe);
    const run_client_step = b.step("run_client", "Run the client");
    const run_client_cmd = b.addRunArtifact(client_exe);
    run_client_step.dependOn(&run_client_cmd.step);

    run_client_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_client_cmd.addArgs(args);
    }
}
