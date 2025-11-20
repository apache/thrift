// swift-tools-version:5.5
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

import PackageDescription

let package = Package(
    name: "ThriftFuzzTesting",
    dependencies: [
        .package(name: "Thrift", path: "../")
    ],
    targets: [
        // Generated code from Thrift definitions
        .target(
            name: "Fuzz",
            dependencies: ["Thrift"],
            path: "Sources/Fuzz"
        ),
        // Common utilities for fuzzing
        .target(
            name: "FuzzCommon",
            dependencies: ["Thrift", "Fuzz"],
            path: "Sources/FuzzCommon"
        ),
        .executableTarget(
            name: "FuzzParseBinary",
            dependencies: ["FuzzCommon", "Thrift", "Fuzz"],
            path: "Sources/FuzzParseBinary",
            linkerSettings: [
                .unsafeFlags(["-sanitize=fuzzer"])
            ]
        ),
        .executableTarget(
            name: "FuzzRoundtripBinary",
            dependencies: ["FuzzCommon", "Thrift", "Fuzz"],
            path: "Sources/FuzzRoundtripBinary",
            linkerSettings: [
                .unsafeFlags(["-sanitize=fuzzer"])
            ]
        ),
        .executableTarget(
            name: "FuzzParseCompact",
            dependencies: ["FuzzCommon", "Thrift", "Fuzz"],
            path: "Sources/FuzzParseCompact",
            linkerSettings: [
                .unsafeFlags(["-sanitize=fuzzer"])
            ]
        ),
        .executableTarget(
            name: "FuzzRoundtripCompact",
            dependencies: ["FuzzCommon", "Thrift", "Fuzz"],
            path: "Sources/FuzzRoundtripCompact",
            linkerSettings: [
                .unsafeFlags(["-sanitize=fuzzer"])
            ]
        ),
        .executableTarget(
            name: "FuzzParseJSON",
            dependencies: ["FuzzCommon", "Thrift", "Fuzz"],
            path: "Sources/FuzzParseJSON",
            linkerSettings: [
                .unsafeFlags(["-sanitize=fuzzer"])
            ]
        ),
        .executableTarget(
            name: "FuzzRoundtripJSON",
            dependencies: ["FuzzCommon", "Thrift", "Fuzz"],
            path: "Sources/FuzzRoundtripJSON",
            linkerSettings: [
                .unsafeFlags(["-sanitize=fuzzer"])
            ]
        )
    ]
) 