// swift-tools-version:5.1
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
    name: "CrossTests",
    products: [
        .executable(name: "TestServer", targets: ["TestServer"]),
        .executable(name: "TestClient", targets: ["TestClient"]),
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
       .package(path: "../../../lib/swift")
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages which this package depends on.
        .target(
            name: "Common",
            dependencies: ["Thrift"]),
        .target(
            name: "TestServer",
            dependencies: ["Thrift", "Common"]),
        .target(
            name: "TestClient",
            dependencies: ["Thrift", "Common"])
    ]
)
