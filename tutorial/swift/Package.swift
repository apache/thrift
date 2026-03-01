// swift-tools-version:5.1
/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import PackageDescription

let thriftDependency: Target.Dependency = .product(name: "Thrift", package: "swift-dep")
let package = Package(
    name: "swift-tutorial",
    platforms: [
        .macOS(.v10_13)
    ],
    products: [
        // Products define the executables and libraries a package produces, and make them visible to other packages.
        .executable(name: "TutorialServer", targets: ["TutorialServer"]),
        .executable(name: "TutorialClient", targets: ["TutorialClient"]),
        .executable(name: "TutorialRunner", targets: ["TutorialRunner"])
    ],
    dependencies: [
        // Dependencies declare other packages that this package depends on.
        .package(path: "./swift-dep"),
    ],
    targets: [
        // Targets are the basic building blocks of a package. A target can define a module or a test suite.
        // Targets can depend on other targets in this package, and on products in packages this package depends on.
        .target(
            name: "Common",
            dependencies: [thriftDependency]),
        .target(
            name: "TutorialServer",
            dependencies: [thriftDependency, "Common"]),
        .target(
            name: "TutorialClient",
            dependencies: [thriftDependency, "Common"]),
        .target(name: "TutorialRunner")
    ]
)
