// swift-tools-version:5.1
// The swift-tools-version declares the minimum version of Swift required to build this package.

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
