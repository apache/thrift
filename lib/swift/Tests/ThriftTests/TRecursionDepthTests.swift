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

import XCTest
@testable import Thrift

class TRecursionDepthTests: XCTestCase {

  var transport: TMemoryBufferTransport!
  var proto: TBinaryProtocol!

  override func setUp() {
    super.setUp()
    transport = TMemoryBufferTransport(flushHandler: { $0.reset(readBuffer: $1) })
    proto = TBinaryProtocol(on: transport)
  }

  func testInitialDepthIsZero() {
    XCTAssertEqual(proto.recursionDepth, 0)
  }

  func testIncrementAllowsUpToLimit() {
    XCTAssertNoThrow(try {
      for _ in 0..<64 {
        try self.proto.incrementRecursionDepth()
      }
    }())
    XCTAssertEqual(proto.recursionDepth, 64)
  }

  func testIncrementThrowsAtLimitPlusOne() {
    for _ in 0..<64 { try? proto.incrementRecursionDepth() }
    XCTAssertThrowsError(try proto.incrementRecursionDepth()) { error in
      guard let err = error as? TProtocolError else {
        XCTFail("Expected TProtocolError, got \(error)")
        return
      }
      if case .depthLimit = err.error { } else {
        XCTFail("Expected .depthLimit, got \(err.error)")
      }
    }
  }

  func testDepthStaysAtLimitAfterFailedIncrement() {
    for _ in 0..<64 { try? proto.incrementRecursionDepth() }
    try? proto.incrementRecursionDepth()
    XCTAssertEqual(proto.recursionDepth, 64)
  }

  func testDecrementRestoresCapacity() {
    for _ in 0..<64 { try? proto.incrementRecursionDepth() }
    proto.decrementRecursionDepth()
    XCTAssertEqual(proto.recursionDepth, 63)
    XCTAssertNoThrow(try proto.incrementRecursionDepth())
  }

  static var allTests = [
    ("testInitialDepthIsZero", testInitialDepthIsZero),
    ("testIncrementAllowsUpToLimit", testIncrementAllowsUpToLimit),
    ("testIncrementThrowsAtLimitPlusOne", testIncrementThrowsAtLimitPlusOne),
    ("testDepthStaysAtLimitAfterFailedIncrement", testDepthStaysAtLimitAfterFailedIncrement),
    ("testDecrementRestoresCapacity", testDecrementRestoresCapacity),
  ]
}
