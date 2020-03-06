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
import Foundation
@testable import Thrift

/// Testig TFramedTransport
/// 
class TFramedTransportTests: XCTestCase {
  var underlyingTransport: TMemoryBufferTransport = TMemoryBufferTransport(flushHandler: {
    $0.reset(readBuffer: $1)
  })

  var proto: TBinaryProtocol!
  var transport: TFramedTransport!

  override func setUp() {
    super.setUp()
    transport = TFramedTransport(transport:underlyingTransport)
    proto = TBinaryProtocol(on: transport)
    underlyingTransport.reset()
  }

  override func tearDown() {
    super.tearDown()
    underlyingTransport.reset()
  }
  func testInt8WriteRead() {
    let writeVal: UInt8 = 250
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: UInt8 = (try? proto.read()) ?? 0
    XCTAssertEqual(writeVal, readVal, "Error with UInt8, wrote \(writeVal) but read \(readVal)")
  }

  func testInt16WriteRead() {

    let writeVal: Int16 = 12312
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: Int16 = (try? proto.read()) ?? 0
    XCTAssertEqual(writeVal, readVal, "Error with Int16, wrote \(writeVal) but read \(readVal)")
  }

  func testInt32WriteRead() {
    let writeVal: Int32 = 2029234
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: Int32 = (try? proto.read()) ?? 0
    XCTAssertEqual(writeVal, readVal, "Error with Int32, wrote \(writeVal) but read \(readVal)")
  }

  func testInt64WriteRead() {
    let writeVal: Int64 = 234234981374134
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: Int64 = (try? proto.read()) ?? 0
    XCTAssertEqual(writeVal, readVal, "Error with Int64, wrote \(writeVal) but read \(readVal)")
  }

  func testDoubleWriteRead() {
    let writeVal: Double = 3.1415926
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: Double = (try? proto.read()) ?? 0.0
    XCTAssertEqual(writeVal, readVal, "Error with Double, wrote \(writeVal) but read \(readVal)")
  }

  func testBoolWriteRead() {
    let writeVal: Bool = true
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: Bool = (try? proto.read()) ?? false
    XCTAssertEqual(writeVal, readVal, "Error with Bool, wrote \(writeVal) but read \(readVal)")
  }

  func testStringWriteRead() {
    let writeVal: String = "Hello World"
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: String!
    do {
      readVal = try proto.read()
    } catch let error {
      XCTAssertFalse(true, "Error reading \(error)")
      return
    }

    XCTAssertEqual(writeVal, readVal, "Error with String, wrote \(writeVal) but read \(readVal)")
  }

  func testDataWriteRead() {
    let writeVal: Data = "Data World".data(using: .utf8)!
    try? proto.write(writeVal)
    try? transport.flush()

    let readVal: Data = (try? proto.read()) ?? "Goodbye World".data(using: .utf8)!
    XCTAssertEqual(writeVal, readVal, "Error with Data, wrote \(writeVal) but read \(readVal)")
  }

  func testStructWriteRead() {
    let msg = "Test Protocol Error"
    let writeVal = TApplicationError(error: .protocolError, message: msg)
    do {
      try writeVal.write(to: proto)
      try? transport.flush()
    } catch let error {
      XCTAssertFalse(true, "Caught Error attempting to write \(error)")
    }

    do {
      let readVal = try TApplicationError.read(from: proto)
      XCTAssertEqual(readVal.error.thriftErrorCode, writeVal.error.thriftErrorCode, "Error case mismatch, expected \(readVal.error) got \(writeVal.error)")
      XCTAssertEqual(readVal.message, writeVal.message, "Error message mismatch, expected \(readVal.message) got \(writeVal.message)")
    } catch let error {
      XCTAssertFalse(true, "Caught Error attempting to read \(error)")
    }
  }

  func testUnsafeBitcastUpdate() {
    let value: Double = 3.14159
    let val: Int64 = 31415926
    let uval: UInt64 = 31415926

    let i64 = Int64(bitPattern: value.bitPattern)
    let ubc = unsafeBitCast(value, to: Int64.self)

    XCTAssertEqual(i64, ubc, "Bitcast Double-> i64 Values don't match")

    let dbl = Double(bitPattern: UInt64(val))
    let ubdb = unsafeBitCast(val, to: Double.self)

    XCTAssertEqual(dbl, ubdb, "Bitcast i64 -> Double Values don't match")

    let db2 = Double(bitPattern: uval)
    let usbc2 = unsafeBitCast(uval, to: Double.self)

    XCTAssertEqual(db2, usbc2, "Bitcast u64 -> Double Values don't match")
  }

  static var allTests : [(String, (TFramedTransportTests) -> () throws -> Void)] {
    return [
      ("testInt8WriteRead", testInt8WriteRead),
      ("testInt16WriteRead", testInt16WriteRead),
      ("testInt32WriteRead", testInt32WriteRead),
      ("testInt64WriteRead", testInt64WriteRead),
      ("testDoubleWriteRead", testDoubleWriteRead),
      ("testBoolWriteRead", testBoolWriteRead),
      ("testStringWriteRead", testStringWriteRead),
      ("testDataWriteRead", testDataWriteRead),
      ("testStructWriteRead", testStructWriteRead),
      ("testUnsafeBitcastUpdate", testUnsafeBitcastUpdate)
    ]
  }
}
