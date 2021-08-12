//
//  TCompactProtocolTests.swift
//  Thrift
//
//  Created by Christopher Simpson on 8/19/16.
//
//

import XCTest
import Foundation
@testable import Thrift


/// Testing Binary protocol read/write against itself
/// Uses separate read/write transport/protocols
class TCompactProtocolTests: XCTestCase {
  var transport: TMemoryBufferTransport = TMemoryBufferTransport(flushHandler: {
    $0.reset(readBuffer: $1)
  })
  var proto: TCompactProtocol!
  
  override func setUp() {
    super.setUp()
    proto = TCompactProtocol(on: transport)
    transport.reset()
  }
  
  override func tearDown() {
    super.tearDown()
    transport.reset()
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
      try transport.flush()

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
  
  func testInt32ZigZag() {
    let zero: Int32 = 0
    let one: Int32 = 1
    let nOne: Int32 = -1
    let two: Int32 = 2
    let nTwo: Int32 = -2
    let max = Int32.max
    let min = Int32.min

    XCTAssertEqual(proto.i32ToZigZag(zero), UInt32(0), "Error 32bit zigzag on \(zero)")
    XCTAssertEqual(proto.zigZagToi32(0), zero, "Error 32bit zigzag on \(zero)")

    XCTAssertEqual(proto.i32ToZigZag(nOne), UInt32(1), "Error 32bit zigzag on \(nOne)")
    XCTAssertEqual(proto.zigZagToi32(1), nOne, "Error 32bit zigzag on \(nOne)")

    XCTAssertEqual(proto.i32ToZigZag(one), UInt32(2), "Error 32bit zigzag on \(one)")
    XCTAssertEqual(proto.zigZagToi32(2), one, "Error 32bit zigzag on \(one)")

    XCTAssertEqual(proto.i32ToZigZag(nTwo), UInt32(3), "Error 32bit zigzag on \(nTwo)")
    XCTAssertEqual(proto.zigZagToi32(3), nTwo, "Error 32bit zigzag on \(nTwo)")

    XCTAssertEqual(proto.i32ToZigZag(two), UInt32(4), "Error 32bit zigzag on \(two)")
    XCTAssertEqual(proto.zigZagToi32(4), two, "Error 32bit zigzag on \(two)")

    let uMaxMinusOne: UInt32 = UInt32.max - 1
    XCTAssertEqual(proto.i32ToZigZag(max), uMaxMinusOne, "Error 32bit zigzag on \(max)")
    XCTAssertEqual(proto.zigZagToi32(uMaxMinusOne), max, "Error 32bit zigzag on \(max)")

    XCTAssertEqual(proto.i32ToZigZag(min), UInt32.max, "Error 32bit zigzag on \(min)")
    XCTAssertEqual(proto.zigZagToi32(UInt32.max), min, "Error 32bit zigzag on \(min)")
  }

  func testInt64ZigZag() {
    let zero: Int64 = 0
    let one: Int64 = 1
    let nOne: Int64 = -1
    let two: Int64 = 2
    let nTwo: Int64 = -2
    let max = Int64.max
    let min = Int64.min

    XCTAssertEqual(proto.i64ToZigZag(zero), UInt64(0), "Error 64bit zigzag on \(zero)")
    XCTAssertEqual(proto.zigZagToi64(0), zero, "Error 64bit zigzag on \(zero)")

    XCTAssertEqual(proto.i64ToZigZag(nOne), UInt64(1), "Error 64bit zigzag on \(nOne)")
    XCTAssertEqual(proto.zigZagToi64(1), nOne, "Error 64bit zigzag on \(nOne)")

    XCTAssertEqual(proto.i64ToZigZag(one), UInt64(2), "Error 64bit zigzag on \(one)")
    XCTAssertEqual(proto.zigZagToi64(2), one, "Error 64bit zigzag on \(one)")

    XCTAssertEqual(proto.i64ToZigZag(nTwo), UInt64(3), "Error 64bit zigzag on \(nTwo)")
    XCTAssertEqual(proto.zigZagToi64(3), nTwo, "Error 64bit zigzag on \(nTwo)")

    XCTAssertEqual(proto.i64ToZigZag(two), UInt64(4), "Error 64bit zigzag on \(two)")
    XCTAssertEqual(proto.zigZagToi64(4), two, "Error 64bit zigzag on \(two)")

    let uMaxMinusOne: UInt64 = UInt64.max - 1
    XCTAssertEqual(proto.i64ToZigZag(max), uMaxMinusOne, "Error 64bit zigzag on \(max)")
    XCTAssertEqual(proto.zigZagToi64(uMaxMinusOne), max, "Error 64bit zigzag on \(max)")

    XCTAssertEqual(proto.i64ToZigZag(min), UInt64.max, "Error 64bit zigzag on \(min)")
    XCTAssertEqual(proto.zigZagToi64(UInt64.max), min, "Error 64bit zigzag on \(min)")
  }
  
  static var allTests : [(String, (TCompactProtocolTests) -> () throws -> Void)] {
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
      ("testInt32ZigZag", testInt32ZigZag),
      ("testInt64ZigZag", testInt64ZigZag)
    ]
  }
}
