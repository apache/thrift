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

class TJSONProtocolTests: XCTestCase {
    var transport: TMemoryBufferTransport = TMemoryBufferTransport(flushHandler: {
        $0.reset(readBuffer: $1)
    })

    var proto: TJSONProtocol!

    override func setUp() {
      super.setUp()
      proto = TJSONProtocol(on: transport)
      transport.reset()
    }

    override func tearDown() {
      super.tearDown()
      transport.reset()
    }

    func testUInt8WriteRead() {
      let writeVal: UInt8 = 250
      try? proto.write(writeVal)
      try? transport.flush()

      let readVal: UInt8 = (try? proto.read()) ?? 0
      XCTAssertEqual(writeVal, readVal, "Error with UInt8, wrote \(writeVal) but read \(readVal)")
    }

    func testInt8WriteRead() {
      let writeVal: Int8 = -120
      try? proto.write(writeVal)
      try? transport.flush()

      let readVal: Int8 = (try? proto.read()) ?? 0
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

      let readVal: String
      do {
        readVal = try proto.read()
      } catch let error {
        XCTAssertFalse(true, "Error reading \(error)")
        return
      }

      XCTAssertEqual(writeVal, readVal, "Error with String, wrote \(writeVal) but read \(readVal)")
    }

    func testStringWriteRead2() {
      let writeVal: String = "你好世界 means hello world!"
      try? proto.write(writeVal)
      try? transport.flush()

      let readVal: String
      do {
        print(writeVal)
        readVal = try proto.read()
        print(readVal)
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

    func testUUIDWriteRead() {
        let writeVal: UUID = UUID()
        try? proto.write(writeVal)
        try? transport.flush()

        let newUuid = UUID()
        let readVal: UUID = (try? proto.read()) ?? newUuid
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
          let readVal: TApplicationError = try TApplicationError.read(from: proto)
          XCTAssertEqual(readVal.error.thriftErrorCode, writeVal.error.thriftErrorCode, "Error case mismatch, expected \(readVal.error) got \(writeVal.error)")
          let readValMessage = readVal.message ?? "", writeValMessage = writeVal.message ?? ""
          XCTAssertEqual(readVal.message, writeVal.message, "Error message mismatch, expected \(readValMessage) got \(writeValMessage)")
      } catch let error {
        XCTAssertFalse(true, "Caught Error attempting to read \(error)")
      }
    }

    func testBase64WriteRead() {
        let writeText = "!testing base64 read and write ..."
        let writeData = writeText.data(using: .utf8)!
        let writeVal = [UInt8](writeData)
        try? proto.writeJsonBase64(bytes: writeVal)
        try? transport.flush()

        var data = Data()
        if let readVal = try? proto.readJsonBase64() {
            data = Data(bytes: readVal, count: readVal.count)
        }
        let readText = String(decoding: data, as: UTF8.self)
        XCTAssertEqual(readText, writeText, "Error message mismatch, expected \(readText) got \(writeText)")
    }

    func testBase64WriteRead2() {
        let writeText = "你好世界 means hello world!"
        let writeData = writeText.data(using: .utf8)!
        let writeVal = [UInt8](writeData)
        try? proto.writeJsonBase64(bytes: writeVal)
        try? transport.flush()

        var data = Data()
        if let readVal = try? proto.readJsonBase64() {
            data = Data(bytes: readVal, count: readVal.count)
        }
        let readText = String(decoding: data, as: UTF8.self)
        XCTAssertEqual(readText, writeText, "Error message mismatch, expected \(readText) got \(writeText)")
    }

    static var allTests : [(String, (TJSONProtocolTests) -> () throws -> Void)] {
      return [
        ("testUInt8WriteRead", testUInt8WriteRead),
        ("testInt8WriteRead", testInt8WriteRead),
        ("testInt16WriteRead", testInt16WriteRead),
        ("testInt32WriteRead", testInt32WriteRead),
        ("testInt64WriteRead", testInt64WriteRead),
        ("testDoubleWriteRead", testDoubleWriteRead),
        ("testBoolWriteRead", testBoolWriteRead),
        ("testStringWriteRead", testStringWriteRead),
        ("testStringWriteRead2", testStringWriteRead2),
        ("testDataWriteRead", testDataWriteRead),
        ("testUUIDWriteRead", testUUIDWriteRead),
        ("testStructWriteRead", testStructWriteRead),
        ("testBase64WriteRead", testBase64WriteRead),
        ("testBase64WriteRead2", testBase64WriteRead2)
      ]
    }
}
