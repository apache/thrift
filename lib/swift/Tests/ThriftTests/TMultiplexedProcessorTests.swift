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

private protocol CalculatorService { }

private class Calculator: CalculatorService { }

private class CalculatorProcessor: TProcessor {
  private let service: CalculatorService

  init(service: CalculatorService) {
    self.service = service
  }

  var processCalled = false
  func process(on inProtocol: TProtocol, outProtocol: TProtocol) throws {
    processCalled = true
  }
}

class TMultiplexedProcessorTests: XCTestCase {

  let sut = MultiplexedProcessor()
  var transport: TMemoryBufferTransport = TMemoryBufferTransport { $0.reset(readBuffer: $1) }
  lazy var proto = TMultiplexedProtocol<TCompactProtocol>(on: transport)

  override func setUp() {
    super.setUp()
    transport.reset()
  }

  override func tearDown() {
    super.tearDown()
    transport.reset()
  }

  func testExceptionMessageThrowsError() throws {
    try proto.writeMessageBegin(name: "message", type: .exception, sequenceID: 1)
    try transport.flush()
    XCTAssertThrowsError(try sut.process(on: proto, outProtocol: proto)) { error in
      guard case MultiplexedProcessor.Error.incompatibleMessageType(let type) = error else {
        XCTFail()
        return
      }
      XCTAssertEqual(type, .exception)
    }
  }

  func testReplyMessageThrowsError() throws {
    try proto.writeMessageBegin(name: "message", type: .reply, sequenceID: 1)
    try transport.flush()
    XCTAssertThrowsError(try sut.process(on: proto, outProtocol: proto)) { error in
      guard case MultiplexedProcessor.Error.incompatibleMessageType(let type) = error else {
        XCTFail()
        return
      }
      XCTAssertEqual(type, .reply)
    }
  }

  func testMissingDefaultProcessorThrowsError() throws {
    try proto.writeMessageBegin(name: "message", type: .call, sequenceID: 1)
    try transport.flush()
    XCTAssertThrowsError(try sut.process(on: proto, outProtocol: proto)) { error in
      guard case MultiplexedProcessor.Error.missingDefaultProcessor = error else {
        XCTFail()
        return
      }
    }
  }

  func testUsesDefaultProcessorForNonMultiplexedMessage() throws {
    let calculator = Calculator()
    let calculatorProcessor = CalculatorProcessor(service: calculator)
    sut.register(defaultProcessor: calculatorProcessor)
    try proto.writeMessageBegin(name: "message", type: .call, sequenceID: 1)
    try transport.flush()
    try sut.process(on: proto, outProtocol: proto)
    XCTAssertTrue(calculatorProcessor.processCalled)
  }

  func testUsesProcessorForMultiplexedMessage() throws {
    let calculator = Calculator()
    let calculatorProcessor = CalculatorProcessor(service: calculator)
    sut.register(processor: calculatorProcessor, for: "Calculator")
    try proto.writeMessageBegin(name: "Calculator:message", type: .call, sequenceID: 1)
    try transport.flush()
    try sut.process(on: proto, outProtocol: proto)
    XCTAssertTrue(calculatorProcessor.processCalled)
  }

  func testMissingProcessorForMultiplexedMessageThrowsError() throws {
    try proto.writeMessageBegin(name: "Calculator:message", type: .call, sequenceID: 1)
    try transport.flush()
    XCTAssertThrowsError(try sut.process(on: proto, outProtocol: proto)) { error in
      guard case MultiplexedProcessor.Error.missingProcessor(let serviceName) = error else {
        XCTFail()
        return
      }
      XCTAssertEqual(serviceName, "Calculator")
    }
  }

  func testCallMessageDoesNotThrowError() throws {
    let calculator = Calculator()
    let calculatorProcessor = CalculatorProcessor(service: calculator)
    sut.register(defaultProcessor: calculatorProcessor)
    try proto.writeMessageBegin(name: "message", type: .call, sequenceID: 1)
    try transport.flush()
    try sut.process(on: proto, outProtocol: proto)
  }

  func testOneWayMessageDoesNotThrowError() throws {
    let calculator = Calculator()
    let calculatorProcessor = CalculatorProcessor(service: calculator)
    sut.register(defaultProcessor: calculatorProcessor)
    try proto.writeMessageBegin(name: "message", type: .oneway, sequenceID: 1)
    try transport.flush()
    try sut.process(on: proto, outProtocol: proto)
  }

  static var allTests : [(String, (TMultiplexedProcessorTests) -> () throws -> Void)] {
    return [
      ("testExceptionMessageThrowsError", testExceptionMessageThrowsError),
      ("testReplyMessageThrowsError", testReplyMessageThrowsError),
      ("testMissingDefaultProcessorThrowsError", testMissingDefaultProcessorThrowsError),
      ("testUsesDefaultProcessorForNonMultiplexedMessage", testUsesDefaultProcessorForNonMultiplexedMessage),
      ("testUsesProcessorForMultiplexedMessage", testUsesProcessorForMultiplexedMessage),
      ("testMissingProcessorForMultiplexedMessageThrowsError", testMissingProcessorForMultiplexedMessageThrowsError),
      ("testCallMessageDoesNotThrowError", testCallMessageDoesNotThrowError),
      ("testOneWayMessageDoesNotThrowError", testOneWayMessageDoesNotThrowError)
    ]
  }
}
