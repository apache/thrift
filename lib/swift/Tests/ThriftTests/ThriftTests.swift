import XCTest
@testable import Thrift

class ThriftTests: XCTestCase {
  func testVersion() {
    XCTAssertEqual(Thrift().version, "0.14.1")
  }

  static var allTests : [(String, (ThriftTests) -> () throws -> Void)] {
    return [
      ("testVersion", testVersion),
    ]
  }
}
