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

import Foundation
import Thrift
import Common

class TestServer {
  var server: Any?
  
  func run() throws {
    let parameters = try TestServerParameters(arguments: CommandLine.arguments)
    
    if parameters.showHelp {
      parameters.printHelp()
      return
    }
    
    let service = ThriftTestImpl()
    let processor = ThriftTestProcessor(service: service)
    
    
    switch (parameters.proto, parameters.transport, parameters.domainSocket) {
    case (.binary, .buffered, .none):
      let proto = TBinaryProtocol.self
      server = try TSocketServer(port: parameters.port!, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.binary, .framed, .none):
      let proto = TBinaryProtocol.self
      server = try TFramedSocketServer(port: parameters.port!, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.compact, .buffered, .none):
      let proto = TCompactProtocol.self
      server = try TSocketServer(port: parameters.port!, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.compact, .framed, .none):
      let proto = TCompactProtocol.self
      server = try TFramedSocketServer(port: parameters.port!, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.binary, .buffered, .some(let domainSocket)):
      let proto = TBinaryProtocol.self
      server = try TSocketServer(path: domainSocket, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.binary, .framed, .some(let domainSocket)):
      let proto = TBinaryProtocol.self
      server = try TFramedSocketServer(path: domainSocket, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.compact, .buffered, .some(let domainSocket)):
      let proto = TCompactProtocol.self
      server = try TSocketServer(path: domainSocket, inProtocol: proto, outProtocol: proto, processor: processor)
    case (.compact, .framed, .some(let domainSocket)):
      let proto = TCompactProtocol.self
      server = try TFramedSocketServer(path: domainSocket, inProtocol: proto, outProtocol: proto, processor: processor)
    default:
      throw ParserError.unsupportedOption
    }
  }
}

let server = TestServer()
try server.run()

RunLoop.main.run()
