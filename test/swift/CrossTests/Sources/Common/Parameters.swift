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

public enum Protocol: String {
  case binary
  case compact
  case header
  case json
}

public enum Transport: String {
  case buffered
  case framed
  case http
  case anonpipe
  case zlib
}

public enum ServerType: String {
  case simple
  case threadPool = "thread-pool"
  case threaded
  case nonblocking
}

public enum ParserError: Error {
  case unknownArgument(argument: String)
  case missingParameter(argument: String)
  case invalidParameter(argument: String, parameter: String)

  case unsupportedOption
}

public class ParametersBase {
  public var showHelp = false
  public var port: Int?
  public var domainSocket: String?
  public var namedPipe: String?
  public var proto: Protocol?
  public var transport: Transport?
  public var multiplex = false
  public var abstractNamespace = false
  public var ssl = false
  public var zlib = false

  public init (arguments: [String]) throws {
    if arguments.count > 1 { 
      for argument in arguments[1...] {
        let equalSignPos = argument.firstIndex(of: "=") ?? argument.endIndex
        let name = String(argument[..<equalSignPos])
        let value: String? = (equalSignPos < argument.endIndex) ? String(argument[argument.index(equalSignPos, offsetBy: 1)..<argument.endIndex]) : nil

        try processArgument(name: name, value: value)
      }
    }

    fillDefaults()
    try checkSupported()
  }

  open func processArgument(name: String, value: String?) throws {
    switch name {
      case "-h", "--help":
        showHelp = true
      case "--port":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        port = Int(value!)
        guard port != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
      case "--domain-socket":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        domainSocket = value!
      case "--named-pipe":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        namedPipe = value!
      case "--transport":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        transport = Transport(rawValue: value!)
        guard transport != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
      case "--protocol":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        proto = Protocol(rawValue: value!)
        guard proto != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
      case "--multiplex":
        multiplex = true
      case "--abstract-namespace":
        abstractNamespace = true
      case "--ssl":
        ssl = true
      case "--zlib":
        zlib = true
      default:
        throw ParserError.unknownArgument(argument: name)
    }
  }

  open func fillDefaults() {
    if port == nil && domainSocket == nil && namedPipe == nil {
      port = 9090
    }

    if transport == nil {
      transport = .buffered
    }

    if proto == nil {
      proto = .binary
    }
  }

  open func checkSupported() throws {
    guard transport == .buffered || transport == .framed else { throw ParserError.unsupportedOption }
    guard proto == .binary || proto == .compact else { throw ParserError.unsupportedOption }
  }
}

public class TestClientParameters: ParametersBase {
  public var host: String?
  public var testLoops: Int?
  public var threads: Int?

  public func printHelp() {
    print("""
Allowed options:
  -h | --help                  produce help message
  --host=arg (localhost)       Host to connect
  --port=arg (9090)            Port number to connect
  --domain-socket=arg          Domain Socket (e.g. /tmp/ThriftTest.thrift),
                               instead of host and port
  --named-pipe=arg             Windows Named Pipe (e.g. MyThriftPipe)
  --anon-pipes hRead hWrite    Windows Anonymous Pipes pair (handles)
  --abstract-namespace         Create the domain socket in the Abstract Namespace
                               (no connection with filesystem pathnames)
  --transport=arg (buffered)   Transport: buffered, framed, http, evhttp, zlib
  --protocol=arg (binary)      Protocol: binary, compact, header, json
  --multiplex                  Add TMultiplexedProtocol service name "ThriftTest"
  --ssl                        Encrypted Transport using SSL
  --zlib                       Wrap Transport with Zlib
  -n=arg | --testloops=arg (1) Number of Tests
  -t=arg | --threads=arg (1)   Number of Test threads
""")
  }

  open override func processArgument(name: String, value: String?) throws {
    switch name {
      case "--host":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        host = value!
      case "-n", "--testloops":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        testLoops = Int(value!) 
        guard testLoops != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
      case "-t", "--threads":
        guard value != nil else { throw ParserError.missingParameter(argument: name) }
        threads = Int(value!) 
        guard threads != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
      default:
        try super.processArgument(name: name, value: value)
    }
  }

  open override func fillDefaults() {
    super.fillDefaults()

    if host == nil {
      host = "localhost"
    }

    if testLoops == nil {
      testLoops = 1
    }

    if threads == nil {
      threads = 4
    }
  }
}

public class TestServerParameters: ParametersBase {
  public var serverType: ServerType?
  public var processorEvents = false
  public var workers: Int?

  public func printHelp() {
    print("""
Allowed options:
  -h | --help                 produce help message
  --port=arg (=9090)          Port number to listen
  --domain-socket=arg         Unix Domain Socket (e.g. /tmp/ThriftTest.thrift)
  --named-pipe=arg            Windows Named Pipe (e.g. MyThriftPipe)
  --server-type=arg (=simple) type of server, "simple", "thread-pool",
                              "threaded", or "nonblocking"
  --transport=arg (=buffered) transport: buffered, framed, http, anonpipe, zlib
  --protocol=arg (=binary)    protocol: binary, compact, header, json
  --multiplex                 Add TMultiplexedProtocol service name "ThriftTest"
  --abstract-namespace        Create the domain socket in the Abstract Namespace 
                              (no connection with filesystem pathnames)
  --ssl                       Encrypted Transport using SSL
  --zlib                      Wrapped Transport using Zlib
  --processor-events          processor-events
  -n=arg | --workers=arg (=4) Number of thread pools workers. Only valid for
                              thread-pool server type
""")
  }

  open override func processArgument(name: String, value: String?) throws {
    switch name {
    case "--server-type":
      guard value != nil else { throw ParserError.missingParameter(argument: name) }
      serverType = ServerType(rawValue: value!)
      guard serverType != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
    case "--processor-events":
      processorEvents = true
    case "-n", "--workers":
      guard value != nil else { throw ParserError.missingParameter(argument: name) }
      workers = Int(value!) 
      guard workers != nil else { throw ParserError.invalidParameter(argument: name, parameter: value!) }
    default:
      try super.processArgument(name: name, value: value)
    }
  }
  
  open override func fillDefaults() {
    super.fillDefaults()

    if serverType == nil {
      serverType = .simple
    }

    if workers == nil {
      workers = 4
    }
  }

  open override func checkSupported() throws {
    try super.checkSupported()
    guard serverType == .simple else { throw ParserError.unsupportedOption }
  }
}

