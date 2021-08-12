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

/**
 `TMultiplexedProcessor` is a `TProcessor` allowing
 a single `TServer` to provide multiple services.

 To do so, you instantiate the processor and then register additional
 processors with it, as shown in the following example:

 let processor = MultiplexedProcessor()

 processor.register(CalculatorProcessor(service: CalculatorService()), for: "Calculator")
 processor.register(WeatherProcessor(service: CalculatorService()), for: "Weather")

 let server = TPerfectServer(port: 9090, processor: processor, TCompactProtocol.self, TCompactProtocol.self)
 try server.start()

 */
public class MultiplexedProcessor: TProcessor {

  enum Error: Swift.Error {
    case incompatibleMessageType(TMessageType)
    case missingProcessor(String)
    case missingDefaultProcessor
  }

  private var processors = [String: TProcessor]()
  private var defaultProcessor: TProcessor?

  public init(defaultProcessor: TProcessor? = nil) {
    self.defaultProcessor = defaultProcessor
  }

  public func register(defaultProcessor processor: TProcessor) {
    defaultProcessor = processor
  }

  public func register(processor: TProcessor, for service: String) {
    processors[service] = processor
  }

  public func process(on inProtocol: TProtocol, outProtocol: TProtocol) throws {
    let message = try inProtocol.readMessageBegin()
    guard message.1 == .call || message.1 == .oneway else { throw Error.incompatibleMessageType(message.1) }
    if let separatorIndex = message.0.firstIndex(of: Character(.multiplexSeparator)) {
      let serviceName = String(message.0.prefix(upTo: separatorIndex))
      let messageName = String(message.0.suffix(from: message.0.index(after: separatorIndex)))
      guard let processor = processors[serviceName] else { throw Error.missingProcessor(serviceName)}
      let storedMessage = StoredMessage(message: (messageName, message.1, message.2), proto: inProtocol)
      try processor.process(on: storedMessage, outProtocol: outProtocol)
    } else {
      guard let processor = defaultProcessor else { throw Error.missingDefaultProcessor }
      try processor.process(on: inProtocol, outProtocol: outProtocol)
    }
  }
}

private final class StoredMessage: TProtocolDecorator {

  private let message: (String, TMessageType, Int32)

  init(message: (String, TMessageType, Int32), proto: TProtocol) {
    self.message = message
    super.init(proto: proto)
  }

  required init(on transport: TTransport) {
    fatalError("init(on:) has not been implemented")
  }

  override func readMessageBegin() throws -> (String, TMessageType, Int32) {
    return message
  }
}
