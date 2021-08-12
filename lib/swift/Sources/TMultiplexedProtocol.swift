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

extension String {
    static let multiplexSeparator = ":"
}

/**
 `TMultiplexedProtocol` is a protocol-independent concrete decorator
 that allows a Thrift client to communicate with a multiplexing Thrift server,
 by prepending the service name to the function name during function calls.

 - Note: THIS IS NOT USED BY SERVERS.  On the server, use `TMultiplexedProcessor` to handle request
 from a multiplexing client.
 */
public class TMultiplexedProtocol<Protocol: TProtocol>: TWrappedProtocol<Protocol> {

  public var serviceName = ""
  
  public convenience init(on transport: TTransport, serviceName: String) {
    self.init(on: transport)
    self.serviceName = serviceName    
  }

  override public func writeMessageBegin(name: String,
                                         type messageType: TMessageType,
                                         sequenceID: Int32) throws {
    switch messageType {
    case .call, .oneway:
      var serviceFunction = serviceName
      serviceFunction += serviceName == "" ? "" : .multiplexSeparator
      serviceFunction += name
      return try super.writeMessageBegin(name: serviceFunction,
                                         type: messageType,
                                         sequenceID: sequenceID)
    default:
      return try super.writeMessageBegin(name: name,
                                         type: messageType,
                                         sequenceID: sequenceID)
    }
  }
}
