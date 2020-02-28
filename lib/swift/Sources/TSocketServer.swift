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

#if os(OSX) || os(iOS) || os(watchOS) || os(tvOS)
  import Darwin
#elseif os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
  import Glibc
  import Dispatch
#endif

import Foundation
import CoreFoundation

public let TSocketServerClientConnectionFinished = "TSocketServerClientConnectionFinished"
public let TSocketServerProcessorKey = "TSocketServerProcessor"
public let TSocketServerTransportKey = "TSocketServerTransport"

open class TSocketServer<InProtocol: TProtocol, OutProtocol: TProtocol, Processor: TProcessor> {
  var socketFileHandle: FileHandle
  var processingQueue =  DispatchQueue(label: "TSocketServer.processing",
                                       qos: .background,
                                       attributes: .concurrent)
  let processor: Processor

  public init(port: Int,
              inProtocol: InProtocol.Type,
              outProtocol: OutProtocol.Type,
              processor: Processor) throws {
    self.processor = processor

    // create a socket
    var fd: Int32 = -1
    #if os(Linux)
      let sock = CFSocketCreate(kCFAllocatorDefault, PF_INET, Int32(SOCK_STREAM.rawValue), Int32(IPPROTO_TCP), 0, nil, nil)
    #else
      let sock = CFSocketCreate(kCFAllocatorDefault, PF_INET, SOCK_STREAM, IPPROTO_TCP, 0, nil, nil)
    #endif
    if sock != nil {
      CFSocketSetSocketFlags(sock, CFSocketGetSocketFlags(sock) & ~CFOptionFlags(kCFSocketCloseOnInvalidate))

      fd = CFSocketGetNative(sock)
      var yes = 1
      setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, UInt32(MemoryLayout<Int>.size))
      let inPort = in_port_t(UInt16(truncatingIfNeeded: port).bigEndian)
      #if os(Linux)
        var addr = sockaddr_in(sin_family: sa_family_t(AF_INET),
                               sin_port: inPort,
                               sin_addr: in_addr(s_addr: in_addr_t(0)),
                               sin_zero: (0, 0, 0, 0, 0, 0, 0, 0))
      #else
        var addr = sockaddr_in(sin_len: UInt8(MemoryLayout<sockaddr_in>.size),
                               sin_family: sa_family_t(AF_INET),
                               sin_port: inPort,
                               sin_addr: in_addr(s_addr: in_addr_t(0)),
                               sin_zero: (0, 0, 0, 0, 0, 0, 0, 0))
      #endif

      let ptr = withUnsafePointer(to: &addr) {
        return UnsafePointer<UInt8>(OpaquePointer($0))
      }

      let address = Data(bytes: ptr, count: MemoryLayout<sockaddr_in>.size)

      let cfaddr = address.withUnsafeBytes {
        CFDataCreateWithBytesNoCopy(kCFAllocatorDefault, $0.bindMemory(to: UInt8.self).baseAddress!, address.count, kCFAllocatorNull)
      }
      if CFSocketSetAddress(sock, cfaddr) != CFSocketError.success { //kCFSocketSuccess {
        CFSocketInvalidate(sock)
        print("TSocketServer: Could not bind to address")
        throw TTransportError(error: .notOpen, message: "Could not bind to address")
      }

    } else {
      print("TSocketServer: No server socket")
      throw TTransportError(error: .notOpen, message: "Could not create socket")
    }

    // wrap it in a file handle so we can get messages from it
    socketFileHandle = FileHandle(fileDescriptor: fd, closeOnDealloc: true)

    // throw away our socket
    CFSocketInvalidate(sock)

    // register for notifications of accepted incoming connections
    _ = NotificationCenter.default.addObserver(forName: .NSFileHandleConnectionAccepted,
                                               object: nil, queue: nil) {
                                                [weak self] notification in
                                                guard let strongSelf = self else { return }
                                                guard let clientSocket = notification.userInfo?[NSFileHandleNotificationFileHandleItem] as? FileHandle else { return }
                                                strongSelf.connectionAccepted(clientSocket)
    }

    // tell socket to listen
    socketFileHandle.acceptConnectionInBackgroundAndNotify()

    print("TSocketServer: Listening on TCP port \(port)")
  }

  deinit {
    NotificationCenter.default.removeObserver(self)
  }

  func connectionAccepted(_ clientSocket: FileHandle) {
    // Now that we have a client connected, handle the request on queue
    processingQueue.async {
      self.handleClientConnection(clientSocket)
    }

    // continue accepting connections
    socketFileHandle.acceptConnectionInBackgroundAndNotify()
  }

  open func createTransport(fileHandle: FileHandle) -> TTransport {
    return TFileHandleTransport(fileHandle: fileHandle)
  }

  func handleClientConnection(_ clientSocket: FileHandle) {
    let transport = createTransport(fileHandle: clientSocket)
    let inProtocol = InProtocol(on: transport)
    let outProtocol = OutProtocol(on: transport)

    do {
      while true {
        try processor.process(on: inProtocol, outProtocol: outProtocol)
      }
    } catch let error {
      print("Error processign request: \(error)")
    }
    DispatchQueue.main.async {
      NotificationCenter.default
        .post(name: Notification.Name(rawValue: TSocketServerClientConnectionFinished),
              object: self,
              userInfo: [TSocketServerProcessorKey: self.processor,
                         TSocketServerTransportKey: transport])
    }
  }
}

public class TFramedSocketServer<InProtocol: TProtocol, OutProtocol: TProtocol, Processor: TProcessor>: TSocketServer<InProtocol, OutProtocol, Processor> {
  open override func createTransport(fileHandle: FileHandle) -> TTransport {
    return TFramedTransport(transport: super.createTransport(fileHandle: fileHandle))
  }
}
