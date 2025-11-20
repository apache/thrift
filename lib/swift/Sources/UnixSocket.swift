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

private struct Sys {
#if os(Linux)
  static let read = Glibc.read
  static let write = Glibc.write
  static let close = Glibc.close
  static let socket = Glibc.socket
  static let connect = Glibc.connect
  static let bind = Glibc.bind
  static let recv = Glibc.recv
#else
  static let read = Darwin.read
  static let write = Darwin.write
  static let close = Darwin.close
  static let socket = Darwin.socket
  static let connect = Darwin.connect
  static let bind = Darwin.bind
  static let recv = Darwin.recv
#endif
}


public class UnixSocket {
  public var fd: Int32
  private var socketAddress: sockaddr_un

  public init(path: String) {
    socketAddress = sockaddr_un()
    socketAddress.sun_family = sa_family_t(AF_UNIX)

    let lengthOfPath = path.withCString { Int(strlen($0)) }

    guard lengthOfPath < MemoryLayout.size(ofValue: socketAddress.sun_path) else {
      fatalError()
    }

    _ = withUnsafeMutablePointer(to: &socketAddress.sun_path.0) { ptr in
      path.withCString {
        strncpy(ptr, $0, lengthOfPath)
      }
    }

#if os(Linux)
    fd = Sys.socket(AF_UNIX, 1 /*SOCK_STREAM*/, 0);
#else
    fd = Sys.socket(AF_UNIX, SOCK_STREAM, 0);
#endif

  }
  public func connect() -> Int32 {
    let socketAddressCasted = withUnsafePointer(to: &socketAddress) {
      $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
        return $0
      }
    }
    return Sys.connect(fd, socketAddressCasted, socklen_t(MemoryLayout<sockaddr_un>.size(ofValue: socketAddress)))
  }
  public func bind() -> Int32 {
    let socketAddressCasted = withUnsafePointer(to: &socketAddress) {
      $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
        return $0
      }
    }
    return Sys.bind(fd, socketAddressCasted, socklen_t(MemoryLayout<sockaddr_un>.size(ofValue: socketAddress)))
  }
}
