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

import Foundation

class TProtocolDecorator: TProtocol {

  private let proto: TProtocol
  var transport: TTransport

  init(proto: TProtocol) {
    self.proto = proto
    self.transport = proto.transport
  }

  required init(on transport: TTransport) {
    fatalError("init(on:) has not been implemented")
  }

  func readMessageBegin() throws -> (String, TMessageType, Int32) {
    return try proto.readMessageBegin()
  }

  func readMessageEnd() throws {
    try proto.readMessageEnd()
  }

  func readStructBegin() throws -> String {
    return try proto.readStructBegin()
  }

  func readStructEnd() throws {
    try proto.readStructEnd()
  }

  func readFieldBegin() throws -> (String, TType, Int32) {
    return try proto.readFieldBegin()
  }

  func readFieldEnd() throws {
    try proto.readFieldEnd()
  }

  func readMapBegin() throws -> (TType, TType, Int32) {
    return try proto.readMapBegin()
  }

  func readMapEnd() throws {
    try proto.readMapEnd()
  }

  func readSetBegin() throws -> (TType, Int32) {
    return try proto.readSetBegin()
  }

  func readSetEnd() throws {
    try proto.readSetEnd()
  }

  func readListBegin() throws -> (TType, Int32) {
    return try proto.readListBegin()
  }

  func readListEnd() throws {
    try proto.readListEnd()
  }

  func read() throws -> String {
    return try proto.read()
  }

  func read() throws -> Bool {
    return try proto.read()
  }

  func read() throws -> UInt8 {
    return try proto.read()
  }

  func read() throws -> Int16 {
    return try proto.read()
  }

  func read() throws -> Int32 {
    return try proto.read()
  }

  func read() throws -> Int64 {
    return try proto.read()
  }

  func read() throws -> Double {
    return try proto.read()
  }

  func read() throws -> Data {
    return try proto.read()
  }

  func writeMessageBegin(name: String, type messageType: TMessageType, sequenceID: Int32) throws {
    try proto.writeMessageBegin(name: name, type: messageType, sequenceID: sequenceID)
  }

  func writeMessageEnd() throws {
    try proto.writeMessageEnd()
  }

  func writeStructBegin(name: String) throws {
    try proto.writeStructBegin(name: name)
  }

  func writeStructEnd() throws {
    try proto.writeStructEnd()
  }

  func writeFieldBegin(name: String, type fieldType: TType, fieldID: Int32) throws {
    try proto.writeFieldBegin(name: name, type: fieldType, fieldID: fieldID)
  }

  func writeFieldStop() throws {
    try proto.writeFieldStop()
  }

  func writeFieldEnd() throws {
    try proto.writeFieldEnd()
  }

  func writeMapBegin(keyType: TType, valueType: TType, size: Int32) throws {
    try proto.writeMapBegin(keyType: keyType, valueType: valueType, size: size)
  }

  func writeMapEnd() throws {
    try proto.writeMapEnd()
  }

  func writeSetBegin(elementType: TType, size: Int32) throws {
    try proto.writeSetBegin(elementType: elementType, size: size)
  }

  func writeSetEnd() throws {
    try proto.writeSetEnd()
  }

  func writeListBegin(elementType: TType, size: Int32) throws {
    try proto.writeListBegin(elementType: elementType, size: size)
  }

  func writeListEnd() throws {
    try proto.writeListEnd()
  }

  func write(_ value: String) throws {
    try proto.write(value)
  }

  func write(_ value: Bool) throws {
    try proto.write(value)
  }

  func write(_ value: UInt8) throws {
    try proto.write(value)
  }

  func write(_ value: Int16) throws {
    try proto.write(value)
  }

  func write(_ value: Int32) throws {
    try proto.write(value)
  }

  func write(_ value: Int64) throws {
    try proto.write(value)
  }

  func write(_ value: Double) throws {
    try proto.write(value)
  }

  func write(_ value: Data) throws {
    try proto.write(value)
  }
}
