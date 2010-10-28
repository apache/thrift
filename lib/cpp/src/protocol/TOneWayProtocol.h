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

#ifndef _THRIFT_PROTOCOL_TONEWAYPROTOCOL_H_
#define _THRIFT_PROTOCOL_TONEWAYPROTOCOL_H_ 1

#include "TProtocol.h"

namespace apache { namespace thrift { namespace protocol {

/**
 * Abstract class for implementing a protocol that can only be written,
 * not read.
 *
 */
class TWriteOnlyProtocol : public TProtocol {
 public:
  /**
   * @param subclass_name  The name of the concrete subclass.
   */
  TWriteOnlyProtocol(boost::shared_ptr<TTransport> trans,
                     const std::string& subclass_name)
    : TProtocol(trans)
    , subclass_(subclass_name)
  {}

  // All writing functions remain abstract.

  /**
   * Reading functions all throw an exception.
   */

  uint32_t readMessageBegin(std::string& name,
                            TMessageType& messageType,
                            int32_t& seqid) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readMessageEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readStructBegin(std::string& name) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readStructEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readFieldBegin(std::string& name,
                          TType& fieldType,
                          int16_t& fieldId) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readFieldEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readMapBegin(TType& keyType,
                        TType& valType,
                        uint32_t& size) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readMapEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readListBegin(TType& elemType,
                         uint32_t& size) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readListEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readSetBegin(TType& elemType,
                        uint32_t& size) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readSetEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readBool(bool& value) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readByte(int8_t& byte) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readI16(int16_t& i16) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readI32(int32_t& i32) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readI64(int64_t& i64) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readDouble(double& dub) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readString(std::string& str) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

  uint32_t readBinary(std::string& str) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support reading (yet).");
  }

 private:
  std::string subclass_;
};


/**
 * Abstract class for implementing a protocol that can only be read,
 * not written.
 *
 */
class TReadOnlyProtocol : public TProtocol {
 public:
  /**
   * @param subclass_name  The name of the concrete subclass.
   */
  TReadOnlyProtocol(boost::shared_ptr<TTransport> trans,
                    const std::string& subclass_name)
    : TProtocol(trans)
    , subclass_(subclass_name)
  {}

  // All reading functions remain abstract.

  /**
   * Writing functions all throw an exception.
   */

  uint32_t writeMessageBegin(const std::string& name,
                             const TMessageType messageType,
                             const int32_t seqid) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeMessageEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }


  uint32_t writeStructBegin(const char* name) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeStructEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeFieldBegin(const char* name,
                           const TType fieldType,
                           const int16_t fieldId) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeFieldEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeFieldStop() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeMapBegin(const TType keyType,
                         const TType valType,
                         const uint32_t size) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeMapEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeListBegin(const TType elemType,
                          const uint32_t size) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeListEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeSetBegin(const TType elemType,
                         const uint32_t size) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeSetEnd() {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeBool(const bool value) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeByte(const int8_t byte) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeI16(const int16_t i16) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeI32(const int32_t i32) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeI64(const int64_t i64) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeDouble(const double dub) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeString(const std::string& str) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

  uint32_t writeBinary(const std::string& str) {
    throw TProtocolException(TProtocolException::NOT_IMPLEMENTED,
        subclass_ + " does not support writing (yet).");
  }

 private:
  std::string subclass_;
};

}}} // apache::thrift::protocol

#endif // #ifndef _THRIFT_PROTOCOL_TBINARYPROTOCOL_H_
