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

#include "TBinaryProtocol.h"

#include <limits>

using std::string;

namespace apache { namespace thrift { namespace protocol {

uint32_t TBinaryProtocol::writeMessageBegin(const std::string& name,
                                            const TMessageType messageType,
                                            const int32_t seqid) {
  if (strict_write_) {
    int32_t version = (VERSION_1) | ((int32_t)messageType);
    uint32_t wsize = 0;
    wsize += writeI32(version);
    wsize += writeString(name);
    wsize += writeI32(seqid);
    return wsize;
  } else {
    uint32_t wsize = 0;
    wsize += writeString(name);
    wsize += writeByte((int8_t)messageType);
    wsize += writeI32(seqid);
    return wsize;
  }
}

uint32_t TBinaryProtocol::writeMessageEnd() {
  return 0;
}

uint32_t TBinaryProtocol::writeStructBegin(const char* name) {
  return 0;
}

uint32_t TBinaryProtocol::writeStructEnd() {
  return 0;
}

uint32_t TBinaryProtocol::writeFieldBegin(const char* name,
                                          const TType fieldType,
                                          const int16_t fieldId) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)fieldType);
  wsize += writeI16(fieldId);
  return wsize;
}

uint32_t TBinaryProtocol::writeFieldEnd() {
  return 0;
}

uint32_t TBinaryProtocol::writeFieldStop() {
  return
    writeByte((int8_t)T_STOP);
}

uint32_t TBinaryProtocol::writeMapBegin(const TType keyType,
                                        const TType valType,
                                        const uint32_t size) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)keyType);
  wsize += writeByte((int8_t)valType);
  wsize += writeI32((int32_t)size);
  return wsize;
}

uint32_t TBinaryProtocol::writeMapEnd() {
  return 0;
}

uint32_t TBinaryProtocol::writeListBegin(const TType elemType,
                                         const uint32_t size) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t) elemType);
  wsize += writeI32((int32_t)size);
  return wsize;
}

uint32_t TBinaryProtocol::writeListEnd() {
  return 0;
}

uint32_t TBinaryProtocol::writeSetBegin(const TType elemType,
                                        const uint32_t size) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)elemType);
  wsize += writeI32((int32_t)size);
  return wsize;
}

uint32_t TBinaryProtocol::writeSetEnd() {
  return 0;
}

uint32_t TBinaryProtocol::writeBool(const bool value) {
  uint8_t tmp =  value ? 1 : 0;
  trans_->write(&tmp, 1);
  return 1;
}

uint32_t TBinaryProtocol::writeByte(const int8_t byte) {
  trans_->write((uint8_t*)&byte, 1);
  return 1;
}

uint32_t TBinaryProtocol::writeI16(const int16_t i16) {
  int16_t net = (int16_t)htons(i16);
  trans_->write((uint8_t*)&net, 2);
  return 2;
}

uint32_t TBinaryProtocol::writeI32(const int32_t i32) {
  int32_t net = (int32_t)htonl(i32);
  trans_->write((uint8_t*)&net, 4);
  return 4;
}

uint32_t TBinaryProtocol::writeI64(const int64_t i64) {
  int64_t net = (int64_t)htonll(i64);
  trans_->write((uint8_t*)&net, 8);
  return 8;
}

uint32_t TBinaryProtocol::writeDouble(const double dub) {
  BOOST_STATIC_ASSERT(sizeof(double) == sizeof(uint64_t));
  BOOST_STATIC_ASSERT(std::numeric_limits<double>::is_iec559);

  uint64_t bits = bitwise_cast<uint64_t>(dub);
  bits = htonll(bits);
  trans_->write((uint8_t*)&bits, 8);
  return 8;
}


uint32_t TBinaryProtocol::writeString(const string& str) {
  uint32_t size = str.size();
  uint32_t result = writeI32((int32_t)size);
  if (size > 0) {
    trans_->write((uint8_t*)str.data(), size);
  }
  return result + size;
}

uint32_t TBinaryProtocol::writeBinary(const string& str) {
  return TBinaryProtocol::writeString(str);
}

/**
 * Reading functions
 */

uint32_t TBinaryProtocol::readMessageBegin(std::string& name,
                                           TMessageType& messageType,
                                           int32_t& seqid) {
  uint32_t result = 0;
  int32_t sz;
  result += readI32(sz);

  if (sz < 0) {
    // Check for correct version number
    int32_t version = sz & VERSION_MASK;
    if (version != VERSION_1) {
      throw TProtocolException(TProtocolException::BAD_VERSION, "Bad version identifier");
    }
    messageType = (TMessageType)(sz & 0x000000ff);
    result += readString(name);
    result += readI32(seqid);
  } else {
    if (strict_read_) {
      throw TProtocolException(TProtocolException::BAD_VERSION, "No version identifier... old protocol client in strict mode?");
    } else {
      // Handle pre-versioned input
      int8_t type;
      result += readStringBody(name, sz);
      result += readByte(type);
      messageType = (TMessageType)type;
      result += readI32(seqid);
    }
  }
  return result;
}

uint32_t TBinaryProtocol::readMessageEnd() {
  return 0;
}

uint32_t TBinaryProtocol::readStructBegin(string& name) {
  name = "";
  return 0;
}

uint32_t TBinaryProtocol::readStructEnd() {
  return 0;
}

uint32_t TBinaryProtocol::readFieldBegin(string& name,
                                         TType& fieldType,
                                         int16_t& fieldId) {
  uint32_t result = 0;
  int8_t type;
  result += readByte(type);
  fieldType = (TType)type;
  if (fieldType == T_STOP) {
    fieldId = 0;
    return result;
  }
  result += readI16(fieldId);
  return result;
}

uint32_t TBinaryProtocol::readFieldEnd() {
  return 0;
}

uint32_t TBinaryProtocol::readMapBegin(TType& keyType,
                                       TType& valType,
                                       uint32_t& size) {
  int8_t k, v;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(k);
  keyType = (TType)k;
  result += readByte(v);
  valType = (TType)v;
  result += readI32(sizei);
  if (sizei < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  } else if (container_limit_ && sizei > container_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }
  size = (uint32_t)sizei;
  return result;
}

uint32_t TBinaryProtocol::readMapEnd() {
  return 0;
}

uint32_t TBinaryProtocol::readListBegin(TType& elemType,
                                        uint32_t& size) {
  int8_t e;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(e);
  elemType = (TType)e;
  result += readI32(sizei);
  if (sizei < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  } else if (container_limit_ && sizei > container_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }
  size = (uint32_t)sizei;
  return result;
}

uint32_t TBinaryProtocol::readListEnd() {
  return 0;
}

uint32_t TBinaryProtocol::readSetBegin(TType& elemType,
                                       uint32_t& size) {
  int8_t e;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(e);
  elemType = (TType)e;
  result += readI32(sizei);
  if (sizei < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  } else if (container_limit_ && sizei > container_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }
  size = (uint32_t)sizei;
  return result;
}

uint32_t TBinaryProtocol::readSetEnd() {
  return 0;
}

uint32_t TBinaryProtocol::readBool(bool& value) {
  uint8_t b[1];
  trans_->readAll(b, 1);
  value = *(int8_t*)b != 0;
  return 1;
}

uint32_t TBinaryProtocol::readByte(int8_t& byte) {
  uint8_t b[1];
  trans_->readAll(b, 1);
  byte = *(int8_t*)b;
  return 1;
}

uint32_t TBinaryProtocol::readI16(int16_t& i16) {
  uint8_t b[2];
  trans_->readAll(b, 2);
  i16 = *(int16_t*)b;
  i16 = (int16_t)ntohs(i16);
  return 2;
}

uint32_t TBinaryProtocol::readI32(int32_t& i32) {
  uint8_t b[4];
  trans_->readAll(b, 4);
  i32 = *(int32_t*)b;
  i32 = (int32_t)ntohl(i32);
  return 4;
}

uint32_t TBinaryProtocol::readI64(int64_t& i64) {
  uint8_t b[8];
  trans_->readAll(b, 8);
  i64 = *(int64_t*)b;
  i64 = (int64_t)ntohll(i64);
  return 8;
}

uint32_t TBinaryProtocol::readDouble(double& dub) {
  BOOST_STATIC_ASSERT(sizeof(double) == sizeof(uint64_t));
  BOOST_STATIC_ASSERT(std::numeric_limits<double>::is_iec559);

  uint64_t bits;
  uint8_t b[8];
  trans_->readAll(b, 8);
  bits = *(uint64_t*)b;
  bits = ntohll(bits);
  dub = bitwise_cast<double>(bits);
  return 8;
}

uint32_t TBinaryProtocol::readString(string& str) {
  uint32_t result;
  int32_t size;
  result = readI32(size);
  return result + readStringBody(str, size);
}

uint32_t TBinaryProtocol::readBinary(string& str) {
  return TBinaryProtocol::readString(str);
}

uint32_t TBinaryProtocol::readStringBody(string& str, int32_t size) {
  uint32_t result = 0;

  // Catch error cases
  if (size < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  }
  if (string_limit_ > 0 && size > string_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }

  // Catch empty string case
  if (size == 0) {
    str = "";
    return result;
  }

  // Use the heap here to prevent stack overflow for v. large strings
  if (size > string_buf_size_ || string_buf_ == NULL) {
    void* new_string_buf = std::realloc(string_buf_, (uint32_t)size);
    if (new_string_buf == NULL) {
      throw TProtocolException(TProtocolException::UNKNOWN, "Out of memory in TBinaryProtocol::readString");
    }
    string_buf_ = (uint8_t*)new_string_buf;
    string_buf_size_ = size;
  }
  trans_->readAll(string_buf_, size);
  str = string((char*)string_buf_, size);
  return (uint32_t)size;
}

}}} // apache::thrift::protocol
