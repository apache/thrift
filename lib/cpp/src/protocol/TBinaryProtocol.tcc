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

#ifndef _THRIFT_PROTOCOL_TBINARYPROTOCOL_TCC_
#define _THRIFT_PROTOCOL_TBINARYPROTOCOL_TCC_ 1

#include "TBinaryProtocol.h"

#include <limits>


namespace apache { namespace thrift { namespace protocol {

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeMessageBegin(const std::string& name,
                                                         const TMessageType messageType,
                                                         const int32_t seqid) {
  if (this->strict_write_) {
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

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeMessageEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeStructBegin(const char* name) {
  (void) name;
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeStructEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeFieldBegin(const char* name,
                                                       const TType fieldType,
                                                       const int16_t fieldId) {
  (void) name;
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)fieldType);
  wsize += writeI16(fieldId);
  return wsize;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeFieldEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeFieldStop() {
  return
    writeByte((int8_t)T_STOP);
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeMapBegin(const TType keyType,
                                                     const TType valType,
                                                     const uint32_t size) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)keyType);
  wsize += writeByte((int8_t)valType);
  wsize += writeI32((int32_t)size);
  return wsize;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeMapEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeListBegin(const TType elemType,
                                                      const uint32_t size) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t) elemType);
  wsize += writeI32((int32_t)size);
  return wsize;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeListEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeSetBegin(const TType elemType,
                                                     const uint32_t size) {
  uint32_t wsize = 0;
  wsize += writeByte((int8_t)elemType);
  wsize += writeI32((int32_t)size);
  return wsize;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeSetEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeBool(const bool value) {
  uint8_t tmp =  value ? 1 : 0;
  this->trans_->write(&tmp, 1);
  return 1;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeByte(const int8_t byte) {
  this->trans_->write((uint8_t*)&byte, 1);
  return 1;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeI16(const int16_t i16) {
  int16_t net = (int16_t)htons(i16);
  this->trans_->write((uint8_t*)&net, 2);
  return 2;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeI32(const int32_t i32) {
  int32_t net = (int32_t)htonl(i32);
  this->trans_->write((uint8_t*)&net, 4);
  return 4;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeI64(const int64_t i64) {
  int64_t net = (int64_t)htonll(i64);
  this->trans_->write((uint8_t*)&net, 8);
  return 8;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeDouble(const double dub) {
  BOOST_STATIC_ASSERT(sizeof(double) == sizeof(uint64_t));
  BOOST_STATIC_ASSERT(std::numeric_limits<double>::is_iec559);

  uint64_t bits = bitwise_cast<uint64_t>(dub);
  bits = htonll(bits);
  this->trans_->write((uint8_t*)&bits, 8);
  return 8;
}


template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeString(const std::string& str) {
  uint32_t size = str.size();
  uint32_t result = writeI32((int32_t)size);
  if (size > 0) {
    this->trans_->write((uint8_t*)str.data(), size);
  }
  return result + size;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::writeBinary(const std::string& str) {
  return TBinaryProtocolT<Transport_>::writeString(str);
}

/**
 * Reading functions
 */

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readMessageBegin(std::string& name,
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
    if (this->strict_read_) {
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

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readMessageEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readStructBegin(std::string& name) {
  name = "";
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readStructEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readFieldBegin(std::string& name,
                                                      TType& fieldType,
                                                      int16_t& fieldId) {
  (void) name;
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

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readFieldEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readMapBegin(TType& keyType,
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
  } else if (this->container_limit_ && sizei > this->container_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }
  size = (uint32_t)sizei;
  return result;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readMapEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readListBegin(TType& elemType,
                                                     uint32_t& size) {
  int8_t e;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(e);
  elemType = (TType)e;
  result += readI32(sizei);
  if (sizei < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  } else if (this->container_limit_ && sizei > this->container_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }
  size = (uint32_t)sizei;
  return result;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readListEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readSetBegin(TType& elemType,
                                                    uint32_t& size) {
  int8_t e;
  uint32_t result = 0;
  int32_t sizei;
  result += readByte(e);
  elemType = (TType)e;
  result += readI32(sizei);
  if (sizei < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  } else if (this->container_limit_ && sizei > this->container_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }
  size = (uint32_t)sizei;
  return result;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readSetEnd() {
  return 0;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readBool(bool& value) {
  uint8_t b[1];
  this->trans_->readAll(b, 1);
  value = *(int8_t*)b != 0;
  return 1;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readByte(int8_t& byte) {
  uint8_t b[1];
  this->trans_->readAll(b, 1);
  byte = *(int8_t*)b;
  return 1;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readI16(int16_t& i16) {
  union bytes {
    uint8_t b[2];
    int16_t all;
  } theBytes;
  this->trans_->readAll(theBytes.b, 2);
  i16 = (int16_t)ntohs(theBytes.all);
  return 2;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readI32(int32_t& i32) {
  union bytes {
    uint8_t b[4];
    int32_t all;
  } theBytes;
  this->trans_->readAll(theBytes.b, 4);
  i32 = (int32_t)ntohl(theBytes.all);
  return 4;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readI64(int64_t& i64) {
  union bytes {
    uint8_t b[8];
    int64_t all;
  } theBytes;
  this->trans_->readAll(theBytes.b, 8);
  i64 = (int64_t)ntohll(theBytes.all);
  return 8;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readDouble(double& dub) {
  BOOST_STATIC_ASSERT(sizeof(double) == sizeof(uint64_t));
  BOOST_STATIC_ASSERT(std::numeric_limits<double>::is_iec559);

  union bytes {
    uint8_t b[8];
    uint64_t all;
  } theBytes;
  this->trans_->readAll(theBytes.b, 8);
  theBytes.all = ntohll(theBytes.all);
  dub = bitwise_cast<double>(theBytes.all);
  return 8;
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readString(std::string& str) {
  uint32_t result;
  int32_t size;
  result = readI32(size);
  return result + readStringBody(str, size);
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readBinary(std::string& str) {
  return TBinaryProtocolT<Transport_>::readString(str);
}

template <class Transport_>
uint32_t TBinaryProtocolT<Transport_>::readStringBody(std::string& str,
                                                      int32_t size) {
  uint32_t result = 0;

  // Catch error cases
  if (size < 0) {
    throw TProtocolException(TProtocolException::NEGATIVE_SIZE);
  }
  if (this->string_limit_ > 0 && size > this->string_limit_) {
    throw TProtocolException(TProtocolException::SIZE_LIMIT);
  }

  // Catch empty string case
  if (size == 0) {
    str = "";
    return result;
  }

  // Try to borrow first
  const uint8_t* borrow_buf;
  uint32_t got = size;
  if ((borrow_buf = this->trans_->borrow(NULL, &got))) {
    str.assign((const char*)borrow_buf, size);
    this->trans_->consume(size);
    return size;
  }

  // Use the heap here to prevent stack overflow for v. large strings
  if (size > this->string_buf_size_ || this->string_buf_ == NULL) {
    void* new_string_buf = std::realloc(this->string_buf_, (uint32_t)size);
    if (new_string_buf == NULL) {
      throw std::bad_alloc();
    }
    this->string_buf_ = (uint8_t*)new_string_buf;
    this->string_buf_size_ = size;
  }
  this->trans_->readAll(this->string_buf_, size);
  str = std::string((char*)this->string_buf_, size);
  return (uint32_t)size;
}

}}} // apache::thrift::protocol

#endif // #ifndef _THRIFT_PROTOCOL_TBINARYPROTOCOL_TCC_
