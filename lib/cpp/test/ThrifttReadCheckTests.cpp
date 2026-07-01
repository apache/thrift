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

#define  MAX_MESSAGE_SIZE  2

#include <boost/test/unit_test.hpp>
#include <iostream>
#include <climits>
#include <vector>
#include <thrift/TConfiguration.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/protocol/TCompactProtocol.h>
#include <thrift/protocol/TJSONProtocol.h>
#include <thrift/Thrift.h>
#include <memory>
#include <thrift/transport/TTransportUtils.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/THeaderTransport.h>
#include <thrift/transport/TSimpleFileTransport.h>
#include <thrift/transport/TFileTransport.h>
#include <thrift/protocol/TEnum.h>
#include <thrift/protocol/TList.h>
#include <thrift/protocol/TSet.h>
#include <thrift/protocol/TMap.h>

BOOST_AUTO_TEST_SUITE(ThriftReadCheckExceptionTest)

using apache::thrift::TConfiguration;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TCompactProtocol;
using apache::thrift::protocol::TJSONProtocol;
using apache::thrift::protocol::TType;
using apache::thrift::transport::TPipedTransport;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TSimpleFileTransport;
using apache::thrift::transport::TFileTransport;
using apache::thrift::transport::TFDTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TBufferedTransport;
using apache::thrift::transport::TFramedTransport;
using std::shared_ptr;
using std::string;
using std::memset;
using namespace apache::thrift;
using namespace apache::thrift::protocol;


BOOST_AUTO_TEST_CASE(test_tmemorybuffer_read_check_exception) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(MAX_MESSAGE_SIZE));
  TMemoryBuffer trans_out(config);
  uint8_t buffer[6] = {1, 2, 3, 4, 5, 6};
  trans_out.write((const uint8_t*)buffer, sizeof(buffer));
  trans_out.close();

  TMemoryBuffer trans_in(config);
  memset(buffer, 0, sizeof(buffer));
  BOOST_CHECK_THROW(trans_in.read(buffer, sizeof(buffer)), TTransportException);
  trans_in.close();
}

BOOST_AUTO_TEST_CASE(test_tpipedtransport_read_check_exception) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(MAX_MESSAGE_SIZE));
  std::shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  std::shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  std::shared_ptr<TPipedTransport> trans(new TPipedTransport(underlying, pipe, config));

  uint8_t buffer[4];

  underlying->write((uint8_t*)"abcd", 4);
  BOOST_CHECK_THROW(trans->read(buffer, sizeof(buffer)), TTransportException);
  BOOST_CHECK_THROW(trans->readAll(buffer, sizeof(buffer)), TTransportException);
  trans->readEnd();
  pipe->resetBuffer();
  underlying->write((uint8_t*)"ef", 2);
  BOOST_CHECK_THROW(trans->read(buffer, sizeof(buffer)), TTransportException);
  BOOST_CHECK_THROW(trans->readAll(buffer, sizeof(buffer)), TTransportException);
  trans->readEnd();
}

BOOST_AUTO_TEST_CASE(test_tsimplefiletransport_read_check_exception) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(MAX_MESSAGE_SIZE));
  TSimpleFileTransport trans_out("data", false, true, config);
  uint8_t buffer[6] = {1, 2, 3, 4, 5, 6};
  trans_out.write((const uint8_t*)buffer, sizeof(buffer));
  trans_out.close();

  TSimpleFileTransport trans_in("data",true, false, config);
  memset(buffer, 0, sizeof(buffer));
  BOOST_CHECK_THROW(trans_in.read(buffer, sizeof(buffer)), TTransportException);
  trans_in.close();

  remove("./data");
}

BOOST_AUTO_TEST_CASE(test_tfiletransport_read_check_exception) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(MAX_MESSAGE_SIZE));
  TFileTransport trans_out("data", false, config);
  uint8_t buffer[6] = {1, 2, 3, 4, 5, 6};
  trans_out.write((const uint8_t*)buffer, sizeof(buffer));

  TFileTransport trans_in("data", false, config);
  memset(buffer, 0, sizeof(buffer));
  BOOST_CHECK_THROW(trans_in.read(buffer, sizeof(buffer)), TTransportException);

  remove("./data");
}

BOOST_AUTO_TEST_CASE(test_tbufferedtransport_read_check_exception) {
  uint8_t arr[4] = {1, 2, 3, 4};
  std::shared_ptr<TMemoryBuffer> buffer (new TMemoryBuffer(arr, sizeof(arr)));
  std::shared_ptr<TConfiguration> config (new TConfiguration(MAX_MESSAGE_SIZE));
  std::shared_ptr<TBufferedTransport> trans (new TBufferedTransport(buffer, config));

  trans->write((const uint8_t*)arr, sizeof(arr));
  BOOST_CHECK_THROW(trans->read(arr, sizeof(arr)), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_tframedtransport_read_check_exception) {
  uint8_t arr[4] = {1, 2, 3, 4};
  std::shared_ptr<TMemoryBuffer> buffer (new TMemoryBuffer(arr, sizeof(arr)));
  std::shared_ptr<TConfiguration> config (new TConfiguration(MAX_MESSAGE_SIZE));
  std::shared_ptr<TFramedTransport> trans (new TFramedTransport(buffer, config));

  trans->write((const uint8_t*)arr, sizeof(arr));
  BOOST_CHECK_THROW(trans->read(arr, sizeof(arr)), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_tthriftbinaryprotocol_read_check_exception) {
  std::shared_ptr<TConfiguration> config (new TConfiguration(MAX_MESSAGE_SIZE));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  TType elemType1 = apache::thrift::protocol::T_STOP;
  TList list(T_I32, 8);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, val), TTransportException);
  protocol->readListEnd();

  TSet set(T_I32, 8);
  protocol->writeSetBegin(set.elemType_, set.size_);
  protocol->writeSetEnd();
  BOOST_CHECK_THROW(protocol->readSetBegin(elemType, val), TTransportException);
  protocol->readSetEnd();

  TMap map(T_I32, T_I32, 8);
  protocol->writeMapBegin(map.keyType_, map.valueType_, map.size_);
  protocol->writeMapEnd();
  BOOST_CHECK_THROW(protocol->readMapBegin(elemType, elemType1, val), TTransportException);
  protocol->readMapEnd();
}

BOOST_AUTO_TEST_CASE(test_tthriftcompactprotocol_read_check_exception) {
  // Set Max Message Size to 11 since all structs are 12B long
  std::shared_ptr<TConfiguration> config (new TConfiguration(11));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TCompactProtocol> protocol(new TCompactProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  TType elemType1 = apache::thrift::protocol::T_STOP;

  // This list needs 12B
  TList list(T_I32, 12);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, val), TTransportException);
  protocol->readListEnd();

  // This set needs 12B
  TSet set(T_I32, 12);
  protocol->writeSetBegin(set.elemType_, set.size_);
  protocol->writeSetEnd();
  BOOST_CHECK_THROW(protocol->readSetBegin(elemType, val), TTransportException);
  protocol->readSetEnd();


  // This map needs 12B (2x elem)
  TMap map(T_I32, T_I32, 6);
  protocol->writeMapBegin(map.keyType_, map.valueType_, map.size_);
  protocol->writeMapEnd();
  BOOST_CHECK_THROW(protocol->readMapBegin(elemType, elemType1, val), TTransportException);
  protocol->readMapEnd();

  // This string needs 12B (1 for size + str)
  string eleven = "1234567890A";
  protocol->writeString(eleven);
  BOOST_CHECK_THROW(protocol->readString(eleven), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_tthriftcompactprotocol_read_check_pass) {
  // Set Max Message Size to 12 to check the edge case
  std::shared_ptr<TConfiguration> config (new TConfiguration(12));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TCompactProtocol> protocol(new TCompactProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  TType elemType1 = apache::thrift::protocol::T_STOP;

  // This list needs 12B
  TList list(T_I32, 12);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_NO_THROW(protocol->readListBegin(elemType, val));
  protocol->readListEnd();

  // This set needs 12B
  TSet set(T_I32, 12);
  protocol->writeSetBegin(set.elemType_, set.size_);
  protocol->writeSetEnd();
  BOOST_CHECK_NO_THROW(protocol->readSetBegin(elemType, val));
  protocol->readSetEnd();

  // This map needs 12B (2x elem)
  TMap map(T_I32, T_I32, 6);
  protocol->writeMapBegin(map.keyType_, map.valueType_, map.size_);
  protocol->writeMapEnd();
  BOOST_CHECK_NO_THROW(protocol->readMapBegin(elemType, elemType1, val));
  protocol->readMapEnd();

  // This string needs 12B (1 for size + str)
  string eleven = "1234567890A";
  protocol->writeString(eleven);
  BOOST_CHECK_NO_THROW(protocol->readString(eleven));
}

BOOST_AUTO_TEST_CASE(test_tthriftbinaryprotocol_container_size_overflow) {
  std::shared_ptr<TConfiguration> config (new TConfiguration(1024));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  // 0x40000000 elements of min size 4 require 4 GiB; the product wraps to 0 in
  // 32-bit math and used to slip past the MaxMessageSize check.
  TList list(T_I32, 0x40000000);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, val), TTransportException);
  protocol->readListEnd();
}

BOOST_AUTO_TEST_CASE(test_tthriftcompactprotocol_container_size_overflow) {
  std::shared_ptr<TConfiguration> config (new TConfiguration(1024));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TCompactProtocol> protocol(new TCompactProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  // 0x10000000 elements of min size 16 (UUID) require 4 GiB; the product wraps
  // to 0 in 32-bit math and used to slip past the MaxMessageSize check.
  TList list(T_UUID, 0x10000000);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, val), TTransportException);
  protocol->readListEnd();
}

BOOST_AUTO_TEST_CASE(test_tthriftjsonprotocol_container_size_overflow) {
  std::shared_ptr<TConfiguration> config (new TConfiguration(1024));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TJSONProtocol> protocol(new TJSONProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  // 0x10000000 elements of min size 16 (UUID) require 4 GiB; the product wraps
  // to 0 in 32-bit math and used to slip past the MaxMessageSize check.
  TList list(T_UUID, 0x10000000);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, val), TTransportException);
  protocol->readListEnd();
}

BOOST_AUTO_TEST_CASE(test_tthriftjsonprotocol_read_check_exception) {
  std::shared_ptr<TConfiguration> config (new TConfiguration(MAX_MESSAGE_SIZE));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TJSONProtocol> protocol(new TJSONProtocol(transport));

  uint32_t val = 0;
  TType elemType = apache::thrift::protocol::T_STOP;
  TType elemType1 = apache::thrift::protocol::T_STOP;
  TList list(T_I32, 8);
  protocol->writeListBegin(list.elemType_, list.size_);
  protocol->writeListEnd();
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, val), TTransportException);
  protocol->readListEnd();

  TSet set(T_I32, 8);
  protocol->writeSetBegin(set.elemType_, set.size_);
  protocol->writeSetEnd();
  BOOST_CHECK_THROW(protocol->readSetBegin(elemType, val), TTransportException);
  protocol->readSetEnd();

  TMap map(T_I32, T_I32, 8);
  protocol->writeMapBegin(map.keyType_, map.valueType_, map.size_);
  protocol->writeMapEnd();
  BOOST_CHECK_THROW(protocol->readMapBegin(elemType, elemType1, val), TTransportException);
  protocol->readMapEnd();
}

BOOST_AUTO_TEST_CASE(test_theadertransport_header_size_exceeds_frame) {
  using apache::thrift::transport::THeaderTransport;
  // Header-format frame whose declared header size (3 * 4 = 12) leaves fewer
  // than the 10 common-header bytes inside the 14-byte frame. The trailing
  // varint bytes are all continuation bytes, so the reader used to run off the
  // end of the receive buffer.
  uint8_t frame[] = {
      0x00, 0x00, 0x00, 0x0E, // frame length = 14
      0x0F, 0xFF, 0x00, 0x00, // header magic
      0x00, 0x00, 0x00, 0x00, // seqId
      0x00, 0x03,             // header size field (3 -> 12 bytes)
      0x02,                   // protocol id varint
      0x00,                   // num transforms = 0
      0x80, 0x80              // info-header varint, all continuation
  };
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(frame, sizeof(frame)));
  std::shared_ptr<THeaderTransport> trans(new THeaderTransport(buffer));

  uint8_t out[1];
  BOOST_CHECK_THROW(trans->read(out, sizeof(out)), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_theadertransport_info_header_string_overrun) {
  using apache::thrift::transport::THeaderTransport;
  // Header-format frame whose info-header key length (4) does not fit within the
  // header bytes that remain once the length varint itself is accounted for. The
  // header section (8 bytes) exactly fills the frame, so the boundary sits at the
  // buffer end; the key length has to be bounded against the remaining bytes and
  // rejected.
  uint8_t frame[] = {
      0x00, 0x00, 0x00, 0x12, // frame length = 18
      0x0F, 0xFF, 0x00, 0x00, // header magic
      0x00, 0x00, 0x00, 0x00, // seqId
      0x00, 0x02,             // header size field (2 -> 8 bytes)
      0x02,                   // protocol id varint
      0x00,                   // num transforms = 0
      0x01,                   // info id = key/value
      0x01,                   // one key/value pair
      0x04,                   // key length = 4 (only 3 bytes remain)
      0xAA, 0xBB, 0xCC        // key bytes
  };
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(frame, sizeof(frame)));
  std::shared_ptr<THeaderTransport> trans(new THeaderTransport(buffer));

  uint8_t out[1];
  BOOST_CHECK_THROW(trans->read(out, sizeof(out)), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_theadertransport_info_header_string_negative_length) {
  using apache::thrift::transport::THeaderTransport;
  // Header-format frame whose info-header key length varint decodes to a
  // negative int32 (top bit set). The length has to be treated as out of range
  // rather than converted to a size_t, so the read is rejected instead of
  // reaching the string assignment. The three trailing bytes only pad the
  // header section out to its declared size and are never reached.
  uint8_t frame[] = {
      0x00, 0x00, 0x00, 0x16,       // frame length = 22
      0x0F, 0xFF, 0x00, 0x00,       // header magic
      0x00, 0x00, 0x00, 0x00,       // seqId
      0x00, 0x03,                   // header size field (3 -> 12 bytes)
      0x02,                         // protocol id varint
      0x00,                         // num transforms = 0
      0x01,                         // info id = key/value
      0x01,                         // one key/value pair
      0x80, 0x80, 0x80, 0x80, 0x08, // key length varint = INT32_MIN
      0x00, 0x00, 0x00              // padding to fill the header section
  };
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(frame, sizeof(frame)));
  std::shared_ptr<THeaderTransport> trans(new THeaderTransport(buffer));

  uint8_t out[1];
  BOOST_CHECK_THROW(trans->read(out, sizeof(out)), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_theadertransport_zlib_roundtrip) {
  using apache::thrift::transport::THeaderTransport;
  // A run of identical bytes compresses to far fewer bytes than it occupies
  // once expanded again, so the result of the zlib transform is much larger
  // than the frame section it is read from.  This drives the full write/read
  // round trip through the zlib transform path.  Keep the payload small enough
  // to stay within the transform buffer the reader sizes from its write buffer.
  const std::size_t N = 700;
  std::vector<uint8_t> payload(N, 0x42);

  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  std::shared_ptr<THeaderTransport> writer(new THeaderTransport(buffer));
  writer->setTransform(THeaderTransport::ZLIB_TRANSFORM);
  writer->write(payload.data(), static_cast<uint32_t>(payload.size()));
  writer->flush();

  std::shared_ptr<THeaderTransport> reader(new THeaderTransport(buffer));
  std::vector<uint8_t> out(N, 0x00);
  reader->readAll(out.data(), static_cast<uint32_t>(out.size()));

  BOOST_CHECK(out == payload);
}

BOOST_AUTO_TEST_SUITE_END()
