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

#include <boost/test/auto_unit_test.hpp>
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
using std::cout;
using std::endl;
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
  std::shared_ptr<TConfiguration> config (new TConfiguration(MAX_MESSAGE_SIZE));
  std::shared_ptr<TMemoryBuffer> transport(new TMemoryBuffer(config));
  std::shared_ptr<TCompactProtocol> protocol(new TCompactProtocol(transport));

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

BOOST_AUTO_TEST_SUITE_END()
