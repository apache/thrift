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

#include <thrift/Thrift.h>
#include <thrift/TConfiguration.h>
#include <memory>
#include <string>
#include <vector>
#include <thrift/transport/TTransportUtils.h>
#include <thrift/transport/TBufferTransports.h>

#define BOOST_TEST_MODULE TPipedTransportTest
#include <boost/test/unit_test.hpp>

using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TPipedTransport;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TTransport;
using namespace apache::thrift;

BOOST_AUTO_TEST_CASE(test_read_write) {
  std::shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  std::shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  std::shared_ptr<TPipedTransport> trans(new TPipedTransport(underlying, pipe));

  uint8_t buffer[4];

  underlying->write((uint8_t*)"abcd", 4);
  trans->readAll(buffer, 2);
  BOOST_CHECK(std::string((char*)buffer, 2) == "ab");
  trans->readEnd();
  BOOST_CHECK(pipe->getBufferAsString() == "ab");
  pipe->resetBuffer();
  underlying->write((uint8_t*)"ef", 2);
  trans->readAll(buffer, 2);
  BOOST_CHECK(std::string((char*)buffer, 2) == "cd");
  trans->readAll(buffer, 2);
  BOOST_CHECK(std::string((char*)buffer, 2) == "ef");
  trans->readEnd();
  BOOST_CHECK(pipe->getBufferAsString() == "cdef");
}

// The rest of a pipelined request, read ahead before readEnd(), is still there
// afterwards, also when it is longer than the request before it.
BOOST_AUTO_TEST_CASE(test_read_ahead_survives_read_end) {
  std::shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  std::shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  std::shared_ptr<TPipedTransport> trans(new TPipedTransport(underlying, pipe));

  std::string next(100, 'x');
  for (size_t i = 0; i < next.size(); ++i) {
    next[i] = static_cast<char>('a' + i % 26);
  }
  underlying->write((const uint8_t*)"ab", 2);
  underlying->write((const uint8_t*)next.data(), static_cast<uint32_t>(next.size()));

  uint8_t buffer[100];
  trans->readAll(buffer, 2);
  trans->readEnd();
  BOOST_CHECK_EQUAL("ab", pipe->getBufferAsString());
  trans->readAll(buffer, 100);
  BOOST_CHECK(std::string((char*)buffer, 100) == next);
}

// The pipe keeps every byte of a message until readEnd(), so all of them count
// against the maximum message size; readEnd() starts the count anew.
BOOST_AUTO_TEST_CASE(test_retained_message_is_bounded) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(1024));
  std::shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  std::shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  std::shared_ptr<TPipedTransport> trans(new TPipedTransport(underlying, pipe, config));

  std::vector<uint8_t> data(4096, 0x55);
  underlying->write(data.data(), static_cast<uint32_t>(data.size()));

  std::vector<uint8_t> buffer(512);
  trans->readAll(buffer.data(), 512);
  trans->readAll(buffer.data(), 512);
  BOOST_CHECK_THROW(trans->readAll(buffer.data(), 512), TTransportException);

  trans->readEnd();
  trans->readAll(buffer.data(), 512);
}

namespace {

// A pipe whose buffers look as if they already held 2 GiB and 4 GiB (the
// allocations themselves stay small).
class FullPipe : public TPipedTransport {
public:
  FullPipe(std::shared_ptr<TTransport> src, std::shared_ptr<TTransport> dst)
    : TTransport(nullptr), TPipedTransport(src, dst) {}

  void readBufferFull() { rBufSize_ = rLen_ = rPos_ = 0x80000000u; }
  void writeBufferFull() {
    wBufSize_ = 0xFFFFFFFFu;
    wLen_ = 0xFFFFFFF0u;
  }
};

} // namespace

// The read buffer refuses to grow past what its uint32_t size can hold.  Keep
// these cases last.
BOOST_AUTO_TEST_CASE(test_read_buffer_stops_growing_at_its_limit) {
  std::shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  std::shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  underlying->write((const uint8_t*)"abcd", 4);
  uint8_t buffer[4];
  {
    FullPipe trans(underlying, pipe);
    trans.readBufferFull();
    BOOST_CHECK_THROW(trans.read(buffer, 1), TTransportException);
  }
  {
    FullPipe trans(underlying, pipe);
    trans.readBufferFull();
    BOOST_CHECK_THROW(trans.peek(), TTransportException);
  }
}

BOOST_AUTO_TEST_CASE(test_write_buffer_stops_growing_at_its_limit) {
  std::shared_ptr<TMemoryBuffer> underlying(new TMemoryBuffer);
  std::shared_ptr<TMemoryBuffer> pipe(new TMemoryBuffer);
  FullPipe trans(underlying, pipe);
  trans.writeBufferFull();
  uint8_t data[32] = {0};
  BOOST_CHECK_THROW(trans.write(data, sizeof(data)), TTransportException);
}
