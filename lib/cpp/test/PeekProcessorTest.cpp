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

#include <atomic>
#include <boost/test/unit_test.hpp>
#include <memory>
#include <string>
#include <thread>
#include <vector>

#include <thrift/processor/PeekProcessor.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/protocol/TProtocolException.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/TTransportUtils.h>

BOOST_AUTO_TEST_SUITE(PeekProcessorTest)

using apache::thrift::TProcessor;
using apache::thrift::processor::PeekProcessor;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TBinaryProtocolFactory;
using apache::thrift::protocol::TMessageType;
using apache::thrift::protocol::TProtocol;
using apache::thrift::protocol::TProtocolException;
using apache::thrift::protocol::TType;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TPipedTransportFactory;
using apache::thrift::transport::TTransport;

namespace {

// A call "check(1: i32 first, 2: i32 second)", as a client writes it.
std::shared_ptr<TMemoryBuffer> request(int32_t first, int32_t second) {
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  TBinaryProtocol protocol(buffer);
  protocol.writeMessageBegin("check", apache::thrift::protocol::T_CALL, 1);
  protocol.writeStructBegin("check_args");
  protocol.writeFieldBegin("first", apache::thrift::protocol::T_I32, 1);
  protocol.writeI32(first);
  protocol.writeFieldEnd();
  protocol.writeFieldBegin("second", apache::thrift::protocol::T_I32, 2);
  protocol.writeI32(second);
  protocol.writeFieldEnd();
  protocol.writeFieldStop();
  protocol.writeStructEnd();
  protocol.writeMessageEnd();
  return buffer;
}

// Reads "check" calls as a generated processor does, and counts the ones that
// arrived intact: the right name and both fields equal.  A first field of -1
// makes it throw before it has read the rest, as a generated processor does
// when an argument does not decode.
class CheckingProcessor : public TProcessor {
public:
  CheckingProcessor() : intact(0), damaged(0) {}

  bool process(std::shared_ptr<TProtocol> in, std::shared_ptr<TProtocol> out, void*) override {
    (void)out;
    std::string name;
    TMessageType type;
    int32_t seqid;
    in->readMessageBegin(name, type, seqid);
    std::string fieldName;
    TType ftype;
    int16_t fid;
    in->readStructBegin(fieldName);
    in->readFieldBegin(fieldName, ftype, fid);
    int32_t first = 0;
    in->readI32(first);
    if (first == -1) {
      throw TProtocolException(TProtocolException::INVALID_DATA,
                               "the first argument does not decode");
    }
    in->readFieldEnd();
    in->readFieldBegin(fieldName, ftype, fid);
    int32_t second = 0;
    in->readI32(second);
    in->readFieldEnd();
    in->readFieldBegin(fieldName, ftype, fid);
    in->readStructEnd();
    in->readMessageEnd();
    in->getTransport()->readEnd();
    if (name == "check" && first == second && ftype == apache::thrift::protocol::T_STOP) {
      ++intact;
    } else {
      ++damaged;
    }
    return true;
  }

  std::atomic<int> intact;
  std::atomic<int> damaged;
};

struct Peeking {
  Peeking() : checker(new CheckingProcessor()), peek(new PeekProcessor()) {
    peek->initialize(checker,
                     std::make_shared<TBinaryProtocolFactory>(),
                     std::make_shared<TPipedTransportFactory>());
  }

  // One call on a connection of its own, as a server makes it through the
  // piped transport the PeekProcessor hands out.
  void call(std::shared_ptr<TMemoryBuffer> request) {
    std::shared_ptr<TTransport> input = peek->getPipedTransport(request);
    std::shared_ptr<TProtocol> in(new TBinaryProtocol(input));
    std::shared_ptr<TProtocol> out(new TBinaryProtocol(std::make_shared<TMemoryBuffer>()));
    peek->process(in, out, nullptr);
  }

  std::shared_ptr<CheckingProcessor> checker;
  std::shared_ptr<PeekProcessor> peek;
};

} // namespace

// A call that fails while the wrapped processor reads it must not leave bytes
// behind for the next connection's call.
BOOST_AUTO_TEST_CASE(test_failed_call_leaves_nothing_for_the_next) {
  Peeking peeking;
  BOOST_CHECK_THROW(peeking.call(request(-1, 7)), TProtocolException);

  try {
    peeking.call(request(5, 5));
  } catch (const std::exception& ex) {
    BOOST_ERROR("the next call failed: " << ex.what());
  }
  BOOST_CHECK_EQUAL(1, peeking.checker->intact.load());
  BOOST_CHECK_EQUAL(0, peeking.checker->damaged.load());
}

// Connections that call at the same time must each see their own request.
BOOST_AUTO_TEST_CASE(test_concurrent_calls_see_their_own_requests) {
  const int threads = 2;
  const int callsPerThread = 2000;
  Peeking peeking;
  std::atomic<int> failed(0);
  std::vector<std::thread> callers;
  for (int t = 0; t < threads; ++t) {
    callers.emplace_back([&peeking, &failed, t, callsPerThread]() {
      for (int i = 0; i < callsPerThread; ++i) {
        try {
          int32_t value = t * callsPerThread + i;
          peeking.call(request(value, value));
        } catch (const std::exception&) {
          ++failed;
        }
      }
    });
  }
  for (auto& caller : callers) {
    caller.join();
  }
  BOOST_CHECK_EQUAL(0, failed.load());
  BOOST_CHECK_EQUAL(0, peeking.checker->damaged.load());
  BOOST_CHECK_EQUAL(threads * callsPerThread, peeking.checker->intact.load());
}

BOOST_AUTO_TEST_SUITE_END()
