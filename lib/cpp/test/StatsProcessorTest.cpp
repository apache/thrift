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
#include <map>
#include <memory>
#include <string>
#include <thread>
#include <vector>

#include <thrift/processor/StatsProcessor.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/protocol/TProtocolException.h>
#include <thrift/transport/TBufferTransports.h>

BOOST_AUTO_TEST_SUITE(StatsProcessorTest)

using apache::thrift::processor::StatsProcessor;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TProtocol;
using apache::thrift::protocol::TProtocolException;
using apache::thrift::transport::TMemoryBuffer;

namespace {

// Appends a call "name(1: i32 value, 2: Point point)" with Point {1: i32 x, 2: i32 y}.
void writeCall(TProtocol& protocol, const std::string& name, int32_t value) {
  protocol.writeMessageBegin(name, apache::thrift::protocol::T_CALL, 0);
  protocol.writeStructBegin("args");
  protocol.writeFieldBegin("value", apache::thrift::protocol::T_I32, 1);
  protocol.writeI32(value);
  protocol.writeFieldEnd();
  protocol.writeFieldBegin("point", apache::thrift::protocol::T_STRUCT, 2);
  protocol.writeStructBegin("Point");
  protocol.writeFieldBegin("x", apache::thrift::protocol::T_I32, 1);
  protocol.writeI32(value);
  protocol.writeFieldEnd();
  protocol.writeFieldBegin("y", apache::thrift::protocol::T_I32, 2);
  protocol.writeI32(-value);
  protocol.writeFieldEnd();
  protocol.writeFieldStop();
  protocol.writeStructEnd();
  protocol.writeFieldEnd();
  protocol.writeFieldStop();
  protocol.writeStructEnd();
  protocol.writeMessageEnd();
}

int64_t total(const std::map<std::string, int64_t>& frequencies) {
  int64_t sum = 0;
  for (const auto& entry : frequencies) {
    sum += entry.second;
  }
  return sum;
}

} // namespace

// Connections that call at the same time each read their own message, and
// every call is counted.
BOOST_AUTO_TEST_CASE(test_concurrent_calls_are_all_counted) {
  const int threads = 2;
  const int callsPerThread = 2000;
  std::shared_ptr<StatsProcessor> stats(new StatsProcessor(false, true));
  std::atomic<int> failed(0);
  std::vector<std::thread> callers;
  for (int t = 0; t < threads; ++t) {
    callers.emplace_back([&stats, &failed, callsPerThread]() {
      std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
      std::shared_ptr<TProtocol> in(new TBinaryProtocol(buffer));
      std::shared_ptr<TProtocol> out(new TBinaryProtocol(std::make_shared<TMemoryBuffer>()));
      for (int i = 0; i < callsPerThread; ++i) {
        writeCall(*in, "call", i);
      }
      for (int i = 0; i < callsPerThread; ++i) {
        try {
          stats->process(in, out, nullptr);
        } catch (const std::exception&) {
          ++failed;
        }
      }
    });
  }
  for (auto& caller : callers) {
    caller.join();
  }
  std::map<std::string, int64_t> frequencies = stats->get_frequency_map();
  BOOST_CHECK_EQUAL(0, failed.load());
  BOOST_CHECK_EQUAL(1u, frequencies.size());
  BOOST_CHECK_EQUAL(threads * callsPerThread, frequencies["call"]);
}

// The frequency map keeps at most 1024 method names of their own; the calls
// beyond that are still counted, together.
BOOST_AUTO_TEST_CASE(test_frequency_map_is_bounded) {
  const int names = 5000;
  StatsProcessor stats(false, true);
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  std::shared_ptr<TProtocol> in(new TBinaryProtocol(buffer));
  std::shared_ptr<TProtocol> out(new TBinaryProtocol(std::make_shared<TMemoryBuffer>()));
  for (int i = 0; i < names; ++i) {
    writeCall(*in, "method" + std::to_string(i), i);
    stats.process(in, out, nullptr);
  }
  std::map<std::string, int64_t> frequencies = stats.get_frequency_map();
  BOOST_CHECK_LE(frequencies.size(), 1025u);
  BOOST_CHECK_EQUAL(names, total(frequencies));
}

// A message nested deeper than the recursion limit is refused, as skip()
// refuses it.  Keep this case last.
BOOST_AUTO_TEST_CASE(test_deep_nesting_is_refused) {
  const int depth = 200000;
  StatsProcessor stats(false, false);
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer());
  std::shared_ptr<TProtocol> in(new TBinaryProtocol(buffer));
  std::shared_ptr<TProtocol> out(new TBinaryProtocol(std::make_shared<TMemoryBuffer>()));
  in->writeMessageBegin("deep", apache::thrift::protocol::T_ONEWAY, 0);
  in->writeStructBegin("args");
  in->writeFieldBegin("nested", apache::thrift::protocol::T_LIST, 1);
  for (int i = 0; i < depth; ++i) {
    in->writeListBegin(apache::thrift::protocol::T_LIST, 1);
  }
  in->writeListBegin(apache::thrift::protocol::T_I32, 0);
  for (int i = 0; i <= depth; ++i) {
    in->writeListEnd();
  }
  in->writeFieldEnd();
  in->writeFieldStop();
  in->writeStructEnd();
  in->writeMessageEnd();

  try {
    stats.process(in, out, nullptr);
    BOOST_ERROR("a message nested " << depth << " levels deep was accepted");
  } catch (const TProtocolException& ex) {
    BOOST_CHECK_EQUAL(TProtocolException::DEPTH_LIMIT, ex.getType());
  }
}

BOOST_AUTO_TEST_SUITE_END()
