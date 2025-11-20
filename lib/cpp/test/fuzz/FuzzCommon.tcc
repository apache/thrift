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

#ifndef THRIFT_TEST_FUZZ_COMMON_TCC_
#define THRIFT_TEST_FUZZ_COMMON_TCC_

#include <stddef.h>
#include <stdint.h>
#include <memory>
#include <cmath>
#include <iostream>

#include <thrift/protocol/TDebugProtocol.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/TToString.h>
#include <thrift/TConfiguration.h>

#include "gen-cpp/FuzzTest_types.h"

namespace apache { namespace thrift { namespace fuzzer {

using namespace apache::thrift::transport;
using namespace apache::thrift::protocol;
using namespace fuzz;

// 10MB message size limit to prevent over-allocation during fuzzing
const int FUZZ_MAX_MESSAGE_SIZE = 10 * 1024 * 1024;

inline bool is_nan_false_positive(FuzzTest& test1, FuzzTest& test2) {
  BasicTypes& b1 = test1.basic;
  BasicTypes& b2 = test2.basic;
  if (std::isnan(b1.double_field) && std::isnan(b2.double_field)) {
    b1.double_field = 0.0;
    b2.double_field = 0.0;
  }

  // Check for NaN in containers if they contain doubles
  // This is a simplified version - may need adjustment based on actual schema
  
  return test1 == test2;
}

// Simple parse-only fuzzer
template<typename ProtocolType>
int fuzz_parse(const uint8_t* data, size_t size) {
  try {
    std::shared_ptr<TConfiguration> config(new TConfiguration(FUZZ_MAX_MESSAGE_SIZE));
    std::shared_ptr<TMemoryBuffer> trans(new TMemoryBuffer(const_cast<uint8_t*>(data), size, TMemoryBuffer::OBSERVE, config));
    std::shared_ptr<TProtocol> proto(new ProtocolType(trans));

    FuzzTest test;
    test.read(proto.get());
  } catch (const TException&) {
    // Ignore any Thrift exceptions - they're expected when fuzzing
  }
  return 0;
}

// Roundtrip fuzzer that verifies serialization/deserialization
template<typename ProtocolType>
int fuzz_roundtrip(const uint8_t* data, size_t size) {
  try {
    std::shared_ptr<TConfiguration> config(new TConfiguration(FUZZ_MAX_MESSAGE_SIZE));
    
    // First parse
    std::shared_ptr<TMemoryBuffer> trans(new TMemoryBuffer(const_cast<uint8_t*>(data), size, TMemoryBuffer::OBSERVE, config));
    std::shared_ptr<TProtocol> proto(new ProtocolType(trans));

    FuzzTest test1;
    test1.read(proto.get());

    // Serialize back
    std::shared_ptr<TMemoryBuffer> outTrans(new TMemoryBuffer(config));
    std::shared_ptr<TProtocol> outProto(new ProtocolType(outTrans));
    test1.write(outProto.get());

    // Get serialized data
    std::string serialized = outTrans->getBufferAsString();

    // Deserialize again
    std::shared_ptr<TMemoryBuffer> reTrans(new TMemoryBuffer(config));
    reTrans->write((const uint8_t*)serialized.data(), static_cast<uint32_t>(serialized.size()));
    std::shared_ptr<TProtocol> reProto(new ProtocolType(reTrans));

    FuzzTest test2;
    test2.read(reProto.get());

    // Verify equality
    if (!(test1 == test2) && !is_nan_false_positive(test1, test2)) {
      const std::string str1(apache::thrift::ThriftDebugString(test1));
      const std::string str2(apache::thrift::ThriftDebugString(test2));

      std::cout << "Expected:\n" << str1 << "\nGotten:\n" << str2 << std::endl;

      throw std::runtime_error("Roundtrip failed");
    }
  } catch (const TException&) {
    // Ignore any Thrift exceptions - they're expected when fuzzing
  }
  return 0;
}

}}} // apache::thrift::fuzzer

#ifndef FUZZING_BUILD_MODE_UNSAFE_FOR_PRODUCTION
__attribute__((weak)) int main(int argc, char** argv) {
  return 0;
}
#endif 

#endif // THRIFT_TEST_FUZZ_COMMON_TCC_ 