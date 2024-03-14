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

#include <array>
#include <boost/test/unit_test.hpp>
#include <climits>
#include <cstdlib>
#include <iostream>
#include <memory>
#include <numeric>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TBufferTransports.h>
#include <vector>

#include "gen-cpp/ThriftTest_types.h"

BOOST_AUTO_TEST_SUITE(TMemoryBufferTest)

using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TTransportException;
using std::shared_ptr;
using std::string;

BOOST_AUTO_TEST_CASE(test_read_write_grow) {
  // Added to test the fix for THRIFT-1248
  TMemoryBuffer uut;
  const int maxSize = 65536;
  uint8_t verify[maxSize];
  std::vector<uint8_t> buf;
  buf.resize(maxSize);

  for (uint32_t i = 0; i < maxSize; ++i) {
    buf[i] = static_cast<uint8_t>(i);
  }

  for (uint32_t i = 1; i < maxSize; i *= 2) {
    uut.write(&buf[0], i);
  }

  for (uint32_t i = 1; i < maxSize; i *= 2) {
    uut.read(verify, i);
    BOOST_CHECK_EQUAL(0, ::memcmp(verify, &buf[0], i));
  }
}

BOOST_AUTO_TEST_CASE(test_roundtrip) {
  shared_ptr<TMemoryBuffer> strBuffer(new TMemoryBuffer());
  shared_ptr<TBinaryProtocol> binaryProtcol(new TBinaryProtocol(strBuffer));

  thrift::test::Xtruct a;
  a.i32_thing = 10;
  a.i64_thing = 30;
  a.string_thing = "holla back a";

  a.write(binaryProtcol.get());
  std::string serialized = strBuffer->getBufferAsString();

  shared_ptr<TMemoryBuffer> strBuffer2(new TMemoryBuffer());
  shared_ptr<TBinaryProtocol> binaryProtcol2(new TBinaryProtocol(strBuffer2));

  strBuffer2->resetBuffer((uint8_t*)serialized.data(), static_cast<uint32_t>(serialized.length()));
  thrift::test::Xtruct a2;
  a2.read(binaryProtcol2.get());

  BOOST_CHECK(a == a2);
}

BOOST_AUTO_TEST_CASE(test_readAppendToString) {
  string str1 = "abcd1234";
  TMemoryBuffer buf((uint8_t*)str1.data(),
                    static_cast<uint32_t>(str1.length()),
                    TMemoryBuffer::COPY);

  string str3 = "wxyz", str4 = "6789";
  buf.readAppendToString(str3, 4);
  buf.readAppendToString(str4, INT_MAX);

  BOOST_CHECK(str3 == "wxyzabcd");
  BOOST_CHECK(str4 == "67891234");
}

BOOST_AUTO_TEST_CASE(test_exceptions) {
  char data[] = "foo\0bar";

  TMemoryBuffer buf1((uint8_t*)data, 7, TMemoryBuffer::OBSERVE);
  string str = buf1.getBufferAsString();
  BOOST_CHECK(str.length() == 7);

  buf1.resetBuffer();

  BOOST_CHECK_THROW(buf1.write((const uint8_t*)"foo", 3), TTransportException);

  TMemoryBuffer buf2((uint8_t*)data, 7, TMemoryBuffer::COPY);
  BOOST_CHECK_NO_THROW(buf2.write((const uint8_t*)"bar", 3));
}

BOOST_AUTO_TEST_CASE(test_default_maximum_buffer_size)
{
  BOOST_CHECK_EQUAL((std::numeric_limits<uint32_t>::max)(), TMemoryBuffer().getMaxBufferSize());
}

BOOST_AUTO_TEST_CASE(test_default_buffer_size)
{
  BOOST_CHECK_EQUAL(1024, TMemoryBuffer().getBufferSize());
}

BOOST_AUTO_TEST_CASE(test_error_set_max_buffer_size_too_small)
{
  TMemoryBuffer buf;
  BOOST_CHECK_THROW(buf.setMaxBufferSize(buf.getBufferSize() - 1), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_observe) {
#ifdef _MSC_VER
  #define N 73
#else
  constexpr size_t N = 73;
#endif
  constexpr size_t M = 42;
  uint8_t one_byte = 42;
  std::vector<uint8_t> scratch;
  auto filler = [=]() {
    std::array<uint8_t, N> x;
    // Fill buf_mem with a sequence from 0 to N - 1
    std::iota(x.begin(), x.end(), 0);
    return x;
  };
  static const std::array<uint8_t, N> buf_mem = filler();

  BOOST_STATIC_ASSERT(M < N);

  TMemoryBuffer buf((uint8_t*)&buf_mem.front(), N, TMemoryBuffer::MemoryPolicy::OBSERVE);

  // Readable
  BOOST_CHECK_EQUAL(N, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Not writeable
  BOOST_CHECK_THROW(buf.write(&one_byte, 1), TTransportException);

  // Read some but not all
  scratch.resize(M);
  BOOST_CHECK_EQUAL(M, buf.read(&scratch[0], M));
  // Check remaining
  BOOST_CHECK_EQUAL(N - M, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Not writeable
  BOOST_CHECK_THROW(buf.write(&one_byte, 1), TTransportException);
  // Contents
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), buf_mem.begin(),
                                buf_mem.begin() + M);

  // Readable (drain remaining)
  scratch.resize(N);
  BOOST_CHECK_EQUAL(N - M, buf.read(&scratch[M], N - M));
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Not writeable
  BOOST_CHECK_THROW(buf.write(&one_byte, 1), TTransportException);
  // Contents
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), buf_mem.begin(), buf_mem.end());

  // Not readable
  BOOST_CHECK_EQUAL(0, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Not writeable
  BOOST_CHECK_THROW(buf.write(&one_byte, 1), TTransportException);

  /* OBSERVE buffer cannot be reread with the default reset */

  buf.resetBuffer();
  // Not Readable
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Not writeable
  BOOST_CHECK_THROW(buf.write(&one_byte, 1), TTransportException);

  /* OBSERVE buffers do not auto-resize when written to (implicit) */
  /* OBSERVE buffers can be appended-to (implicit) */
}

BOOST_AUTO_TEST_CASE(test_copy) {
#ifdef _MSC_VER
  #define N 73
#else
  constexpr size_t N = 73;
#endif
  constexpr size_t M = 42;
  uint8_t one_byte = 42;
  std::vector<uint8_t> scratch;
  auto filler = [&]() {
    std::array<uint8_t, N> x;
    // Fill buf_mem with a sequence from 0 to N - 1
    std::iota(x.begin(), x.end(), 0);
    return x;
  };
  static const std::array<uint8_t, N> buf_mem = filler();

  BOOST_STATIC_ASSERT(M < N);

  TMemoryBuffer buf((uint8_t*)&buf_mem.front(), N, TMemoryBuffer::MemoryPolicy::COPY);

  // Readable
  BOOST_CHECK_EQUAL(N, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());

  // Read some but not all
  scratch.resize(M);
  BOOST_CHECK_EQUAL(M, buf.read(&scratch[0], M));
  // Check remaining
  BOOST_CHECK_EQUAL(N - M, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Contents
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), buf_mem.begin(),
                                buf_mem.begin() + M);

  // Readable (drain remaining)
  scratch.resize(N);
  BOOST_CHECK_EQUAL(N - M, buf.read(&scratch[M], N - M));
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Contents
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), buf_mem.begin(), buf_mem.end());

  // Not readable
  BOOST_CHECK_EQUAL(0, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());

  /* COPY buffer cannot be reread with the default reset */

  buf.resetBuffer();
  // Not readable
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // Has available write space
  BOOST_CHECK_EQUAL(N, buf.available_write());

  /* COPY buffers auto-resize when written to */

  // Not readable
  BOOST_CHECK_EQUAL(0, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // No available write space
  BOOST_CHECK_GT(buf.available_write(), 0);
  // Writeable
  one_byte = M;
  BOOST_CHECK_NO_THROW(buf.write(&one_byte, 1));
  // Readable
  one_byte = 0xff;
  BOOST_CHECK_EQUAL(1, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(one_byte, M);

  /* COPY buffers can be appended-to (and auto-resize) */

  buf.resetBuffer((uint8_t*)&buf_mem.front(), N, TMemoryBuffer::MemoryPolicy::COPY);
  // Appendable
  one_byte = N + 1;
  BOOST_CHECK_NO_THROW(buf.write(&one_byte, 1));
  BOOST_CHECK_EQUAL(N, buf.read(&scratch[0], N));
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), buf_mem.begin(),
                                buf_mem.begin() + N);
  one_byte = 0xff;
  BOOST_CHECK_EQUAL(1, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(one_byte, N + 1);
}

BOOST_AUTO_TEST_CASE(test_take_ownership)
{
#ifdef _MSC_VER
  #define N 73
#else
  constexpr size_t N = 73;
#endif
  constexpr size_t M = 42;
  uint8_t one_byte = 42;
  std::vector<uint8_t> scratch;
  auto filler = [&]() {
    /* TAKE_OWNERSHIP buffers MUST be malloc'ed */
    uint8_t* x = static_cast<uint8_t*>(malloc(N));
    // Fill buf_mem with a sequence from 0 to N - 1
    std::iota(&x[0], &x[N], 0);
    return x;
  };
  uint8_t* buf_mem = filler();

  BOOST_STATIC_ASSERT(M < N);

  TMemoryBuffer buf(buf_mem, N, TMemoryBuffer::MemoryPolicy::TAKE_OWNERSHIP);

  // Readable
  BOOST_CHECK_EQUAL(N, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());

  // Read some but not all
  scratch.resize(M);
  BOOST_CHECK_EQUAL(M, buf.read(&scratch[0], M));
  // Check remaining
  BOOST_CHECK_EQUAL(N - M, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Contents
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), &buf_mem[0], &buf_mem[M]);

  // Readable (drain remaining)
  scratch.resize(N);
  BOOST_CHECK_EQUAL(N - M, buf.read(&scratch[M], N - M));
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());
  // Contents
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), &buf_mem[0], &buf_mem[N]);

  // Not readable
  BOOST_CHECK_EQUAL(0, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(0, buf.available_write());

  /* TAKE_OWNERSHIP buffers auto-resize when written to */

  // Not readable
  BOOST_CHECK_EQUAL(0, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(0, buf.available_read());
  // No available write space
  BOOST_CHECK_EQUAL(buf.available_write(), 0);
  // Writeable
  one_byte = M;
  BOOST_CHECK_NO_THROW(buf.write(&one_byte, 1));
  // Readable
  one_byte = 0xff;
  BOOST_CHECK_EQUAL(1, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(one_byte, M);

  /* TAKE_OWNERSHIP buffers can be appended-to (and auto-resize) */

  buf_mem = filler();
  buf.resetBuffer(buf_mem, N, TMemoryBuffer::MemoryPolicy::COPY);
  // Appendable
  one_byte = N + 1;
  BOOST_CHECK_NO_THROW(buf.write(&one_byte, 1));
  BOOST_CHECK_EQUAL(N, buf.read(&scratch[0], N));
  BOOST_CHECK_EQUAL_COLLECTIONS(scratch.begin(), scratch.end(), &buf_mem[0], &buf_mem[N]);
  one_byte = 0xff;
  BOOST_CHECK_EQUAL(1, buf.read(&one_byte, 1));
  BOOST_CHECK_EQUAL(one_byte, N + 1);
}

BOOST_AUTO_TEST_CASE(test_maximum_buffer_size)
{
  TMemoryBuffer buf;
  buf.setMaxBufferSize(8192);
  std::vector<uint8_t> small_buff(1);

  for (size_t i = 0; i < 8192; ++i)
  {
    buf.write(&small_buff[0], 1);
  }

  BOOST_CHECK_THROW(buf.write(&small_buff[0], 1), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_buffer_overflow)
{
  TMemoryBuffer buf;
  std::vector<uint8_t> small_buff(1);
  buf.write(&small_buff[0], 1);
  BOOST_CHECK_THROW(buf.getWritePtr(std::numeric_limits<uint32_t>::max()), TTransportException);
}

BOOST_AUTO_TEST_CASE(test_memory_buffer_to_get_sizeof_objects)
{
  // This is a demonstration of how to use TMemoryBuffer to determine
  // the serialized size of a thrift object in the Binary protocol.
  // See THRIFT-3480

  shared_ptr<TMemoryBuffer> memBuffer(new TMemoryBuffer());
  shared_ptr<TBinaryProtocol> binaryProtcol(new TBinaryProtocol(memBuffer));

  thrift::test::Xtruct object;
  object.i32_thing = 10;
  object.i64_thing = 30;
  object.string_thing = "who's your daddy?";

  uint32_t size = object.write(binaryProtcol.get());
  BOOST_CHECK_EQUAL(47, size);
}

BOOST_AUTO_TEST_SUITE_END()
