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

#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <cstring>
#include <locale>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#include <thrift/TConfiguration.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/THttpClient.h>
#include <thrift/transport/THttpServer.h>
#include <thrift/transport/TTransportException.h>
#include <thrift/transport/TVirtualTransport.h>

/*
 * THttpTransport keeps the line it is reading in httpBuf_, and grows that
 * buffer by doubling httpBufSize_ whenever a line does not fit.  Nothing
 * bounded the doubling, so a peer that never sends a CRLF decided how large
 * the buffer became: first by making the process hold it, and then, once
 * httpBufSize_ reached 2 GiB, by wrapping the uint32_t on the next doubling.
 *
 * These tests hold the growth to TConfiguration::maxMessageSize.  The inner
 * transport is given a budget of its own, large enough that it is not what
 * ends the read -- otherwise the test would pass on the unmodified library
 * for the wrong reason.
 */

BOOST_AUTO_TEST_SUITE(THttpBufferBoundTest)

using apache::thrift::TConfiguration;
using apache::thrift::transport::THttpClient;
using apache::thrift::transport::THttpServer;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TTransportException;

namespace {

// Exposes the buffer size so a test can tell how far the doubling went.
class TestHttpServer : public THttpServer {
public:
  TestHttpServer(std::shared_ptr<apache::thrift::transport::TTransport> transport,
                 std::shared_ptr<TConfiguration> config)
    : THttpServer(transport, config) {}

  uint32_t bufferSize() const { return httpBufSize_; }
};

class TestHttpClient : public THttpClient {
public:
  TestHttpClient(std::shared_ptr<apache::thrift::transport::TTransport> transport,
                 std::string host,
                 std::string path,
                 std::shared_ptr<TConfiguration> config)
    : THttpClient(transport, host, path, config) {}
};

// Counts what the peer was asked to hand over.  A bound on a declared length
// is only worth anything if it is applied before the bytes are read, and a
// test that asks whether something threw cannot tell that apart from running
// the stream out.
class CountingTransport
  : public apache::thrift::transport::TVirtualTransport<CountingTransport> {
public:
  explicit CountingTransport(std::string wire) : wire_(std::move(wire)), pos_(0), served_(0) {}

  uint32_t read(uint8_t* buf, uint32_t len) {
    if (pos_ >= wire_.size()) {
      return 0;
    }
    const uint32_t n = static_cast<uint32_t>((std::min)(static_cast<size_t>(len),
                                                        wire_.size() - pos_));
    std::memcpy(buf, wire_.data() + pos_, n);
    pos_ += n;
    served_ += n;
    return n;
  }

  void write(const uint8_t*, uint32_t) {}
  bool isOpen() const override { return true; }
  void open() override {}
  void close() override {}

  uint64_t served() const { return served_; }

private:
  std::string wire_;
  size_t pos_;
  uint64_t served_;
};

const uint32_t kMaxMessageSize = 64 * 1024;

std::shared_ptr<TestHttpServer> makeServer(const std::string& wire,
                                           std::shared_ptr<std::string>* keepAlive) {
  // The inner transport keeps the library default, so the only limit that can
  // stop the growth below is the one THttpServer enforces on itself.
  *keepAlive = std::make_shared<std::string>(wire);
  auto inner = std::make_shared<TMemoryBuffer>(
      reinterpret_cast<uint8_t*>(const_cast<char*>((*keepAlive)->data())),
      static_cast<uint32_t>((*keepAlive)->size()));
  auto config = std::make_shared<TConfiguration>(static_cast<int>(kMaxMessageSize));
  return std::make_shared<TestHttpServer>(inner, config);
}

} // namespace

BOOST_AUTO_TEST_CASE(header_line_without_crlf_is_bounded) {
  // A request line that never ends.  Before the bound, the buffer doubled
  // until it had swallowed all of it.
  std::string wire = "POST / HTTP/1.1\r\nX-Filler: " + std::string(4 * 1024 * 1024, 'A');

  std::shared_ptr<std::string> keepAlive;
  auto trans = makeServer(wire, &keepAlive);

  uint8_t out[16];
  BOOST_CHECK_THROW(trans->read(out, sizeof(out)), TTransportException);
  BOOST_CHECK_LE(trans->bufferSize(), kMaxMessageSize);
}

BOOST_AUTO_TEST_CASE(many_short_headers_are_bounded) {
  // Every line is short, so the buffer never has to grow, but the headers
  // together are far larger than the buffer.  This has to keep working.
  std::string wire = "POST / HTTP/1.1\r\n";
  for (int i = 0; i < 2000; ++i) {
    wire += "X-Filler-" + std::to_string(i) + ": " + std::string(60, 'A') + "\r\n";
  }
  wire += "Content-Length: 5\r\n\r\nhello";

  std::shared_ptr<std::string> keepAlive;
  auto trans = makeServer(wire, &keepAlive);

  uint8_t out[5];
  BOOST_CHECK_EQUAL(trans->read(out, sizeof(out)), 5u);
  BOOST_CHECK_EQUAL(std::string(reinterpret_cast<char*>(out), 5), "hello");
  BOOST_CHECK_LE(trans->bufferSize(), kMaxMessageSize);
}

BOOST_AUTO_TEST_CASE(a_long_but_bounded_header_line_still_works) {
  // One header line of 16 KB: longer than the initial buffer, well inside the
  // configured maximum, so the doubling still has to happen.
  std::string wire = "POST / HTTP/1.1\r\nX-Filler: " + std::string(16 * 1024, 'A')
                     + "\r\nContent-Length: 5\r\n\r\nhello";

  std::shared_ptr<std::string> keepAlive;
  auto trans = makeServer(wire, &keepAlive);

  uint8_t out[5];
  BOOST_CHECK_EQUAL(trans->read(out, sizeof(out)), 5u);
  BOOST_CHECK_EQUAL(std::string(reinterpret_cast<char*>(out), 5), "hello");
  BOOST_CHECK_GT(trans->bufferSize(), 1024u);
  BOOST_CHECK_LE(trans->bufferSize(), kMaxMessageSize);
}

BOOST_AUTO_TEST_CASE(a_declared_body_larger_than_the_maximum_is_refused) {
  // Four times the maximum is actually on the wire, so a transport that reads
  // the body before looking at the number will take all of it and only then
  // run out. The verdict is how much the peer was asked for.
  const std::string body(4 * kMaxMessageSize, 'A');
  const std::string wire = "POST / HTTP/1.1\r\nContent-length: 1000000\r\n\r\n" + body;

  auto inner = std::make_shared<CountingTransport>(wire);
  auto config = std::make_shared<TConfiguration>(static_cast<int>(kMaxMessageSize));
  TestHttpServer trans(inner, config);

  uint8_t out[16];
  BOOST_CHECK_THROW(trans.read(out, sizeof(out)), TTransportException);
  BOOST_CHECK_LT(inner->served(), kMaxMessageSize);
}

BOOST_AUTO_TEST_CASE(a_body_inside_the_maximum_is_read) {
  const std::string body(kMaxMessageSize / 2, 'A');
  const std::string wire = "POST / HTTP/1.1\r\nContent-length: " + std::to_string(body.size())
                           + "\r\n\r\n" + body;

  auto inner = std::make_shared<CountingTransport>(wire);
  auto config = std::make_shared<TConfiguration>(static_cast<int>(kMaxMessageSize));
  TestHttpServer trans(inner, config);

  std::vector<uint8_t> out(body.size());
  BOOST_CHECK_EQUAL(trans.readAll(out.data(), static_cast<uint32_t>(out.size())), out.size());
  BOOST_CHECK_EQUAL(std::string(reinterpret_cast<char*>(out.data()), out.size()), body);
}

BOOST_AUTO_TEST_CASE(chunks_are_bounded_by_their_sum_and_not_one_at_a_time) {
  // Every chunk on its own is far inside the maximum; what the peer chooses is
  // how many of them there are. Without a running total this stream ends only
  // when the peer stops sending.
  const uint32_t chunkBytes = 4 * 1024;
  std::ostringstream size;
  // A chunk size is hex, and a stream follows whatever global locale is in
  // force, so a grouping locale turns 4096 into "1.000" and it stops being a
  // chunk size at all. THRIFT-6194 fixed the case that used to leave one set
  // for the rest of this binary; this stays as the local guarantee, which is
  // what TToString.h does for the same reason.
  size.imbue(std::locale::classic());
  size << std::hex << chunkBytes;
  std::string wire = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n";
  for (uint32_t sent = 0; sent < 4 * kMaxMessageSize; sent += chunkBytes) {
    wire += size.str() + "\r\n" + std::string(chunkBytes, 'A') + "\r\n";
  }
  wire += "0\r\n\r\n";

  auto inner = std::make_shared<CountingTransport>(wire);
  auto config = std::make_shared<TConfiguration>(static_cast<int>(kMaxMessageSize));
  TestHttpClient client(inner, "localhost", "/", config);

  // Read in pieces well under the maximum: read() has always refused a single
  // request larger than it, and asking for the whole body at once would be
  // stopped by that check instead of by the one under test.
  std::vector<uint8_t> out(4096);
  bool threw = false;
  try {
    for (uint32_t taken = 0; taken < 4 * kMaxMessageSize; taken += static_cast<uint32_t>(out.size())) {
      client.readAll(out.data(), static_cast<uint32_t>(out.size()));
    }
  } catch (const TTransportException&) {
    threw = true;
  }
  BOOST_CHECK(threw);
  // The chunk framing costs a few bytes per chunk on top of the payload, so
  // this is the maximum plus that overhead rather than the maximum exactly.
  BOOST_CHECK_LT(inner->served(), 2 * kMaxMessageSize);
}

BOOST_AUTO_TEST_CASE(a_second_request_gets_its_own_allowance) {
  // The maximum is per message. Two bodies that each fit have to both be read
  // on one connection, or a keep-alive connection would run out of allowance.
  const std::string body(kMaxMessageSize - 1024, 'A');
  const std::string one = "POST / HTTP/1.1\r\nContent-length: " + std::to_string(body.size())
                          + "\r\n\r\n" + body;

  auto inner = std::make_shared<CountingTransport>(one + one);
  auto config = std::make_shared<TConfiguration>(static_cast<int>(kMaxMessageSize));
  TestHttpServer trans(inner, config);

  std::vector<uint8_t> out(body.size());
  BOOST_CHECK_EQUAL(trans.readAll(out.data(), static_cast<uint32_t>(out.size())), out.size());
  trans.flush();
  BOOST_CHECK_EQUAL(trans.readAll(out.data(), static_cast<uint32_t>(out.size())), out.size());
}

BOOST_AUTO_TEST_CASE(a_chunked_response_still_works) {
  // Chunked bodies go through readLine() for every chunk size, so they share
  // the buffer that is now bounded.
  std::string wire = "HTTP/1.1 200 OK\r\n"
                     "Transfer-Encoding: chunked\r\n"
                     "\r\n"
                     "3\r\nhel\r\n"
                     "2\r\nlo\r\n"
                     "0\r\n\r\n";

  auto keepAlive = std::make_shared<std::string>(wire);
  auto inner = std::make_shared<TMemoryBuffer>(
      reinterpret_cast<uint8_t*>(const_cast<char*>(keepAlive->data())),
      static_cast<uint32_t>(keepAlive->size()));
  auto config = std::make_shared<TConfiguration>(static_cast<int>(kMaxMessageSize));
  THttpClient client(inner, "localhost", "/", config);

  uint8_t out[5];
  BOOST_CHECK_EQUAL(client.readAll(out, sizeof(out)), 5u);
  BOOST_CHECK_EQUAL(std::string(reinterpret_cast<char*>(out), 5), "hello");
}

BOOST_AUTO_TEST_SUITE_END()
