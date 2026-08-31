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

#include <memory>
#include <string>

#include <thrift/TConfiguration.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/THttpClient.h>
#include <thrift/transport/THttpServer.h>
#include <thrift/transport/TTransportException.h>

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
