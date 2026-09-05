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
 * THttpServer::parseHeader compared a header name for as many characters as
 * the *received* name happened to have:
 *
 *   size_t sz = colon - header;
 *   if (THRIFT_strncasecmp(header, "Content-length", sz) == 0) ...
 *
 * so every name that is a prefix of a name the transport knows was accepted
 * as that name -- "C: 5" set the content length and "T: chunked" switched on
 * chunked decoding.  A header name means what RFC 9110 5.1 says it means, and
 * a transport that reads it otherwise ends up disagreeing with every other
 * party on the connection about where the message ends.
 *
 * The value side is the same question about a number: atoi() cannot report
 * that it was handed a negative value or something that is not a number at
 * all, and "Content-length: -1" therefore arrived in the uint32_t member as
 * 4294967295.
 */

BOOST_AUTO_TEST_SUITE(THttpHeaderParseTest)

using apache::thrift::TConfiguration;
using apache::thrift::transport::THttpClient;
using apache::thrift::transport::THttpServer;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;

namespace {

// The parsed framing is what these tests are about, so they read it directly
// rather than inferring it from how many bytes came back.
class TestHttpServer : public THttpServer {
public:
  TestHttpServer(std::shared_ptr<TTransport> transport, std::shared_ptr<TConfiguration> config)
    : THttpServer(transport, config) {}
  uint32_t contentLength() const { return contentLength_; }
  bool chunked() const { return chunked_; }
  const std::string& origin() const { return origin_; }
};

class TestHttpClient : public THttpClient {
public:
  TestHttpClient(std::shared_ptr<TTransport> transport,
                 std::string host,
                 std::string path,
                 std::shared_ptr<TConfiguration> config)
    : THttpClient(transport, host, path, config) {}
  uint32_t contentLength() const { return contentLength_; }
  bool chunked() const { return chunked_; }
};

std::shared_ptr<TMemoryBuffer> wireBuffer(const std::string& wire,
                                          std::shared_ptr<std::string>* keepAlive) {
  *keepAlive = std::make_shared<std::string>(wire);
  return std::make_shared<TMemoryBuffer>(
      reinterpret_cast<uint8_t*>(const_cast<char*>((*keepAlive)->data())),
      static_cast<uint32_t>((*keepAlive)->size()));
}

// A request whose only interesting part is the one header under test, with a
// five byte body behind it so that a transport which believes the header can
// go and fetch one.
std::string request(const std::string& header) {
  return "POST / HTTP/1.1\r\nHost: localhost\r\n" + header + "\r\n\r\nhello";
}

} // namespace

BOOST_AUTO_TEST_CASE(a_full_content_length_header_is_read) {
  std::shared_ptr<std::string> keepAlive;
  TestHttpServer trans(wireBuffer(request("Content-length: 5"), &keepAlive),
                       std::make_shared<TConfiguration>());

  uint8_t out[5];
  BOOST_CHECK_EQUAL(trans.read(out, sizeof(out)), 5u);
  BOOST_CHECK_EQUAL(std::string(reinterpret_cast<char*>(out), 5), "hello");
  BOOST_CHECK_EQUAL(trans.contentLength(), 5u);
}

BOOST_AUTO_TEST_CASE(the_header_name_is_matched_in_full_and_not_by_prefix) {
  // "C" is not a header name this transport knows, and no other party on the
  // connection would read it as one either.
  const char* const prefixes[] = {"C: 5", "Co: 5", "Content: 5", "Content-len: 5"};

  for (const char* header : prefixes) {
    std::shared_ptr<std::string> keepAlive;
    TestHttpServer trans(wireBuffer(request(header), &keepAlive),
                         std::make_shared<TConfiguration>());

    uint8_t out[5];
    // No content length, so there is no body to hand out.
    trans.read(out, sizeof(out));
    BOOST_CHECK_MESSAGE(trans.contentLength() == 0u,
                        std::string("\"") + header + "\" was read as a content length");
  }
}

BOOST_AUTO_TEST_CASE(a_prefix_of_transfer_encoding_does_not_switch_on_chunking) {
  const char* const prefixes[] = {"T: chunked", "Tr: chunked", "Transfer: chunked"};

  for (const char* header : prefixes) {
    std::shared_ptr<std::string> keepAlive;
    TestHttpServer trans(wireBuffer(request(header), &keepAlive),
                         std::make_shared<TConfiguration>());

    uint8_t out[5];
    // Reading a body that is not chunked as though it were runs the stream
    // out, so both outcomes end in an exception here and only the parsed flag
    // tells them apart.
    try {
      trans.read(out, sizeof(out));
    } catch (const TTransportException&) {
    }
    BOOST_CHECK_MESSAGE(!trans.chunked(),
                        std::string("\"") + header + "\" switched on chunked decoding");
  }
}

BOOST_AUTO_TEST_CASE(a_prefix_of_x_forwarded_for_does_not_set_the_origin) {
  std::shared_ptr<std::string> keepAlive;
  TestHttpServer trans(wireBuffer(request("X: 203.0.113.7"), &keepAlive),
                       std::make_shared<TConfiguration>());

  uint8_t out[5];
  trans.read(out, sizeof(out));
  BOOST_CHECK(trans.origin().empty());
}

BOOST_AUTO_TEST_CASE(the_full_transfer_encoding_header_still_works) {
  std::string wire = "HTTP/1.1 200 OK\r\n"
                     "Transfer-Encoding: chunked\r\n"
                     "\r\n"
                     "3\r\nhel\r\n"
                     "2\r\nlo\r\n"
                     "0\r\n\r\n";

  std::shared_ptr<std::string> keepAlive;
  TestHttpClient client(wireBuffer(wire, &keepAlive), "localhost", "/",
                        std::make_shared<TConfiguration>());

  uint8_t out[5];
  BOOST_CHECK_EQUAL(client.readAll(out, sizeof(out)), 5u);
  BOOST_CHECK_EQUAL(std::string(reinterpret_cast<char*>(out), 5), "hello");
  BOOST_CHECK(client.chunked());
}

BOOST_AUTO_TEST_CASE(a_negative_content_length_is_refused_by_the_server) {
  // atoi() gave -1, which the uint32_t member then held as 4294967295.
  std::shared_ptr<std::string> keepAlive;
  TestHttpServer trans(wireBuffer(request("Content-length: -1"), &keepAlive),
                       std::make_shared<TConfiguration>());

  uint8_t out[5];
  BOOST_CHECK_THROW(trans.read(out, sizeof(out)), TTransportException);
  // Not "did it throw" -- an over-declared length outruns a five byte body and
  // throws END_OF_FILE on the unmodified library too. What is under test is
  // that no length was taken from "-1" at all.
  BOOST_CHECK_EQUAL(trans.contentLength(), 0u);
}

BOOST_AUTO_TEST_CASE(a_negative_content_length_is_refused_by_the_client) {
  std::string wire = "HTTP/1.1 200 OK\r\nContent-Length: -1\r\n\r\nhello";

  std::shared_ptr<std::string> keepAlive;
  TestHttpClient client(wireBuffer(wire, &keepAlive), "localhost", "/",
                        std::make_shared<TConfiguration>());

  uint8_t out[5];
  BOOST_CHECK_THROW(client.readAll(out, sizeof(out)), TTransportException);
  BOOST_CHECK_EQUAL(client.contentLength(), 0u);
}

BOOST_AUTO_TEST_CASE(a_content_length_that_is_not_a_number_is_refused) {
  const char* const values[] = {"Content-length: abc", "Content-length: ", "Content-length: 5x"};

  for (const char* header : values) {
    std::shared_ptr<std::string> keepAlive;
    TestHttpServer trans(wireBuffer(request(header), &keepAlive),
                         std::make_shared<TConfiguration>());

    uint8_t out[5];
    BOOST_CHECK_THROW(trans.read(out, sizeof(out)), TTransportException);
    BOOST_CHECK_EQUAL(trans.contentLength(), 0u);
  }
}

BOOST_AUTO_TEST_CASE(a_content_length_beyond_uint32_is_refused) {
  // atoi() has no way to say "that does not fit"; it is undefined behaviour,
  // and in practice the value arrived truncated.
  std::shared_ptr<std::string> keepAlive;
  TestHttpServer trans(wireBuffer(request("Content-length: 99999999999"), &keepAlive),
                       std::make_shared<TConfiguration>());

  uint8_t out[5];
  BOOST_CHECK_THROW(trans.read(out, sizeof(out)), TTransportException);
  // Same trap as above: the unmodified library throws here as well, having
  // stored the truncated 1215752191 and then run out of body. The number is
  // the test.
  BOOST_CHECK_EQUAL(trans.contentLength(), 0u);
}

BOOST_AUTO_TEST_SUITE_END()
