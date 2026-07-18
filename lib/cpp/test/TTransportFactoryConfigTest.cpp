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

// Regression coverage for the standard TTransportFactory implementations:
// getTransport() must hand the newly created wrapper the same TConfiguration
// instance the wrapped endpoint already carries, rather than letting the
// wrapper default-construct its own. doc/specs/thrift-tconfiguration.md
// requires every layer of the transport/protocol stack to share one
// TConfiguration instance by reference so that limits set by the caller
// (e.g. maxFrameSize) stay in effect once a transport is wrapped by a
// factory.

// TBinaryWebSocketServerTransportFactory / TTextWebSocketServerTransportFactory
// received the identical one-line fix (they share THttpServer's constructor
// chain) but are intentionally not exercised here: TWebSocketServer.h pulls
// in a hard, unconditional OpenSSL dependency (used for the RFC 6455
// handshake, unrelated to TConfiguration) that the rest of this file does
// not otherwise need.

#include <memory>
#include <vector>

#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/THeaderTransport.h>
#include <thrift/transport/THttpServer.h>
#include <thrift/transport/TTransportUtils.h>

#define BOOST_TEST_MODULE TTransportFactoryConfigTest
#include <boost/test/unit_test.hpp>

using namespace apache::thrift;
using namespace apache::thrift::transport;

namespace {

std::shared_ptr<TMemoryBuffer> endpointWithConfig(const std::shared_ptr<TConfiguration>& config) {
  return std::make_shared<TMemoryBuffer>(config);
}

template <typename Factory>
void checkPreservesConfiguration(Factory& factory) {
  auto config = std::make_shared<TConfiguration>();
  auto endpoint = endpointWithConfig(config);
  auto wrapped = factory.getTransport(endpoint);
  BOOST_REQUIRE(wrapped != nullptr);
  BOOST_CHECK_EQUAL(wrapped->getConfiguration().get(), config.get());
}

} // namespace

BOOST_AUTO_TEST_CASE(test_buffered_transport_factory_preserves_configuration) {
  TBufferedTransportFactory factory;
  checkPreservesConfiguration(factory);
}

BOOST_AUTO_TEST_CASE(test_framed_transport_factory_preserves_configuration) {
  TFramedTransportFactory factory;
  checkPreservesConfiguration(factory);
}

BOOST_AUTO_TEST_CASE(test_header_transport_factory_preserves_configuration) {
  THeaderTransportFactory factory;
  checkPreservesConfiguration(factory);
}

BOOST_AUTO_TEST_CASE(test_http_server_transport_factory_preserves_configuration) {
  THttpServerTransportFactory factory;
  checkPreservesConfiguration(factory);
}

BOOST_AUTO_TEST_CASE(test_piped_transport_factory_preserves_configuration) {
  auto config = std::make_shared<TConfiguration>();
  auto endpoint = endpointWithConfig(config);
  auto sink = std::make_shared<TMemoryBuffer>();
  TPipedTransportFactory factory(sink);
  auto wrapped = factory.getTransport(endpoint);
  BOOST_REQUIRE(wrapped != nullptr);
  BOOST_CHECK_EQUAL(wrapped->getConfiguration().get(), config.get());
}

// Behavioral check mirroring the concrete impact of the configuration loss:
// a transport wrapped by TFramedTransportFactory must enforce the caller's
// maxFrameSize, not the library default (256 MB). The frame carries a real
// (if dummy) 1024-byte payload -- with a fresh default configuration
// (unfixed factory), a 1024-byte frame is well within the default limit and
// is read successfully, so this only rejects for the intended reason
// (CORRUPTED_DATA from the maxFrameSize check) rather than incidentally
// failing on a truncated buffer.
BOOST_AUTO_TEST_CASE(test_framed_transport_factory_enforces_custom_max_frame_size) {
  const int kMaxFrameSize = 64;
  auto config = std::make_shared<TConfiguration>(TConfiguration::DEFAULT_MAX_MESSAGE_SIZE, kMaxFrameSize);

  // A 4-byte big-endian frame-length prefix declaring a 1024-byte frame,
  // well beyond kMaxFrameSize, followed by 1024 bytes of payload.
  const uint32_t declaredFrameSize = 1024;
  std::vector<uint8_t> frame = {
      static_cast<uint8_t>((declaredFrameSize >> 24) & 0xFF),
      static_cast<uint8_t>((declaredFrameSize >> 16) & 0xFF),
      static_cast<uint8_t>((declaredFrameSize >> 8) & 0xFF),
      static_cast<uint8_t>(declaredFrameSize & 0xFF),
  };
  frame.resize(frame.size() + declaredFrameSize, 0x41);
  auto endpoint = std::make_shared<TMemoryBuffer>(frame.data(),
                                                   static_cast<uint32_t>(frame.size()),
                                                   TMemoryBuffer::COPY,
                                                   config);

  TFramedTransportFactory factory;
  auto wrapped = factory.getTransport(endpoint);
  BOOST_CHECK_EQUAL(wrapped->getConfiguration().get(), config.get());

  uint8_t dummy = 0;
  bool caught = false;
  try {
    wrapped->read(&dummy, 1);
  } catch (const TTransportException& e) {
    caught = true;
    BOOST_CHECK_EQUAL(e.getType(), TTransportException::CORRUPTED_DATA);
  }
  BOOST_CHECK(caught);
}
