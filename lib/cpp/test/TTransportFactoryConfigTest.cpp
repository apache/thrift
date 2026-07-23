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
#include <string>
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

// Behavioral coverage for THeaderTransport: like TFramedTransport, the header
// receive path must reject a frame larger than the caller-configured
// maxFrameSize, not only the internal MAX_FRAME_SIZE ceiling (~1 GiB). The
// frame below is a well-formed framed-binary frame (4-byte length prefix, a
// TBinaryProtocol VERSION_1 magic word, then padding to the declared length);
// an unfixed reader accepts it, so a rejection here is specifically the
// configured maxFrameSize check and not an incidental parse failure.
BOOST_AUTO_TEST_CASE(test_header_transport_enforces_custom_max_frame_size) {
  const int kMaxFrameSize = 64;
  auto config
      = std::make_shared<TConfiguration>(TConfiguration::DEFAULT_MAX_MESSAGE_SIZE, kMaxFrameSize);

  const uint32_t declaredFrameSize = 1024; // well beyond kMaxFrameSize
  const uint32_t version1 = 0x80010000u;   // TBinaryProtocol::VERSION_1
  std::vector<uint8_t> frame = {
      static_cast<uint8_t>((declaredFrameSize >> 24) & 0xFF),
      static_cast<uint8_t>((declaredFrameSize >> 16) & 0xFF),
      static_cast<uint8_t>((declaredFrameSize >> 8) & 0xFF),
      static_cast<uint8_t>(declaredFrameSize & 0xFF),
      static_cast<uint8_t>((version1 >> 24) & 0xFF),
      static_cast<uint8_t>((version1 >> 16) & 0xFF),
      static_cast<uint8_t>((version1 >> 8) & 0xFF),
      static_cast<uint8_t>(version1 & 0xFF),
  };
  // Pad the remainder of the declared frame body (magic already accounts for 4).
  frame.resize(frame.size() + declaredFrameSize - 4, 0x00);

  auto endpoint = std::make_shared<TMemoryBuffer>(frame.data(),
                                                  static_cast<uint32_t>(frame.size()),
                                                  TMemoryBuffer::COPY,
                                                  config);
  auto transport = std::make_shared<THeaderTransport>(endpoint, config);

  uint8_t dummy = 0;
  bool caught = false;
  try {
    transport->read(&dummy, 1);
  } catch (const TTransportException& e) {
    caught = true;
    BOOST_CHECK_EQUAL(e.getType(), TTransportException::CORRUPTED_DATA);
  }
  BOOST_CHECK(caught);
}

// Behavioral coverage for the post-transform path: a small ZLIB-compressed
// header frame that inflates beyond the configured maxFrameSize must be
// rejected, so a compressed frame cannot deliver a larger payload than an
// uncompressed frame would be permitted to. The frame is produced by the real
// header write path, so its framing and zlib stream are genuine.
BOOST_AUTO_TEST_CASE(test_header_transport_enforces_max_frame_size_after_transform) {
  auto writeBuffer = std::make_shared<TMemoryBuffer>();
  THeaderTransport writer(writeBuffer);
  writer.setTransform(THeaderTransport::ZLIB_TRANSFORM);
  const std::string payload(600, 'B');
  writer.write(reinterpret_cast<const uint8_t*>(payload.data()),
               static_cast<uint32_t>(payload.size()));
  writer.flush();
  const std::string frame = writeBuffer->getBufferAsString();

  // The compressed frame must be admitted by the size gate (so it is smaller
  // than kMaxFrameSize) while the 600-byte inflated payload exceeds it, leaving
  // the post-transform check as the only thing that can reject the frame.
  const int kMaxFrameSize = 256;
  BOOST_REQUIRE_LT(frame.size(), static_cast<size_t>(kMaxFrameSize));
  BOOST_REQUIRE_GT(payload.size(), static_cast<size_t>(kMaxFrameSize));

  auto config
      = std::make_shared<TConfiguration>(TConfiguration::DEFAULT_MAX_MESSAGE_SIZE, kMaxFrameSize);
  auto endpoint
      = std::make_shared<TMemoryBuffer>(reinterpret_cast<uint8_t*>(const_cast<char*>(frame.data())),
                                        static_cast<uint32_t>(frame.size()),
                                        TMemoryBuffer::COPY,
                                        config);
  auto reader = std::make_shared<THeaderTransport>(endpoint, config);

  uint8_t dummy = 0;
  bool caught = false;
  try {
    reader->read(&dummy, 1);
  } catch (const TTransportException& e) {
    caught = true;
    BOOST_CHECK_EQUAL(e.getType(), TTransportException::CORRUPTED_DATA);
  }
  BOOST_CHECK(caught);
}

// Positive control: the same compressed frame read under a generous limit (the
// library default maxFrameSize) must still round-trip. This guards against the
// rejection tests above passing vacuously -- if maxFrameSize_ were left at 0
// for THeaderTransport, every frame would be rejected and those tests would
// still pass. Here the default limit (~15.6 MiB) is far above the 600-byte
// inflated payload, so a correct implementation must accept and deliver it.
BOOST_AUTO_TEST_CASE(test_header_transport_accepts_frame_within_max_frame_size) {
  auto writeBuffer = std::make_shared<TMemoryBuffer>();
  THeaderTransport writer(writeBuffer);
  writer.setTransform(THeaderTransport::ZLIB_TRANSFORM);
  const std::string payload(600, 'B');
  writer.write(reinterpret_cast<const uint8_t*>(payload.data()),
               static_cast<uint32_t>(payload.size()));
  writer.flush();
  const std::string frame = writeBuffer->getBufferAsString();

  // Default configuration -> default maxFrameSize; no custom limit is set.
  auto endpoint
      = std::make_shared<TMemoryBuffer>(reinterpret_cast<uint8_t*>(const_cast<char*>(frame.data())),
                                        static_cast<uint32_t>(frame.size()),
                                        TMemoryBuffer::COPY);
  auto reader = std::make_shared<THeaderTransport>(endpoint);

  uint8_t first = 0;
  BOOST_CHECK_NO_THROW(reader->readAll(&first, 1));
  BOOST_CHECK_EQUAL(first, static_cast<uint8_t>('B'));
}
