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
#include <cstdint>
#include <cstring>
#include <deque>
#include <memory>
#include <string>

#include <thrift/TConfiguration.h>
#include <thrift/transport/TVirtualTransport.h>
#include <thrift/transport/TWebSocketServer.h>

/*
 * A WebSocket frame header carries the payload length, and readFrame() used it
 * to size the read buffer before any payload byte had arrived.  Fourteen bytes
 * on the wire therefore decided how much memory the server committed, up to
 * the 4 GiB a 64-bit length field can name.
 *
 * These tests hold the declared length to TConfiguration::maxFrameSize, the
 * same ceiling TFramedTransport applies to its own frames.  What they measure
 * is the largest read the server asked of the transport underneath it: a frame
 * that was refused before the allocation never asks for its payload, while one
 * that was believed asks for all of it whether or not it ever arrives.  Asking
 * only "did the read fail?" would pass on the unmodified library too, because
 * a payload that never arrives ends the frame either way.
 */

BOOST_AUTO_TEST_SUITE(TWebSocketServerTest)

using apache::thrift::TConfiguration;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TVirtualTransport;
using apache::thrift::transport::TWebSocketServer;

namespace {

// A transport whose inbound side is a script of chunks: one read() is served
// from one chunk, so a test says exactly how the peer's bytes arrive.  It also
// remembers the largest read it was asked for, which is what distinguishes a
// declared length that was believed from one that was not.
class ScriptedTransport : public TVirtualTransport<ScriptedTransport> {
public:
  void feed(const std::string& chunk) { inbound_.push_back(chunk); }

  uint32_t read(uint8_t* buf, uint32_t len) {
    largestRead_ = (std::max)(largestRead_, len);
    noteStackDepth();
    if (inbound_.empty()) {
      return 0;
    }
    std::string& front = inbound_.front();
    auto give = (std::min)(static_cast<size_t>(len), front.size());
    std::memcpy(buf, front.data(), give);
    front.erase(0, give);
    if (front.empty()) {
      inbound_.pop_front();
    }
    return static_cast<uint32_t>(give);
  }

  void write(const uint8_t* buf, uint32_t len) {
    outbound_.append(reinterpret_cast<const char*>(buf), len);
  }

  void open() override { open_ = true; }
  bool isOpen() const override { return open_; }
  void close() override { open_ = false; }

  uint32_t largestRead() const { return largestRead_; }
  const std::string& outbound() const { return outbound_; }

  // How far below the caller's own frame the reader got. A reader that
  // re-enters itself per frame shows up here and nowhere else: the frames it
  // consumes and the bytes it hands over are the same either way.
  void anchorStack(const void* here) {
    anchor_ = reinterpret_cast<uintptr_t>(here);
    deepestStack_ = 0;
  }
  size_t deepestStack() const { return deepestStack_; }

private:
  void noteStackDepth() {
    if (anchor_ == 0) {
      return;
    }
    char here;
    auto now = reinterpret_cast<uintptr_t>(&here);
    size_t depth = (now > anchor_) ? (now - anchor_) : (anchor_ - now);
    deepestStack_ = (std::max)(deepestStack_, depth);
  }

  std::deque<std::string> inbound_;
  std::string outbound_;
  uint32_t largestRead_ = 0;
  uintptr_t anchor_ = 0;
  size_t deepestStack_ = 0;
  bool open_ = true;
};

const char* kHandshake
    = "GET / HTTP/1.1\r\n"
      "Upgrade: websocket\r\n"
      "Connection: Upgrade\r\n"
      "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
      "Sec-WebSocket-Version: 13\r\n"
      "\r\n";

// Everything the handshake itself reads: refill() asks for the initial HTTP
// buffer, and the frame header reads that follow ask for 2, 8 and 4 bytes.
// Any read larger than this came from a declared payload length.
const uint32_t kHandshakeReadCeiling = 1024;

const uint8_t kMask[4] = {0x37, 0xFA, 0x21, 0x3D};

void appendBigEndian(std::string& out, uint64_t value, int bytes) {
  for (int i = bytes - 1; i >= 0; --i) {
    out.push_back(static_cast<char>((value >> (i * 8)) & 0xFF));
  }
}

// A masked client frame declaring `declaredLength` bytes of payload and
// actually carrying `payload`.  The two differ in the tests where the point is
// that the declaration alone must not be acted on.
std::string clientFrame(uint64_t declaredLength, const std::string& payload, uint8_t opcode = 0x2) {
  std::string frame;
  frame.push_back(static_cast<char>(0x80 | opcode)); // FIN
  if (declaredLength < 126) {
    frame.push_back(static_cast<char>(0x80 | declaredLength)); // MASK
  } else if (declaredLength < 65536) {
    frame.push_back(static_cast<char>(0x80 | 126));
    appendBigEndian(frame, declaredLength, 2);
  } else {
    frame.push_back(static_cast<char>(0x80 | 127));
    appendBigEndian(frame, declaredLength, 8);
  }
  frame.append(reinterpret_cast<const char*>(kMask), 4);
  for (size_t i = 0; i < payload.size(); ++i) {
    frame.push_back(static_cast<char>(payload[i] ^ kMask[i % 4]));
  }
  return frame;
}

std::shared_ptr<TConfiguration> withMaxFrameSize(int maxFrameSize) {
  return std::make_shared<TConfiguration>(TConfiguration::DEFAULT_MAX_MESSAGE_SIZE, maxFrameSize);
}

// The server is handed back as a TTransport, which is how a transport factory
// hands it to a protocol: readAll() on the concrete type resolves to the CRTP
// helper in TVirtualTransport and would never reach the WebSocket framing.
std::shared_ptr<ScriptedTransport> connect(std::shared_ptr<TTransport>* server,
                                           std::shared_ptr<TConfiguration> config = nullptr) {
  auto inner = std::make_shared<ScriptedTransport>();
  inner->feed(kHandshake);
  *server = std::make_shared<TWebSocketServer<true> >(inner, config);
  return inner;
}

// True if the last frame written to the peer is a Close carrying `code`.  The
// length byte is deliberately not asserted on: what matters here is that the
// peer was told why the connection ended.
bool closedWith(const std::string& outbound, uint16_t code) {
  if (outbound.size() < 4) {
    return false;
  }
  const auto* tail = reinterpret_cast<const uint8_t*>(outbound.data()) + outbound.size() - 4;
  return tail[0] == 0x88 && ((tail[2] << 8) | tail[3]) == code;
}

const uint16_t kMessageTooBig = 1009;

const uint8_t kPingOpcode = 0x9;

// Enough pings to separate a reader that recurses from one that does not, and
// far too few to run any stack out: at the ~160 bytes a frame costs, the
// unmodified reader passes 300 kB here while the whole run stays four orders of
// magnitude inside the eight megabytes a Linux thread starts with.
const int kPings = 2000;
const size_t kStackCeiling = 64 * 1024;

} // namespace

BOOST_AUTO_TEST_CASE(a_frame_over_the_default_maximum_is_refused) {
  // Sixty-four megabytes declared, fourteen bytes sent.  The library default
  // for maxFrameSize is 16 MB, so this frame has to be refused on its header.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  inner->feed(clientFrame(64 * 1024 * 1024, ""));

  uint8_t out[16];
  BOOST_CHECK_EQUAL(server->readAll(out, sizeof(out)), 0u);
  BOOST_CHECK_LE(inner->largestRead(), kHandshakeReadCeiling);
  BOOST_CHECK(closedWith(inner->outbound(), kMessageTooBig));
}

BOOST_AUTO_TEST_CASE(the_bound_is_the_configured_maximum) {
  // An operator who lowers maxFrameSize gets the lower bound, and one who
  // raises it gets the higher one.  4 KiB is far below any default, so a frame
  // of 32 KiB tells the two apart.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server, withMaxFrameSize(4096));
  inner->feed(clientFrame(32 * 1024, ""));

  uint8_t out[16];
  BOOST_CHECK_EQUAL(server->readAll(out, sizeof(out)), 0u);
  BOOST_CHECK_LE(inner->largestRead(), kHandshakeReadCeiling);
  BOOST_CHECK(closedWith(inner->outbound(), kMessageTooBig));
}

BOOST_AUTO_TEST_CASE(a_frame_of_the_maximum_size_is_still_accepted) {
  // The bound is a maximum, not a limit one below it: a frame of exactly
  // maxFrameSize still has to be read.
  const uint32_t kMax = 4096;
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server, withMaxFrameSize(kMax));
  std::string payload(kMax, 'x');
  inner->feed(clientFrame(kMax, payload));

  std::string got(kMax, '\0');
  BOOST_CHECK_EQUAL(server->readAll(reinterpret_cast<uint8_t*>(&got[0]), kMax), kMax);
  BOOST_CHECK_EQUAL(got, payload);
}

BOOST_AUTO_TEST_CASE(an_ordinary_frame_still_reads) {
  // The regression guard: a small frame is unmasked and handed over whole.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  const std::string payload = "the quick brown fox jumps over the lazy dog";
  inner->feed(clientFrame(payload.size(), payload));

  std::string got(payload.size(), '\0');
  BOOST_CHECK_EQUAL(server->readAll(reinterpret_cast<uint8_t*>(&got[0]),
                                    static_cast<uint32_t>(payload.size())),
                    payload.size());
  BOOST_CHECK_EQUAL(got, payload);
}

BOOST_AUTO_TEST_CASE(consecutive_frames_still_read) {
  // Each frame is bounded on its own, so a second one has to be readable after
  // the first.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  inner->feed(clientFrame(5, "alpha"));
  inner->feed(clientFrame(4, "beta"));

  char got[5];
  BOOST_CHECK_EQUAL(server->readAll(reinterpret_cast<uint8_t*>(got), 5), 5u);
  BOOST_CHECK_EQUAL(std::string(got, 5), "alpha");
  BOOST_CHECK_EQUAL(server->readAll(reinterpret_cast<uint8_t*>(got), 4), 4u);
  BOOST_CHECK_EQUAL(std::string(got, 4), "beta");
}

BOOST_AUTO_TEST_CASE(a_payload_split_across_reads_still_arrives_whole) {
  // The transport underneath hands over one chunk per read, which is what a
  // socket does with anything larger than a segment. The frame is ordinary and
  // well inside every limit; the only thing unusual about it is that it does
  // not arrive all at once.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  const std::string payload(4000, 'A');
  const std::string frame = clientFrame(payload.size(), payload);
  inner->feed(frame.substr(0, 8));
  inner->feed(frame.substr(8, 1000));
  inner->feed(frame.substr(1008));

  std::string got(payload.size(), '\0');
  BOOST_CHECK_EQUAL(server->readAll(reinterpret_cast<uint8_t*>(&got[0]),
                                    static_cast<uint32_t>(payload.size())),
                    payload.size());
  BOOST_CHECK_EQUAL(got, payload);
}

BOOST_AUTO_TEST_CASE(a_payload_that_stops_early_is_still_end_of_stream) {
  // Waiting for the rest of a frame must not turn a peer that went away into a
  // half-read frame handed to the protocol.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  const std::string payload(4000, 'A');
  const std::string frame = clientFrame(payload.size(), payload);
  inner->feed(frame.substr(0, 500));

  uint8_t out[16];
  BOOST_CHECK_EQUAL(server->readAll(out, sizeof(out)), 0u);
}

BOOST_AUTO_TEST_CASE(a_run_of_pings_is_answered_from_one_stack_frame) {
  // A Ping carries nothing for the caller, so the reader has to go on to the
  // next frame to satisfy it. Doing that by re-entering itself costs a stack
  // frame per Ping, and a Ping is seven bytes on the wire.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  std::string wire;
  for (int i = 0; i < kPings; ++i) {
    wire += clientFrame(1, "x", kPingOpcode);
  }
  wire += clientFrame(5, "hello");
  inner->feed(wire);

  char anchor;
  inner->anchorStack(&anchor);

  char got[5];
  BOOST_CHECK_EQUAL(server->readAll(reinterpret_cast<uint8_t*>(got), 5), 5u);
  BOOST_CHECK_EQUAL(std::string(got, 5), "hello");
  BOOST_CHECK_LE(inner->deepestStack(), kStackCeiling);
}

BOOST_AUTO_TEST_CASE(a_length_with_the_high_bit_set_is_still_refused) {
  // Unchanged behaviour, kept here so the new bound cannot be mistaken for the
  // only thing standing between the header and the allocation.
  std::shared_ptr<TTransport> server;
  auto inner = connect(&server);
  inner->feed(clientFrame(0x8000000000000000ULL, ""));

  uint8_t out[16];
  BOOST_CHECK_THROW(server->readAll(out, sizeof(out)),
                    apache::thrift::transport::TTransportException);
  BOOST_CHECK_LE(inner->largestRead(), kHandshakeReadCeiling);
}

BOOST_AUTO_TEST_SUITE_END()
