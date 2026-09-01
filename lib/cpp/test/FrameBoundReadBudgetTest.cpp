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

#include <cstring>
#include <memory>
#include <string>
#include <vector>

#include <thrift/TConfiguration.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/protocol/TList.h>
#include <thrift/transport/TBufferTransports.h>
#include <thrift/transport/TTransportException.h>

/*
 * TFramedTransport::readFrame() learns the exact size of the message it is
 * about to hand to the protocol, but the read budget it enforces on the
 * protocol's behalf stays at TConfiguration::maxMessageSize for the life of
 * the connection.  A 68-byte frame is therefore allowed to declare a 64 MB
 * field, and the mismatch is only discovered after the 64 MB has been
 * allocated.  These tests bind the budget to the frame instead.
 *
 * Three groups, and the distinction matters when reading a failure:
 *
 *   - The first group describes traffic that has always been legitimate.  It
 *     passes on the unmodified library and must keep passing.  It exists
 *     because the obvious implementation breaks it:
 *     resetConsumedMessageSize() refuses to grow a budget, so a second frame
 *     larger than the first is rejected unless every frame performs a full
 *     reset before binding; and updateKnownMessageSize() alone carries the
 *     previous frame's consumption forward, so a budget bound that way shrinks
 *     frame by frame.
 *
 *   - The second group describes the behaviour being added to the framed
 *     transport: a small frame may not declare a field or a container larger
 *     than itself, and each frame is entitled to a budget of its own.  It
 *     fails on the unmodified library.
 *
 *   - The third group covers TMemoryBuffer::bindMessageSizeToBuffer(), which
 *     TNonblockingServer uses because it takes the frame apart itself and
 *     never goes through TFramedTransport at all.
 */

BOOST_AUTO_TEST_SUITE(FrameBoundReadBudgetTest)

using apache::thrift::TConfiguration;
using apache::thrift::protocol::TBinaryProtocol;
using apache::thrift::protocol::TType;
using apache::thrift::transport::TFramedTransport;
using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::transport::TTransportException;

namespace {

void appendI32(std::vector<uint8_t>& out, int32_t value) {
  out.push_back(static_cast<uint8_t>((value >> 24) & 0xff));
  out.push_back(static_cast<uint8_t>((value >> 16) & 0xff));
  out.push_back(static_cast<uint8_t>((value >> 8) & 0xff));
  out.push_back(static_cast<uint8_t>(value & 0xff));
}

/// Appends one frame: a 4-byte big-endian length followed by that many bytes.
void appendFrame(std::vector<uint8_t>& out, const std::vector<uint8_t>& payload) {
  appendI32(out, static_cast<int32_t>(payload.size()));
  out.insert(out.end(), payload.begin(), payload.end());
}

std::vector<uint8_t> filler(size_t n) {
  std::vector<uint8_t> b(n);
  for (size_t i = 0; i < n; i++) {
    b[i] = static_cast<uint8_t>(i);
  }
  return b;
}

/**
 * A binary-protocol string field body: a 4-byte length followed by that many
 * bytes.  Passing declaredLength > actualBytes builds a payload whose declared
 * length disagrees with the bytes actually present.
 */
std::vector<uint8_t> declaredString(int32_t declaredLength, size_t actualBytes) {
  std::vector<uint8_t> body;
  appendI32(body, declaredLength);
  std::vector<uint8_t> data = filler(actualBytes);
  body.insert(body.end(), data.begin(), data.end());
  return body;
}

/**
 * Wraps wire data in a TFramedTransport.
 *
 * The underlying TMemoryBuffer is only a stand-in for a socket: unlike
 * netstd's TMemoryBufferTransport it does not bind the budget in its
 * constructor, and in C++ the budget under test lives on the TFramedTransport
 * itself rather than on the endpoint below it, so what the inner transport
 * does with its own counters is not observable here.
 */
std::shared_ptr<TFramedTransport> framedOver(std::vector<uint8_t>& wire, int maxMessageSize) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(maxMessageSize));
  std::shared_ptr<TMemoryBuffer> buffer(
      new TMemoryBuffer(wire.data(), static_cast<uint32_t>(wire.size()), TMemoryBuffer::OBSERVE));
  return std::shared_ptr<TFramedTransport>(new TFramedTransport(buffer, config));
}

} // namespace

// ---------------------------------------------------------------------------
// Traffic that has always been legitimate, and must remain so.
// ---------------------------------------------------------------------------

/**
 * Two frames on one connection, the second 256x the first, with no readEnd()
 * in between.  A budget that is bound to each frame without first being reset
 * to the configured maximum cannot grow again, and rejects the second frame.
 */
BOOST_AUTO_TEST_CASE(test_consecutive_frames_growing_in_size) {
  std::vector<uint8_t> wire;
  appendFrame(wire, filler(16));
  appendFrame(wire, filler(4096));

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 1024 * 1024);

  uint8_t first[16];
  BOOST_CHECK_EQUAL(trans->readAll(first, sizeof(first)), 16u);

  // Second frame. Nothing has reset the budget in between.
  std::vector<uint8_t> second(4096);
  BOOST_CHECK_EQUAL(trans->readAll(second.data(), static_cast<uint32_t>(second.size())), 4096u);
}

/**
 * read() is allowed to return fewer bytes than it was asked for, and a caller
 * reading a framed stream into a buffer of its own choosing relies on that --
 * TransportTest does it throughout with random chunk sizes.  Asking for more
 * than the frame holds must give a short read, not a budget failure, both from
 * inside a frame and with the previous one spent.
 */
BOOST_AUTO_TEST_CASE(test_oversized_request_gives_a_short_read) {
  std::vector<uint8_t> wire;
  appendFrame(wire, filler(100));
  appendFrame(wire, filler(50));

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 1024 * 1024);

  std::vector<uint8_t> buf(4096);

  // From an empty buffer: fetches the frame and hands over all 100 bytes.
  BOOST_CHECK_EQUAL(trans->read(buf.data(), 4096u), 100u);
  // Mid-frame, after taking 30 of the next frame's 50.
  BOOST_CHECK_EQUAL(trans->read(buf.data(), 30u), 30u);
  BOOST_CHECK_EQUAL(trans->read(buf.data(), 4096u), 20u);
}

/**
 * Two frames of the same size, each holding a string the protocol reads
 * through the borrow/consume fast path.  consume() is the one read path that
 * decrements the budget, so this is what makes a binding that carries the
 * previous frame's consumption forward shrink from frame to frame.
 */
BOOST_AUTO_TEST_CASE(test_consecutive_frames_with_consumed_strings) {
  const size_t kStringSize = 200;

  std::vector<uint8_t> wire;
  appendFrame(wire, declaredString(static_cast<int32_t>(kStringSize), kStringSize));
  appendFrame(wire, declaredString(static_cast<int32_t>(kStringSize), kStringSize));

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 1024 * 1024);
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(trans));

  std::string first;
  protocol->readBinary(first);
  BOOST_CHECK_EQUAL(first.size(), kStringSize);

  // No readEnd() in between: the second frame must still get a full budget.
  std::string second;
  protocol->readBinary(second);
  BOOST_CHECK_EQUAL(second.size(), kStringSize);
}

/**
 * Many frames in a row, alternating size, within a budget that comfortably
 * covers all of them.  Binding must not turn a long stream of small messages
 * into a failure.
 */
BOOST_AUTO_TEST_CASE(test_many_consecutive_frames) {
  const size_t kStringSize = 100;
  const int kFrames = 64;

  std::vector<uint8_t> wire;
  for (int i = 0; i < kFrames; i++) {
    size_t sz = (i % 2 == 0) ? kStringSize : kStringSize * 4;
    appendFrame(wire, declaredString(static_cast<int32_t>(sz), sz));
  }

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 1024 * 1024);
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(trans));

  for (int i = 0; i < kFrames; i++) {
    size_t expected = (i % 2 == 0) ? kStringSize : kStringSize * 4;
    std::string str;
    protocol->readBinary(str);
    BOOST_CHECK_EQUAL(str.size(), expected);
  }
}

/** A field that fits inside its frame is read normally. */
BOOST_AUTO_TEST_CASE(test_field_fitting_in_frame_is_accepted) {
  std::vector<uint8_t> wire;
  appendFrame(wire, declaredString(64, 64));

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 100 * 1024 * 1024);
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(trans));

  std::string str;
  protocol->readBinary(str);
  BOOST_CHECK_EQUAL(str.size(), 64u);
}

/**
 * readEnd() returns the budget to the configured maximum.  Generated code
 * calls it after every message, one-way included, which is what lets the next
 * message start from a full budget however small the previous frame was.
 */
BOOST_AUTO_TEST_CASE(test_read_end_restores_the_full_budget) {
  std::vector<uint8_t> wire;
  appendFrame(wire, filler(16));
  appendFrame(wire, filler(4096));

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 1024 * 1024);

  uint8_t first[16];
  BOOST_CHECK_EQUAL(trans->readAll(first, sizeof(first)), 16u);
  trans->readEnd();

  // A 4096-byte read straight after a 16-byte frame only succeeds because
  // readEnd() reset the budget before readFrame() bound it to the new frame.
  std::vector<uint8_t> second(4096);
  BOOST_CHECK_EQUAL(trans->readAll(second.data(), static_cast<uint32_t>(second.size())), 4096u);
}

// ---------------------------------------------------------------------------
// The behaviour being added.
// ---------------------------------------------------------------------------

/**
 * A 68-byte frame declares a 64 MB field.  The bytes are not present and never
 * will be, but while the declared size is checked against the configured
 * maximum the 64 MB is allocated first and the shortfall only surfaces
 * afterwards, when the read runs out of data.
 *
 * Both outcomes raise TTransportException(END_OF_FILE), so the exception alone
 * does not tell them apart.  The string the protocol was asked to fill does:
 * readStringBody() resizes it before reading, and the caller's string keeps
 * that size when the read throws.  Unmodified, str.size() is 64 MB here.
 */
BOOST_AUTO_TEST_CASE(test_declared_field_larger_than_frame_is_rejected) {
  const int32_t kDeclared = 64 * 1024 * 1024;

  std::vector<uint8_t> wire;
  appendFrame(wire, declaredString(kDeclared, 64));

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 100 * 1024 * 1024);
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(trans));

  std::string str;
  BOOST_CHECK_THROW(protocol->readBinary(str), TTransportException);
  BOOST_CHECK_LT(str.size(), 1024u);
}

/**
 * A 5-byte frame declares a list of 8 million i32s.  The protocol's own
 * element-count check needs 32 MB of budget to reject it, and the configured
 * maximum grants 100 MB, so unmodified this readListBegin() succeeds and the
 * caller goes on to size a container from a number the frame cannot support.
 */
BOOST_AUTO_TEST_CASE(test_declared_container_larger_than_frame_is_rejected) {
  std::vector<uint8_t> payload;
  payload.push_back(static_cast<uint8_t>(apache::thrift::protocol::T_I32));
  appendI32(payload, 8000000);

  std::vector<uint8_t> wire;
  appendFrame(wire, payload);

  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 100 * 1024 * 1024);
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(trans));

  TType elemType = apache::thrift::protocol::T_STOP;
  uint32_t size = 0;
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, size), TTransportException);
}

/**
 * Each frame is entitled to a budget of its own.  Unmodified, the budget is a
 * per-connection allowance that only consume() draws down and only readEnd()
 * or flush() restores, so a long stream of frames read without either -- a run
 * of one-way calls on a raw transport, for instance -- exhausts it and starts
 * refusing frames that are individually well within the limit.  Here 64 frames
 * consume 16000 bytes in total against a 4096-byte maximum, and the stream
 * fails partway through.
 */
BOOST_AUTO_TEST_CASE(test_frame_budget_does_not_accumulate_across_frames) {
  const size_t kStringSize = 100;
  const int kFrames = 64;

  std::vector<uint8_t> wire;
  for (int i = 0; i < kFrames; i++) {
    size_t sz = (i % 2 == 0) ? kStringSize : kStringSize * 4;
    appendFrame(wire, declaredString(static_cast<int32_t>(sz), sz));
  }

  // Room for the largest single frame, but not for all of them together.
  std::shared_ptr<TFramedTransport> trans = framedOver(wire, 4096);
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(trans));

  for (int i = 0; i < kFrames; i++) {
    size_t expected = (i % 2 == 0) ? kStringSize : kStringSize * 4;
    std::string str;
    protocol->readBinary(str);
    BOOST_CHECK_EQUAL(str.size(), expected);
  }
}

// ---------------------------------------------------------------------------
// TMemoryBuffer::bindMessageSizeToBuffer(), used by TNonblockingServer.
//
// That server reads and strips the frame itself and hands the bare message to
// a TMemoryBuffer through resetBuffer(), so a change confined to
// TFramedTransport does not reach it: with the default transport factory the
// protocol reads straight from that buffer.  These tests exercise the binding
// the server now applies -- they call new API, so unlike the tests above they
// do not have a before-and-after to show; what they establish is that the
// binding rejects what it should and, in the last one, that it does not break
// a connection serving one message after another.
// ---------------------------------------------------------------------------

/**
 * A message of 68 bytes declares a 64 MB field.  The buffer holds the whole
 * message and nothing more can arrive, so the declared size is refused before
 * anything is allocated for it.
 */
BOOST_AUTO_TEST_CASE(test_declared_field_larger_than_buffer_is_rejected) {
  const int32_t kDeclared = 64 * 1024 * 1024;

  std::vector<uint8_t> message = declaredString(kDeclared, 64);

  std::shared_ptr<TConfiguration> config(new TConfiguration(100 * 1024 * 1024));
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(config));
  buffer->resetBuffer(message.data(), static_cast<uint32_t>(message.size()), TMemoryBuffer::OBSERVE);
  buffer->bindMessageSizeToBuffer();

  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(buffer));

  std::string str;
  BOOST_CHECK_THROW(protocol->readBinary(str), TTransportException);
  BOOST_CHECK_LT(str.size(), 1024u);
}

/** As above, for a container element count the message cannot support. */
BOOST_AUTO_TEST_CASE(test_declared_container_larger_than_buffer_is_rejected) {
  std::vector<uint8_t> message;
  message.push_back(static_cast<uint8_t>(apache::thrift::protocol::T_I32));
  appendI32(message, 8000000);

  std::shared_ptr<TConfiguration> config(new TConfiguration(100 * 1024 * 1024));
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(config));
  buffer->resetBuffer(message.data(), static_cast<uint32_t>(message.size()), TMemoryBuffer::OBSERVE);
  buffer->bindMessageSizeToBuffer();

  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(buffer));

  TType elemType = apache::thrift::protocol::T_STOP;
  uint32_t size = 0;
  BOOST_CHECK_THROW(protocol->readListBegin(elemType, size), TTransportException);
}

/**
 * Successive messages handed to the same buffer must each start from a budget
 * of their own, whether the next one is larger or smaller than the last.  This
 * is what a connection serving one request after another looks like, and it is
 * the case a binding built on updateKnownMessageSize() alone gets wrong:
 * resetConsumedMessageSize() will not let a budget grow again.
 */
BOOST_AUTO_TEST_CASE(test_consecutive_buffers_growing_in_size) {
  std::shared_ptr<TConfiguration> config(new TConfiguration(1024 * 1024));
  std::shared_ptr<TMemoryBuffer> buffer(new TMemoryBuffer(config));
  std::shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(buffer));

  const size_t sizes[] = {16, 4096, 64, 8192};
  for (size_t expected : sizes) {
    std::vector<uint8_t> message = declaredString(static_cast<int32_t>(expected), expected);
    buffer->resetBuffer(message.data(), static_cast<uint32_t>(message.size()),
                        TMemoryBuffer::OBSERVE);
    buffer->bindMessageSizeToBuffer();

    std::string str;
    protocol->readBinary(str);
    BOOST_CHECK_EQUAL(str.size(), expected);
  }
}

BOOST_AUTO_TEST_SUITE_END()
