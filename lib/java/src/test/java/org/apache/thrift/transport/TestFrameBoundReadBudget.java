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
package org.apache.thrift.transport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import org.apache.thrift.TConfiguration;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.layered.TFastFramedTransport;
import org.apache.thrift.transport.layered.TFramedTransport;
import org.junit.jupiter.api.Test;

/**
 * The read budget is bound to the size of the frame that carries the message, rather than staying
 * at the configured maximum for the life of the connection.
 *
 * <p>Two groups of tests, and the distinction matters when reading a failure:
 *
 * <ul>
 *   <li>The {@code testConsecutive*} tests describe traffic that has always been legitimate. They
 *       pass on the unmodified library and must keep passing. They exist because the natural
 *       implementation of frame binding breaks them: {@link
 *       TEndpointTransport#resetConsumedMessageSize} refuses to grow a budget, so a second frame
 *       larger than the first is rejected unless each frame performs a full reset before binding.
 *       Java is exposed here in a way the other bindings are not, because its only reset on the
 *       socket path lives in {@code TIOStreamTransport.flush()} — the write side — which a oneway
 *       call never reaches.
 *   <li>The {@code testDeclared*} tests describe the behaviour being added: a small frame may not
 *       declare a field larger than itself.
 * </ul>
 */
public class TestFrameBoundReadBudget {

  /** Writes one frame: a 4-byte big-endian length followed by that many bytes. */
  private static void writeFrame(DataOutputStream dos, byte[] payload) throws IOException {
    dos.writeInt(payload.length);
    dos.write(payload);
  }

  private static byte[] filler(int n) {
    byte[] b = new byte[n];
    for (int i = 0; i < n; i++) {
      b[i] = (byte) i;
    }
    return b;
  }

  /**
   * A binary-protocol string field body: a 4-byte length followed by that many bytes. Used to build
   * a payload whose declared length disagrees with the bytes actually present.
   */
  private static byte[] declaredString(int declaredLength, int actualBytes) throws IOException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(declaredLength);
    dos.write(filler(actualBytes));
    return baos.toByteArray();
  }

  /**
   * An endpoint that behaves like a socket for budget purposes: it never binds the budget to
   * anything, and its only reset lives on the write side. {@code TMemoryBuffer} cannot stand in
   * here — its constructor calls {@code updateKnownMessageSize}, which is the very behaviour under
   * test.
   */
  private static TEndpointTransport socketLike(byte[] wire, int maxMessageSize)
      throws TTransportException {
    TConfiguration config = new TConfiguration();
    config.setMaxMessageSize(maxMessageSize);
    return new TIOStreamTransport(config, new ByteArrayInputStream(wire));
  }

  private static TTransport framedOver(byte[] wire, int maxMessageSize) throws TTransportException {
    return new TFramedTransport(socketLike(wire, maxMessageSize));
  }

  private static TTransport fastFramedOver(byte[] wire, int maxMessageSize)
      throws TTransportException {
    return new TFastFramedTransport(socketLike(wire, maxMessageSize), 64, maxMessageSize);
  }

  // ---------------------------------------------------------------------------
  // Traffic that has always been legitimate, and must remain so.
  // ---------------------------------------------------------------------------

  /**
   * Two frames on one connection, the second larger than the first, with nothing in between. This
   * is what a stream of oneway calls looks like: {@code ProcessFunction} skips the response write
   * for a oneway function, so no flush occurs and nothing resets the budget between messages.
   */
  @Test
  public void testConsecutiveFramesGrowingInSize() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    writeFrame(dos, filler(16));
    writeFrame(dos, filler(4096));

    TTransport trans = framedOver(baos.toByteArray(), 1024 * 1024);

    byte[] first = new byte[16];
    trans.readAll(first, 0, 16);

    // Second frame is 256x the first. Nothing has reset the budget in between.
    byte[] second = new byte[4096];
    assertEquals(4096, trans.readAll(second, 0, 4096));
  }

  /** As above, for the second framed implementation. */
  @Test
  public void testConsecutiveFramesGrowingInSizeFastFramed()
      throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    writeFrame(dos, filler(16));
    writeFrame(dos, filler(4096));

    TTransport trans = fastFramedOver(baos.toByteArray(), 1024 * 1024);

    byte[] first = new byte[16];
    trans.readAll(first, 0, 16);

    byte[] second = new byte[4096];
    assertEquals(4096, trans.readAll(second, 0, 4096));
  }

  /**
   * Many frames in a row, alternating size. A budget that is bound but never reset would be
   * consumed cumulatively and start rejecting valid frames partway through.
   */
  @Test
  public void testManyConsecutiveFrames() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    final int frames = 64;
    for (int i = 0; i < frames; i++) {
      writeFrame(dos, filler(i % 2 == 0 ? 32 : 512));
    }

    // A budget large enough for any single frame, but far smaller than their sum.
    TTransport trans = framedOver(baos.toByteArray(), 4096);

    for (int i = 0; i < frames; i++) {
      int expected = i % 2 == 0 ? 32 : 512;
      byte[] buf = new byte[expected];
      assertEquals(expected, trans.readAll(buf, 0, expected), "frame " + i);
    }
  }

  /** A field that fits inside its frame is read normally. */
  @Test
  public void testFieldFittingInFrameIsAccepted() throws IOException, TException {
    byte[] payload = declaredString(64, 64);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    writeFrame(new DataOutputStream(baos), payload);

    TTransport trans = framedOver(baos.toByteArray(), 100 * 1024 * 1024);
    TProtocol proto = new TBinaryProtocol(trans);

    assertEquals(64, proto.readBinary().remaining());
  }

  // ---------------------------------------------------------------------------
  // The behaviour being added.
  // ---------------------------------------------------------------------------

  /**
   * A 68-byte frame declares a 64 MB field. The bytes are not present and never will be, but while
   * the declared size is checked against the full configured maximum the 64 MB array is allocated
   * first and the mismatch only surfaces afterwards, when the read runs out of data.
   *
   * <p>The exception type is what distinguishes the two: {@code END_OF_FILE} means the size was
   * accepted and discovered to be wrong after allocating; {@code MESSAGE_SIZE_LIMIT} means it was
   * refused on the budget before anything was allocated. Unmodified, this throws {@code
   * END_OF_FILE}.
   */
  @Test
  public void testDeclaredFieldLargerThanFrameIsRejected() throws IOException, TTransportException {
    byte[] payload = declaredString(64 * 1024 * 1024, 64);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    writeFrame(new DataOutputStream(baos), payload);

    TTransport trans = framedOver(baos.toByteArray(), 100 * 1024 * 1024);
    TProtocol proto = new TBinaryProtocol(trans);

    TTransportException e = assertThrows(TTransportException.class, proto::readBinary);
    assertEquals(TTransportException.MESSAGE_SIZE_LIMIT, e.getType());
  }

  /** As above, for the second framed implementation. */
  @Test
  public void testDeclaredFieldLargerThanFrameIsRejectedFastFramed()
      throws IOException, TTransportException {
    byte[] payload = declaredString(64 * 1024 * 1024, 64);

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    writeFrame(new DataOutputStream(baos), payload);

    TTransport trans = fastFramedOver(baos.toByteArray(), 100 * 1024 * 1024);
    TProtocol proto = new TBinaryProtocol(trans);

    TTransportException e = assertThrows(TTransportException.class, proto::readBinary);
    assertEquals(TTransportException.MESSAGE_SIZE_LIMIT, e.getType());
  }

  /**
   * The path the nonblocking servers take. They strip the frame themselves and hand the payload to
   * a {@link TMemoryInputTransport} via {@code reset}, so a fix confined to the framed transports
   * would not cover them. Unmodified, this throws {@code UNKNOWN} — the buffer simply runs out.
   */
  @Test
  public void testDeclaredFieldLargerThanResetBufferIsRejected() throws TTransportException {
    TConfiguration config = new TConfiguration();
    config.setMaxMessageSize(100 * 1024 * 1024);

    TMemoryInputTransport trans = new TMemoryInputTransport(config, new byte[0]);
    TProtocol proto = new TBinaryProtocol(trans);

    // A 68-byte message declaring a 64 MB field, as AbstractNonblockingServer would supply it.
    byte[] payload;
    try {
      payload = declaredString(64 * 1024 * 1024, 64);
    } catch (IOException io) {
      throw new AssertionError(io);
    }
    trans.reset(payload);

    TTransportException e = assertThrows(TTransportException.class, proto::readBinary);
    assertEquals(TTransportException.MESSAGE_SIZE_LIMIT, e.getType());
  }

  /**
   * The frame must be readable even when getting it off the wire spent the inner transport's whole
   * budget -- which is the shape {@code AbstractNonblockingServer} has, and the reason the bound
   * cannot simply be put on the inner transport.
   *
   * <p>{@link TFramedTransport} inherits {@code checkReadBytesAvailable} from {@link
   * org.apache.thrift.transport.layered.TLayeredTransport}, which forwards it to the inner
   * transport. Reading the frame is also done through the inner transport, so when that one
   * decrements on read -- as {@link TMemoryInputTransport} does, and as the nonblocking server's
   * {@code frameTrans_} therefore does -- the framing reads consume the budget before the protocol
   * asks its first question. Here a 68-byte frame arrives in a 72-byte buffer bound to 72, and
   * readFrame() takes all 72.
   *
   * <p>Without the full reset in readFrame(), the 64-byte field below is refused with {@code
   * MESSAGE_SIZE_LIMIT} even though every one of its bytes is sitting in the frame buffer. This
   * test failed exactly that way before the reset was added.
   */
  @Test
  public void testFrameSurvivesTheFramingSpendingTheInnerBudget() throws IOException, TException {
    byte[] payload = declaredString(64, 64); // 68 bytes
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    writeFrame(new DataOutputStream(baos), payload); // 72 bytes with the header

    TConfiguration config = new TConfiguration();
    config.setMaxMessageSize(100 * 1024 * 1024);
    // The constructor binds the budget to the 72 bytes handed to it.
    TMemoryInputTransport inner = new TMemoryInputTransport(config, baos.toByteArray());
    TFramedTransport trans = new TFramedTransport(inner);
    TProtocol proto = new TBinaryProtocol(trans);

    assertEquals(64, proto.readBinary().remaining());
  }

  /** Successive {@code reset} calls must each start from a full budget, growing or shrinking. */
  @Test
  public void testConsecutiveResetsGrowingInSize() throws TTransportException {
    TConfiguration config = new TConfiguration();
    config.setMaxMessageSize(1024 * 1024);

    TMemoryInputTransport trans = new TMemoryInputTransport(config, new byte[0]);

    trans.reset(filler(16));
    byte[] first = new byte[16];
    assertEquals(16, trans.read(first, 0, 16));

    trans.reset(filler(4096));
    byte[] second = new byte[4096];
    assertEquals(4096, trans.read(second, 0, 4096));
  }
}
