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

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import org.apache.thrift.transport.layered.TFastFramedTransport;
import org.apache.thrift.transport.layered.TFramedTransport;
import org.junit.jupiter.api.Test;

public class TestTFramedTransport {

  protected TTransport getTransport(TTransport underlying) throws TTransportException {
    return new TFramedTransport(underlying);
  }

  protected TTransport getTransport(TTransport underlying, int maxLength)
      throws TTransportException {
    return new TFramedTransport(underlying, maxLength);
  }

  public static byte[] byteSequence(int start, int end) {
    byte[] result = new byte[end - start + 1];
    for (int i = 0; i <= (end - start); i++) {
      result[i] = (byte) (start + i);
    }
    return result;
  }

  @Test
  public void testRead() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(50);
    dos.write(byteSequence(0, 49));

    dos.writeInt(220);
    dos.write(byteSequence(0, 219));

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TTransport trans = getTransport(countTrans);

    byte[] readBuf = new byte[10];
    trans.read(readBuf, 0, 10);
    assertArrayEquals(readBuf, byteSequence(0, 9));
    assertEquals(2, countTrans.readCount);

    trans.read(readBuf, 0, 10);
    assertArrayEquals(readBuf, byteSequence(10, 19));
    assertEquals(2, countTrans.readCount);

    assertEquals(30, trans.read(new byte[30], 0, 30));
    assertEquals(2, countTrans.readCount);

    // Known message size exceeded
    readBuf = new byte[220];
    assertEquals(220, trans.read(readBuf, 0, 220));
    assertArrayEquals(readBuf, byteSequence(0, 219));
    assertEquals(4, countTrans.readCount);
  }

  @Test
  public void testInvalidFrameSize() throws IOException, TTransportException {
    int maxLength = 128;

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(130);
    dos.write(byteSequence(0, 129));

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TTransport trans = getTransport(countTrans, maxLength);

    byte[] readBuf = new byte[10];
    TTransportException e =
        assertThrows(TTransportException.class, () -> trans.read(readBuf, 0, 4));
    // We expect this exception because the frame we're trying to read is larger than our max frame
    // length
    assertEquals(TTransportException.CORRUPTED_DATA, e.getType());
    assertFalse(trans.isOpen());

    TTransportException e2 =
        assertThrows(TTransportException.class, () -> trans.read(readBuf, 0, 4));
    // This time we get an exception indicating the connection was closed
    assertEquals(TTransportException.NOT_OPEN, e2.getType());
  }

  @Test
  public void testWrite() throws TTransportException, IOException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    WriteCountingTransport countingTrans =
        new WriteCountingTransport(new TIOStreamTransport(new BufferedOutputStream(baos)));
    TTransport trans = getTransport(countingTrans);

    trans.write(byteSequence(0, 100));
    assertEquals(0, countingTrans.writeCount);
    trans.write(byteSequence(101, 200));
    trans.write(byteSequence(201, 255));
    assertEquals(0, countingTrans.writeCount);

    trans.flush();
    assertEquals(1, countingTrans.writeCount);

    trans.write(byteSequence(0, 245));
    trans.flush();
    assertEquals(2, countingTrans.writeCount);

    DataInputStream din = new DataInputStream(new ByteArrayInputStream(baos.toByteArray()));
    assertEquals(256, din.readInt());

    byte[] buf = new byte[256];
    int readBytes = din.read(buf, 0, 256);
    assertArrayEquals(byteSequence(0, 255), buf);

    assertEquals(246, din.readInt());
    buf = new byte[246];
    int readBytes2 = din.read(buf, 0, 246);
    assertArrayEquals(byteSequence(0, 245), buf);
  }

  @Test
  public void testDirectRead() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(50);
    dos.write(byteSequence(0, 49));
    dos.writeInt(50);
    dos.write(byteSequence(125, 175));

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TTransport trans = getTransport(countTrans);

    assertEquals(0, trans.getBytesRemainingInBuffer());

    byte[] readBuf = new byte[10];
    trans.read(readBuf, 0, 10);
    assertArrayEquals(readBuf, byteSequence(0, 9));

    assertEquals(40, trans.getBytesRemainingInBuffer());
    assertEquals(10, trans.getBufferPosition());

    trans.consumeBuffer(5);
    assertEquals(35, trans.getBytesRemainingInBuffer());
    assertEquals(15, trans.getBufferPosition());

    assertEquals(2, countTrans.readCount);

    assertEquals(35, trans.read(new byte[35], 0, 35));
    assertEquals(0, trans.getBytesRemainingInBuffer());
    assertEquals(50, trans.getBufferPosition());

    // Known message size exceeded
    trans.read(readBuf, 0, 10);
    assertEquals(4, countTrans.readCount);
    assertArrayEquals(readBuf, byteSequence(125, 134));
    assertEquals(40, trans.getBytesRemainingInBuffer());
    assertEquals(10, trans.getBufferPosition());
  }

  @Test
  public void testClear() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DataOutputStream dos = new DataOutputStream(baos);
    dos.writeInt(220);
    dos.write(byteSequence(0, 219));

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TTransport trans = getTransport(countTrans);

    byte[] readBuf = new byte[220];
    trans.read(readBuf, 0, 220);
    assertArrayEquals(readBuf, byteSequence(0, 219));

    assertTrue(trans instanceof TFramedTransport || trans instanceof TFastFramedTransport);
    if (trans instanceof TFramedTransport) {
      assertTrue(trans.getBuffer() != null && trans.getBuffer().length > 0);
      ((TFramedTransport) trans).clear();
      assertNull(trans.getBuffer());
    } else {
      assertTrue(trans.getBuffer().length > TestTFastFramedTransport.INITIAL_CAPACITY);
      ((TFastFramedTransport) trans).clear();
      assertEquals(TestTFastFramedTransport.INITIAL_CAPACITY, trans.getBuffer().length);
    }
  }
}
