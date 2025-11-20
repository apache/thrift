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

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.zip.DataFormatException;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;
import org.junit.jupiter.api.Test;

public class TestTZlibTransport {

  protected TTransport getTransport(TTransport underlying) throws TTransportException {
    return new TZlibTransport(underlying);
  }

  public static byte[] byteSequence(int start, int end) {
    byte[] result = new byte[end - start + 1];
    for (int i = 0; i <= (end - start); i++) {
      result[i] = (byte) (start + i);
    }
    return result;
  }

  @Test
  public void testClose() throws TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    WriteCountingTransport countingTrans =
        new WriteCountingTransport(new TIOStreamTransport(new BufferedOutputStream(baos)));
    TTransport trans = getTransport(countingTrans);
    trans.write(byteSequence(0, 245));
    countingTrans.close();
    trans.close();
  }

  @Test
  public void testCloseOpen() throws TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    TTransport trans = getTransport(new TIOStreamTransport(baos));
    byte[] uncompressed = byteSequence(0, 245);
    trans.write(uncompressed);
    trans.close();
    final byte[] compressed = baos.toByteArray();

    final byte[] buf = new byte[255];
    TTransport transRead =
        getTransport(new TIOStreamTransport(new ByteArrayInputStream(compressed)));
    int readBytes = transRead.read(buf, 0, buf.length);
    assertEquals(uncompressed.length, readBytes);
    transRead.close();
  }

  @Test
  public void testRead() throws IOException, TTransportException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    DeflaterOutputStream deflaterOutputStream = new DeflaterOutputStream(baos);
    DataOutputStream dos = new DataOutputStream(deflaterOutputStream);
    dos.write(byteSequence(0, 49));
    dos.write(byteSequence(0, 219));

    deflaterOutputStream.finish();

    TMemoryBuffer membuf = new TMemoryBuffer(0);
    membuf.write(baos.toByteArray());

    ReadCountingTransport countTrans = new ReadCountingTransport(membuf);
    TTransport trans = getTransport(countTrans);

    byte[] readBuf = new byte[10];
    trans.read(readBuf, 0, 10);
    assertArrayEquals(readBuf, byteSequence(0, 9));
    assertEquals(1, countTrans.readCount);

    trans.read(readBuf, 0, 10);
    assertArrayEquals(readBuf, byteSequence(10, 19));
    assertEquals(1, countTrans.readCount);

    assertEquals(30, trans.read(new byte[30], 0, 30));
    assertEquals(1, countTrans.readCount);

    readBuf = new byte[220];
    assertEquals(220, trans.read(readBuf, 0, 220));
    assertArrayEquals(readBuf, byteSequence(0, 219));
    assertEquals(1, countTrans.readCount);
  }

  @Test
  public void testWrite() throws TTransportException, IOException, DataFormatException {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    WriteCountingTransport countingTrans =
        new WriteCountingTransport(new TIOStreamTransport(new BufferedOutputStream(baos)));
    TTransport trans = getTransport(countingTrans);

    trans.write(byteSequence(0, 100));
    assertEquals(1, countingTrans.writeCount);
    trans.write(byteSequence(101, 200));
    trans.write(byteSequence(201, 255));
    assertEquals(1, countingTrans.writeCount);

    trans.flush();
    assertEquals(2, countingTrans.writeCount);

    trans.write(byteSequence(0, 245));
    trans.flush();
    assertEquals(3, countingTrans.writeCount);

    DataInputStream din =
        new DataInputStream(new InflaterInputStream(new ByteArrayInputStream(baos.toByteArray())));
    byte[] buf = new byte[256];
    int n = din.read(buf, 0, 256);
    assertEquals(n, 256);
    assertArrayEquals(byteSequence(0, 255), buf);

    buf = new byte[246];
    n = din.read(buf, 0, 246);
    assertEquals(n, 246);
    for (int i = 0; i < buf.length; i++) {
      assertEquals(byteSequence(0, 245)[i], buf[i], "for " + i);
    }

    assertArrayEquals(byteSequence(0, 245), buf);
  }
}
