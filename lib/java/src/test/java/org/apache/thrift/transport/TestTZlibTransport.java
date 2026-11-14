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
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Arrays;
import java.util.zip.DataFormatException;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.InflaterInputStream;
import org.apache.thrift.TConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

public class TestTZlibTransport {

  @ParameterizedTest
  @CsvSource({"false, false", "true, false", "true, true"})
  public void testCloseFinishesCompressedStreamOverSocket(
      boolean writeData, boolean flushBeforeClose) throws Exception {
    byte[] expected = writeData ? new byte[] {1, 2, 3} : new byte[0];
    try (ServerSocket listener = new ServerSocket(0, 1, InetAddress.getLoopbackAddress());
        TSocket socket =
            new TSocket(new Socket(listener.getInetAddress(), listener.getLocalPort()));
        Socket peer = listener.accept();
        TTransport transport = getTransport(socket)) {
      peer.setSoTimeout(5000);
      if (writeData) {
        transport.write(expected);
      }
      if (flushBeforeClose) {
        transport.flush();
      }

      transport.close();

      try (DataInputStream input =
          new DataInputStream(new InflaterInputStream(peer.getInputStream()))) {
        byte[] actual = new byte[expected.length];
        input.readFully(actual);
        assertArrayEquals(expected, actual);
        // Reading to the end also verifies that the compression trailer reached the peer.
        assertEquals(-1, input.read());
      }
    }
  }

  protected TTransport getTransport(TTransport underlying) throws TTransportException {
    return new TZlibTransport(underlying);
  }

  @Test
  public void testCheckedFinalFlushFailureStillClosesUnderlyingTransport() throws Exception {
    TTransport underlying = mock(TTransport.class);
    when(underlying.isOpen()).thenReturn(true);
    TZlibTransport transport = new TZlibTransport(underlying);
    doThrow(new TTransportException("Final flush failure")).when(underlying).flush();

    transport.close();

    verify(underlying).close();
    assertNull(transport.inputStream_);
    assertNull(transport.outputStream_);
  }

  @Test
  public void testUncheckedFinalFlushFailureStillClosesUnderlyingTransport() throws Exception {
    TTransport underlying = mock(TTransport.class);
    when(underlying.isOpen()).thenReturn(true);
    TZlibTransport transport = new TZlibTransport(underlying);
    IllegalStateException failure = new IllegalStateException("Final flush failure");
    doThrow(failure).when(underlying).flush();

    assertSame(failure, assertThrows(IllegalStateException.class, transport::close));

    verify(underlying).close();
    assertNull(transport.inputStream_);
    assertNull(transport.outputStream_);
  }

  @ParameterizedTest
  @CsvSource({"false, false", "false, true", "true, false", "true, true"})
  public void testClosePreservesFirstUncheckedFailure(boolean primaryError, boolean cleanupError)
      throws Exception {
    TTransport underlying = mock(TTransport.class);
    when(underlying.isOpen()).thenReturn(true);
    TZlibTransport transport = new TZlibTransport(underlying);
    Throwable primary =
        primaryError
            ? new AssertionError("Final flush failure")
            : new IllegalStateException("Final flush failure");
    Throwable cleanup =
        cleanupError
            ? new AssertionError("Underlying close failure")
            : new IllegalArgumentException("Underlying close failure");
    doThrow(primary).when(underlying).flush();
    doThrow(cleanup).when(underlying).close();

    Throwable actual = assertThrows(Throwable.class, transport::close);

    assertSame(primary, actual);
    assertArrayEquals(new Throwable[] {cleanup}, actual.getSuppressed());
    verify(underlying).close();
    assertNull(transport.inputStream_);
    assertNull(transport.outputStream_);
  }

  @ParameterizedTest
  @ValueSource(booleans = {false, true})
  public void testClosePropagatesUncheckedCleanupFailure(boolean cleanupError) throws Exception {
    TTransport underlying = mock(TTransport.class);
    when(underlying.isOpen()).thenReturn(true);
    TZlibTransport transport = new TZlibTransport(underlying);
    Throwable failure =
        cleanupError
            ? new AssertionError("Underlying close failure")
            : new IllegalStateException("Underlying close failure");
    doThrow(failure).when(underlying).close();

    assertSame(failure, assertThrows(Throwable.class, transport::close));

    assertEquals(0, failure.getSuppressed().length);
    verify(underlying).close();
    assertNull(transport.inputStream_);
    assertNull(transport.outputStream_);
  }

  @Test
  public void testCloseDoesNotSuppressFailureOntoItself() throws Exception {
    TTransport underlying = mock(TTransport.class);
    when(underlying.isOpen()).thenReturn(true);
    TZlibTransport transport = new TZlibTransport(underlying);
    IllegalStateException failure = new IllegalStateException("Transport failure");
    doThrow(failure).when(underlying).flush();
    doThrow(failure).when(underlying).close();

    assertSame(failure, assertThrows(IllegalStateException.class, transport::close));

    assertEquals(0, failure.getSuppressed().length);
    verify(underlying).close();
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

  @Test
  public void testMessageSizeLimit() throws Exception {
    byte[] data = new byte[4096];
    Arrays.fill(data, (byte) 'a');

    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    TZlibTransport writer = new TZlibTransport(new TIOStreamTransport(baos));
    writer.write(data);
    writer.flush();
    writer.close();

    // Pass a TConfiguration with a 1 KB limit before constructing TZlibTransport
    // so that remainingMessageSize is initialised from the correct limit.
    TConfiguration cfg = new TConfiguration();
    cfg.setMaxMessageSize(1024);
    TZlibTransport reader =
        new TZlibTransport(
            new TIOStreamTransport(cfg, new ByteArrayInputStream(baos.toByteArray())));

    TTransportException ex =
        assertThrows(TTransportException.class, () -> reader.read(new byte[4096], 0, 4096));
    assertEquals(TTransportException.MESSAGE_SIZE_LIMIT, ex.getType());
  }
}
