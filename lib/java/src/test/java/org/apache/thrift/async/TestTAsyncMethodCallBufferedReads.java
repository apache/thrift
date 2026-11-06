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

package org.apache.thrift.async;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingTransport;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

public class TestTAsyncMethodCallBufferedReads {
  private final TNonblockingTransport transport = mock(TNonblockingTransport.class);
  private final SelectionKey key = mock(SelectionKey.class);
  private final CompletableFuture<byte[]> result = new CompletableFuture<>();

  @ParameterizedTest
  @ValueSource(ints = {0, 3})
  public void testCachedResponseCompletesWithoutAnotherEvent(int size) throws Exception {
    byte[] expected = new byte[size];
    for (int i = 0; i < size; i++) expected[i] = (byte) (i + 1);
    ByteBuffer response = ByteBuffer.allocate(4 + size).putInt(size).put(expected);
    response.flip();
    when(transport.hasPendingReads()).thenAnswer(call -> response.hasRemaining());
    when(transport.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer output = call.getArgument(0);
              if (!output.hasRemaining()) return 0;
              output.put(response.get());
              return 1;
            });
    TAsyncMethodCall<byte[]> call = startCall();
    call.transition(key);
    call.transition(key);
    assertTrue(result.isDone(), "Buffered reads required another selector event");
    assertArrayEquals(expected, result.join());
    assertEquals(TAsyncMethodCall.State.RESPONSE_READ, call.getState());
  }

  @Test
  public void testIncompleteCachedRecordYieldsOnZeroProgress() throws Exception {
    AtomicInteger reads = new AtomicInteger();
    when(transport.hasPendingReads()).thenReturn(true);
    when(transport.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              int attempt = reads.incrementAndGet();
              assertTrue(attempt <= 3, "Retried a zero-byte read without readiness");
              if (attempt == 1) {
                ((ByteBuffer) call.getArgument(0)).put(new byte[] {0, 0});
                return 2;
              }
              return 0;
            });
    TAsyncMethodCall<byte[]> call = startCall();
    call.transition(key);
    call.transition(key);
    assertEquals(2, reads.get());
    assertEquals(TAsyncMethodCall.State.READING_RESPONSE_SIZE, call.getState());
    assertFalse(result.isDone());
    call.transition(key); // A subsequent readiness event also yields after one zero-byte read.
    assertEquals(3, reads.get());
  }

  @Test
  public void testHeaderReadImmediatelyDrainsBufferedBody() throws Exception {
    ByteBuffer response = ByteBuffer.wrap(new byte[] {0, 0, 0, 3, 1, 2, 3});
    AtomicBoolean buffered = new AtomicBoolean();
    when(transport.hasPendingReads()).thenAnswer(call -> buffered.get());
    when(transport.read(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer output = call.getArgument(0);
              int size = output.remaining();
              for (int i = 0; i < size; i++) output.put(response.get());
              buffered.set(response.hasRemaining());
              return size;
            });
    TAsyncMethodCall<byte[]> call = startCall();
    call.transition(key);
    call.transition(key);
    assertFalse(result.isDone());
    call.transition(key); // Only the header has socket readiness; the body is already buffered.
    assertTrue(result.isDone(), "Response body required another selector event");
    assertArrayEquals(new byte[] {1, 2, 3}, result.join());
  }

  private TAsyncMethodCall<byte[]> startCall() throws Exception {
    when(key.isValid()).thenReturn(true);
    when(transport.isOpen()).thenReturn(true);
    when(transport.getMaxFrameSize()).thenReturn(1024);
    when(transport.registerSelector(any(), anyInt())).thenReturn(key);
    when(transport.write(any(ByteBuffer.class)))
        .thenAnswer(
            call -> {
              ByteBuffer data = call.getArgument(0);
              int size = data.remaining();
              data.position(data.limit());
              return size;
            });
    TBinaryProtocol.Factory factory = new TBinaryProtocol.Factory();
    TAsyncClient client = new TAsyncClient(factory, null, transport) {};
    TAsyncMethodCall<byte[]> call =
        new TAsyncMethodCall<byte[]>(
            client,
            factory,
            transport,
            new AsyncMethodCallback<byte[]>() {
              @Override
              public void onComplete(byte[] value) {
                result.complete(value);
              }

              @Override
              public void onError(Exception error) {
                result.completeExceptionally(error);
              }
            },
            false) {
          @Override
          protected void write_args(TProtocol protocol) throws TException {
            protocol.writeByte((byte) 7);
          }

          @Override
          protected byte[] getResult() {
            return getFrameBuffer().array();
          }
        };
    call.prepareMethodCall();
    call.start(null);
    return call;
  }
}
