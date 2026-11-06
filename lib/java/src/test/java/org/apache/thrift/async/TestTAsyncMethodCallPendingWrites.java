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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingSocket;
import org.junit.jupiter.api.Test;

public class TestTAsyncMethodCallPendingWrites {
  @Test
  public void testResponseReadWaitsForPendingCiphertext() throws Exception {
    checkPending(false);
  }

  @Test
  public void testOnewayCallbackWaitsForPendingCiphertext() throws Exception {
    checkPending(true);
  }

  private void checkPending(boolean oneway) throws Exception {
    SelectionKey key = mock(SelectionKey.class);
    when(key.isValid()).thenReturn(true);
    try (PendingTransport transport = new PendingTransport(key)) {
      TBinaryProtocol.Factory factory = new TBinaryProtocol.Factory();
      TAsyncClient client = new TAsyncClient(factory, null, transport) {};
      @SuppressWarnings("unchecked")
      AsyncMethodCallback<Void> callback = mock(AsyncMethodCallback.class);
      TAsyncMethodCall<Void> call =
          new TAsyncMethodCall<Void>(client, factory, transport, callback, oneway) {
            @Override
            protected void write_args(TProtocol protocol) throws TException {
              protocol.writeByte((byte) 7);
            }

            @Override
            protected Void getResult() {
              return null;
            }
          };
      call.prepareMethodCall();
      call.start(null);
      call.transition(key); // Consumes the header but retains its ciphertext.
      assertEquals(TAsyncMethodCall.State.WRITING_REQUEST_SIZE, call.getState());
      transport.allowFlush = true;
      call.transition(key);
      assertEquals(TAsyncMethodCall.State.WRITING_REQUEST_BODY, call.getState());
      transport.allowFlush = false;
      call.transition(key); // Consumes the body but retains its ciphertext.
      assertEquals(TAsyncMethodCall.State.WRITING_REQUEST_BODY, call.getState());
      call.transition(key); // A zero-byte socket write must not finish the call.
      verifyNoInteractions(callback);
      transport.allowFlush = true;
      call.transition(key);
      assertEquals(
          oneway
              ? TAsyncMethodCall.State.RESPONSE_READ
              : TAsyncMethodCall.State.READING_RESPONSE_SIZE,
          call.getState());
      if (oneway) verify(callback).onComplete(null);
      else verify(key).interestOps(SelectionKey.OP_READ);
    }
  }

  private static class PendingTransport extends TNonblockingSocket {
    private final SelectionKey key;
    private boolean pending;
    private boolean allowFlush;

    PendingTransport(SelectionKey key) throws Exception {
      super("localhost", 0);
      this.key = key;
    }

    @Override
    public boolean isOpen() {
      return true;
    }

    @Override
    public SelectionKey registerSelector(Selector selector, int interests) {
      return key;
    }

    @Override
    public boolean hasPendingWrites() {
      return pending;
    }

    @Override
    public int write(ByteBuffer buffer) {
      if (pending && !allowFlush) return 0;
      pending = false;
      int size = buffer.remaining();
      buffer.position(buffer.limit());
      pending = size > 0;
      return size;
    }
  }
}
