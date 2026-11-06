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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

import java.nio.channels.SelectionKey;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TNonblockingTransport;
import org.junit.jupiter.api.Test;

public class TestTAsyncMethodCallConnect {
  @Test
  public void testConnectionCanResumeOnReadReadiness() throws Exception {
    TNonblockingTransport transport = mock(TNonblockingTransport.class);
    SelectionKey key = mock(SelectionKey.class);
    when(key.isValid()).thenReturn(true);
    when(key.isConnectable()).thenReturn(true, false);
    when(key.isReadable()).thenReturn(true);
    when(transport.registerSelector(any(), anyInt())).thenReturn(key);
    when(transport.finishConnect()).thenReturn(false, true);
    TBinaryProtocol.Factory factory = new TBinaryProtocol.Factory();
    TAsyncClient client = new TAsyncClient(factory, null, transport) {};
    @SuppressWarnings("unchecked")
    AsyncMethodCallback<Void> callback = mock(AsyncMethodCallback.class);
    TAsyncMethodCall<Void> call =
        new TAsyncMethodCall<Void>(client, factory, transport, callback, false) {
          @Override
          protected void write_args(TProtocol protocol) {}

          @Override
          protected Void getResult() {
            return null;
          }
        };
    call.start(null);
    call.transition(key);
    assertEquals(TAsyncMethodCall.State.CONNECTING, call.getState());
    call.transition(key);
    assertEquals(TAsyncMethodCall.State.WRITING_REQUEST_SIZE, call.getState());
    verifyNoInteractions(callback);
    verify(key).interestOps(SelectionKey.OP_WRITE);
  }
}
