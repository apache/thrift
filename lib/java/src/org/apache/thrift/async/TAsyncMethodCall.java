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

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * Encapsulates an async method call
 * Need to generate:
 *   - private void write_args(TProtocol protocol)
 *   - public T getResult() throws <Exception_1>, <Exception_2>, ...
 * @param <T>
 */
public abstract class TAsyncMethodCall<T extends TAsyncMethodCall> {
  public static enum State {
    WRITING_REQUEST_SIZE,
    WRITING_REQUEST_BODY,
    READING_RESPONSE_SIZE,
    READING_RESPONSE_BODY,
    RESPONSE_READ,
    ERROR;
  }

  private static final int INITIAL_MEMORY_BUFFER_SIZE = 128;

  protected final TNonblockingTransport transport;
  private final TProtocolFactory protocolFactory;
  protected final TAsyncClient client;
  private final AsyncMethodCallback<T> callback;
  private final boolean isOneway;

  private ByteBuffer sizeBuffer;
  private final byte[] sizeBufferArray = new byte[4];

  private ByteBuffer frameBuffer;
  private State state;

  protected TAsyncMethodCall(TAsyncClient client, TProtocolFactory protocolFactory, TNonblockingTransport transport, AsyncMethodCallback<T> callback, boolean isOneway) throws TException {
    this.transport = transport;
    this.callback = callback;
    this.protocolFactory = protocolFactory;
    this.client = client;
    this.isOneway = isOneway;

    this.state = State.WRITING_REQUEST_SIZE;
    prepareMethodCall();
  }

  protected State getState() {
    return state;
  }

  protected abstract void write_args(TProtocol protocol) throws TException;

  private void prepareMethodCall() throws TException {
    TMemoryBuffer memoryBuffer = new TMemoryBuffer(INITIAL_MEMORY_BUFFER_SIZE);
    TProtocol protocol = protocolFactory.getProtocol(memoryBuffer);
    write_args(protocol);

    int length = memoryBuffer.length();
    frameBuffer = ByteBuffer.wrap(memoryBuffer.getArray(), 0, length);

    TFramedTransport.encodeFrameSize(length, sizeBufferArray);
    sizeBuffer = ByteBuffer.wrap(sizeBufferArray);
  }

  SelectionKey registerWithSelector(Selector sel) throws IOException {
    SelectionKey key = transport.registerSelector(sel, SelectionKey.OP_WRITE);
    key.attach(this);
    return key;
  }

  protected ByteBuffer getFrameBuffer() {
    return frameBuffer;
  }

  /**
   * Transition to next state, doing whatever work is required. Since this
   * method is only called by the selector thread, we can make changes to our
   * select interests without worrying about concurrency.
   * @param key
   */
  protected void transition(SelectionKey key) {
    // Ensure key is valid
    if (!key.isValid()) {
      key.cancel();
      Exception e = new TTransportException("Selection key not valid!");
      client.onError(e);
      callback.onError(e);
      return;
    }

    // Transition function
    try {
      switch (state) {
        case WRITING_REQUEST_SIZE:
          doWritingRequestSize();
          break;
        case WRITING_REQUEST_BODY:
          doWritingRequestBody(key);
          break;
        case READING_RESPONSE_SIZE:
          doReadingResponseSize();
          break;
        case READING_RESPONSE_BODY:
          doReadingResponseBody(key);
          break;
        case RESPONSE_READ:
        case ERROR:
          throw new IllegalStateException("Method call in state " + state 
              + " but selector called transition method. Seems like a bug...");
      }
    } catch (Throwable e) {
      state = State.ERROR;
      key.cancel();
      key.attach(null);
      client.onError(e);
      callback.onError(e);
    }
  }

  private void doReadingResponseBody(SelectionKey key) throws IOException {
    if (transport.read(frameBuffer) < 0) {
      throw new IOException("Read call frame failed");
    }
    if (frameBuffer.remaining() == 0) {
      cleanUpAndFireCallback(key);
    }
  }

  private void cleanUpAndFireCallback(SelectionKey key) {
    state = State.RESPONSE_READ;
    key.interestOps(0);
    // this ensures that the TAsyncMethod instance doesn't hang around
    key.attach(null);
    key.cancel();
    client.onComplete();
    callback.onComplete((T)this);
  }

  private void doReadingResponseSize() throws IOException {
    if (transport.read(sizeBuffer) < 0) {
      throw new IOException("Read call frame size failed");
    }
    if (sizeBuffer.remaining() == 0) {
      state = State.READING_RESPONSE_BODY;
      frameBuffer = ByteBuffer.allocate(TFramedTransport.decodeFrameSize(sizeBufferArray));
    }
  }

  private void doWritingRequestBody(SelectionKey key) throws IOException {
    if (transport.write(frameBuffer) < 0) {
      throw new IOException("Write call frame failed");
    }
    if (frameBuffer.remaining() == 0) {
      if (isOneway) {
        cleanUpAndFireCallback(key);
      } else {
        state = State.READING_RESPONSE_SIZE;
        sizeBuffer.rewind();  // Prepare to read incoming frame size
        key.interestOps(SelectionKey.OP_READ);
      }
    }
  }

  private void doWritingRequestSize() throws IOException {
    if (transport.write(sizeBuffer) < 0) {
      throw new IOException("Write call frame size failed");
    }
    if (sizeBuffer.remaining() == 0) {
      state = State.WRITING_REQUEST_BODY;
    }
  }
}
