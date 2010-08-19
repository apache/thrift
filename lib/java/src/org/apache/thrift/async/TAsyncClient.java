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

import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TNonblockingTransport;

public abstract class TAsyncClient {
  protected final TProtocolFactory protocolFactory;
  protected final TNonblockingTransport transport;
  protected final TAsyncClientManager manager;
  private TAsyncMethodCall currentMethod;
  private Throwable error;
  private long timeout;

  public TAsyncClient(TProtocolFactory protocolFactory, TAsyncClientManager manager, TNonblockingTransport transport) {
    this(protocolFactory, manager, transport, 0);
  }

  public TAsyncClient(TProtocolFactory protocolFactory, TAsyncClientManager manager, TNonblockingTransport transport, long timeout) {
    this.protocolFactory = protocolFactory;
    this.manager = manager;
    this.transport = transport;
    this.timeout = timeout;
  }

  public TProtocolFactory getProtocolFactory() {
    return protocolFactory;
  }

  public long getTimeout() { 
    return timeout;
  }

  public boolean hasTimeout() {
    return timeout > 0;
  }

  public void setTimeout(long timeout) {
    this.timeout = timeout;
  }

  /**
   * Is the client in an error state?
   * @return
   */
  public boolean hasError() {
    return error != null;
  }

  /**
   * Get the client's error - returns null if no error
   * @return
   */
  public Throwable getError() {
    return error;
  }

  protected void checkReady() {
    // Ensure we are not currently executing a method
    if (currentMethod != null) {
      throw new IllegalStateException("Client is currently executing another method: " + currentMethod.getClass().getName());
    }

    // Ensure we're not in an error state
    if (error != null) {
      throw new IllegalStateException("Client has an error!", error);
    }
  }

  /**
   * Called by delegate method when finished
   */
  protected void onComplete() {
    currentMethod = null;
  }

  /**
   * Called by delegate method on error
   */
  protected void onError(Throwable throwable) {
    transport.close();
    currentMethod = null;
    error = throwable;
  }
}
