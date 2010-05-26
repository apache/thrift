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
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.SelectorProvider;
import java.util.Iterator;
import java.util.concurrent.ConcurrentLinkedQueue;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Contains selector thread which transitions method call objects
 */
public class TAsyncClientManager {
  private static final Logger LOGGER = LoggerFactory.getLogger(TAsyncClientManager.class.getName());

  private final SelectThread selectThread;
  private final ConcurrentLinkedQueue<TAsyncMethodCall> pendingCalls = new ConcurrentLinkedQueue<TAsyncMethodCall>();

  public TAsyncClientManager() throws IOException {
    this.selectThread = new SelectThread();
    selectThread.start();
  }

  public void call(TAsyncMethodCall method) {
    pendingCalls.add(method);
    selectThread.getSelector().wakeup();
  }

  public void stop() {
    selectThread.finish();
  }

  private class SelectThread extends Thread {
    private final Selector selector;
    private volatile boolean running;

    public SelectThread() throws IOException {
      this.selector = SelectorProvider.provider().openSelector();
      this.running = true;
      // We don't want to hold up the JVM when shutting down
      setDaemon(true);
    }

    public Selector getSelector() {
      return selector;
    }

    public void finish() {
      running = false;
      selector.wakeup();
    }

    public void run() {
      while (running) {
        try {
          selector.select();
        } catch (IOException e) {
          LOGGER.error("Caught IOException in TAsyncClientManager!", e);
        }

        // Handle any ready channels calls
        Iterator<SelectionKey> keys = selector.selectedKeys().iterator();
        while (keys.hasNext()) {
          SelectionKey key = keys.next();
          keys.remove();
          if (!key.isValid()) {
            // this should only have happened if the method call experienced an
            // error and the key was cancelled. just skip it.
            continue;
          }
          TAsyncMethodCall method = (TAsyncMethodCall)key.attachment();
          method.transition(key);
        }

        // Start any new calls
        TAsyncMethodCall methodCall;
        while ((methodCall = pendingCalls.poll()) != null) {
          try {
            SelectionKey key = methodCall.registerWithSelector(selector);
            methodCall.transition(key);
          } catch (IOException e) {
            LOGGER.warn("Caught IOException in TAsyncClientManager!", e);
          }
        }
      }
    }
  }
}
