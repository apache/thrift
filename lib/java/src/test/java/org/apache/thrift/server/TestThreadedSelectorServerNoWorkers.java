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
package org.apache.thrift.server;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.concurrent.atomic.AtomicReference;
import org.apache.thrift.TMultiplexedProcessor;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.junit.jupiter.api.Test;

/** THRIFT-2597: stopping a server with workerThreads(0) used to throw a NullPointerException. */
public class TestThreadedSelectorServerNoWorkers {

  @Test
  public void testStopWithoutWorkerThreads() throws Exception {
    TNonblockingServerSocket socket =
        new TNonblockingServerSocket(
            new TNonblockingServerSocket.NonblockingAbstractServerSocketArgs().port(0));
    final TThreadedSelectorServer server =
        new TThreadedSelectorServer(
            new TThreadedSelectorServer.Args(socket)
                .processor(new TMultiplexedProcessor())
                .workerThreads(0));

    final AtomicReference<Throwable> failure = new AtomicReference<>();
    Thread serveThread = new Thread(server::serve);
    serveThread.setUncaughtExceptionHandler((t, e) -> failure.set(e));
    serveThread.start();

    long deadline = System.currentTimeMillis() + 10_000;
    while (!server.isServing() && serveThread.isAlive() && System.currentTimeMillis() < deadline) {
      Thread.sleep(10);
    }
    assertTrue(server.isServing());

    server.stop();
    serveThread.join(10_000);

    assertFalse(serveThread.isAlive());
    assertNull(failure.get());
  }
}
