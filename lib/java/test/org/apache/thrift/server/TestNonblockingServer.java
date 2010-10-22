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


import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.TNonblockingServer.Args;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TTransport;

public class TestNonblockingServer extends ServerTestBase {

  private Thread serverThread;
  private TServer server;

  protected TServer getServer(TProcessor processor, TNonblockingServerSocket socket, TProtocolFactory protoFactory) {
    return new TNonblockingServer(new Args(socket).processor(processor).protocolFactory(protoFactory));
  }

  @Override
  public void startServer(final TProcessor processor, final TProtocolFactory protoFactory) throws Exception {
    serverThread = new Thread() {
      public void run() {
        try {
          // Transport
          TNonblockingServerSocket tServerSocket =
            new TNonblockingServerSocket(PORT);

          server = getServer(processor, tServerSocket, protoFactory);

          // Run it
          System.out.println("Starting the server on port " + PORT + "...");
          server.serve();
        } catch (Exception e) {
          e.printStackTrace();
          fail();
        }
      }
    };
    serverThread.start();
    Thread.sleep(1000);
  }

  @Override
  public void stopServer() throws Exception {
    server.stop();
    try {
      serverThread.join();
    } catch (InterruptedException e) {}
  }

  @Override
  public TTransport getClientTransport(TTransport underlyingTransport) throws Exception {
    return new TFramedTransport(underlyingTransport);
  }
}
