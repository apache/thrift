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

package org.apache.thrift.test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.ServerContext;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TServer.Args;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.server.ServerTestBase.TestHandler;
import org.apache.thrift.server.TServerEventHandler;
import org.apache.thrift.transport.TTransport;

import thrift.test.Insanity;
import thrift.test.Numberz;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;

public class TestServer {

  static class TestServerContext implements ServerContext {

        int connectionId;

        public TestServerContext(int connectionId) {
            this.connectionId = connectionId;
        }

        public int getConnectionId() {
            return connectionId;
        }

        public void setConnectionId(int connectionId) {
            this.connectionId = connectionId;
        }

  }

  static class TestServerEventHandler implements TServerEventHandler {

        private int nextConnectionId = 1;

        public void preServe() {
            System.out.println("TServerEventHandler.preServe - called only once before server starts accepting connections");
        }

        public ServerContext createContext(TProtocol input, TProtocol output) {
            //we can create some connection level data which is stored while connection is alive & served
            TestServerContext ctx = new TestServerContext(nextConnectionId++);
            System.out.println("TServerEventHandler.createContext - connection #"+ctx.getConnectionId()+" established");
            return ctx;
        }

        public void deleteContext(ServerContext serverContext, TProtocol input, TProtocol output) {
            TestServerContext ctx = (TestServerContext)serverContext;
            System.out.println("TServerEventHandler.deleteContext - connection #"+ctx.getConnectionId()+" terminated");
        }

        public void processContext(ServerContext serverContext, TTransport inputTransport, TTransport outputTransport) {
            TestServerContext ctx = (TestServerContext)serverContext;
            System.out.println("TServerEventHandler.processContext - connection #"+ctx.getConnectionId()+" is ready to process next request");
        }

  }

  public static void main(String [] args) {
    try {
      int port = 9090;
      if (args.length > 1) {
        port = Integer.valueOf(args[0]);
      }
      //@TODO add other protocol and transport types

      // Processor
      TestHandler testHandler =
        new TestHandler();
      ThriftTest.Processor testProcessor =
        new ThriftTest.Processor(testHandler);

      // Transport
      TServerSocket tServerSocket =
        new TServerSocket(port);

      // Protocol factory
      TProtocolFactory tProtocolFactory =
        new TBinaryProtocol.Factory();

      TServer serverEngine;

      // Simple Server
      //serverEngine = new TSimpleServer(new Args(tServerSocket).processor(testProcessor));

      // ThreadPool Server
      serverEngine = new TThreadPoolServer(new TThreadPoolServer.Args(tServerSocket).processor(testProcessor).protocolFactory(tProtocolFactory));

      //Set server event handler
      serverEngine.setServerEventHandler(new TestServerEventHandler());

      // Run it
      System.out.println("Starting the server on port " + port + "...");
      serverEngine.serve();

    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
