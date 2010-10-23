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
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TServer.Args;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.server.ServerTestBase.TestHandler;

import thrift.test.Insanity;
import thrift.test.Numberz;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;

public class TestServer {

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

      // Run it
      System.out.println("Starting the server on port " + port + "...");
      serverEngine.serve();

    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
