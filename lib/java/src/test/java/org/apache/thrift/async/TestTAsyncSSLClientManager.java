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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import org.apache.thrift.server.ServerTestBase;
import org.apache.thrift.server.TThreadPoolServer;
import org.apache.thrift.server.TThreadPoolServer.Args;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TSSLTransportFactory;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.transport.layered.TFramedTransport;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import thrift.test.Srv;

public class TestTAsyncSSLClientManager extends TestTAsyncClientManager {

  protected final List<TNonblockingTransport> clientTransportList =
      Collections.synchronizedList(new ArrayList<>());

  @BeforeEach
  public void setUp() throws Exception {
    server_ =
        new TThreadPoolServer(
            new Args(TSSLTransportFactory.getServerSocket(ServerTestBase.PORT))
                .transportFactory(new TFramedTransport.Factory())
                .processor(new Srv.Processor<>(new SrvHandler())));
    serverThread_ = new Thread(() -> server_.serve());
    serverThread_.start();
    clientManager_ = new TAsyncClientManager();
    Thread.sleep(500);
  }

  @AfterEach
  public void tearDown() throws Exception {
    for (TNonblockingTransport clientTransport : clientTransportList) {
      clientTransport.close();
    }
    super.tearDown();
  }

  @Override
  public TNonblockingTransport getClientTransport() throws TTransportException, IOException {
    TNonblockingTransport clientTransport =
        TSSLTransportFactory.getNonblockingClientSocket(ServerTestBase.HOST, ServerTestBase.PORT);
    clientTransportList.add(clientTransport);
    return clientTransport;
  }
}
