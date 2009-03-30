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

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.transport.TSocket;


public class OverloadNonblockingServer {

  public static void main(String[] args) throws Exception {
    int msg_size_mb = Integer.parseInt(args[0]);
    int msg_size = msg_size_mb * 1024 * 1024;

    TSocket socket = new TSocket("localhost", 9090);
    TBinaryProtocol binprot = new TBinaryProtocol(socket);
    socket.open();
    binprot.writeI32(msg_size);
    binprot.writeI32(1);
    socket.flush();

    System.in.read();
    // Thread.sleep(30000);
    for (int i = 0; i < msg_size_mb; i++) {
      binprot.writeBinary(new byte[1024 * 1024]);
    }

    socket.close();
  }
}
