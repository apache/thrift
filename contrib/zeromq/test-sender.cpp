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

#include <iostream>
#include <cstdlib>
#include <thrift/protocol/TBinaryProtocol.h>

#include "zmq.hpp"
#include "TZmqClient.h"
#include "Storage.h"

using apache::thrift::std::shared_ptr;
using apache::thrift::transport::TZmqClient;
using apache::thrift::protocol::TBinaryProtocol;

int main(int argc, char** argv) {
  const char* endpoint = "epgm://eth0;239.192.1.1:5555";
  int socktype = ZMQ_PUB;
  int incr = 1;
  if (argc > 1) {
    incr = atoi(argv[1]);
  }

  zmq::context_t ctx(1);
  shared_ptr<TZmqClient> transport(new TZmqClient(ctx, endpoint, socktype));
  shared_ptr<TBinaryProtocol> protocol(new TBinaryProtocol(transport));
  StorageClient client(protocol);

  transport->open();

  client.incr(incr);
  usleep(50000);

  return 0;
}
