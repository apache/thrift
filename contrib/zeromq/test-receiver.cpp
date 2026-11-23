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

#include "zmq.hpp"
#include "TZmqServer.h"
#include "Storage.h"

using apache::thrift::std::shared_ptr;
using apache::thrift::TProcessor;
using apache::thrift::server::TZmqServer;
using apache::thrift::server::TZmqMultiServer;

class StorageHandler : virtual public StorageIf {
 public:
  StorageHandler()
    : value_(0)
  {}

  void incr(const int32_t amount) {
    value_ += amount;
    printf("value_: %i\n", value_) ;
  }

  int32_t get() {
    return value_;
  }

 private:
  int32_t value_;

};


int main(int argc, char *argv[]) {
  shared_ptr<StorageHandler> handler(new StorageHandler());
  shared_ptr<TProcessor> processor(new StorageProcessor(handler));

  zmq::context_t ctx(1);
  TZmqServer oneway_server(processor, ctx, "epgm://eth0;239.192.1.1:5555", ZMQ_SUB);
  oneway_server.serve();

  return 0;
}
