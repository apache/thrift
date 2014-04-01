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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

#include "gen-cpp/ThriftTest_types.h"

#include <thrift/transport/TBufferTransports.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include "gen-cpp/ThriftTest_types.h"

using apache::thrift::transport::TMemoryBuffer;
using apache::thrift::protocol::TBinaryProtocol;
using boost::shared_ptr;
using namespace thrift::test;

int main() {
  shared_ptr<TMemoryBuffer> buf(new TMemoryBuffer());
  shared_ptr<TBinaryProtocol> prot(new TBinaryProtocol(buf));

  BoolTest btest;
  btest.__isset.b = false;
  btest.__isset.s = false;

  BoolTest btestread;

  btest.write(prot.get());
  btestread.read(prot.get());
  assert(btest.__isset.b == false);
  assert(btest.__isset.s == false);
  assert(btest.s == "true");

  // Try without optional keyword
  Bonk bonk;
  bonk.type = 1;
  Bonk bonkread;
  bonk.write(prot.get());
  bonkread.read(prot.get());
  assert(bonkread.__isset.message == false);
  assert(bonkread.__isset.type == true);
  assert(bonkread.__isset.type == 1);

  return 0;
}
