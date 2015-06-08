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
#include <sstream>

#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TSocket.h>
#include <thrift/transport/TTransportUtils.h>

#include "../gen-cpp/Calculator.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace tutorial;
using namespace shared;

// Simple benchmark.
//
// Usage: CppBenchmark [iterations]
//
// Start the TutorialServer with output redirected to /dev/null
//

int main(int argc, char* argv[]) {
  size_t iterations = 2000;

  if (argc > 1) {
    stringstream ss(argv[1]);
    ss >> iterations;
  }

  if (!iterations) {
    cout << "Usage: " << argv[0] << " [iterations]" << endl;
    return 0;
  }

  boost::shared_ptr<TTransport> socket(new TSocket("localhost", 9090));
  boost::shared_ptr<TTransport> transport(new TBufferedTransport(socket));
  boost::shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
  CalculatorClient client(protocol);

  try {
    transport->open();

    cout << "C++ Thrift benchmark" << endl;    
    cout << "Running " << iterations << " iterations" << endl;

    for(size_t i = 0; i < iterations; i++) {
      client.add(1, 1);
      if(i % 100 == 0) {
        cout << "." << flush;
      }
    }
    cout << endl << "DONE" << endl;

    transport->close();
  } catch (TException& tx) {
    cout << endl << "ERROR: " << tx.what() << endl;
  }
}
