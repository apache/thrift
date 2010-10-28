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

#include "server/TSimpleServer.h"
#include "transport/TTransportException.h"
#include <string>
#include <iostream>

namespace apache { namespace thrift { namespace server {

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using boost::shared_ptr;

/**
 * A simple single-threaded application server. Perfect for unit tests!
 *
 */
void TSimpleServer::serve() {

  shared_ptr<TTransport> client;
  shared_ptr<TTransport> inputTransport;
  shared_ptr<TTransport> outputTransport;
  shared_ptr<TProtocol> inputProtocol;
  shared_ptr<TProtocol> outputProtocol;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TSimpleServer::run() listen(): " << ttx.what() << endl;
    return;
  }

  // Run the preServe event
  if (eventHandler_ != NULL) {
    eventHandler_->preServe();
  }

  // Fetch client from server
  while (!stop_) {
    try {
      client = serverTransport_->accept();
      inputTransport = inputTransportFactory_->getTransport(client);
      outputTransport = outputTransportFactory_->getTransport(client);
      inputProtocol = inputProtocolFactory_->getProtocol(inputTransport);
      outputProtocol = outputProtocolFactory_->getProtocol(outputTransport);
      if (eventHandler_ != NULL) {
        eventHandler_->clientBegin(inputProtocol, outputProtocol);
      }
      try {
        while (processor_->process(inputProtocol, outputProtocol)) {
          // Peek ahead, is the remote side closed?
          if (!inputTransport->peek()) {
            break;
          }
        }
      } catch (TTransportException& ttx) {
        cerr << "TSimpleServer client died: " << ttx.what() << endl;
      } catch (TException& tx) {
        cerr << "TSimpleServer exception: " << tx.what() << endl;
      }
      if (eventHandler_ != NULL) {
        eventHandler_->clientEnd(inputProtocol, outputProtocol);
      }
      inputTransport->close();
      outputTransport->close();
      client->close();
    } catch (TTransportException& ttx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "TServerTransport died on accept: " << ttx.what() << endl;
      continue;
    } catch (TException& tx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "Some kind of accept exception: " << tx.what() << endl;
      continue;
    } catch (string s) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "TThreadPoolServer: Unknown exception: " << s << endl;
      break;
    }
  }

  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TTransportException &ttx) {
      cerr << "TServerTransport failed on close: " << ttx.what() << endl;
    }
    stop_ = false;
  }
}

}}} // apache::thrift::server
