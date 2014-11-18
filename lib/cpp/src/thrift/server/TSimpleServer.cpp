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

#include <thrift/server/TSimpleServer.h>
#include <thrift/transport/TTransportException.h>
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

  // Start the server listening
  serverTransport_->listen();

  // Run the preServe event
  if (eventHandler_) {
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
    } catch (TTransportException& ttx) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
          string errStr = string("TServerTransport died on accept: ") + ttx.what();
          GlobalOutput(errStr.c_str());
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      string errStr = string("Some kind of accept exception: ") + tx.what();
      GlobalOutput(errStr.c_str());
      continue;
    } catch (string s) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      string errStr = string("Some kind of accept exception: ") + s;
      GlobalOutput(errStr.c_str());
      break;
    }

    // Get the processor
    shared_ptr<TProcessor> processor = getProcessor(inputProtocol,
                                                    outputProtocol, client);

    void* connectionContext = NULL;
    if (eventHandler_) {
      connectionContext = eventHandler_->createContext(inputProtocol, outputProtocol);
    }
    try {
      for (;;) {
        if (eventHandler_) {
          eventHandler_->processContext(connectionContext, client);
        }
        if (!processor->process(inputProtocol, outputProtocol,
                                connectionContext) ||
          // Peek ahead, is the remote side closed?
            !inputProtocol->getTransport()->peek()) {
          break;
        }
      }
    } catch (const TTransportException& ttx) {
      string errStr = string("TSimpleServer client died: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    } catch (const std::exception& x) {
      GlobalOutput.printf("TSimpleServer exception: %s: %s",
                          typeid(x).name(), x.what());
    } catch (...) {
      GlobalOutput("TSimpleServer uncaught exception.");
    }
    if (eventHandler_) {
      eventHandler_->deleteContext(connectionContext, inputProtocol, outputProtocol);
    }

    try {
      inputTransport->close();
    } catch (const TTransportException& ttx) {
      string errStr = string("TSimpleServer input close failed: ")
        + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      outputTransport->close();
    } catch (const TTransportException& ttx) {
      string errStr = string("TSimpleServer output close failed: ")
        + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      client->close();
    } catch (const TTransportException& ttx) {
      string errStr = string("TSimpleServer client close failed: ")
        + ttx.what();
      GlobalOutput(errStr.c_str());
    }
  }

  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TTransportException &ttx) {
      string errStr = string("TServerTransport failed on close: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }
}

}}} // apache::thrift::server
