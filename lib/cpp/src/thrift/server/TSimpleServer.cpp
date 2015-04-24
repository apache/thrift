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

#include <thrift/server/TConnectedClient.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/transport/TTransportException.h>
#include <string>
#include <iostream>

namespace apache {
namespace thrift {
namespace server {

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
      if (inputTransport) {
        inputTransport->close();
      }
      if (outputTransport) {
        outputTransport->close();
      }
      if (client) {
        client->close();
      }
      if (ttx.getType() != TTransportException::INTERRUPTED) {
        string errStr = string("TServerTransport died on accept: ") + ttx.what();
        GlobalOutput(errStr.c_str());
      }
      if (stop_) break; else continue;
    } catch (TException& tx) {
      if (inputTransport) {
        inputTransport->close();
      }
      if (outputTransport) {
        outputTransport->close();
      }
      if (client) {
        client->close();
      }
      string errStr = string("Some kind of accept exception: ") + tx.what();
      GlobalOutput(errStr.c_str());
      continue;
    } catch (const string& s) {
      if (inputTransport) {
        inputTransport->close();
      }
      if (outputTransport) {
        outputTransport->close();
      }
      if (client) {
        client->close();
      }
      string errStr = string("Some kind of accept exception: ") + s;
      GlobalOutput(errStr.c_str());
      break;
    }

    TConnectedClient("TSimpleServer",
            getProcessor(inputProtocol, outputProtocol, client),
            inputProtocol, outputProtocol, eventHandler_, client).run();
  }

  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TTransportException& ttx) {
      string errStr = string("TServerTransport failed on close: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }
}

void TSimpleServer::stop() {
  if (!stop_) {
    stop_ = true;
    serverTransport_->interrupt();
    serverTransport_->interruptChildren();
  }
}

}
}
} // apache::thrift::server
