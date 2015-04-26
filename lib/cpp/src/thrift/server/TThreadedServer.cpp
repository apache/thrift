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

#include <boost/bind.hpp>
#include <thrift/server/TConnectedClient.h>
#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/TTransportException.h>
#include <thrift/concurrency/PlatformThreadFactory.h>

#include <string>
#include <iostream>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace apache {
namespace thrift {
namespace server {

using boost::shared_ptr;
using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::concurrency;

TThreadedServer::~TThreadedServer() {}

void TThreadedServer::serve() {

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

  while (!stop_) {
    try {
      client.reset();
      inputTransport.reset();
      outputTransport.reset();
      inputProtocol.reset();
      outputProtocol.reset();

      // Fetch client from server
      client = serverTransport_->accept();

      // Make IO transports
      inputTransport = inputTransportFactory_->getTransport(client);
      outputTransport = outputTransportFactory_->getTransport(client);
      inputProtocol = inputProtocolFactory_->getProtocol(inputTransport);
      outputProtocol = outputProtocolFactory_->getProtocol(outputTransport);

      shared_ptr<TConnectedClient> pClient(
              new TConnectedClient("TThreadedServer",
                      getProcessor(inputProtocol, outputProtocol, client),
                      inputProtocol, outputProtocol, eventHandler_, client),
              boost::bind(&TThreadedServer::disposeClient, this, _1));

      // Create a thread for this client
      shared_ptr<Thread> thread = shared_ptr<Thread>(threadFactory_->newThread(pClient));

      // Insert thread into the set of threads
      {
        Synchronized s(clientsMonitor_);
        clients_.insert(pClient.get());
      }

      // Start the thread!
      thread->start();

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
        string errStr = string("TThreadedServer: TServerTransport died on accept: ") + ttx.what();
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
      string errStr = string("TThreadedServer: Caught TException: ") + tx.what();
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
      string errStr = "TThreadedServer: Unknown exception: " + s;
      GlobalOutput(errStr.c_str());
      break;
    }
  }

  // If stopped manually, make sure to close server transport
  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TException& tx) {
      string errStr = string("TThreadedServer: Exception shutting down: ") + tx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      Synchronized s(clientsMonitor_);
      while (!clients_.empty()) {
          clientsMonitor_.wait();
      }
    } catch (TException& tx) {
      string errStr = string("TThreadedServer: Exception joining workers: ") + tx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }
}

void TThreadedServer::stop() {
  if (!stop_) {
	stop_ = true;
	serverTransport_->interrupt();
	serverTransport_->interruptChildren();
  }
}

void TThreadedServer::disposeClient(TConnectedClient *pClient) {
  // Remove this task from parent bookkeeping
  {
    Synchronized s(clientsMonitor_);
    clients_.erase(pClient);
    if (clients_.empty()) {
        clientsMonitor_.notify();
    }
  }
  delete pClient;
}

}
}
} // apache::thrift::server
