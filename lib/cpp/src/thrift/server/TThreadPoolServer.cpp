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

#include <thrift/thrift-config.h>

#include <thrift/server/TConnectedClient.h>
#include <thrift/server/TThreadPoolServer.h>
#include <thrift/transport/TTransportException.h>
#include <thrift/concurrency/Thread.h>
#include <thrift/concurrency/ThreadManager.h>
#include <string>
#include <iostream>
#include <boost/make_shared.hpp>

namespace apache {
namespace thrift {
namespace server {

using boost::shared_ptr;
using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::concurrency;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

TThreadPoolServer::~TThreadPoolServer() {}

void TThreadPoolServer::serve() {
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

      shared_ptr<TProcessor> processor = getProcessor(inputProtocol, outputProtocol, client);

      // Add to threadmanager pool
      threadManager_->add(
              boost::make_shared<TConnectedClient>(
                      "TThreadPoolServer",
                      getProcessor(inputProtocol, outputProtocol, client),
                      inputProtocol, outputProtocol, eventHandler_, client),
              timeout_,
              taskExpiration_);

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
        string errStr = string("TThreadPoolServer: TServerTransport died on accept: ") + ttx.what();
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
      string errStr = string("TThreadPoolServer: Caught TException: ") + tx.what();
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
      string errStr = "TThreadPoolServer: Unknown exception: " + s;
      GlobalOutput(errStr.c_str());
      break;
    }
  }

  // If stopped manually, join the existing threads
  if (stop_) {
    try {
      serverTransport_->close();
      threadManager_->join();
    } catch (TException& tx) {
      string errStr = string("TThreadPoolServer: Exception shutting down: ") + tx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }
}

void TThreadPoolServer::stop() {
  if (!stop_) {
    stop_ = true;
    serverTransport_->interrupt();
    serverTransport_->interruptChildren();
  }
}

int64_t TThreadPoolServer::getTimeout() const {
  return timeout_;
}

void TThreadPoolServer::setTimeout(int64_t value) {
  timeout_ = value;
}

int64_t TThreadPoolServer::getTaskExpiration() const {
  return taskExpiration_;
}

void TThreadPoolServer::setTaskExpiration(int64_t value) {
  taskExpiration_ = value;
}
}
}
} // apache::thrift::server
