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

#include "server/TThreadPoolServer.h"
#include "transport/TTransportException.h"
#include "concurrency/Thread.h"
#include "concurrency/ThreadManager.h"
#include <string>
#include <iostream>

namespace apache { namespace thrift { namespace server {

using boost::shared_ptr;
using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::concurrency;
using namespace apache::thrift::protocol;;
using namespace apache::thrift::transport;

class TThreadPoolServer::Task : public Runnable {

public:

  Task(TThreadPoolServer &server,
       shared_ptr<TProcessor> processor,
       shared_ptr<TProtocol> input,
       shared_ptr<TProtocol> output) :
    server_(server),
    processor_(processor),
    input_(input),
    output_(output) {
  }

  ~Task() {}

  void run() {
    boost::shared_ptr<TServerEventHandler> eventHandler =
      server_.getEventHandler();
    if (eventHandler != NULL) {
      eventHandler->clientBegin(input_, output_);
    }
    try {
      while (processor_->process(input_, output_)) {
        if (!input_->getTransport()->peek()) {
          break;
        }
      }
    } catch (TTransportException& ttx) {
      // This is reasonably expected, client didn't send a full request so just
      // ignore him
      // string errStr = string("TThreadPoolServer client died: ") + ttx.what();
      // GlobalOutput(errStr.c_str());
    } catch (TException& x) {
      string errStr = string("TThreadPoolServer exception: ") + x.what();
      GlobalOutput(errStr.c_str());
    } catch (std::exception &x) {
      string errStr = string("TThreadPoolServer, std::exception: ") + x.what();
      GlobalOutput(errStr.c_str());
    }

    if (eventHandler != NULL) {
      eventHandler->clientEnd(input_, output_);
    }

    try {
      input_->getTransport()->close();
    } catch (TTransportException& ttx) {
      string errStr = string("TThreadPoolServer input close failed: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      output_->getTransport()->close();
    } catch (TTransportException& ttx) {
      string errStr = string("TThreadPoolServer output close failed: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }

  }

 private:
  TServer& server_;
  shared_ptr<TProcessor> processor_;
  shared_ptr<TProtocol> input_;
  shared_ptr<TProtocol> output_;

};

TThreadPoolServer::TThreadPoolServer(shared_ptr<TProcessor> processor,
                                     shared_ptr<TServerTransport> serverTransport,
                                     shared_ptr<TTransportFactory> transportFactory,
                                     shared_ptr<TProtocolFactory> protocolFactory,
                                     shared_ptr<ThreadManager> threadManager) :
  TServer(processor, serverTransport, transportFactory, protocolFactory),
  threadManager_(threadManager),
  stop_(false), timeout_(0) {}

TThreadPoolServer::TThreadPoolServer(shared_ptr<TProcessor> processor,
                                     shared_ptr<TServerTransport> serverTransport,
                                     shared_ptr<TTransportFactory> inputTransportFactory,
                                     shared_ptr<TTransportFactory> outputTransportFactory,
                                     shared_ptr<TProtocolFactory> inputProtocolFactory,
                                     shared_ptr<TProtocolFactory> outputProtocolFactory,
                                     shared_ptr<ThreadManager> threadManager) :
  TServer(processor, serverTransport, inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory),
  threadManager_(threadManager),
  stop_(false), timeout_(0) {}


TThreadPoolServer::~TThreadPoolServer() {}

void TThreadPoolServer::serve() {
  shared_ptr<TTransport> client;
  shared_ptr<TTransport> inputTransport;
  shared_ptr<TTransport> outputTransport;
  shared_ptr<TProtocol> inputProtocol;
  shared_ptr<TProtocol> outputProtocol;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    string errStr = string("TThreadPoolServer::run() listen(): ") + ttx.what();
    GlobalOutput(errStr.c_str());
    return;
  }

  // Run the preServe event
  if (eventHandler_ != NULL) {
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

      // Add to threadmanager pool
      threadManager_->add(shared_ptr<TThreadPoolServer::Task>(new TThreadPoolServer::Task(*this, processor_, inputProtocol, outputProtocol)), timeout_);

    } catch (TTransportException& ttx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
        string errStr = string("TThreadPoolServer: TServerTransport died on accept: ") + ttx.what();
        GlobalOutput(errStr.c_str());
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      string errStr = string("TThreadPoolServer: Caught TException: ") + tx.what();
      GlobalOutput(errStr.c_str());
      continue;
    } catch (string s) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
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
    } catch (TException &tx) {
      string errStr = string("TThreadPoolServer: Exception shutting down: ") + tx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }

}

int64_t TThreadPoolServer::getTimeout() const {
  return timeout_;
}

void TThreadPoolServer::setTimeout(int64_t value) {
  timeout_ = value;
}

}}} // apache::thrift::server
