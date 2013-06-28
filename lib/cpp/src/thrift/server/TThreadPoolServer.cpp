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

#include <thrift/server/TThreadPoolServer.h>
#include <thrift/transport/TTransportException.h>
#include <thrift/concurrency/Thread.h>
#include <thrift/concurrency/ThreadManager.h>
#include <string>
#include <iostream>

namespace apache { namespace thrift { namespace server {

using boost::shared_ptr;
using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::concurrency;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

class TThreadPoolServer::Task : public Runnable {

public:

  Task(TThreadPoolServer &server,
       shared_ptr<TProcessor> processor,
       shared_ptr<TProtocol> input,
       shared_ptr<TProtocol> output,
       shared_ptr<TTransport> transport) :
    server_(server),
    processor_(processor),
    input_(input),
    output_(output),
    transport_(transport) {
  }

  ~Task() {}

  void run() {
    boost::shared_ptr<TServerEventHandler> eventHandler =
      server_.getEventHandler();
    void* connectionContext = NULL;
    if (eventHandler) {
      connectionContext = eventHandler->createContext(input_, output_);
    }
    try {
      for (;;) {
        if (eventHandler) {
          eventHandler->processContext(connectionContext, transport_);
        }
        if (!processor_->process(input_, output_, connectionContext) ||
            !input_->getTransport()->peek()) {
          break;
        }
      }
    } catch (const TTransportException&) {
      // This is reasonably expected, client didn't send a full request so just
      // ignore him
      // string errStr = string("TThreadPoolServer client died: ") + ttx.what();
      // GlobalOutput(errStr.c_str());
    } catch (const std::exception& x) {
      GlobalOutput.printf("TThreadPoolServer exception %s: %s",
                          typeid(x).name(), x.what());
    } catch (...) {
      GlobalOutput("TThreadPoolServer, unexpected exception in "
                   "TThreadPoolServer::Task::run()");
    }

    if (eventHandler) {
      eventHandler->deleteContext(connectionContext, input_, output_);
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
  shared_ptr<TTransport> transport_;
};

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

      shared_ptr<TProcessor> processor = getProcessor(inputProtocol,
                                                      outputProtocol, client);

      // Add to threadmanager pool
      shared_ptr<TThreadPoolServer::Task> task(new TThreadPoolServer::Task(
            *this, processor, inputProtocol, outputProtocol, client));
      threadManager_->add(task, timeout_, taskExpiration_);

    } catch (TTransportException& ttx) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
        string errStr = string("TThreadPoolServer: TServerTransport died on accept: ") + ttx.what();
        GlobalOutput(errStr.c_str());
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      string errStr = string("TThreadPoolServer: Caught TException: ") + tx.what();
      GlobalOutput(errStr.c_str());
      continue;
    } catch (string s) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
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

int64_t TThreadPoolServer::getTaskExpiration() const {
  return taskExpiration_;
}

void TThreadPoolServer::setTaskExpiration(int64_t value) {
  taskExpiration_ = value;
}

}}} // apache::thrift::server
