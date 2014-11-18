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

#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/TTransportException.h>
#include <thrift/concurrency/PlatformThreadFactory.h>

#include <string>
#include <iostream>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

namespace apache { namespace thrift { namespace server {

using boost::shared_ptr;
using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::concurrency;

class TThreadedServer::Task: public Runnable {

public:

  Task(TThreadedServer& server,
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
    } catch (const TTransportException& ttx) {
      if (ttx.getType() != TTransportException::END_OF_FILE) {
        string errStr = string("TThreadedServer client died: ") + ttx.what();
        GlobalOutput(errStr.c_str());
      }
    } catch (const std::exception &x) {
      GlobalOutput.printf("TThreadedServer exception: %s: %s",
                          typeid(x).name(), x.what());
    } catch (...) {
      GlobalOutput("TThreadedServer uncaught exception.");
    }
    if (eventHandler) {
      eventHandler->deleteContext(connectionContext, input_, output_);
    }

    try {
      input_->getTransport()->close();
    } catch (TTransportException& ttx) {
      string errStr = string("TThreadedServer input close failed: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      output_->getTransport()->close();
    } catch (TTransportException& ttx) {
      string errStr = string("TThreadedServer output close failed: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }

    // Remove this task from parent bookkeeping
    {
      Synchronized s(server_.tasksMonitor_);
      server_.tasks_.erase(this);
      if (server_.tasks_.empty()) {
        server_.tasksMonitor_.notify();
      }
    }

  }

 private:
  TThreadedServer& server_;
  friend class TThreadedServer;

  shared_ptr<TProcessor> processor_;
  shared_ptr<TProtocol> input_;
  shared_ptr<TProtocol> output_;
  shared_ptr<TTransport> transport_;
};

void TThreadedServer::init() {
  stop_ = false;

  if (!threadFactory_) {
    threadFactory_.reset(new PlatformThreadFactory);
  }
}

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

      shared_ptr<TProcessor> processor = getProcessor(inputProtocol,
                                                      outputProtocol, client);

      TThreadedServer::Task* task = new TThreadedServer::Task(*this,
                                                              processor,
                                                              inputProtocol,
                                                              outputProtocol,
                                                              client);

      // Create a task
      shared_ptr<Runnable> runnable =
        shared_ptr<Runnable>(task);

      // Create a thread for this task
      shared_ptr<Thread> thread =
        shared_ptr<Thread>(threadFactory_->newThread(runnable));

      // Insert thread into the set of threads
      {
        Synchronized s(tasksMonitor_);
        tasks_.insert(task);
      }

      // Start the thread!
      thread->start();

    } catch (TTransportException& ttx) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
        string errStr = string("TThreadedServer: TServerTransport died on accept: ") + ttx.what();
        GlobalOutput(errStr.c_str());
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      string errStr = string("TThreadedServer: Caught TException: ") + tx.what();
      GlobalOutput(errStr.c_str());
      continue;
    } catch (string s) {
      if (inputTransport) { inputTransport->close(); }
      if (outputTransport) { outputTransport->close(); }
      if (client) { client->close(); }
      string errStr = "TThreadedServer: Unknown exception: " + s;
      GlobalOutput(errStr.c_str());
      break;
    }
  }

  // If stopped manually, make sure to close server transport
  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TException &tx) {
      string errStr = string("TThreadedServer: Exception shutting down: ") + tx.what();
      GlobalOutput(errStr.c_str());
    }
    try {
      Synchronized s(tasksMonitor_);
      while (!tasks_.empty()) {
        tasksMonitor_.wait();
      }
    } catch (TException &tx) {
      string errStr = string("TThreadedServer: Exception joining workers: ") + tx.what();
      GlobalOutput(errStr.c_str());
    }
    stop_ = false;
  }

}

}}} // apache::thrift::server
