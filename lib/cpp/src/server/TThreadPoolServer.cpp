// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "server/TThreadPoolServer.h"
#include "transport/TTransportException.h"
#include "concurrency/Thread.h"
#include "concurrency/ThreadManager.h"
#include <string>
#include <iostream>

namespace facebook { namespace thrift { namespace server { 

using boost::shared_ptr;
using namespace std;
using namespace facebook::thrift;
using namespace facebook::thrift::concurrency;
using namespace facebook::thrift::protocol;;
using namespace facebook::thrift::transport;

class TThreadPoolServer::Task: public Runnable {
       
public:
    
  Task(shared_ptr<TProcessor> processor,
       shared_ptr<TProtocol> input,
       shared_ptr<TProtocol> output) :
    processor_(processor),
    input_(input),
    output_(output) {
  }

  ~Task() {}
    
  void run() {
    try {
      while (processor_->process(input_, output_)) {
        if (!input_->getTransport()->peek()) {
          break;
        }
      }
    } catch (TTransportException& ttx) {
      // This is reasonably expected, client didn't send a full request so just
      // ignore him
      //cerr << "TThreadPoolServer client died: " << ttx.what() << endl;
    } catch (TException& x) {
      cerr << "TThreadPoolServer exception: " << x.what() << endl;
    } catch (...) {
      cerr << "TThreadPoolServer uncaught exception." << endl;
    }
    input_->getTransport()->close();
    output_->getTransport()->close();
  }

 private:
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
    cerr << "TThreadPoolServer::run() listen(): " << ttx.what() << endl;
    return;
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
      threadManager_->add(shared_ptr<TThreadPoolServer::Task>(new TThreadPoolServer::Task(processor_, inputProtocol, outputProtocol)), timeout_);

    } catch (TTransportException& ttx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
        cerr << "TThreadPoolServer: TServerTransport died on accept: " << ttx.what() << endl;
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "TThreadPoolServer: Caught TException: " << tx.what() << endl;
      continue;
    } catch (string s) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "TThreadPoolServer: Unknown exception: " << s << endl;
      break;
    }
  }

  // If stopped manually, join the existing threads
  if (stop_) {
    try {
      serverTransport_->close();
      threadManager_->join();
    } catch (TException &tx) {
      cerr << "TThreadPoolServer: Exception shutting down: " << tx.what() << endl;
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

}}} // facebook::thrift::server
