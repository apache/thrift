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

using namespace std;
using namespace facebook::thrift;
using namespace facebook::thrift::concurrency;
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
  threadManager_(threadManager) {}

TThreadPoolServer::TThreadPoolServer(shared_ptr<TProcessor> processor,
                                     shared_ptr<TServerTransport> serverTransport,
                                     shared_ptr<TTransportFactory> inputTransportFactory,
                                     shared_ptr<TTransportFactory> outputTransportFactory,
                                     shared_ptr<TProtocolFactory> inputProtocolFactory,
                                     shared_ptr<TProtocolFactory> outputProtocolFactory, 
                                     shared_ptr<ThreadManager> threadManager) :
  TServer(processor, serverTransport, inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory),
  threadManager_(threadManager) {}


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
  
  while (true) {   
    try {
      // Fetch client from server
      client = serverTransport_->accept();
      // Make IO transports
      inputTransport = inputTransportFactory_->getTransport(client);
      outputTransport = outputTransportFactory_->getTransport(client);
      inputProtocol = inputProtocolFactory_->getProtocol(inputTransport);
      outputProtocol = outputProtocolFactory_->getProtocol(outputTransport);

      // Add to threadmanager pool
      threadManager_->add(shared_ptr<TThreadPoolServer::Task>(new TThreadPoolServer::Task(processor_, 
                                                                                          inputProtocol, 
                                                                                          outputProtocol)));
    } catch (TTransportException& ttx) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadPoolServer: TServerTransport died on accept: " << ttx.what() << endl;
      continue;
    } catch (TException& tx) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadPoolServer: Caught TException: " << tx.what() << endl;
      continue;
    } catch (string s) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadPoolServer: Unknown exception: " << s << endl;
      break;
    }
  }
}

}}} // facebook::thrift::server
