#include "server/TThreadPoolServer.h"
#include "transport/TTransportException.h"
#include "concurrency/Thread.h"
#include "concurrency/ThreadManager.h"
#include <string>
#include <iostream>

namespace facebook { namespace thrift { namespace server { 

using namespace std;
using namespace facebook::thrift::concurrency;
using namespace facebook::thrift::transport;

class TThreadPoolServer::Task: public Runnable {
    
  shared_ptr<TProcessor> _processor;
  shared_ptr<TTransport> _input;
  shared_ptr<TTransport> _output;
    
public:
    
  Task(shared_ptr<TProcessor> processor,
       shared_ptr<TTransport> input,
       shared_ptr<TTransport> output) :
    _processor(processor),
    _input(input),
    _output(output) {
  }

  ~Task() {}
    
  void run() {     
    while(true) {
      try {
	_processor->process(_input, _output);
      } catch (TTransportException& ttx) {
        break;
      } catch(...) {
        break;
      }
    }
    _input->close();
    _output->close();
  }
};
  
TThreadPoolServer::TThreadPoolServer(shared_ptr<TProcessor> processor,
                                     shared_ptr<TServerTransport> serverTransport,
                                     shared_ptr<TTransportFactory> transportFactory,
                                     shared_ptr<ThreadManager> threadManager,
                                     shared_ptr<TServerOptions> options) :
  TServer(processor, serverTransport, transportFactory, options), 
  threadManager_(threadManager) {
}

TThreadPoolServer::~TThreadPoolServer() {}

void TThreadPoolServer::run() {

  shared_ptr<TTransport> client;
  pair<shared_ptr<TTransport>,shared_ptr<TTransport> > io;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TThreadPoolServer::run() listen(): " << ttx.getMessage() << endl;
    return;
  }
  
  while (true) {   
    try {
      // Fetch client from server
      client = serverTransport_->accept();
      // Make IO transports
      io = transportFactory_->getIOTransports(client);
      // Add to threadmanager pool
      threadManager_->add(shared_ptr<TThreadPoolServer::Task>(new TThreadPoolServer::Task(processor_, io.first, io.second)));
    } catch (TTransportException& ttx) {
      break;
    }
  }
}

}}} // facebook::thrift::server
