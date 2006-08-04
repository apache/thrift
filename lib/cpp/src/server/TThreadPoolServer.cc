#include "server/TThreadPoolServer.h"
#include "transport/TBufferedTransport.h"
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
  shared_ptr<TTransport> _transport;
  shared_ptr<TBufferedTransport> _bufferedTransport;
    
public:
    
  Task(shared_ptr<TProcessor> processor,
       shared_ptr<TTransport> transport) :
    _processor(processor),
    _transport(transport),
    _bufferedTransport(new TBufferedTransport(transport)) {
  }

  ~Task() {}
    
  void run() {
      
    while(true) {
	
      try {
	_processor->process(_bufferedTransport);
	
      } catch (TTransportException& ttx) {
	
	break;
	
      } catch(...) {
	
	break;
      }
    }
    
    _bufferedTransport->close();
  }
};
  
TThreadPoolServer::TThreadPoolServer(shared_ptr<TProcessor> processor,
				     shared_ptr<TServerOptions> options,
				     shared_ptr<TServerTransport> serverTransport,
				     shared_ptr<ThreadManager> threadManager) :
  TServer(processor, options), 
  serverTransport_(serverTransport), 
  threadManager_(threadManager) {
}
    
TThreadPoolServer::~TThreadPoolServer() {}

void TThreadPoolServer::run() {

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TThreadPoolServer::run() listen(): " << ttx.getMessage() << endl;
    return;
  }
  
  // Fetch client from server
  
  while (true) {
    
    try {
      
      threadManager_->add(shared_ptr<TThreadPoolServer::Task>(new TThreadPoolServer::Task(processor_, 
											  shared_ptr<TTransport>(serverTransport_->accept()))));
      
    } catch (TTransportException& ttx) {
      break;
    }
  }
}

}}} // facebook::thrift::server
