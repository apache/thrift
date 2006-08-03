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

class TThreadPoolServer : public TServer {

  class Task: public Runnable {
    
    TProcessor* _processor;
    TTransport* _transport;
    TBufferedTransport _bufferedTransport;
    
  public:
    
    Task(TProcessor* processor,
	 TTransport* transport) :
      _processor(processor),
      _transport(transport),
      _bufferedTransport(_transport) {
    }
    
    ~Task() {
      delete _transport;
    }
    
    void run() {
      
      while(true) {
	
	try {
	  _processor->process(&_bufferedTransport);

	} catch (TTransportException& ttx) {

	  break;

	} catch(...) {

	  break;
	}
      }
      
      _bufferedTransport.close();
    }
  };
  
  TThreadPoolServer(TProcessor* processor,
		    TServerOptions* options,
		    TServerTransport* serverTransport,
		    ThreadManager* threadManager) :
    TServer(processor, options), 
    serverTransport_(serverTransport), 
    threadManager_(threadManager) {
  }
    
  ~TThreadPoolServer() {}

 protected:

  TServerTransport* serverTransport_;
  ThreadManager* threadManager_;
  
  void run() {

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

	threadManager_->add(shared_ptr<Task>(new TThreadPoolServer::Task(processor_, serverTransport_->accept())));;

      } catch (TTransportException& ttx) {
	break;
      }
    }
  }
};

}}} // facebook::thrift::server
