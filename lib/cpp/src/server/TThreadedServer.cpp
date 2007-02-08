#include "server/TThreadedServer.h"
#include "transport/TTransportException.h"
#include "concurrency/PosixThreadFactory.h"

#include <string>
#include <iostream>
#include <pthread.h>
#include <unistd.h>

namespace facebook { namespace thrift { namespace server { 

using namespace std;
using namespace facebook::thrift;
using namespace facebook::thrift::transport;
using namespace facebook::thrift::concurrency;

class TThreadedServer::Task: public Runnable {
       
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
      cerr << "TThreadedServer client died: " << ttx.what() << endl;
    } catch (TException& x) {
      cerr << "TThreadedServer exception: " << x.what() << endl;
    } catch (...) {
      cerr << "TThreadedServer uncaught exception." << endl;
    }
    input_->getTransport()->close();
    output_->getTransport()->close();
  }

 private:
  shared_ptr<TProcessor> processor_;
  shared_ptr<TProtocol> input_;
  shared_ptr<TProtocol> output_;

};


TThreadedServer::TThreadedServer(shared_ptr<TProcessor> processor,
                                 shared_ptr<TServerTransport> serverTransport,
                                 shared_ptr<TTransportFactory> transportFactory,
                                 shared_ptr<TProtocolFactory> protocolFactory):
  TServer(processor, serverTransport, transportFactory, protocolFactory) {
  threadFactory_ = shared_ptr<PosixThreadFactory>(new PosixThreadFactory());
}

TThreadedServer::~TThreadedServer() {}

void TThreadedServer::serve() {

  shared_ptr<TTransport> client;
  shared_ptr<TTransport> inputTransport;
  shared_ptr<TTransport> outputTransport;
  shared_ptr<TProtocol> inputProtocol;
  shared_ptr<TProtocol> outputProtocol;

  try {
    // Start the server listening
    serverTransport_->listen();
  } catch (TTransportException& ttx) {
    cerr << "TThreadedServer::run() listen(): " << ttx.what() << endl;
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

      TThreadedServer::Task* t = new TThreadedServer::Task(processor_, 
                                                           inputProtocol,
                                                           outputProtocol);

      // Create a thread for this task
      shared_ptr<Thread> thread =
        shared_ptr<Thread>(threadFactory_->newThread(shared_ptr<Runnable>(t)));
      
      // Start the thread!
      thread->start();

    } catch (TTransportException& ttx) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadedServer: TServerTransport died on accept: " << ttx.what() << endl;
      continue;
    } catch (TException& tx) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadedServer: Caught TException: " << tx.what() << endl;
      continue;
    } catch (string s) {
      inputTransport->close();
      outputTransport->close();
      client->close();
      cerr << "TThreadedServer: Unknown exception: " << s << endl;
      break;
    }
  }
}

}}} // facebook::thrift::server
