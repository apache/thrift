// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#include "server/TThreadedServer.h"
#include "transport/TTransportException.h"
#include "concurrency/PosixThreadFactory.h"

#include <string>
#include <iostream>
#include <pthread.h>
#include <unistd.h>

namespace facebook { namespace thrift { namespace server { 

using boost::shared_ptr;
using namespace std;
using namespace facebook::thrift;
using namespace facebook::thrift::protocol;
using namespace facebook::thrift::transport;
using namespace facebook::thrift::concurrency;

class TThreadedServer::Task: public Runnable {
       
public:
    
  Task(TThreadedServer* server,
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
    
    // Remove this task from parent bookkeeping
    {
      Synchronized s(server_->tasksMonitor_);
      server_->tasks_.erase(this);
      if (server_->tasks_.empty()) {
        server_->tasksMonitor_.notify();
      }
    }

  }

 private:
  TThreadedServer* server_;
  friend class TThreadedServer;

  shared_ptr<TProcessor> processor_;
  shared_ptr<TProtocol> input_;
  shared_ptr<TProtocol> output_;
};


TThreadedServer::TThreadedServer(shared_ptr<TProcessor> processor,
                                 shared_ptr<TServerTransport> serverTransport,
                                 shared_ptr<TTransportFactory> transportFactory,
                                 shared_ptr<TProtocolFactory> protocolFactory):
  TServer(processor, serverTransport, transportFactory, protocolFactory),
  stop_(false) {
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

      TThreadedServer::Task* task = new TThreadedServer::Task(this,
                                                              processor_, 
                                                              inputProtocol,
                                                              outputProtocol);
        
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
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      if (!stop_ || ttx.getType() != TTransportException::INTERRUPTED) {
        cerr << "TThreadedServer: TServerTransport died on accept: " << ttx.what() << endl;
      }
      continue;
    } catch (TException& tx) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "TThreadedServer: Caught TException: " << tx.what() << endl;
      continue;
    } catch (string s) {
      if (inputTransport != NULL) { inputTransport->close(); }
      if (outputTransport != NULL) { outputTransport->close(); }
      if (client != NULL) { client->close(); }
      cerr << "TThreadedServer: Unknown exception: " << s << endl;
      break;
    }
  }

  // If stopped manually, make sure to close server transport
  if (stop_) {
    try {
      serverTransport_->close();
    } catch (TException &tx) {
      cerr << "TThreadedServer: Exception shutting down: " << tx.what() << endl;
    }
    try {
      Synchronized s(tasksMonitor_);
      while (!tasks_.empty()) {
        tasksMonitor_.wait();
      }
    } catch (TException &tx) {
      cerr << "TThreadedServer: Exception joining workers: " << tx.what() << endl;
    }
    stop_ = false;
  }

}

}}} // facebook::thrift::server
