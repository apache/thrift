#ifndef T_THREADPOOL_SERVER_H
#define T_THREADPOOL_SERVER_H

#include <concurrency/ThreadManager.h>
#include <server/TServer.h>
#include <transport/TServerTransport.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace server { 

using namespace facebook::thrift::concurrency;
using namespace facebook::thrift::transport;
using namespace boost;

class TThreadPoolServer : public TServer {
public:

  class Task;
  
  TThreadPoolServer(shared_ptr<TProcessor> processor,
		    shared_ptr<TServerOptions> options,
		    shared_ptr<TServerTransport> serverTransport,
		    shared_ptr<ThreadManager> threadManager);

  virtual ~TThreadPoolServer();

  virtual void run();

protected:

  shared_ptr<TServerTransport> serverTransport_;
  shared_ptr<ThreadManager> threadManager_;
  
};

}}} // facebook::thrift::server

#endif
