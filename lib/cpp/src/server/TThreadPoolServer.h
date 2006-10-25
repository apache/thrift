#ifndef _THRIFT_SERVER_TTHREADPOOLSERVER_H_
#define _THRIFT_SERVER_TTHREADPOOLSERVER_H_ 1

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
		    shared_ptr<TServerTransport> serverTransport,
		    shared_ptr<TTransportFactory> transportFactory,
                    shared_ptr<TProtocolFactory> protocolFactory,
		    shared_ptr<ThreadManager> threadManager);

  virtual ~TThreadPoolServer();

  virtual void serve();

protected:

  shared_ptr<ThreadManager> threadManager_;
  
};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADPOOLSERVER_H_
