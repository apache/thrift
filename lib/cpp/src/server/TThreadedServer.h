#ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
#define _THRIFT_SERVER_TTHREADEDSERVER_H_ 1

#include <server/TServer.h>
#include <transport/TServerTransport.h>
#include <concurrency/Thread.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace server { 

using namespace facebook::thrift::transport;
using namespace facebook::thrift::concurrency;
using namespace boost;

class TThreadedServer : public TServer {

 public:
  class Task;
  
  TThreadedServer(shared_ptr<TProcessor> processor,
                  shared_ptr<TServerTransport> serverTransport,
                  shared_ptr<TTransportFactory> transportFactory,
                  shared_ptr<TProtocolFactory> protocolFactory);

  virtual ~TThreadedServer();

  virtual void serve();

 protected:
  shared_ptr<ThreadFactory> threadFactory_;

};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
