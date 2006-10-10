#ifndef _THRIFT_SERVER_TSERVER_H_
#define _THRIFT_SERVER_TSERVER_H_ 1

#include <TProcessor.h>
#include <transport/TServerTransport.h>
#include <transport/TTransportFactory.h>
#include <concurrency/Thread.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace server { 

using namespace facebook::thrift;
using namespace facebook::thrift::transport;
using namespace boost;

class TServerOptions;

/**
 * Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServer {
public:
  virtual ~TServer() {}
  virtual void serve() = 0;
  
  shared_ptr<TProcessor> getProcessor() {
    return processor_;
  }
  
protected:
  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport,
          shared_ptr<TTransportFactory> transportFactory,
          shared_ptr<TServerOptions> options) :
    processor_(processor),
    serverTransport_(serverTransport),
    transportFactory_(transportFactory),
    options_(options) {}

  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerOptions> options) :
    processor_(processor), options_(options) {}
 
  shared_ptr<TProcessor> processor_;
  shared_ptr<TServerTransport> serverTransport_;
  shared_ptr<TTransportFactory> transportFactory_;
  shared_ptr<TServerOptions> options_;
};
  
/**
 * Class to encapsulate all generic server options.
 */
class TServerOptions {
 public:
  // TODO(mcslee): Fill in getters/setters here
 protected:
  // TODO(mcslee): Fill data members in here
};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSERVER_H_
