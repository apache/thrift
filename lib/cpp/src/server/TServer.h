#ifndef _THRIFT_SERVER_TSERVER_H_
#define _THRIFT_SERVER_TSERVER_H_ 1

#include <TProcessor.h>
#include <transport/TServerTransport.h>
#include <protocol/TBinaryProtocol.h>
#include <concurrency/Thread.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace server { 

using namespace facebook::thrift;
using namespace facebook::thrift::transport;
using namespace boost;

/**
 * Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TServer : public concurrency::Runnable {
public:
  virtual ~TServer() {}

  virtual void serve() = 0;

  // Allows running the server as a Runnable thread
  virtual void run() {
    serve();
  }
  
  shared_ptr<TProcessor> getProcessor() {
    return processor_;
  }
  
  shared_ptr<TProtocolFactory> getProtocolFactory() {
    return protocolFactory_;
  }

protected:
  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport,
          shared_ptr<TTransportFactory> transportFactory,
          shared_ptr<TProtocolFactory> protocolFactory) :
    processor_(processor),
    serverTransport_(serverTransport),
    transportFactory_(transportFactory),
    protocolFactory_(protocolFactory) {}

  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport,
          shared_ptr<TTransportFactory> transportFactory) :
    processor_(processor),
    serverTransport_(serverTransport),
    transportFactory_(transportFactory) {
  protocolFactory_ = boost::shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory());
 }

  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport) :
    processor_(processor),
    serverTransport_(serverTransport) {
    transportFactory_ = boost::shared_ptr<TTransportFactory>(new TTransportFactory());
    protocolFactory_ = boost::shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory());
  }

  TServer(shared_ptr<TProcessor> processor) :
    processor_(processor) {
    transportFactory_ = boost::shared_ptr<TTransportFactory>(new TTransportFactory());
    protocolFactory_ = boost::shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory());
  }
 
  shared_ptr<TProcessor> processor_;
  shared_ptr<TServerTransport> serverTransport_;
  shared_ptr<TTransportFactory> transportFactory_;
  shared_ptr<TProtocolFactory> protocolFactory_;
};
  
}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSERVER_H_
