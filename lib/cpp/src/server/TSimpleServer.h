#ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
#define _THRIFT_SERVER_TSIMPLESERVER_H_ 1

#include "server/TServer.h"
#include "transport/TServerTransport.h"

namespace facebook { namespace thrift { namespace server { 

/**
 * This is the most basic simple server. It is single-threaded and runs a
 * continuous loop of accepting a single connection, processing requests on
 * that connection until it closes, and then repeating. It is a good example
 * of how to extend the TServer interface.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class TSimpleServer : public TServer {
 public:
  TSimpleServer(shared_ptr<TProcessor> processor,
                shared_ptr<TServerTransport> serverTransport,
                shared_ptr<TTransportFactory> transportFactory,
                shared_ptr<TServerOptions> options) :
    TServer(processor, serverTransport, transportFactory, options) {}
    
  ~TSimpleServer() {}

  void run();

};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
