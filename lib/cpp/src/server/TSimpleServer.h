#ifndef T_SIMPLE_SERVER_H
#define T_SIMPLE_SERVER_H

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
                shared_ptr<TServerOptions> options,
                shared_ptr<TServerTransport> serverTransport) :
    TServer(processor, options), serverTransport_(serverTransport) {}
    
  ~TSimpleServer() {}

  void run();

 protected:
  shared_ptr<TServerTransport> serverTransport_;
};

}}} // facebook::thrift::server

#endif
