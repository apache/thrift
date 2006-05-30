#ifndef T_SIMPLE_SERVER_H
#define T_SIMPLE_SERVER_H

#include "server/TServer.h"
#include "transport/TServerTransport.h"

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
  TSimpleServer(TDispatcher* dispatcher,
                TServerOptions* options,
                TServerTransport* serverTransport) :
    TServer(dispatcher, options), serverTransport_(serverTransport) {}
    
  ~TSimpleServer() {}

  void run();

 protected:
  TServerTransport* serverTransport_;
};

#endif
