// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

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
                shared_ptr<TProtocolFactory> protocolFactory) :
    TServer(processor, serverTransport, transportFactory, protocolFactory) {}

  TSimpleServer(shared_ptr<TProcessor> processor,
                shared_ptr<TServerTransport> serverTransport,
                shared_ptr<TTransportFactory> inputTransportFactory,
                shared_ptr<TTransportFactory> outputTransportFactory,
                shared_ptr<TProtocolFactory> inputProtocolFactory,
                shared_ptr<TProtocolFactory> outputProtocolFactory):
    TServer(processor, serverTransport, 
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory) {}
    
  ~TSimpleServer() {}

  void serve();

};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
