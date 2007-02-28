// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

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

  shared_ptr<TServerTransport> getServerTransport() {
    return serverTransport_;
  }

  shared_ptr<TTransportFactory> getInputTransportFactory() {
    return inputTransportFactory_;
  }

  shared_ptr<TTransportFactory> getOutputTransportFactory() {
    return outputTransportFactory_;
  }
  
  shared_ptr<TProtocolFactory> getInputProtocolFactory() {
    return inputProtocolFactory_;
  }

  shared_ptr<TProtocolFactory> getOutputProtocolFactory() {
    return outputProtocolFactory_;
  }

protected:
  TServer(shared_ptr<TProcessor> processor):
    processor_(processor) {
    setInputTransportFactory(shared_ptr<TTransportFactory>(new TTransportFactory()));
    setOutputTransportFactory(shared_ptr<TTransportFactory>(new TTransportFactory()));
    setInputProtocolFactory(shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory()));
    setOutputProtocolFactory(shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory()));
  }

  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport):
    processor_(processor),
    serverTransport_(serverTransport) {
    setInputTransportFactory(shared_ptr<TTransportFactory>(new TTransportFactory()));
    setOutputTransportFactory(shared_ptr<TTransportFactory>(new TTransportFactory()));
    setInputProtocolFactory(shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory()));
    setOutputProtocolFactory(shared_ptr<TProtocolFactory>(new TBinaryProtocolFactory()));
  }

  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport,
          shared_ptr<TTransportFactory> transportFactory,
          shared_ptr<TProtocolFactory> protocolFactory):
    processor_(processor),
    serverTransport_(serverTransport),
    inputTransportFactory_(transportFactory),
    outputTransportFactory_(transportFactory),
    inputProtocolFactory_(protocolFactory),
    outputProtocolFactory_(protocolFactory) {}

  TServer(shared_ptr<TProcessor> processor,
          shared_ptr<TServerTransport> serverTransport,
          shared_ptr<TTransportFactory> inputTransportFactory,
          shared_ptr<TTransportFactory> outputTransportFactory,
          shared_ptr<TProtocolFactory> inputProtocolFactory,
          shared_ptr<TProtocolFactory> outputProtocolFactory):
    processor_(processor),
    serverTransport_(serverTransport),
    inputTransportFactory_(inputTransportFactory),
    outputTransportFactory_(outputTransportFactory),
    inputProtocolFactory_(inputProtocolFactory),
    outputProtocolFactory_(outputProtocolFactory) {}

 
  // Class variables
  shared_ptr<TProcessor> processor_;
  shared_ptr<TServerTransport> serverTransport_;

  shared_ptr<TTransportFactory> inputTransportFactory_;
  shared_ptr<TTransportFactory> outputTransportFactory_;

  shared_ptr<TProtocolFactory> inputProtocolFactory_;
  shared_ptr<TProtocolFactory> outputProtocolFactory_;

  void setInputTransportFactory(shared_ptr<TTransportFactory> inputTransportFactory) {
    inputTransportFactory_ = inputTransportFactory;
  }

  void setOutputTransportFactory(shared_ptr<TTransportFactory> outputTransportFactory) {
    outputTransportFactory_ = outputTransportFactory;
  }

  void setInputProtocolFactory(shared_ptr<TProtocolFactory> inputProtocolFactory) {
    inputProtocolFactory_ = inputProtocolFactory;
  }

  void setOutputProtocolFactory(shared_ptr<TProtocolFactory> outputProtocolFactory) {
    outputProtocolFactory_ = outputProtocolFactory;
  }

};
  
}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSERVER_H_
