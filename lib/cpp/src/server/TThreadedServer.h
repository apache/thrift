// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
#define _THRIFT_SERVER_TTHREADEDSERVER_H_ 1

#include <server/TServer.h>
#include <transport/TServerTransport.h>
#include <concurrency/Thread.h>

#include <boost/shared_ptr.hpp>

namespace facebook { namespace thrift { namespace server { 

using facebook::thrift::TProcessor;
using facebook::thrift::transport::TServerTransport;
using facebook::thrift::transport::TTransportFactory;
using facebook::thrift::concurrency::ThreadFactory;

class TThreadedServer : public TServer {

 public:
  class Task;
  
  TThreadedServer(boost::shared_ptr<TProcessor> processor,
                  boost::shared_ptr<TServerTransport> serverTransport,
                  boost::shared_ptr<TTransportFactory> transportFactory,
                  boost::shared_ptr<TProtocolFactory> protocolFactory);

  virtual ~TThreadedServer();

  virtual void serve();

 protected:
  boost::shared_ptr<ThreadFactory> threadFactory_;

};

}}} // facebook::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
