// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
#define _THRIFT_SERVER_TTHREADEDSERVER_H_ 1

#include <server/TServer.h>
#include <transport/TServerTransport.h>
#include <concurrency/Monitor.h>
#include <concurrency/Thread.h>

#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift { namespace server {

using apache::thrift::TProcessor;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TTransportFactory;
using apache::thrift::concurrency::Monitor;
using apache::thrift::concurrency::ThreadFactory;

class TThreadedServer : public TServer {

 public:
  class Task;

  TThreadedServer(boost::shared_ptr<TProcessor> processor,
                  boost::shared_ptr<TServerTransport> serverTransport,
                  boost::shared_ptr<TTransportFactory> transportFactory,
                  boost::shared_ptr<TProtocolFactory> protocolFactory);

  TThreadedServer(boost::shared_ptr<TProcessor> processor,
                  boost::shared_ptr<TServerTransport> serverTransport,
                  boost::shared_ptr<TTransportFactory> transportFactory,
                  boost::shared_ptr<TProtocolFactory> protocolFactory,
                  boost::shared_ptr<ThreadFactory> threadFactory);

  virtual ~TThreadedServer();

  virtual void serve();

  void stop() {
    stop_ = true;
    serverTransport_->interrupt();
  }

 protected:
  boost::shared_ptr<ThreadFactory> threadFactory_;
  volatile bool stop_;

  Monitor tasksMonitor_;
  std::set<Task*> tasks_;

};

}}} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
