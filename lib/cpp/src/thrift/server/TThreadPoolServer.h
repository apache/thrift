/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#ifndef _THRIFT_SERVER_TTHREADPOOLSERVER_H_
#define _THRIFT_SERVER_TTHREADPOOLSERVER_H_ 1

#include <thrift/concurrency/ThreadManager.h>
#include <thrift/server/TServer.h>
#include <thrift/transport/TServerTransport.h>

#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift { namespace server {

using apache::thrift::concurrency::ThreadManager;
using apache::thrift::protocol::TProtocolFactory;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TTransportFactory;

class TThreadPoolServer : public TServer {
 public:
  class Task;

  template<typename ProcessorFactory>
  TThreadPoolServer(
      const boost::shared_ptr<ProcessorFactory>& processorFactory,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& transportFactory,
      const boost::shared_ptr<TProtocolFactory>& protocolFactory,
      const boost::shared_ptr<ThreadManager>& threadManager,
      THRIFT_OVERLOAD_IF(ProcessorFactory, TProcessorFactory)) :
    TServer(processorFactory, serverTransport, transportFactory,
            protocolFactory),
    threadManager_(threadManager),
    stop_(false),
    timeout_(0),
    taskExpiration_(0) {}

  template<typename Processor>
  TThreadPoolServer(
      const boost::shared_ptr<Processor>& processor,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& transportFactory,
      const boost::shared_ptr<TProtocolFactory>& protocolFactory,
      const boost::shared_ptr<ThreadManager>& threadManager,
      THRIFT_OVERLOAD_IF(Processor, TProcessor)) :
    TServer(processor, serverTransport, transportFactory, protocolFactory),
    threadManager_(threadManager),
    stop_(false),
    timeout_(0),
    taskExpiration_(0) {}

  template<typename ProcessorFactory>
  TThreadPoolServer(
      const boost::shared_ptr<ProcessorFactory>& processorFactory,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& inputTransportFactory,
      const boost::shared_ptr<TTransportFactory>& outputTransportFactory,
      const boost::shared_ptr<TProtocolFactory>& inputProtocolFactory,
      const boost::shared_ptr<TProtocolFactory>& outputProtocolFactory,
      const boost::shared_ptr<ThreadManager>& threadManager,
      THRIFT_OVERLOAD_IF(ProcessorFactory, TProcessorFactory)) :
    TServer(processorFactory, serverTransport,
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory),
    threadManager_(threadManager),
    stop_(false),
    timeout_(0),
    taskExpiration_(0) {}

  template<typename Processor>
  TThreadPoolServer(
      const boost::shared_ptr<Processor>& processor,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& inputTransportFactory,
      const boost::shared_ptr<TTransportFactory>& outputTransportFactory,
      const boost::shared_ptr<TProtocolFactory>& inputProtocolFactory,
      const boost::shared_ptr<TProtocolFactory>& outputProtocolFactory,
      const boost::shared_ptr<ThreadManager>& threadManager,
      THRIFT_OVERLOAD_IF(Processor, TProcessor)) :
    TServer(processor, serverTransport,
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory),
    threadManager_(threadManager),
    stop_(false),
    timeout_(0),
    taskExpiration_(0) {}

  virtual ~TThreadPoolServer();

  virtual void serve();

  virtual int64_t getTimeout() const;

  virtual void setTimeout(int64_t value);

  virtual void stop() {
    stop_ = true;
    serverTransport_->interrupt();
  }

  virtual int64_t getTaskExpiration() const;

  virtual void setTaskExpiration(int64_t value);

 protected:

  boost::shared_ptr<ThreadManager> threadManager_;

  volatile bool stop_;

  volatile int64_t timeout_;

  volatile int64_t taskExpiration_;

};

}}} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADPOOLSERVER_H_
