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

#ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
#define _THRIFT_SERVER_TTHREADEDSERVER_H_ 1

#include <boost/shared_ptr.hpp>
#include <set>
#include <thrift/TProcessor.h>
#include <thrift/concurrency/Monitor.h>
#include <thrift/concurrency/PlatformThreadFactory.h>
#include <thrift/concurrency/Thread.h>
#include <thrift/protocol/TProtocol.h>
#include <thrift/server/TServer.h>
#include <thrift/transport/TServerTransport.h>
#include <thrift/transport/TTransport.h>

namespace apache {
namespace thrift {
namespace server {

class TConnectedClient;

class TThreadedServer : public TServer {
public:
  template <typename ProcessorFactory>
  TThreadedServer(const boost::shared_ptr<ProcessorFactory>& processorFactory,
                  const boost::shared_ptr<transport::TServerTransport>& serverTransport,
                  const boost::shared_ptr<transport::TTransportFactory>& transportFactory,
                  const boost::shared_ptr<protocol::TProtocolFactory>& protocolFactory,
                  THRIFT_OVERLOAD_IF(ProcessorFactory, TProcessorFactory))
    : TServer(processorFactory, serverTransport, transportFactory, protocolFactory),
      threadFactory_(new concurrency::PlatformThreadFactory),
      stop_(false) {}

  template <typename ProcessorFactory>
  TThreadedServer(const boost::shared_ptr<ProcessorFactory>& processorFactory,
                  const boost::shared_ptr<transport::TServerTransport>& serverTransport,
                  const boost::shared_ptr<transport::TTransportFactory>& transportFactory,
                  const boost::shared_ptr<protocol::TProtocolFactory>& protocolFactory,
                  const boost::shared_ptr<concurrency::ThreadFactory>& threadFactory,
                  THRIFT_OVERLOAD_IF(ProcessorFactory, TProcessorFactory))
    : TServer(processorFactory, serverTransport, transportFactory, protocolFactory),
      threadFactory_(threadFactory),
      stop_(false) {}

  template <typename Processor>
  TThreadedServer(const boost::shared_ptr<Processor>& processor,
                  const boost::shared_ptr<transport::TServerTransport>& serverTransport,
                  const boost::shared_ptr<transport::TTransportFactory>& transportFactory,
                  const boost::shared_ptr<protocol::TProtocolFactory>& protocolFactory,
                  THRIFT_OVERLOAD_IF(Processor, TProcessor))
    : TServer(processor, serverTransport, transportFactory, protocolFactory),
      threadFactory_(new concurrency::PlatformThreadFactory),
      stop_(false) {}

  template <typename Processor>
  TThreadedServer(const boost::shared_ptr<Processor>& processor,
                  const boost::shared_ptr<transport::TServerTransport>& serverTransport,
                  const boost::shared_ptr<transport::TTransportFactory>& transportFactory,
                  const boost::shared_ptr<protocol::TProtocolFactory>& protocolFactory,
                  const boost::shared_ptr<concurrency::ThreadFactory>& threadFactory,
                  THRIFT_OVERLOAD_IF(Processor, TProcessor))
    : TServer(processor, serverTransport, transportFactory, protocolFactory),
      threadFactory_(threadFactory),
      stop_(false) {}

  virtual ~TThreadedServer() {};

  virtual void serve();

  virtual void stop() {
    stop_ = true;
    serverTransport_->interrupt();
  }

protected:
  virtual void disposeClient(TConnectedClient *pClient);

  boost::shared_ptr<concurrency::ThreadFactory> threadFactory_;
  volatile bool stop_;

  concurrency::Monitor clientsMonitor_;
  std::set<TConnectedClient*> clients_;
};

}
}
} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
