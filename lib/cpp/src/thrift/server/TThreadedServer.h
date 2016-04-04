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

#include <thrift/concurrency/PlatformThreadFactory.h>
#include <thrift/concurrency/Thread.h>
#include <thrift/server/TThreadPoolServer.h>

namespace apache {
namespace thrift {
namespace server {

/**
 * Manage clients using threads.  Once the refactoring for THRIFT-3083 took place it became
 * obvious that the differences between the two threaded server types was becoming insignificant.
 * Therefore to satisfy THRIFT-3096 and fix THRIFT-3768, TThreadedServer is simply a wrapper
 * around TThreadedPoolServer now.  If backwards compatibility was not a concern, it would have
 * been removed.
 *
 * The default thread pool size is
 */

/* [[deprecated]] */
class TThreadedServer : public TThreadPoolServer {
public:
  TThreadedServer(
      const boost::shared_ptr<apache::thrift::TProcessorFactory>& processorFactory,
      const boost::shared_ptr<apache::thrift::transport::TServerTransport>& serverTransport,
      const boost::shared_ptr<apache::thrift::transport::TTransportFactory>& transportFactory,
      const boost::shared_ptr<apache::thrift::protocol::TProtocolFactory>& protocolFactory,
      const boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>& threadFactory
      = boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::PlatformThreadFactory));

  TThreadedServer(
      const boost::shared_ptr<apache::thrift::TProcessor>& processor,
      const boost::shared_ptr<apache::thrift::transport::TServerTransport>& serverTransport,
      const boost::shared_ptr<apache::thrift::transport::TTransportFactory>& transportFactory,
      const boost::shared_ptr<apache::thrift::protocol::TProtocolFactory>& protocolFactory,
      const boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>& threadFactory
      = boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::PlatformThreadFactory));

  TThreadedServer(
      const boost::shared_ptr<apache::thrift::TProcessorFactory>& processorFactory,
      const boost::shared_ptr<apache::thrift::transport::TServerTransport>& serverTransport,
      const boost::shared_ptr<apache::thrift::transport::TTransportFactory>& inputTransportFactory,
      const boost::shared_ptr<apache::thrift::transport::TTransportFactory>& outputTransportFactory,
      const boost::shared_ptr<apache::thrift::protocol::TProtocolFactory>& inputProtocolFactory,
      const boost::shared_ptr<apache::thrift::protocol::TProtocolFactory>& outputProtocolFactory,
      const boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>& threadFactory
      = boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::PlatformThreadFactory));

  TThreadedServer(
      const boost::shared_ptr<apache::thrift::TProcessor>& processor,
      const boost::shared_ptr<apache::thrift::transport::TServerTransport>& serverTransport,
      const boost::shared_ptr<apache::thrift::transport::TTransportFactory>& inputTransportFactory,
      const boost::shared_ptr<apache::thrift::transport::TTransportFactory>& outputTransportFactory,
      const boost::shared_ptr<apache::thrift::protocol::TProtocolFactory>& inputProtocolFactory,
      const boost::shared_ptr<apache::thrift::protocol::TProtocolFactory>& outputProtocolFactory,
      const boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>& threadFactory
      = boost::shared_ptr<apache::thrift::concurrency::ThreadFactory>(
          new apache::thrift::concurrency::PlatformThreadFactory));

  virtual ~TThreadedServer();

protected:
  virtual void onClientConnected(const boost::shared_ptr<TConnectedClient>& pClient) /* override */;
};

}
}
} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADEDSERVER_H_
