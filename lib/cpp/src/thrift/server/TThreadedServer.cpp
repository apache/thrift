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

#include <thrift/concurrency/PlatformThreadFactory.h>
#include <thrift/server/TThreadedServer.h>

namespace apache {
namespace thrift {
namespace server {

using apache::thrift::concurrency::Synchronized;
using apache::thrift::concurrency::Thread;
using apache::thrift::concurrency::ThreadFactory;
using apache::thrift::protocol::TProtocol;
using apache::thrift::protocol::TProtocolFactory;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TTransportFactory;
using boost::shared_ptr;
using std::string;

TThreadedServer::TThreadedServer(const shared_ptr<TProcessorFactory>& processorFactory,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& transportFactory,
                                 const shared_ptr<TProtocolFactory>& protocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TThreadPoolServer(processorFactory, serverTransport, transportFactory, protocolFactory,
          apache::thrift::concurrency::ThreadManager::newSimpleThreadManager(0, 0)) {
    threadManager_->threadFactory(threadFactory);
    threadManager_->start();
}

TThreadedServer::TThreadedServer(const shared_ptr<TProcessor>& processor,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& transportFactory,
                                 const shared_ptr<TProtocolFactory>& protocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TThreadPoolServer(processor, serverTransport, transportFactory, protocolFactory,
          apache::thrift::concurrency::ThreadManager::newSimpleThreadManager(0, 0)) {
    threadManager_->threadFactory(threadFactory);
    threadManager_->start();
}

TThreadedServer::TThreadedServer(const shared_ptr<TProcessorFactory>& processorFactory,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& inputTransportFactory,
                                 const shared_ptr<TTransportFactory>& outputTransportFactory,
                                 const shared_ptr<TProtocolFactory>& inputProtocolFactory,
                                 const shared_ptr<TProtocolFactory>& outputProtocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TThreadPoolServer(processorFactory,
                      serverTransport,
                      inputTransportFactory,
                      outputTransportFactory,
                      inputProtocolFactory,
                      outputProtocolFactory,
                      apache::thrift::concurrency::ThreadManager::newSimpleThreadManager(0, 0)) {
    threadManager_->threadFactory(threadFactory);
    threadManager_->start();
}

TThreadedServer::TThreadedServer(const shared_ptr<TProcessor>& processor,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& inputTransportFactory,
                                 const shared_ptr<TTransportFactory>& outputTransportFactory,
                                 const shared_ptr<TProtocolFactory>& inputProtocolFactory,
                                 const shared_ptr<TProtocolFactory>& outputProtocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TThreadPoolServer(processor,
                      serverTransport,
                      inputTransportFactory,
                      outputTransportFactory,
                      inputProtocolFactory,
                      outputProtocolFactory,
                      apache::thrift::concurrency::ThreadManager::newSimpleThreadManager(0, 0)) {
    threadManager_->threadFactory(threadFactory);
    threadManager_->start();
}

TThreadedServer::~TThreadedServer() {
}

void TThreadedServer::onClientConnected(const shared_ptr<TConnectedClient>& pClient) {
  if (!threadManager_->idleWorkerCount())
  {
    threadManager_->addWorker();
  }

  TThreadPoolServer::onClientConnected(pClient);
}

}
}
} // apache::thrift::server
