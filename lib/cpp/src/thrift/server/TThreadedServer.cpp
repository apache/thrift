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

#include <boost/bind.hpp>
#include <boost/function.hpp>
#include <boost/make_shared.hpp>
#include <boost/shared_ptr.hpp>
#include <string>
#include <thrift/concurrency/PlatformThreadFactory.h>
#include <thrift/server/TThreadedServer.h>

namespace apache {
namespace thrift {
namespace server {

using apache::thrift::concurrency::Runnable;
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

TThreadedServer::TThreadedServer(const shared_ptr<TProcessorFactory>& processorFactory,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& transportFactory,
                                 const shared_ptr<TProtocolFactory>& protocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TServerFramework(processorFactory, serverTransport, transportFactory, protocolFactory),
    threadFactory_(threadFactory) {
}

TThreadedServer::TThreadedServer(const shared_ptr<TProcessor>& processor,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& transportFactory,
                                 const shared_ptr<TProtocolFactory>& protocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TServerFramework(processor, serverTransport, transportFactory, protocolFactory),
    threadFactory_(threadFactory) {
}

TThreadedServer::TThreadedServer(const shared_ptr<TProcessorFactory>& processorFactory,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& inputTransportFactory,
                                 const shared_ptr<TTransportFactory>& outputTransportFactory,
                                 const shared_ptr<TProtocolFactory>& inputProtocolFactory,
                                 const shared_ptr<TProtocolFactory>& outputProtocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TServerFramework(processorFactory,
                     serverTransport,
                     inputTransportFactory,
                     outputTransportFactory,
                     inputProtocolFactory,
                     outputProtocolFactory),
    threadFactory_(threadFactory) {
}

TThreadedServer::TThreadedServer(const shared_ptr<TProcessor>& processor,
                                 const shared_ptr<TServerTransport>& serverTransport,
                                 const shared_ptr<TTransportFactory>& inputTransportFactory,
                                 const shared_ptr<TTransportFactory>& outputTransportFactory,
                                 const shared_ptr<TProtocolFactory>& inputProtocolFactory,
                                 const shared_ptr<TProtocolFactory>& outputProtocolFactory,
                                 const shared_ptr<ThreadFactory>& threadFactory)
  : TServerFramework(processor,
                     serverTransport,
                     inputTransportFactory,
                     outputTransportFactory,
                     inputProtocolFactory,
                     outputProtocolFactory),
    threadFactory_(threadFactory) {
}

TThreadedServer::~TThreadedServer() {
}

void TThreadedServer::serve() {
  TServerFramework::serve();

  // Ensure post-condition of no active clients
  Synchronized s(clientMonitor_);
  while (!clientMap_.empty()) {
    clientMonitor_.wait();
  }
}

void TThreadedServer::onClientConnected(const shared_ptr<TConnectedClient>& pClient) {
  Synchronized sync(clientMonitor_);
  clientMap_.insert(ClientMap::value_type(pClient.get(), boost::make_shared<TConnectedClientTracker>(pClient)));

  // We do not track the threads themselves
  ClientMap::const_iterator it = clientMap_.find(pClient.get());
  threadFactory_->newThread(it->second)->start();
}

void TThreadedServer::onClientDisconnected(TConnectedClient* pClient) {
  Synchronized sync(clientMonitor_);
  clientMap_.erase(pClient);
  if (clientMap_.empty()) {
    clientMonitor_.notify();
  }
}

TThreadedServer::TConnectedClientTracker::TConnectedClientTracker(const boost::shared_ptr<TConnectedClient>& pClient)
  : pClient_(pClient) {
}

TThreadedServer::TConnectedClientTracker::~TConnectedClientTracker() {
}

void TThreadedServer::TConnectedClientTracker::run() /* override */ {
  pClient_->run();  // Run the client
  pClient_.reset(); // The client is done - release it here rather than in the destructor for safety
}

}
}
} // apache::thrift::server
