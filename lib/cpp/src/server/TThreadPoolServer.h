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

#include <concurrency/ThreadManager.h>
#include <server/TServer.h>
#include <transport/TServerTransport.h>

#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift { namespace server {

using apache::thrift::concurrency::ThreadManager;
using apache::thrift::protocol::TProtocolFactory;
using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TTransportFactory;

class TThreadPoolServer : public TServer {
 public:
  class Task;

  TThreadPoolServer(boost::shared_ptr<TProcessor> processor,
                    boost::shared_ptr<TServerTransport> serverTransport,
                    boost::shared_ptr<TTransportFactory> transportFactory,
                    boost::shared_ptr<TProtocolFactory> protocolFactory,
                    boost::shared_ptr<ThreadManager> threadManager);

  TThreadPoolServer(boost::shared_ptr<TProcessor> processor,
                    boost::shared_ptr<TServerTransport> serverTransport,
                    boost::shared_ptr<TTransportFactory> inputTransportFactory,
                    boost::shared_ptr<TTransportFactory> outputTransportFactory,
                    boost::shared_ptr<TProtocolFactory> inputProtocolFactory,
                    boost::shared_ptr<TProtocolFactory> outputProtocolFactory,
                    boost::shared_ptr<ThreadManager> threadManager);

  virtual ~TThreadPoolServer();

  virtual void serve();

  virtual int64_t getTimeout() const;

  virtual void setTimeout(int64_t value);

  virtual void stop() {
    stop_ = true;
    serverTransport_->interrupt();
  }

 protected:

  boost::shared_ptr<ThreadManager> threadManager_;

  volatile bool stop_;

  volatile int64_t timeout_;

};

}}} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TTHREADPOOLSERVER_H_
