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

#ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
#define _THRIFT_SERVER_TSIMPLESERVER_H_ 1

#include <thrift/server/TServer.h>
#include <thrift/transport/TServerTransport.h>

namespace apache { namespace thrift { namespace server {

/**
 * This is the most basic simple server. It is single-threaded and runs a
 * continuous loop of accepting a single connection, processing requests on
 * that connection until it closes, and then repeating. It is a good example
 * of how to extend the TServer interface.
 *
 */
class TSimpleServer : public TServer {
 public:
  template<typename ProcessorFactory>
  TSimpleServer(
      const boost::shared_ptr<ProcessorFactory>& processorFactory,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& transportFactory,
      const boost::shared_ptr<TProtocolFactory>& protocolFactory,
      THRIFT_OVERLOAD_IF(ProcessorFactory, TProcessorFactory)) :
    TServer(processorFactory, serverTransport, transportFactory,
            protocolFactory),
    stop_(false) {}

  template<typename Processor>
  TSimpleServer(
      const boost::shared_ptr<Processor>& processor,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& transportFactory,
      const boost::shared_ptr<TProtocolFactory>& protocolFactory,
      THRIFT_OVERLOAD_IF(Processor, TProcessor)) :
    TServer(processor, serverTransport, transportFactory, protocolFactory),
    stop_(false) {}

  template<typename ProcessorFactory>
  TSimpleServer(
      const boost::shared_ptr<ProcessorFactory>& processorFactory,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& inputTransportFactory,
      const boost::shared_ptr<TTransportFactory>& outputTransportFactory,
      const boost::shared_ptr<TProtocolFactory>& inputProtocolFactory,
      const boost::shared_ptr<TProtocolFactory>& outputProtocolFactory,
      THRIFT_OVERLOAD_IF(ProcessorFactory, TProcessorFactory)) :
    TServer(processorFactory, serverTransport,
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory),
    stop_(false) {}

  template<typename Processor>
  TSimpleServer(
      const boost::shared_ptr<Processor>& processor,
      const boost::shared_ptr<TServerTransport>& serverTransport,
      const boost::shared_ptr<TTransportFactory>& inputTransportFactory,
      const boost::shared_ptr<TTransportFactory>& outputTransportFactory,
      const boost::shared_ptr<TProtocolFactory>& inputProtocolFactory,
      const boost::shared_ptr<TProtocolFactory>& outputProtocolFactory,
      THRIFT_OVERLOAD_IF(Processor, TProcessor)) :
    TServer(processor, serverTransport,
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory),
    stop_(false) {}

  ~TSimpleServer() {}

  void serve();

  void stop() {
    stop_ = true;
    serverTransport_->interrupt();
  }

 protected:
  bool stop_;

};

}}} // apache::thrift::server

#endif // #ifndef _THRIFT_SERVER_TSIMPLESERVER_H_
