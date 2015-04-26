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
#include <thrift/server/TServerFramework.h>

namespace apache {
namespace thrift {
namespace server {

using apache::thrift::transport::TServerTransport;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TTransportFactory;
using apache::thrift::protocol::TProtocol;
using apache::thrift::protocol::TProtocolFactory;
using boost::bind;
using boost::shared_ptr;
using std::string;

TServerFramework::TServerFramework(
        const shared_ptr<TProcessorFactory>& processorFactory,
        const shared_ptr<TServerTransport>& serverTransport,
        const shared_ptr<TTransportFactory>& transportFactory,
        const shared_ptr<TProtocolFactory>& protocolFactory)
  : TServer(processorFactory, serverTransport, transportFactory, protocolFactory) {}

TServerFramework::TServerFramework(
        const shared_ptr<TProcessor>& processor,
        const shared_ptr<TServerTransport>& serverTransport,
        const shared_ptr<TTransportFactory>& transportFactory,
        const shared_ptr<TProtocolFactory>& protocolFactory)
  : TServer(processor, serverTransport, transportFactory, protocolFactory) {}

TServerFramework::TServerFramework(
        const shared_ptr<TProcessorFactory>& processorFactory,
        const shared_ptr<TServerTransport>& serverTransport,
        const shared_ptr<TTransportFactory>& inputTransportFactory,
        const shared_ptr<TTransportFactory>& outputTransportFactory,
        const shared_ptr<TProtocolFactory>& inputProtocolFactory,
        const shared_ptr<TProtocolFactory>& outputProtocolFactory)
  : TServer(processorFactory, serverTransport,
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory) {}

TServerFramework::TServerFramework(
        const shared_ptr<TProcessor>& processor,
        const shared_ptr<TServerTransport>& serverTransport,
        const shared_ptr<TTransportFactory>& inputTransportFactory,
        const shared_ptr<TTransportFactory>& outputTransportFactory,
        const shared_ptr<TProtocolFactory>& inputProtocolFactory,
        const shared_ptr<TProtocolFactory>& outputProtocolFactory)
  : TServer(processor, serverTransport,
            inputTransportFactory, outputTransportFactory,
            inputProtocolFactory, outputProtocolFactory) {}

TServerFramework::~TServerFramework() {}

template<typename T>
static void releaseOneDescriptor(const string& name, T& pTransport) {
  if (pTransport) {
    try {
      pTransport->close();
    } catch (const TTransportException& ttx) {
      string errStr = string("TServerFramework " + name + " close failed: ") + ttx.what();
      GlobalOutput(errStr.c_str());
    }
  }
}

void TServerFramework::serve() {
  shared_ptr<TTransport> client;
  shared_ptr<TTransport> inputTransport;
  shared_ptr<TTransport> outputTransport;
  shared_ptr<TProtocol> inputProtocol;
  shared_ptr<TProtocol> outputProtocol;

  // Start the server listening
  serverTransport_->listen();

  // Run the preServe event to indicate server is now listening
  // and that it is safe to connect.
  if (eventHandler_) {
    eventHandler_->preServe();
  }

  // Fetch client from server
  for (;;) {
    try {
      // Dereference any resources from any previous client creation
      // such that a blocking accept does not hold them indefinitely.
      outputProtocol.reset();
      inputProtocol.reset();
      outputTransport.reset();
      inputTransport.reset();
      client.reset();

      client = serverTransport_->accept();

      inputTransport = inputTransportFactory_->getTransport(client);
      outputTransport = outputTransportFactory_->getTransport(client);
      inputProtocol = inputProtocolFactory_->getProtocol(inputTransport);
      outputProtocol = outputProtocolFactory_->getProtocol(outputTransport);

      onClientConnected(
              shared_ptr<TConnectedClient>(
                      new TConnectedClient(getProcessor(inputProtocol, outputProtocol, client),
                                           inputProtocol, outputProtocol, eventHandler_, client),
                      bind(&TServerFramework::disposeConnectedClient, this, _1)));
    } catch (TTransportException& ttx) {
      releaseOneDescriptor("inputTransport", inputTransport);
      releaseOneDescriptor("outputTransport", outputTransport);
      releaseOneDescriptor("client", client);
      if (ttx.getType() == TTransportException::TIMED_OUT) {
        // Accept timeout - continue processing.
        continue;
      } else if (ttx.getType() == TTransportException::END_OF_FILE ||
                 ttx.getType() == TTransportException::INTERRUPTED) {
        // Server was interrupted.  This only happens when stopping.
        break;
      } else {
        // All other transport exceptions are logged.
        // State of connection is unknown.  Done.
        string errStr = string("TServerTransport died: ") + ttx.what();
        GlobalOutput(errStr.c_str());
        break;
      }
    }
  }

  releaseOneDescriptor("serverTransport", serverTransport_);
}

void TServerFramework::stop() {
  serverTransport_->interrupt();
  serverTransport_->interruptChildren();
}

void TServerFramework::disposeConnectedClient(TConnectedClient *pClient) {
  onClientDisconnected(pClient);
  delete pClient;
}

}
}
} // apache::thrift::server

