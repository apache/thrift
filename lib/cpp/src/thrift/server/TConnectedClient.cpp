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

#include <thrift/server/TConnectedClient.h>

namespace apache { namespace thrift { namespace server {

using apache::thrift::TProcessor;
using apache::thrift::protocol::TProtocol;
using apache::thrift::server::TServerEventHandler;
using apache::thrift::transport::TTransport;
using apache::thrift::transport::TTransportException;
using boost::shared_ptr;
using std::string;

TConnectedClient::TConnectedClient(const string& serverType,
                                   shared_ptr<TProcessor> processor,
                                   shared_ptr<TProtocol> inputProtocol,
                                   shared_ptr<TProtocol> outputProtocol,
                                   shared_ptr<TServerEventHandler> eventHandler,
                                   shared_ptr<TTransport> client)
                        
  : serverType_(serverType),
    processor_(processor),
    inputProtocol_(inputProtocol),
    outputProtocol_(outputProtocol),
    eventHandler_(eventHandler),
    client_(client),
    opaqueContext_(0)
{}

TConnectedClient::~TConnectedClient()
{}

void TConnectedClient::run()
{
  if (eventHandler_) {
    opaqueContext_ = eventHandler_->createContext(inputProtocol_, outputProtocol_);
  }

  for (;;) {
    if (eventHandler_) {
      eventHandler_->processContext(opaqueContext_, client_);
    }

    try {
      if (!processor_->process(inputProtocol_, outputProtocol_, opaqueContext_)) {
        break;
      }
    } catch (const TTransportException& ttx) {
      if (ttx.getType() != TTransportException::END_OF_FILE &&
          ttx.getType() != TTransportException::INTERRUPTED) {
            string errStr = (serverType_ + " client died: ") + ttx.what();
            GlobalOutput(errStr.c_str());
      }
      break;
    }
  }

  cleanup();
}

void TConnectedClient::cleanup()
{
  if (eventHandler_) {
    eventHandler_->deleteContext(opaqueContext_, inputProtocol_, outputProtocol_);
  }

  try {
    inputProtocol_->getTransport()->close();
  } catch (const TTransportException& ttx) {
    string errStr = string(serverType_ + " input close failed: ") + ttx.what();
    GlobalOutput(errStr.c_str());
  }
  try {
    outputProtocol_->getTransport()->close();
  } catch (const TTransportException& ttx) {
    string errStr = string(serverType_ + " output close failed: ") + ttx.what();
    GlobalOutput(errStr.c_str());
  }
  try {
    client_->close();
  } catch (const TTransportException& ttx) {
    string errStr = string(serverType_ + " client close failed: ") + ttx.what();
    GlobalOutput(errStr.c_str());
  }
}

}}} // apache::thrift::server
