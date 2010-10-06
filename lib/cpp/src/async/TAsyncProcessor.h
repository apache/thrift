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

#ifndef _THRIFT_TASYNCPROCESSOR_H_
#define _THRIFT_TASYNCPROCESSOR_H_ 1

#include <tr1/functional>
#include <boost/shared_ptr.hpp>
#include <protocol/TProtocol.h>
#include <TProcessor.h>

namespace apache { namespace thrift { namespace async {

/**
 * Async version of a TProcessor.  It is not expected to complete by the time
 * the call to process returns.  Instead, it calls a cob to signal completion.
 */

class TEventServer; // forward declaration

class TAsyncProcessor {
 public:
  virtual ~TAsyncProcessor() {}

  virtual void process(std::tr1::function<void(bool success)> _return,
                       boost::shared_ptr<protocol::TProtocol> in,
                       boost::shared_ptr<protocol::TProtocol> out) = 0;

  void process(std::tr1::function<void(bool success)> _return,
               boost::shared_ptr<apache::thrift::protocol::TProtocol> io) {
    return process(_return, io, io);
  }

  boost::shared_ptr<TProcessorEventHandler> getEventHandler() {
    return eventHandler_;
  }

  void setEventHandler(boost::shared_ptr<TProcessorEventHandler> eventHandler) {
    eventHandler_ = eventHandler;
  }

  const TEventServer* getAsyncServer() {
    return asyncServer_;
  }
 protected:
  TAsyncProcessor() {}

  boost::shared_ptr<TProcessorEventHandler> eventHandler_;
  const TEventServer* asyncServer_;
 private:
  friend class TEventServer;
  void setAsyncServer(const TEventServer* server) {
    asyncServer_ = server;
  }
};

}}} // apache::thrift::async

// XXX I'm lazy for now
namespace apache { namespace thrift {
using apache::thrift::async::TAsyncProcessor;
}}

#endif // #ifndef _THRIFT_TASYNCPROCESSOR_H_
