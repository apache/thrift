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

#ifndef _THRIFT_TPROCESSOR_H_
#define _THRIFT_TPROCESSOR_H_ 1

#include <string>
#include <protocol/TProtocol.h>
#include <boost/shared_ptr.hpp>

namespace apache { namespace thrift {

/**
 * Virtual interface class that can handle events from the processor. To
 * use this you should subclass it and implement the methods that you care
 * about. Your subclass can also store local data that you may care about,
 * such as additional "arguments" to these methods (stored in the object
 * instance's state).
 */
class TProcessorEventHandler {
 public:

  virtual ~TProcessorEventHandler() {}

  /**
   * Called before calling other callback methods.
   * Expected to return some sort of context object.
   * The return value is passed to all other callbacks
   * for that function invocation.
   */
  virtual void* getContext(const char* fn_name, void* serverContext) {
    (void) fn_name;
    (void) serverContext;
    return NULL;
  }

  /**
   * Expected to free resources associated with a context.
   */
  virtual void freeContext(void* ctx, const char* fn_name) {
    (void) ctx;
    (void) fn_name;
  }

  /**
   * Called before reading arguments.
   */
  virtual void preRead(void* ctx, const char* fn_name) {
    (void) ctx;
    (void) fn_name;
  }

  /**
   * Called between reading arguments and calling the handler.
   */
  virtual void postRead(void* ctx, const char* fn_name, uint32_t bytes) {
    (void) ctx;
    (void) fn_name;
    (void) bytes;
  }

  /**
   * Called between calling the handler and writing the response.
   */
  virtual void preWrite(void* ctx, const char* fn_name) {
    (void) ctx;
    (void) fn_name;
  }

  /**
   * Called after writing the response.
   */
  virtual void postWrite(void* ctx, const char* fn_name, uint32_t bytes) {
    (void) ctx;
    (void) fn_name;
    (void) bytes;
  }

  /**
   * Called when an async function call completes successfully.
   */
  virtual void asyncComplete(void* ctx, const char* fn_name) {
    (void) ctx;
    (void) fn_name;
  }

  /**
   * Called if the handler throws an undeclared exception.
   */
  virtual void handlerError(void* ctx, const char* fn_name) {
    (void) ctx;
    (void) fn_name;
  }

 protected:
  TProcessorEventHandler() {}
};

/**
 * A helper class used by the generated code to free each context.
 */
class TProcessorContextFreer {
 public:
  TProcessorContextFreer(TProcessorEventHandler* handler, void* context, const char* method) :
    handler_(handler), context_(context), method_(method) {}
  ~TProcessorContextFreer() { if (handler_ != NULL) handler_->freeContext(context_, method_); }
  void unregister() { handler_ = NULL; }
 private:
  apache::thrift::TProcessorEventHandler* handler_;
  void* context_;
  const char* method_;
};

/**
 * A processor is a generic object that acts upon two streams of data, one
 * an input and the other an output. The definition of this object is loose,
 * though the typical case is for some sort of server that either generates
 * responses to an input stream or forwards data from one pipe onto another.
 *
 */
class TProcessor {
 public:
  virtual ~TProcessor() {}

  virtual bool process(boost::shared_ptr<protocol::TProtocol> in,
                       boost::shared_ptr<protocol::TProtocol> out,
                       void* connectionContext) = 0;

  bool process(boost::shared_ptr<apache::thrift::protocol::TProtocol> io,
               void* connectionContext) {
    return process(io, io, connectionContext);
  }

  boost::shared_ptr<TProcessorEventHandler> getEventHandler() {
    return eventHandler_;
  }

  void setEventHandler(boost::shared_ptr<TProcessorEventHandler> eventHandler) {
    eventHandler_ = eventHandler;
  }

 protected:
  TProcessor() {}

  boost::shared_ptr<TProcessorEventHandler> eventHandler_;
};

}} // apache::thrift

#endif // #ifndef _THRIFT_TPROCESSOR_H_
