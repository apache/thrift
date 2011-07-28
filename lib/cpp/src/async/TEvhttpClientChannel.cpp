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

#include "TEvhttpClientChannel.h"
#include <evhttp.h>
#include "transport/TBufferTransports.h"

namespace apache { namespace thrift { namespace async {


TEvhttpClientChannel::TEvhttpClientChannel(
    const std::string& host,
    const std::string& path,
    const char* address,
    int port,
    struct event_base* eb)
  : host_(host)
  , path_(path)
  , recvBuf_(NULL)
  , conn_(NULL)
{
  conn_ = evhttp_connection_new(address, port);
  if (conn_ == NULL) {
    abort(); // XXX
  }
  evhttp_connection_set_base(conn_, eb);
}


TEvhttpClientChannel::~TEvhttpClientChannel() {
  if (conn_ != NULL) {
    evhttp_connection_free(conn_);
  }
}


void TEvhttpClientChannel::sendAndRecvMessage(
    const VoidCallback& cob,
    apache::thrift::transport::TMemoryBuffer* sendBuf,
    apache::thrift::transport::TMemoryBuffer* recvBuf) {
  cob_ = cob;
  recvBuf_ = recvBuf;

  struct evhttp_request* req = evhttp_request_new(response, this);
  if (req == NULL) {
    abort(); // XXX
  }

  int rv;

  rv = evhttp_add_header(req->output_headers, "Host", host_.c_str());
  if (rv != 0) {
    abort(); // XXX
  }

  rv = evhttp_add_header(req->output_headers, "Content-Type", "application/x-thrift");
  if (rv != 0) {
    abort(); // XXX
  }

  uint8_t* obuf;
  uint32_t sz;
  sendBuf->getBuffer(&obuf, &sz);
  rv = evbuffer_add(req->output_buffer, obuf, sz);
  if (rv != 0) {
    abort(); // XXX
  }

  rv = evhttp_make_request(conn_, req, EVHTTP_REQ_POST, path_.c_str());
  if (rv != 0) {
    abort(); // XXX
  }
}


void TEvhttpClientChannel::sendMessage(
    const VoidCallback& cob, apache::thrift::transport::TMemoryBuffer* message) {
  (void) cob;
  (void) message;
  abort(); // XXX
}


void TEvhttpClientChannel::recvMessage(
    const VoidCallback& cob, apache::thrift::transport::TMemoryBuffer* message) {
  (void) cob;
  (void) message;
  abort(); // XXX
}


void TEvhttpClientChannel::finish(struct evhttp_request* req) {
  if (req == NULL) {
    cob_();
	return;
  } else if (req->response_code != 200) {
    cob_();
	return;
  }
  recvBuf_->resetBuffer(
      EVBUFFER_DATA(req->input_buffer),
      EVBUFFER_LENGTH(req->input_buffer));
  cob_();
  return;
}


/* static */ void TEvhttpClientChannel::response(struct evhttp_request* req, void* arg) {
  TEvhttpClientChannel* self = (TEvhttpClientChannel*)arg;
  self->finish(req);
}


}}} // apache::thrift::async
