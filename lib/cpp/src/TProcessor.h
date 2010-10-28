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
                       boost::shared_ptr<protocol::TProtocol> out) = 0;

  bool process(boost::shared_ptr<apache::thrift::protocol::TProtocol> io) {
    return process(io, io);
  }

 protected:
  TProcessor() {}
};

}} // apache::thrift

#endif // #ifndef _THRIFT_PROCESSOR_H_
