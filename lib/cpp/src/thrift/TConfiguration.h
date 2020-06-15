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

#ifndef THRIFT_TCONFIGURATION_H
#define THRIFT_TCONFIGURATION_H

namespace apache {
namespace thrift {

class TConfiguration
{
public:
  TConfiguration(int maxMessageSize = DEFAULT_MAX_MESSAGE_SIZE, 
                int maxFrameSize = DEFAULT_MAX_FRAME_SIZE, int recursionLimit = DEFAULT_RECURSION_DEPTH)
    : maxMessageSize_(maxMessageSize), maxFrameSize_(maxFrameSize), recursionLimit_(recursionLimit) {}

  const static int DEFAULT_MAX_MESSAGE_SIZE = 100 * 1024 * 1024;
  const static int DEFAULT_MAX_FRAME_SIZE = 16384000;      // this value is used consistently across all Thrift libraries
  const static int DEFAULT_RECURSION_DEPTH = 64;

  inline int  getMaxMessageSize() { return maxMessageSize_; }
  inline void setMaxMessageSize(int maxMessageSize) { maxMessageSize_ = maxMessageSize; } 
  inline int getMaxFrameSize() { return maxFrameSize_; }
  inline void setMaxFrameSize(int maxFrameSize) { maxFrameSize_ = maxFrameSize; }
  inline int getRecursionLimit() { return recursionLimit_; }
  inline void setRecursionLimit(int recursionLimit) { recursionLimit_ = recursionLimit; }

private:
  int maxMessageSize_ = DEFAULT_MAX_MESSAGE_SIZE;
  int maxFrameSize_ = DEFAULT_MAX_FRAME_SIZE;
  int recursionLimit_ = DEFAULT_RECURSION_DEPTH;

  // TODO(someone_smart): add connection and i/o timeouts
};
}
} // apache::thrift

#endif /* THRIFT_TCONFIGURATION_H */

