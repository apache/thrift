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

#ifndef _THRIFT_TLIST_H_
#define _THRIFT_TLIST_H_

#include <thrift/protocol/TEnum.h>

namespace apache {
namespace thrift {
namespace protocol {

// using namespace apache::thrift::protocol;

/**
 * Helper class that encapsulates list metadata.
 *
 */
class TList {
public:
  TList() : elemType_(T_STOP), 
  size_(0) {

  }

  TList(TType t = T_STOP, int s = 0)
    : elemType_(t),
      size_(s) { 
        
  }

  TType elemType_;
  int  size_;
};
}
}
} // apache::thrift::protocol

#endif // #ifndef _THRIFT_TLIST_H_
