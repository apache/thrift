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

#ifndef _THRIFT_STDCXX_H_
#define _THRIFT_STDCXX_H_ 1

#include <boost/config.hpp>
#include <boost/version.hpp>

///////////////////////////////////////////////////////////////////
//
// functional (function, bind)
//
///////////////////////////////////////////////////////////////////

#include <functional>

namespace apache { namespace thrift { namespace stdcxx {
  using ::std::bind;
  using ::std::function;

  namespace placeholders {
    using ::std::placeholders::_1;
    using ::std::placeholders::_2;
    using ::std::placeholders::_3;
    using ::std::placeholders::_4;
    using ::std::placeholders::_5;
    using ::std::placeholders::_6;
    using ::std::placeholders::_7;
    using ::std::placeholders::_8;
    using ::std::placeholders::_9;
  } // apache::thrift::stdcxx::placeholders
}}} // apache::thrift::stdcxx


///////////////////////////////////////////////////////////////////
//
// Smart Pointers
//
///////////////////////////////////////////////////////////////////

#include <memory>

namespace apache { namespace thrift { namespace stdcxx {

using ::std::const_pointer_cast;
using ::std::dynamic_pointer_cast;
using ::std::enable_shared_from_this;
using ::std::make_shared;
template <typename T> using scoped_ptr = std::unique_ptr<T>;		// compiler must support template aliasing
using ::std::shared_ptr;
using ::std::static_pointer_cast;
using ::std::weak_ptr;


}}} // apache::thrift::stdcxx

#endif // #ifndef _THRIFT_STDCXX_H_
