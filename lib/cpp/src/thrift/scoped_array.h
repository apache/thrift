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

#ifndef _THRIFT_SCOPED_ARRAY_H_
#define _THRIFT_SCOPED_ARRAY_H_ 1

// Boost is a general requirement on most platforms
#ifdef __ZEPHYR__
#undef HAVE_BOOST_SCOPED_ARRAY_H
#else
#define HAVE_BOOST_SCOPED_ARRAY_H 1
#endif

#ifdef HAVE_BOOST_SCOPED_ARRAY_H
#include <boost/scoped_array.hpp>
#else
#include <algorithm>
#endif

namespace apache {
namespace thrift {

#ifdef HAVE_BOOST_SCOPED_ARRAY_H
template<class T>
using scoped_array = boost::scoped_array<T>;
#else
/**
 * @brief Scoped array abstraction
 *
 * This is an abstraction for `boost::scoped_array` for systems
 * that do not include boost.
 * 
 * @tparam T element type
 */
template<class T> class scoped_array {
public:
  typedef T element_type;

  explicit scoped_array(T* p = nullptr) : _p(p) {}
  ~scoped_array() {}

  void reset(T* p = nullptr)
  {
    _p = p;
  }

  T& operator[](std::ptrdiff_t i) const
  {
    return _p[i];
  }

  T * get() const {
    return _p;
  }
  
  operator bool() const
  {
    return get() != nullptr;
  }

  void swap(scoped_array& other)
  {
    std::swap(other._p, _p);
  }
  
protected:
  T* _p;
};
#endif
}
} // apache::thrift

#endif /* _THRIFT_SCOPED_ARRAY_H_ */
