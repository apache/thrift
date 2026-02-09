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

#ifndef _THRIFT_TPRINTTO_H_
#define _THRIFT_TPRINTTO_H_ 1

#include <map>
#include <set>
#include <vector>

namespace apache {
namespace thrift {

// Generic printTo template - streams value directly to output
template <typename OStream, typename T>
void printTo(OStream& out, const T& t) {
  out << t;
}

// Special handling of i8 datatypes (THRIFT-5272) - cast to int to avoid char output
template <typename OStream>
void printTo(OStream& out, const int8_t& t) {
  out << static_cast<int>(t);
}

// Forward declarations for collection types
template <typename OStream, typename K, typename V>
void printTo(OStream& out, const std::map<K, V>& m);

template <typename OStream, typename T>
void printTo(OStream& out, const std::set<T>& s);

template <typename OStream, typename T>
void printTo(OStream& out, const std::vector<T>& t);

// Pair support
template <typename OStream, typename K, typename V>
void printTo(OStream& out, const std::pair<K, V>& v) {
  printTo(out, v.first);
  out << ": ";
  printTo(out, v.second);
}

// Iterator range support
template <typename OStream, typename Iterator>
void printTo(OStream& out, Iterator beg, Iterator end) {
  for (Iterator it = beg; it != end; ++it) {
    if (it != beg)
      out << ", ";
    printTo(out, *it);
  }
}

// Vector support
template <typename OStream, typename T>
void printTo(OStream& out, const std::vector<T>& t) {
  out << "[";
  printTo(out, t.begin(), t.end());
  out << "]";
}

// Map support
template <typename OStream, typename K, typename V>
void printTo(OStream& out, const std::map<K, V>& m) {
  out << "{";
  printTo(out, m.begin(), m.end());
  out << "}";
}

// Set support
template <typename OStream, typename T>
void printTo(OStream& out, const std::set<T>& s) {
  out << "{";
  printTo(out, s.begin(), s.end());
  out << "}";
}

} // namespace thrift
} // namespace apache

#endif // _THRIFT_TPRINTTO_H_
