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

#ifndef THRIFT_NUMERIC_CAST_H
#define THRIFT_NUMERIC_CAST_H

#include <limits>
#include <stdexcept>

#if defined(_MSC_VER)
// avoid compiler warnings and errors in MSVC if max is defined as a macro
#undef max
#endif

namespace apache {
namespace thrift {

/**
 * @brief Perform a safe numeric cast
 *
 * Previously this was provided by `boost::numeric_cast`. This
 * implementation reduces the dependency on `boost`.
 *
 * @tparam Dst The destination type
 * @tparam Src The source type
 * @param value The value to be converted
 * @return Dst The converted value
 *
 * @see <a href="https://stackoverflow.com/a/49658950/5636218">SA49658182</a>
 */
template <typename Dst, typename Src>
inline Dst numeric_cast(Src value) {
  typedef std::numeric_limits<Dst> DstLim;
  typedef std::numeric_limits<Src> SrcLim;

  const bool positive_overflow_possible = DstLim::max() < SrcLim::max();
  const bool negative_overflow_possible = DstLim::lowest() > SrcLim::lowest();

  if (positive_overflow_possible && value > DstLim::max()) {
    throw std::bad_cast();
  }

  if (negative_overflow_possible && (value < DstLim::lowest())) {
    throw std::bad_cast();
  }

  // limits have been checked, therefore safe to cast
  return static_cast<Dst>(value);
}

} // namespace thrift
} // namespace apache

#endif
