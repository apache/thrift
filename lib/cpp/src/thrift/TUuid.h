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

#ifndef _THRIFT_TUUID_H_
#define _THRIFT_TUUID_H_ 1

#include <thrift/Thrift.h>

#include <algorithm>

namespace apache {
namespace thrift {

/**
 * Thrift wrapper class for a UUID type.
 */
class TUuid {
public:
  typedef uint8_t value_type;
  typedef uint8_t* iterator;
  typedef uint8_t const* const_iterator;
  typedef std::size_t size_type;
  typedef std::ptrdiff_t difference_type;

  TUuid() = default;
  TUuid(const TUuid& other) = default;
  TUuid(TUuid&& other) = default;
  TUuid& operator=(const TUuid&) = default;
  TUuid& operator=(TUuid&&) = default;
  ~TUuid() = default;

  /**
   * Construct the object from the specified string.
   *
   * Supported string formats are:
   *   - "hhhhhhhh-hhhh-hhhh-hhhh-hhhhhhhhhhhh"
   *   - "{hhhhhhhh-hhhh-hhhh-hhhh-hhhhhhhhhhhh}"
   *   - "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
   *   - "{hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh}"
   *
   * If the string is invalid, the object will be set to a
   * nil (empty) UUID.
   */
  explicit TUuid(const std::string& str) noexcept;

  /**
   * Copy assignment from a UUID string.
   *
   * This function will throw an exception if the string is not
   * a valid UUID.
   */
  TUuid& operator=(const std::string& str) noexcept(false);

  /**
   * Check if the UUID is nil.
   */
  bool is_nil() const noexcept;

  /**
   * Compare two TUuid objects for equality.
   */
  inline bool operator==(const TUuid& other) const;

  /**
   * Compare two TUuid objects for inequality.
   */
  inline bool operator!=(const TUuid& other) const;

  iterator begin() noexcept { return data_; }
  const_iterator begin() const noexcept { return data_; }
  iterator end() noexcept { return data_ + size(); }
  const_iterator end() const noexcept { return data_ + size(); }
  size_type size() const noexcept { return 16; }
  inline const_iterator data() const { return data_; }
  inline iterator data() { return data_; }

  void swap(TUuid& other) noexcept { std::swap(data_, other.data_); }

private:
  /**
   * The UUID data.
   */
  uint8_t data_[16] = {};
};

std::string to_string(const TUuid& uuid) noexcept(false);

inline void swap(TUuid& lhs, TUuid& rhs) noexcept {
  lhs.swap(rhs);
}

inline bool TUuid::operator==(const TUuid& other) const {
  // Compare using temporary strings.
  // Can't use strcmp() since we expect embeded zeros
  // Perhaps the reason we should use std::array instead
  return std::string(this->begin(), this->end()) == std::string(other.begin(), other.end());
}

inline bool TUuid::operator!=(const TUuid& other) const {
  return !(*this == other);
}

inline std::ostream& operator<<(std::ostream& out, const TUuid& obj) {
  out << to_string(obj);
  return out;
}

} // namespace thrift
} // namespace apache

#endif // #ifndef _THRIFT_TUUID_H_
