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

#ifdef THRIFT_TUUID_SUPPORT_BOOST_UUID
#include <boost/uuid/uuid.hpp>
#endif // THRIFT_TUUID_SUPPORT_BOOST_UUID

#include <algorithm>

namespace apache {
namespace thrift {

/**
 * Thrift wrapper class for a UUID type.
 *
 * The UUID is stored as a 16 byte buffer.
 * This class stores the UUID in network order when assigned from a string.
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
   * Construct the object from a 16 byte buffer.
   */
  explicit TUuid(const uint8_t (&data)[16]) noexcept
  {
    std::copy(std::begin(data), std::end(data), std::begin(this->data_));
  }

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

#ifdef THRIFT_TUUID_SUPPORT_BOOST_UUID
  /**
   * Construct the TUuid from a boost::uuids::uuid.
   *
   * This constructor will only be available if the <tt>THRIFT_TUUID_SUPPORT_BOOST_UUID</tt>
   * compiler directive is set when this file is included.
   *
   * This constructor is by default implicit. It can be made explicit by defining the
   * <tt>THRIFT_TUUID_BOOST_CONSTRUCTOR_EXPLICIT</tt> compiler directive.
   */
  #ifdef THRIFT_TUUID_BOOST_CONSTRUCTOR_EXPLICIT
  explicit
  #endif // THRIFT_TUUID_BOOST_CONSTRUCTOR_EXPLICIT
  TUuid(const boost::uuids::uuid& buuid) noexcept
  {
    std::copy(std::begin(buuid.data), std::end(buuid.data), std::begin(this->data_));
  }
#endif // THRIFT_TUUID_SUPPORT_BOOST_UUID

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

/**
 * Get the String representation of a TUUID.
 *
 * The format returned is:
 *   - "hhhhhhhh-hhhh-hhhh-hhhh-hhhhhhhhhhhh"
 */
std::string to_string(const TUuid& uuid) noexcept(false);

/**
 * Swap two TUuid objects
 */
inline void swap(TUuid& lhs, TUuid& rhs) noexcept {
  lhs.swap(rhs);
}

/**
 * TUuid equality comparison operator implementation
 */
inline bool TUuid::operator==(const TUuid& other) const {
  // Compare using temporary strings.
  // Can't use strcmp() since we expect embeded zeros
  // Perhaps the reason we should use std::array instead
  return std::string(this->begin(), this->end()) == std::string(other.begin(), other.end());
}

/**
 * TUuid inequality comparison operator implementation
 */
inline bool TUuid::operator!=(const TUuid& other) const {
  return !(*this == other);
}

/**
 * TUuid ostream stream operator implementation
 */
inline std::ostream& operator<<(std::ostream& out, const TUuid& obj) {
  out << to_string(obj);
  return out;
}

} // namespace thrift
} // namespace apache

#endif // #ifndef _THRIFT_TUUID_H_
