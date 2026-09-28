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

#include <thrift/TUuid.h>

#include <algorithm>
#include <string>

namespace apache {
namespace thrift {

namespace {

int hexValue(char c) noexcept {
  if (c >= '0' && c <= '9') {
    return c - '0';
  }
  if (c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  }
  if (c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  }
  return -1;
}

// Accepts 32 hex digits, optionally in braces, and either without dashes or
// with all four of them in the 8-4-4-4-12 positions.
bool parseUuid(const std::string& str, uint8_t (&out)[16]) noexcept {
  std::string::size_type pos = 0;
  std::string::size_type end = str.size();
  if (!str.empty() && str[0] == '{') {
    if (str.size() < 2 || str[end - 1] != '}') {
      return false;
    }
    pos = 1;
    end -= 1;
  }

  bool dashes = false;
  for (int i = 0; i < 16; ++i) {
    if (i == 4) {
      dashes = pos < end && str[pos] == '-';
    }
    if (dashes && (i == 4 || i == 6 || i == 8 || i == 10)) {
      if (pos >= end || str[pos] != '-') {
        return false;
      }
      ++pos;
    }
    if (pos + 2 > end) {
      return false;
    }
    const int high = hexValue(str[pos]);
    const int low = hexValue(str[pos + 1]);
    if (high < 0 || low < 0) {
      return false;
    }
    out[i] = static_cast<uint8_t>((high << 4) | low);
    pos += 2;
  }
  return pos == end;
}

} // namespace

TUuid::TUuid(const std::string& str) noexcept {
  uint8_t parsed[16];
  if (parseUuid(str, parsed)) {
    std::copy(std::begin(parsed), std::end(parsed), this->begin());
  } else {
    std::fill(this->begin(), this->end(), 0);
  }
}

bool TUuid::is_nil() const noexcept {
  return std::all_of(this->begin(), this->end(), [](uint8_t b) { return b == 0; });
}

std::string to_string(const TUuid& in) {
  static const char digits[] = "0123456789abcdef";
  std::string result;
  result.reserve(36);
  int i = 0;
  for (const uint8_t b : in) {
    if (i == 4 || i == 6 || i == 8 || i == 10) {
      result += '-';
    }
    result += digits[b >> 4];
    result += digits[b & 0x0f];
    ++i;
  }
  return result;
}


}
} // apache::thrift