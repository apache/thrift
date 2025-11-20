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

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace apache {
namespace thrift {

namespace {
static const boost::uuids::string_generator gen;
}

TUuid::TUuid(const std::string& str) noexcept {
  std::fill(this->begin(), this->end(), 0);
  if (str.empty()) {
    return ;
  }

  try {
    const boost::uuids::uuid uuid = gen(str);
    std::copy(uuid.begin(), uuid.end(), this->begin());
  } catch (const std::runtime_error&) {
    // Invalid string most probably
  }
}

bool TUuid::is_nil() const noexcept {
  boost::uuids::uuid uuid_tmp{};
  std::copy(this->begin(), this->end(), std::begin(uuid_tmp));
  return uuid_tmp.is_nil();
}

std::string to_string(const TUuid& in) {
  boost::uuids::uuid uuid_tmp{};
  std::copy(std::begin(in), std::end(in), std::begin(uuid_tmp));
  return boost::uuids::to_string(uuid_tmp);
}


}
} // apache::thrift