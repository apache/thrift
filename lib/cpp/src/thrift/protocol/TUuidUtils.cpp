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

#include <thrift/protocol/TUuidUtils.hpp>

#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace apache {
namespace thrift {
namespace protocol {

bool uuid_encode(const std::string& in, std::string& out) {
  static const boost::uuids::string_generator gen;
  static const std::string empty_uuid(boost::uuids::uuid::static_size(), '\0');
  out = empty_uuid;
  if (in.empty()) {
    return true;
  }
  try {
    const boost::uuids::uuid uuid{gen(in)};
    std::copy(uuid.begin(), uuid.end(), out.begin());
    return true;
  } catch (const std::runtime_error&) {
    // Invalid string most probably
    return false;
  }
}

void uuid_decode(const std::string& in, std::string& out) {
  boost::uuids::uuid uuid{};
  const size_t to_copy = std::min(in.size(), uuid.size());
  std::copy(in.begin(), in.begin() + to_copy, uuid.begin());
  out = boost::uuids::to_string(uuid);
}

}
}
} // apache::thrift::protocol
