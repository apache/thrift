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

#ifndef _THRIFT_PROTOCOL_TUUIDUTILS_H_
#define _THRIFT_PROTOCOL_TUUIDUTILS_H_

#include <string>

namespace apache {
namespace thrift {
namespace protocol {

// Encode canonical UUID string to a 16 char representation
// Supported formats for in:
//   - "hhhhhhhh-hhhh-hhhh-hhhh-hhhhhhhhhhhh"
//   - "{hhhhhhhh-hhhh-hhhh-hhhh-hhhhhhhhhhhh}"
//   - "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh"
//   - "{hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh}"
// Returns false if the string was invalid and the value was not encoded.
bool uuid_encode(const std::string& in, std::string& out);

// Decode 16 char UUID buffer to 36 characted string
void uuid_decode(const std::string& in, std::string& out);

}
}
} // apache::thrift::protocol

#endif // #define _THRIFT_PROTOCOL_TUUIDUTILS_H_
