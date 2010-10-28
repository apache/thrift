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

#include "TBase64Utils.h"

#include <boost/static_assert.hpp>

using std::string;

namespace apache { namespace thrift { namespace protocol {


static const uint8_t *kBase64EncodeTable = (const uint8_t *)
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void  base64_encode(const uint8_t *in, uint32_t len, uint8_t *buf) {
  buf[0] = kBase64EncodeTable[(in[0] >> 2) & 0x3F];
  if (len == 3) {
    buf[1] = kBase64EncodeTable[((in[0] << 4) + (in[1] >> 4)) & 0x3f];
    buf[2] = kBase64EncodeTable[((in[1] << 2) + (in[2] >> 6)) & 0x3f];
    buf[3] = kBase64EncodeTable[in[2] & 0x3f];
  } else if (len == 2) {
    buf[1] = kBase64EncodeTable[((in[0] << 4) + (in[1] >> 4)) & 0x3f];
    buf[2] = kBase64EncodeTable[(in[1] << 2) & 0x3f];
  } else  { // len == 1
    buf[1] = kBase64EncodeTable[(in[0] << 4) & 0x3f];
  }
}

static const uint8_t kBase64DecodeTable[256] ={
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,62,-1,-1,-1,63,
  52,53,54,55,56,57,58,59,60,61,-1,-1,-1,-1,-1,-1,
  -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,
  15,16,17,18,19,20,21,22,23,24,25,-1,-1,-1,-1,-1,
  -1,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,
  41,42,43,44,45,46,47,48,49,50,51,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
  -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,
};

void base64_decode(uint8_t *buf, uint32_t len) {
  buf[0] = (kBase64DecodeTable[buf[0]] << 2) |
           (kBase64DecodeTable[buf[1]] >> 4);
  if (len > 2) {
    buf[1] = ((kBase64DecodeTable[buf[1]] << 4) & 0xf0) |
              (kBase64DecodeTable[buf[2]] >> 2);
    if (len > 3) {
      buf[2] = ((kBase64DecodeTable[buf[2]] << 6) & 0xc0) |
                (kBase64DecodeTable[buf[3]]);
    }
  }
}


}}} // apache::thrift::protocol
