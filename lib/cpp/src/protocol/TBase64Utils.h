// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef _THRIFT_PROTOCOL_TBASE64UTILS_H_
#define _THRIFT_PROTOCOL_TBASE64UTILS_H_

#include <string>

namespace apache { namespace thrift { namespace protocol {

// in must be at least len bytes
// len must be 1, 2, or 3
// buf must be a buffer of at least 4 bytes and may not overlap in
// the data is not padded with '='; the caller can do this if desired
void base64_encode(const uint8_t *in, uint32_t len, uint8_t *buf);

// buf must be a buffer of at least 4 bytes and contain base64 encoded values
// buf will be changed to contain output bytes
// len is number of bytes to consume from input (must be 2, 3, or 4)
// no '=' padding should be included in the input
void base64_decode(uint8_t *buf, uint32_t len);

}}} // apache::thrift::protocol

#endif // #define _THRIFT_PROTOCOL_TBASE64UTILS_H_
