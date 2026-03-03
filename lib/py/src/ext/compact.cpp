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

#define PY_SSIZE_T_CLEAN
#include "ext/compact.h"

namespace apache {
namespace thrift {
namespace py {

/** Mapping of Compact type to Thrift Type according.
 * This list must match the TType enum in TEnum.h */
const uint8_t CompactProtocol::TTypeToCType[] = {
/*  0 */    CT_STOP,         // T_STOP
/*  1 */    0,               // unused
/*  2 */    CT_BOOLEAN_TRUE, // T_BOOL
/*  3 */    CT_BYTE,         // T_BYTE
/*  4 */    CT_DOUBLE,       // T_DOUBLE
/*  5 */    0,               // unused
/*  6 */    CT_I16,          // T_I16
/*  7 */    0,               // unused
/*  8 */    CT_I32,          // T_I32
/*  9 */    0,               // unused
/* 10 */    CT_I64,          // T_I64
/* 11 */    CT_BINARY,       // T_STRING
/* 12 */    CT_STRUCT,       // T_STRUCT
/* 13 */    CT_MAP,          // T_MAP
/* 14 */    CT_SET,          // T_SET
/* 15 */    CT_LIST,         // T_LIST
/* 16 */    CT_UUID,         // T_UUID
};

bool CompactProtocol::readFieldBegin(TType& type, int16_t& tag) {
  uint8_t b;
  if (!readByte(b)) {
    return false;
  }
  uint8_t ctype = b & 0xf;
  type = getTType(ctype);
  if (type == -1) {
    return false;
  } else if (type == T_STOP) {
    tag = 0;
    return true;
  }
  uint8_t diff = (b & 0xf0) >> 4;
  if (diff) {
    tag = readTags_.top() + diff;
  } else if (!readI16(tag)) {
    readTags_.top() = -1;
    return false;
  }
  if (ctype == CT_BOOLEAN_FALSE || ctype == CT_BOOLEAN_TRUE) {
    readBool_.exists = true;
    readBool_.value = ctype == CT_BOOLEAN_TRUE;
  }
  readTags_.top() = tag;
  return true;
}

TType CompactProtocol::getTType(uint8_t type) {
  switch (type) {
  case T_STOP:
    return T_STOP;
  case CT_BOOLEAN_FALSE:
  case CT_BOOLEAN_TRUE:
    return T_BOOL;
  case CT_BYTE:
    return T_BYTE;
  case CT_I16:
    return T_I16;
  case CT_I32:
    return T_I32;
  case CT_I64:
    return T_I64;
  case CT_DOUBLE:
    return T_DOUBLE;
  case CT_BINARY:
    return T_STRING;
  case CT_LIST:
    return T_LIST;
  case CT_SET:
    return T_SET;
  case CT_MAP:
    return T_MAP;
  case CT_STRUCT:
    return T_STRUCT;
  case CT_UUID:
    return T_UUID;
  default:
    PyErr_Format(PyExc_TypeError, "don't know what type: %d", type);
    return static_cast<TType>(-1);
  }
}
}
}
}
