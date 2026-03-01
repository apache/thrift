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

#ifndef T_BASE_TYPE_H
#define T_BASE_TYPE_H

#include <cstdlib>
#include "thrift/parse/t_type.h"

/**
 * A thrift base type, which must be one of the defined enumerated types inside
 * this definition.
 *
 */
class t_base_type : public t_type {
public:
  /**
   * Enumeration of thrift base types
   */
  enum t_base {
    TYPE_VOID,
    TYPE_STRING,
    TYPE_UUID,
    TYPE_BOOL,
    TYPE_I8,
    TYPE_I16,
    TYPE_I32,
    TYPE_I64,
    TYPE_DOUBLE
  };

  t_base_type(std::string name, t_base base)
    : t_type(name), base_(base), binary_(false) {}

  t_base get_base() const { return base_; }

  bool is_void() const override { return base_ == TYPE_VOID; }

  bool is_string() const override { return base_ == TYPE_STRING; }

  bool is_bool() const override { return base_ == TYPE_BOOL; }

  bool is_uuid() const override { return base_ == TYPE_UUID; }

  void set_binary(bool val) { binary_ = val; }

  bool is_binary() const override { return binary_ && (base_ == TYPE_STRING); }

  bool is_base_type() const override { return true; }

  static std::string t_base_name(t_base tbase) {
    switch (tbase) {
    case TYPE_VOID:
      return "void";
      break;
    case TYPE_STRING:
      return "string";
      break;
    case TYPE_UUID:
      return "uuid";
      break;
    case TYPE_BOOL:
      return "bool";
      break;
    case TYPE_I8:
      return "i8";
      break;
    case TYPE_I16:
      return "i16";
      break;
    case TYPE_I32:
      return "i32";
      break;
    case TYPE_I64:
      return "i64";
      break;
    case TYPE_DOUBLE:
      return "double";
      break;
    default:
      return "(unknown)";
      break;
    }
  }

private:
  t_base base_;

  bool binary_;
};

#endif
