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

#ifndef T_CONST_VALUE_H
#define T_CONST_VALUE_H

#include "t_const.h"
#include <stdint.h>
#include <map>
#include <vector>

/**
 * A const value is something parsed that could be a map, set, list, struct
 * or whatever.
 *
 */
class t_const_value {
 public:

  enum t_const_value_type {
    CV_INTEGER,
    CV_DOUBLE,
    CV_STRING,
    CV_MAP,
    CV_LIST
  };

  t_const_value() {}

  t_const_value(int64_t val) {
    set_integer(val);
  }

  t_const_value(std::string val) {
    set_string(val);
  }

  void set_string(std::string val) {
    valType_ = CV_STRING;
    stringVal_ = val;
  }

  std::string get_string() const {
    return stringVal_;
  }

  void set_integer(int64_t val) {
    valType_ = CV_INTEGER;
    intVal_ = val;
  }

  int64_t get_integer() const {
    return intVal_;
  }

  void set_double(double val) {
    valType_ = CV_DOUBLE;
    doubleVal_ = val;
  }

  double get_double() const {
    return doubleVal_;
  }

  void set_map() {
    valType_ = CV_MAP;
  }

  void add_map(t_const_value* key, t_const_value* val) {
    mapVal_[key] = val;
  }

  const std::map<t_const_value*, t_const_value*>& get_map() const {
    return mapVal_;
  }

  void set_list() {
    valType_ = CV_LIST;
  }

  void add_list(t_const_value* val) {
    listVal_.push_back(val);
  }

  const std::vector<t_const_value*>& get_list() const {
    return listVal_;
  }

  t_const_value_type get_type() const {
    return valType_;
  }

 private:
  std::map<t_const_value*, t_const_value*> mapVal_;
  std::vector<t_const_value*> listVal_;
  std::string stringVal_;
  int64_t intVal_;
  double doubleVal_;

  t_const_value_type valType_;

};

#endif

