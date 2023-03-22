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

#ifndef T_VALIDATOR_GENERATOR_H
#define T_VALIDATOR_GENERATOR_H

#include "thrift/generate/t_generator.h"
#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <vector>

class validation_value {
public:
  struct validation_function {
  public:
    std::string name;
    std::vector<validation_value*> arguments;
  };

  enum validation_value_type {
    VV_INTEGER,
    VV_DOUBLE,
    VV_BOOL,
    VV_ENUM,
    VV_STRING,
    VV_FUNCTION,
    VV_FIELD_REFERENCE,
    VV_UNKNOWN
  };

  validation_value() : val_type(VV_UNKNOWN) {}
  validation_value(const int64_t val) : int_val(val), val_type(VV_INTEGER) {}
  validation_value(const double val) : double_val(val), val_type(VV_DOUBLE) {}
  validation_value(const bool val) : bool_val(val), val_type(VV_BOOL) {}
  validation_value(t_enum_value* val) : enum_val(val), val_type(VV_ENUM) {}
  validation_value(const std::string val) : string_val(val), val_type(VV_STRING) {}
  validation_value(validation_function* val) : function_val(val), val_type(VV_FUNCTION) {}
  validation_value(t_field* val) : field_reference_val(val), val_type(VV_FIELD_REFERENCE) {}

  void set_int(const int64_t val) {
    int_val = val;
    val_type = VV_INTEGER;
  }
  int64_t get_int() const { return int_val; };

  void set_double(const double val) {
    double_val = val;
    val_type = VV_DOUBLE;
  }
  double get_double() { return double_val; };

  void set_bool(const bool val) {
    bool_val = val;
    val_type = VV_BOOL;
  }
  bool get_bool() const { return bool_val; };

  void set_enum(t_enum_value* val) {
    enum_val = val;
    val_type = VV_ENUM;
  }
  t_enum_value* get_enum() const { return enum_val; };

  void set_string(const std::string val) {
    string_val = val;
    val_type = VV_STRING;
  }
  std::string get_string() const { return string_val; };

  void set_function(validation_function* val) {
    function_val = val;
    val_type = VV_FUNCTION;
  }

  validation_function* get_function() { return function_val; };

  void set_field_reference(t_field* val) {
    field_reference_val = val;
    val_type = VV_FIELD_REFERENCE;
  }
  t_field* get_field_reference() const { return field_reference_val; };

  bool is_field_reference() const { return val_type == VV_FIELD_REFERENCE; };

  bool is_validation_function() const { return val_type == VV_FUNCTION; };

  validation_value_type get_type() const { return val_type; };

private:
  int64_t int_val = 0;
  double double_val = 0.0;
  bool bool_val = false;
  t_enum_value* enum_val = nullptr;
  std::string string_val;
  validation_function* function_val = nullptr;
  t_field* field_reference_val = nullptr;

  validation_value_type val_type;
};

class validation_rule {
public:
  validation_rule(){};
  validation_rule(std::string name) : name(name){};
  validation_rule(std::string name, validation_rule* inner) : name(name), inner(inner){};

  std::string get_name() { return name; };
  void append_value(validation_value* value) { values.push_back(value); }
  const std::vector<validation_value*>& get_values() { return values; };
  validation_rule* get_inner() { return inner; };

private:
  std::string name;
  std::vector<validation_value*> values;
  validation_rule* inner = nullptr;
};

class validation_parser {
public:
  validation_parser() {}
  validation_parser(t_struct* reference) : reference(reference) {}
  std::vector<validation_rule*> parse_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  void set_reference(t_struct* reference) { this->reference = reference; };

private:
  std::vector<validation_rule*> parse_bool_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_enum_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_double_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_integer_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_string_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_set_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_list_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_map_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_struct_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_xception_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  std::vector<validation_rule*> parse_union_field(
      t_type* type,
      std::map<std::string, std::vector<std::string>>& annotations);
  bool is_reference_field(std::string value);
  bool is_validation_function(std::string value);
  void add_bool_rule(std::vector<validation_rule*>& rules,
                     std::string key,
                     std::map<std::string, std::vector<std::string>>& annotations);
  void add_double_rule(std::vector<validation_rule*>& rules,
                       std::string key,
                       std::map<std::string, std::vector<std::string>>& annotations);
  void add_double_list_rule(std::vector<validation_rule*>& rules,
                            std::string key,
                            std::map<std::string, std::vector<std::string>>& annotations);
  void add_integer_rule(std::vector<validation_rule*>& rules,
                        std::string key,
                        std::map<std::string, std::vector<std::string>>& annotations);
  void add_integer_list_rule(std::vector<validation_rule*>& rules,
                             std::string key,
                             std::map<std::string, std::vector<std::string>>& annotations);
  void add_string_rule(std::vector<validation_rule*>& rules,
                       std::string key,
                       std::map<std::string, std::vector<std::string>>& annotations);
  void add_enum_list_rule(std::vector<validation_rule*>& rules,
                                t_enum* enum_,
                                std::string key,
                                std::map<std::string, std::vector<std::string>>& annotations);
  t_field* get_referenced_field(std::string annotation_value);
  validation_value::validation_function* get_validation_function(std::string annotation_value);
  t_struct* reference = nullptr;
};

#endif
