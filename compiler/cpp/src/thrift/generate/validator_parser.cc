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

/*
 * This file is programmatically sanitized for style:
 * astyle --style=1tbs -f -p -H -j -U t_validator_parser.cc
 *
 * The output of astyle should not be taken unquestioningly, but it is a good
 * guide for ensuring uniformity and readability.
 */

#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

#include "thrift/generate/t_generator.h"
#include "thrift/generate/validator_parser.h"
#include "thrift/platform.h"
#include "thrift/version.h"
#include <algorithm>
#include <clocale>
#include <sstream>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

const char* list_delimiter = "[], ";

std::vector<validation_rule*> validation_parser::parse_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  std::vector<validation_rule*> empty_rules;
  if (type->is_typedef()) {
    type = type->get_true_type();
  }
  if (type->is_enum()) {
    return parse_enum_field(type, annotations);
  } else if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_UUID:
    case t_base_type::TYPE_VOID:
      return empty_rules;
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      return parse_integer_field(type, annotations);
    case t_base_type::TYPE_DOUBLE:
      return parse_double_field(type, annotations);
    case t_base_type::TYPE_STRING:
      return parse_string_field(type, annotations);
    case t_base_type::TYPE_BOOL:
      return parse_bool_field(type, annotations);
    }
  } else if (type->is_list()) {
    return parse_list_field(type, annotations);
  } else if (type->is_set()) {
    return parse_set_field(type, annotations);
  } else if (type->is_map()) {
    return parse_map_field(type, annotations);
  } else if (type->is_struct()) {
    if (((t_struct*)type)->is_union()) {
      return parse_union_field(type, annotations);
    }
    return parse_struct_field(type, annotations);
  } else if (type->is_xception()) {
    return parse_xception_field(type, annotations);
  }
  throw "validator error: unsupported type: " + type->get_name();
}

std::vector<validation_rule*> validation_parser::parse_bool_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  std::vector<validation_rule*> rules;
  add_bool_rule(rules, "vt.const", annotations);
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_enum_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  std::vector<validation_rule*> rules;
  add_bool_rule(rules, "vt.defined_only", annotations);
  add_enum_list_rule(rules, (t_enum*)type, "vt.in", annotations);
  add_enum_list_rule(rules, (t_enum*)type, "vt.not_in", annotations);
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_double_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  std::vector<validation_rule*> rules;
  add_double_rule(rules, "vt.lt", annotations);
  add_double_rule(rules, "vt.le", annotations);
  add_double_rule(rules, "vt.gt", annotations);
  add_double_rule(rules, "vt.ge", annotations);
  add_double_list_rule(rules, "vt.in", annotations);
  add_double_list_rule(rules, "vt.not_in", annotations);
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_integer_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  std::vector<validation_rule*> rules;
  add_integer_rule(rules, "vt.lt", annotations);
  add_integer_rule(rules, "vt.le", annotations);
  add_integer_rule(rules, "vt.gt", annotations);
  add_integer_rule(rules, "vt.ge", annotations);
  add_integer_list_rule(rules, "vt.in", annotations);
  add_integer_list_rule(rules, "vt.not_in", annotations);
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_string_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  std::vector<validation_rule*> rules;
  add_string_rule(rules, "vt.const", annotations);
  add_integer_rule(rules, "vt.min_size", annotations);
  add_integer_rule(rules, "vt.max_size", annotations);
  add_string_rule(rules, "vt.pattern", annotations);
  add_string_rule(rules, "vt.prefix", annotations);
  add_string_rule(rules, "vt.suffix", annotations);
  add_string_rule(rules, "vt.contains", annotations);
  add_string_rule(rules, "vt.not_contains", annotations);
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_set_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  return parse_list_field(type, annotations);
}

std::vector<validation_rule*> validation_parser::parse_list_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  std::vector<validation_rule*> rules;
  add_integer_rule(rules, "vt.min_size", annotations);
  add_integer_rule(rules, "vt.max_size", annotations);
  std::string elem_prefix("vt.elem");
  std::map<std::string, std::vector<std::string>> elem_annotations;
  for (auto it = annotations.begin(); it != annotations.end(); it++) {
    if (it->first.compare(0, elem_prefix.size(), elem_prefix) == 0) {
      std::string elem_key = "vt" + it->first.substr(elem_prefix.size());
      elem_annotations[elem_key] = it->second;
    }
  }
  std::vector<validation_rule*> elem_rules;
  if (type->is_list()) {
    elem_rules = parse_field(((t_list*)type)->get_elem_type(), elem_annotations);
  } else if (type->is_set()) {
    elem_rules = parse_field(((t_set*)type)->get_elem_type(), elem_annotations);
  }
  for (auto it = elem_rules.begin(); it != elem_rules.end(); it++) {
    validation_rule* rule = new validation_rule(elem_prefix, *it);
    rules.push_back(rule);
  }
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_map_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  std::vector<validation_rule*> rules;
  add_integer_rule(rules, "vt.min_size", annotations);
  add_integer_rule(rules, "vt.max_size", annotations);
  std::string key_prefix("vt.key");
  std::map<std::string, std::vector<std::string>> key_annotations;
  for (auto it = annotations.begin(); it != annotations.end(); it++) {
    if (it->first.compare(0, key_prefix.size(), key_prefix) == 0) {
      std::string key_key = "vt" + it->first.substr(key_prefix.size());
      key_annotations[key_key] = it->second;
    }
  }
  std::vector<validation_rule*> key_rules;
  key_rules = parse_field(((t_map*)type)->get_key_type(), key_annotations);
  for (auto it = key_rules.begin(); it != key_rules.end(); it++) {
    validation_rule* rule = new validation_rule(key_prefix, *it);
    rules.push_back(rule);
  }

  std::string value_prefix("vt.value");
  std::map<std::string, std::vector<std::string>> value_annotations;
  for (auto it = annotations.begin(); it != annotations.end(); it++) {
    if (it->first.compare(0, value_prefix.size(), value_prefix) == 0) {
      std::string value_key = "vt" + it->first.substr(value_prefix.size());
      value_annotations[value_key] = it->second;
    }
  }
  std::vector<validation_rule*> value_rules;
  value_rules = parse_field(((t_map*)type)->get_val_type(), value_annotations);
  for (auto it = value_rules.begin(); it != value_rules.end(); it++) {
    validation_rule* rule = new validation_rule(value_prefix, *it);
    rules.push_back(rule);
  }
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_struct_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  (void)type;
  std::vector<validation_rule*> rules;
  add_bool_rule(rules, "vt.skip", annotations);
  return rules;
}

std::vector<validation_rule*> validation_parser::parse_xception_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  return parse_struct_field(type, annotations);
}

std::vector<validation_rule*> validation_parser::parse_union_field(
    t_type* type,
    std::map<std::string, std::vector<std::string>>& annotations) {
  return parse_struct_field(type, annotations);
}

bool validation_parser::is_reference_field(std::string value) {
  if (value[0] != '$') {
    return false;
  }
  value.erase(value.begin());
  t_field* field = this->reference->get_field_by_name(value);
  return field != nullptr;
}

bool validation_parser::is_validation_function(std::string value) {
  if (value[0] != '@') {
    return false;
  }
  return true;
}

void validation_parser::add_bool_rule(
    std::vector<validation_rule*>& rules,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      validation_rule* rule = new validation_rule(key);
      validation_value* value;
      if (is_reference_field(annotation_value)) {
        t_field* field = get_referenced_field(annotation_value);
        value = new validation_value(field);
      } else {
        bool constant;
        std::istringstream(it->second.back()) >> std::boolalpha >> constant;
        value = new validation_value(constant);
      }
      rule->append_value(value);
      rules.push_back(rule);
    }
  }
}

void validation_parser::add_double_rule(
    std::vector<validation_rule*>& rules,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      if (annotation_value.size() == 0) {
        continue;
      }
      validation_rule* rule = new validation_rule(key);
      validation_value* value;
      if (is_validation_function(annotation_value)) {
        validation_value::validation_function* function = get_validation_function(annotation_value);
        value = new validation_value(function);
      } else if (is_reference_field(annotation_value)) {
        t_field* field = get_referenced_field(annotation_value);
        value = new validation_value(field);
      } else {
        double constant = std::stod(annotation_value);
        value = new validation_value(constant);
      }
      rule->append_value(value);
      rules.push_back(rule);
    }
  }
}

void validation_parser::add_enum_list_rule(
    std::vector<validation_rule*>& rules,
    t_enum* enum_,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      if (annotation_value.size() == 0) {
        continue;
      }
      validation_rule* rule = new validation_rule(key);
      if (annotation_value[0] == '[') {
        validation_value* value;
        char* str = strdup(annotation_value.c_str());
        char* pch = strtok(str, list_delimiter);
        std::string val;
        while (pch != NULL) {
          std::string temp(pch);
          if (is_validation_function(temp)) {
            validation_value::validation_function* function = get_validation_function(temp);
            value = new validation_value(function);
          } else if (is_reference_field(temp)) {
            t_field* field = get_referenced_field(temp);
            value = new validation_value(field);
          } else if (std::stringstream(temp) >> val) {
            std::string::size_type dot = val.rfind('.');
            if (dot != std::string::npos) {
              val = val.substr(dot + 1);
            }
            t_enum_value* enum_val = enum_->get_constant_by_name(val);
            value = new validation_value(enum_val);
          } else {
            delete rule;
            throw "validator error: validation double list parse failed: " + temp;
          }
          rule->append_value(value);
          pch = strtok(NULL, list_delimiter);
        }
      } else {
        validation_value* value;
        std::string val = annotation_value;
        std::string::size_type dot = val.rfind('.');
        if (dot != std::string::npos) {
          val = val.substr(dot + 1);
        }
        t_enum_value* enum_val = enum_->get_constant_by_name(val);
        value = new validation_value(enum_val);
        rule->append_value(value);
      }
      rules.push_back(rule);
    }
  }
}

void validation_parser::add_double_list_rule(
    std::vector<validation_rule*>& rules,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  std::map<std::string, std::vector<std::string>> double_rules;
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      if (annotation_value.size() == 0) {
        continue;
      }
      if (annotation_value[0] == '[') {
        validation_rule* rule = new validation_rule(key);
        validation_value* value;
        char* str = strdup(annotation_value.c_str());
        char* pch = strtok(str, list_delimiter);
        double val;
        while (pch != NULL) {
          std::string temp(pch);
          if (is_validation_function(temp)) {
            validation_value::validation_function* function = get_validation_function(temp);
            value = new validation_value(function);
          } else if (is_reference_field(temp)) {
            t_field* field = get_referenced_field(temp);
            value = new validation_value(field);
          } else if (std::stringstream(temp) >> val) {
            value = new validation_value(val);
          } else {
            delete rule;
            throw "validator error: validation double list parse failed: " + temp;
          }
          rule->append_value(value);
          pch = strtok(NULL, list_delimiter);
        }
        rules.push_back(rule);
      } else {
        double_rules[key].push_back(annotation_value);
      }
    }
  }
  if (double_rules[key].size() > 0) {
    add_double_rule(rules, key, double_rules);
  }
}

void validation_parser::add_integer_rule(
    std::vector<validation_rule*>& rules,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      if (annotation_value.size() == 0) {
        continue;
      }
      validation_rule* rule = new validation_rule(key);
      validation_value* value;
      if (is_reference_field(annotation_value)) {
        t_field* field = get_referenced_field(annotation_value);
        value = new validation_value(field);
      } else if (is_validation_function(annotation_value)) {
        validation_value::validation_function* function = get_validation_function(annotation_value);
        value = new validation_value(function);
      } else {
        int64_t constant = std::stoll(annotation_value);
        value = new validation_value(constant);
      }
      rule->append_value(value);
      rules.push_back(rule);
    }
  }
}

void validation_parser::add_integer_list_rule(
    std::vector<validation_rule*>& rules,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  std::map<std::string, std::vector<std::string>> integer_rules;
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      if (annotation_value.size() == 0) {
        continue;
      }
      if (annotation_value[0] == '[') {
        validation_rule* rule = new validation_rule(key);
        validation_value* value;
        char* str = strdup(annotation_value.c_str());
        char* pch = strtok(str, list_delimiter);
        int64_t val;
        while (pch != NULL) {
          std::string temp(pch);
          if (is_validation_function(temp)) {
            validation_value::validation_function* function = get_validation_function(temp);
            value = new validation_value(function);
          } else if (is_reference_field(temp)) {
            t_field* field = get_referenced_field(temp);
            value = new validation_value(field);
          } else if (std::stringstream(temp) >> val) {
            value = new validation_value(val);
          } else {
            delete rule;
            throw "validator error: validation integer list parse failed: " + temp;
          }
          rule->append_value(value);
          pch = strtok(NULL, list_delimiter);
        }
        rules.push_back(rule);
      } else {
        integer_rules[key].push_back(annotation_value);
      }
    }
  }
  if (integer_rules[key].size() > 0) {
    add_integer_rule(rules, key, integer_rules);
  }
}

void validation_parser::add_string_rule(
    std::vector<validation_rule*>& rules,
    std::string key,
    std::map<std::string, std::vector<std::string>>& annotations) {
  auto it = annotations.find(key);
  if (it != annotations.end() && !it->second.empty()) {
    for (auto& annotation_value : it->second) {
      validation_rule* rule = new validation_rule(key);
      validation_value* value;
      if (is_reference_field(annotation_value)) {
        t_field* field = get_referenced_field(annotation_value);
        value = new validation_value(field);
      } else {
        value = new validation_value(annotation_value);
      }
      rule->append_value(value);
      rules.push_back(rule);
    }
  }
}

t_field* validation_parser::get_referenced_field(std::string annotation_value) {
  annotation_value.erase(annotation_value.begin());
  return reference->get_field_by_name(annotation_value);
}

validation_value::validation_function* validation_parser::get_validation_function(
    std::string annotation_value) {
  std::string value = annotation_value;
  value.erase(value.begin());
  validation_value::validation_function* function = new validation_value::validation_function;

  size_t name_end = value.find_first_of('(');
  if (name_end >= value.size()) {
    delete function;
    throw "validator error: validation function parse failed: " + annotation_value;
  }
  function->name = value.substr(0, name_end);
  value.erase(0, name_end + 1); // name(

  if (function->name == "len") {
    size_t argument_end = value.find_first_of(')');
    if (argument_end >= value.size()) {
      delete function;
      throw "validator error: validation function parse failed: " + annotation_value;
    }
    std::string argument = value.substr(0, argument_end);
    if (argument.size() > 0 && argument[0] == '$') {
      t_field* field = get_referenced_field(argument);
      validation_value* value = new validation_value(field);
      function->arguments.push_back(value);
    } else {
      delete function;
      throw "validator error: validation function parse failed, unrecognized argument: "
          + annotation_value;
    }
  } else {
    delete function;
    throw "validator error: validation function parse failed, function not supported: "
        + annotation_value;
  }
  return function;
}
