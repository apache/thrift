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

#ifndef T_SERVICE_H
#define T_SERVICE_H

#include "thrift/parse/t_function.h"
#include <vector>

class t_program;

/**
 * A service consists of a set of functions.
 *
 */
class t_service : public t_type {
public:
  t_service(t_program* program) : t_type(program), extends_(nullptr) {}

  bool is_service() const override { return true; }

  void set_extends(t_service* extends) { extends_ = extends; }

  void add_function(t_function* func) {
    if (get_function_by_name(func->get_name()) != NULL) {
      throw "Function " + func->get_name() + " is already defined";
    }
    functions_.push_back(func);
  }

  void validate_unique_members() {
    std::vector<t_function*>::const_iterator iter;
    for (iter = functions_.begin(); iter != functions_.end(); ++iter) {
      // throw exception when there is a conflict of names with super class
      if (extends_ != NULL) {
        if (extends_->get_function_by_name((*iter)->get_name()) != NULL) {
          throw "Function " + (*iter)->get_name() + " is already defined in service " + name_;
        }
      }
    }
  }

  t_function* get_function_by_name(std::string func_name) {
    if (extends_ != NULL) {
      t_function* func = NULL;
      if ((func = extends_->get_function_by_name(func_name)) != NULL) {
        return func;
      }
    }

    std::vector<t_function*>::const_iterator iter;
    for (iter = functions_.begin(); iter != functions_.end(); ++iter) {
      if ((*iter)->get_name() == func_name) {
        return *iter;
      }
    }
    return NULL;
  }

  const t_function* get_function_by_name(std::string func_name) const {
    if (extends_ != NULL) {
      t_function* func = NULL;
      if ((func = extends_->get_function_by_name(func_name)) != NULL) {
        return func;
      }
    }

    std::vector<t_function*>::const_iterator iter;
    for (iter = functions_.begin(); iter != functions_.end(); ++iter) {
      if ((*iter)->get_name() == func_name) {
        return *iter;
      }
    }
    return NULL;
  }

  const std::vector<t_function*>& get_functions() const { return functions_; }

  t_service* get_extends() { return extends_; }

  const t_service* get_extends() const { return extends_; }

private:
  std::vector<t_function*> functions_;
  t_service* extends_;
};

#endif
