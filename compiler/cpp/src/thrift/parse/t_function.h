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

#ifndef T_FUNCTION_H
#define T_FUNCTION_H

#include "thrift/parse/t_doc.h"
#include "thrift/parse/t_struct.h"
#include "thrift/parse/t_type.h"
#include <string>

/**
 * Representation of a function. Key parts are return type, function name,
 * optional modifiers, and an argument list, which is implemented as a thrift
 * struct.
 *
 */
class t_function : public t_doc {
public:
  t_function(t_type* returntype, std::string name, t_struct* arglist, bool oneway = false)
    : returntype_(returntype),
      name_(name),
      arglist_(arglist),
      xceptions_(new t_struct(nullptr)),
      own_xceptions_(true),
      oneway_(oneway) {
    xceptions_->set_method_xcepts(true);
    if (oneway_ && (!returntype_->is_void())) {
      pwarning(1, "Oneway methods should return void.\n");
    }
  }

  t_function(t_type* returntype,
             std::string name,
             t_struct* arglist,
             t_struct* xceptions,
             bool oneway = false)
    : returntype_(returntype),
      name_(name),
      arglist_(arglist),
      xceptions_(xceptions),
      own_xceptions_(false),
      oneway_(oneway) {
    xceptions_->set_method_xcepts(true);
    if (oneway_ && !xceptions_->get_members().empty()) {
      throw std::string("Oneway methods can't throw exceptions.");
    }
    if (oneway_ && (!returntype_->is_void())) {
      pwarning(1, "Oneway methods should return void.\n");
    }
  }

  ~t_function() override {
    if (own_xceptions_)
      delete xceptions_;
  }

  t_type* get_returntype() const { return returntype_; }

  const std::string& get_name() const { return name_; }

  t_struct* get_arglist() const { return arglist_; }

  t_struct* get_xceptions() const { return xceptions_; }

  bool is_oneway() const { return oneway_; }

  std::map<std::string, std::vector<std::string>> annotations_;

  void validate() const {
    get_returntype()->validate();

#ifndef ALLOW_EXCEPTIONS_AS_TYPE
    if (get_returntype()->get_true_type()->is_xception()) {
      failure("method %s(): exception type \"%s\" cannot be used as function return", get_name().c_str(), get_returntype()->get_name().c_str());
    }
#endif

    std::vector<t_field*>::const_iterator it;
    std::vector<t_field*> list = get_arglist()->get_members();
    for(it=list.begin(); it != list.end(); ++it) {
      (*it)->get_type()->validate();

#ifndef ALLOW_EXCEPTIONS_AS_TYPE
      if( (*it)->get_type()->get_true_type()->is_xception()) {
        failure("method %s(): exception type \"%s\" cannot be used as function argument %s", get_name().c_str(), (*it)->get_type()->get_name().c_str(), (*it)->get_name().c_str());
      }
#endif
    }
  }

private:
  t_type* returntype_;
  std::string name_;
  t_struct* arglist_;
  t_struct* xceptions_;
  bool own_xceptions_;
  bool oneway_;
};

#endif
