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

#ifndef T_STRUCT_H
#define T_STRUCT_H

#include <algorithm>
#include <vector>
#include <utility>
#include <string>

#include "thrift/parse/t_type.h"
#include "thrift/parse/t_field.h"

// Forward declare that puppy
class t_program;

/**
 * A struct is a container for a set of member fields that has a name. Structs
 * are also used to implement exception types.
 *
 */
class t_struct : public t_type {
public:
  typedef std::vector<t_field*> members_type;

  t_struct(t_program* program)
    : t_type(program),
      is_xception_(false),
      is_union_(false),
      is_method_xcepts_(false),
      union_validated_(false),
      xcepts_validated_(false),
      members_with_value_(0),
      xsd_all_(false) {}

  t_struct(t_program* program, const std::string& name)
    : t_type(program, name),
      is_xception_(false),
      is_union_(false),
      is_method_xcepts_(false),
      union_validated_(false),
      xcepts_validated_(false),
      members_with_value_(0),
      xsd_all_(false) {}

  void set_name(const std::string& name) override {
    name_ = name;
    union_validated_= false;
    validate_members();
  }

  void set_xception(bool is_xception) { is_xception_ = is_xception; }

  void set_method_xcepts(bool is_method_xcepts) {
    is_method_xcepts_ = is_method_xcepts;
    xcepts_validated_ = false;
    validate_members();
  }

  void set_union(bool is_union) {
    is_union_ = is_union;
    union_validated_= false;
    validate_members();
  }

  void set_xsd_all(bool xsd_all) { xsd_all_ = xsd_all; }

  bool get_xsd_all() const { return xsd_all_; }

  bool append(t_field* elem) {
    typedef members_type::iterator iter_type;
    std::pair<iter_type, iter_type> bounds = std::equal_range(members_in_id_order_.begin(),
                                                              members_in_id_order_.end(),
                                                              elem,
                                                              t_field::key_compare());
    if (bounds.first != bounds.second) {
      return false;
    }
    // returns false when there is a conflict of field names
    if (get_field_by_name(elem->get_name()) != nullptr) {
      return false;
    }
    members_.push_back(elem);
    members_in_id_order_.insert(bounds.second, elem);
    if (needs_validation()) {
      validate_members();
    } else {
      validate_member_field(elem);
    }
    return true;
  }

  const members_type& get_members() const { return members_; }

  const members_type& get_sorted_members() const { return members_in_id_order_; }

  bool is_struct() const override { return !is_xception_; }
  bool is_xception() const override { return is_xception_; }
  bool is_method_xcepts() const override { return is_method_xcepts_; }
  bool is_union() const { return is_union_; }

  t_field* get_field_by_name(std::string field_name) {
    return const_cast<t_field*>(const_cast<const t_struct&>(*this).get_field_by_name(field_name));
  }

  const t_field* get_field_by_name(std::string field_name) const {
    members_type::const_iterator m_iter;
    for (m_iter = members_in_id_order_.begin(); m_iter != members_in_id_order_.end(); ++m_iter) {
      if ((*m_iter)->get_name() == field_name) {
        return *m_iter;
      }
    }
    return nullptr;
  }

  void validate() const {
    std::string what = "struct";
    if( is_union()) {
      what = "union";
    }
    if( is_xception()) {
      what = "exception";
    }

    std::vector<t_field*>::const_iterator it;
    std::vector<t_field*> list = get_members();
    for(it=list.begin(); it != list.end(); ++it) {
      (*it)->get_type()->validate();

#ifndef ALLOW_EXCEPTIONS_AS_TYPE
      if (!is_method_xcepts_) {  // this is in fact the only legal usage for any exception type
        if( (*it)->get_type()->get_true_type()->is_xception()) {
          failure("%s %s: exception type \"%s\" cannot be used as member field type %s", what.c_str(), get_name().c_str(), (*it)->get_type()->get_name().c_str(), (*it)->get_name().c_str());
        }
      }
#endif
    }
  }

private:
  members_type members_;
  members_type members_in_id_order_;
  bool is_xception_;       // struct is an IDL exception
  bool is_union_;          // struct is an IDL union
  bool is_method_xcepts_;  // struct holds the exceptions declared at a service method
  bool union_validated_;
  bool xcepts_validated_;
  int members_with_value_;

  bool xsd_all_;

  void validate_member_field(t_field* field) {
    validate_union_member(field);
    validate_method_exception_field(field);
  }
  
  void validate_union_member(t_field* field) {
    if (is_union_ && (!name_.empty())) {
      union_validated_ = true;

      // 1) unions can't have required fields
      // 2) union members are implicitly optional, otherwise bugs like THRIFT-3650 wait to happen
      if (field->get_req() != t_field::T_OPTIONAL) {
        // no warning on default requiredness, but do warn on anything else that is explicitly asked for
        if(field->get_req() != t_field::T_OPT_IN_REQ_OUT) {
          pwarning(1,
                   "Union %s field %s: union members must be optional, ignoring specified requiredness.\n",
                   name_.c_str(),
                   field->get_name().c_str());
        }
        field->set_req(t_field::T_OPTIONAL);
      }

      // unions may have up to one member defaulted, but not more
      if (field->get_value() != nullptr) {
        if (1 < ++members_with_value_) {
          throw "Error: Field " + field->get_name() + " provides another default value for union "
              + name_;
        }
      }
    }
  }

  void validate_method_exception_field(t_field* field) {
    if (is_method_xcepts_) {
      xcepts_validated_ = true;

      // THRIFT-5669: "required" makes no sense at "throws" clauses
      if (field->get_req() == t_field::T_REQUIRED) {
        field->set_req(t_field::T_OPT_IN_REQ_OUT);
        pwarning(1,
                 "Exception field %s: \"required\" is illegal here, ignoring.\n",
                 field->get_name().c_str());
      }
    }
  }

  bool needs_validation() {
    if (is_method_xcepts_) {
      return !xcepts_validated_;
    }
    if (is_union_) {
      return !union_validated_;
    }
    return false;
  }

  void validate_members() {
    if (needs_validation()) {
      members_type::const_iterator m_iter;
      for (m_iter = members_in_id_order_.begin(); m_iter != members_in_id_order_.end(); ++m_iter) {
        validate_member_field(*m_iter);
      }
    }
  }

};

#endif
