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

#include "t_type.h"
#include "t_field.h"

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

  t_struct(t_program* program) :
    t_type(program),
    is_xception_(false),
    xsd_all_(false) {}

  t_struct(t_program* program, const std::string& name) :
    t_type(program, name),
    is_xception_(false),
    xsd_all_(false) {}

  void set_name(const std::string& name) {
    name_ = name;
  }

  void set_xception(bool is_xception) {
    is_xception_ = is_xception;
  }

  void set_union(bool is_union) {
    is_union_ = is_union;
  }

  void set_xsd_all(bool xsd_all) {
    xsd_all_ = xsd_all;
  }

  bool get_xsd_all() const {
    return xsd_all_;
  }

  bool append(t_field* elem) {
    members_.push_back(elem);

    typedef members_type::iterator iter_type;
    std::pair<iter_type, iter_type> bounds = std::equal_range(
            members_in_id_order_.begin(), members_in_id_order_.end(), elem, t_field::key_compare()
        );
    if (bounds.first != bounds.second) {
      return false;
    }
    members_in_id_order_.insert(bounds.second, elem);
    return true;
  }

  const members_type& get_members() {
    return members_;
  }

  const members_type& get_sorted_members() {
    return members_in_id_order_;
  }

  bool is_struct() const {
    return !is_xception_;
  }

  bool is_xception() const {
    return is_xception_;
  }
  
  bool is_union() const {
    return is_union_;
  }

  virtual std::string get_fingerprint_material() const {
    std::string rv = "{";
    members_type::const_iterator m_iter;
    for (m_iter = members_in_id_order_.begin(); m_iter != members_in_id_order_.end(); ++m_iter) {
      rv += (*m_iter)->get_fingerprint_material();
      rv += ";";
    }
    rv += "}";
    return rv;
  }

  virtual void generate_fingerprint() {
    t_type::generate_fingerprint();
    members_type::const_iterator m_iter;
    for (m_iter = members_in_id_order_.begin(); m_iter != members_in_id_order_.end(); ++m_iter) {
      (*m_iter)->get_type()->generate_fingerprint();
    }
  }

 private:

  members_type members_;
  members_type members_in_id_order_;
  bool is_xception_;
  bool is_union_;

  bool xsd_all_;
};

#endif
