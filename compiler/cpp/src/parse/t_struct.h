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

#include <vector>
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

  void set_xsd_all(bool xsd_all) {
    xsd_all_ = xsd_all;
  }

  bool get_xsd_all() const {
    return xsd_all_;
  }

  void append(t_field* elem) {
    members_.push_back(elem);
  }

  const std::vector<t_field*>& get_members() {
    return members_;
  }

  bool is_struct() const {
    return !is_xception_;
  }

  bool is_xception() const {
    return is_xception_;
  }

  virtual std::string get_fingerprint_material() const {
    std::string rv = "{";
    std::vector<t_field*>::const_iterator m_iter;
    for (m_iter = members_.begin(); m_iter != members_.end(); ++m_iter) {
      rv += (*m_iter)->get_fingerprint_material();
      rv += ";";
    }
    rv += "}";
    return rv;
  }

  virtual void generate_fingerprint() {
    t_type::generate_fingerprint();
    std::vector<t_field*>::const_iterator m_iter;
    for (m_iter = members_.begin(); m_iter != members_.end(); ++m_iter) {
      (*m_iter)->get_type()->generate_fingerprint();
    }
  }

  bool validate_field(t_field* field) {
    int key = field->get_key();
    std::vector<t_field*>::const_iterator m_iter;
    for (m_iter = members_.begin(); m_iter != members_.end(); ++m_iter) {
      if ((*m_iter)->get_key() == key) {
        return false;
      }
    }
    return true;
  }

 private:

  std::vector<t_field*> members_;
  bool is_xception_;

  bool xsd_all_;
};

#endif
