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

#ifndef T_TYPEDEF_H
#define T_TYPEDEF_H

#include <string>
#include "t_type.h"

/**
 * A typedef is a mapping from a symbolic name to another type. In dymanically
 * typed languages (i.e. php/python) the code generator can actually usually
 * ignore typedefs and just use the underlying type directly, though in C++
 * the symbolic naming can be quite useful for code clarity.
 *
 */
class t_typedef : public t_type {
 public:
  t_typedef(t_program* program, t_type* type, std::string symbolic) :
    t_type(program, symbolic),
    type_(type),
    symbolic_(symbolic) {}

  ~t_typedef() {}

  t_type* get_type() const {
    return type_;
  }

  const std::string& get_symbolic() const {
    return symbolic_;
  }

  bool is_typedef() const {
    return true;
  }

  virtual std::string get_fingerprint_material() const {
    return type_->get_fingerprint_material();
  }

  virtual void generate_fingerprint() {
    t_type::generate_fingerprint();
    if (!type_->has_fingerprint()) {
      type_->generate_fingerprint();
    }
  }

 private:
  t_type* type_;
  std::string symbolic_;
};

#endif
