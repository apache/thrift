// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_ENUM_H
#define T_ENUM_H

#include "t_enum_value.h"
#include <vector>

/**
 * An enumerated type. A list of constant objects with a name for the type.
 *
 */
class t_enum : public t_type {
 public:
  t_enum(t_program* program) :
    t_type(program) {}

  void set_name(const std::string& name) {
    name_ = name;
  }

  void append(t_enum_value* constant) {
    constants_.push_back(constant);
  }

  const std::vector<t_enum_value*>& get_constants() {
    return constants_;
  }

  bool is_enum() const {
    return true;
  }

  virtual std::string get_fingerprint_material() const {
    return "enum";
  }

 private:
  std::vector<t_enum_value*> constants_;
};

#endif
