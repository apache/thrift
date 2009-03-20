// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_CONST_H
#define T_CONST_H

#include "t_type.h"
#include "t_const_value.h"

/**
 * A const is a constant value defined across languages that has a type and
 * a value. The trick here is that the declared type might not match the type
 * of the value object, since that is not determined until after parsing the
 * whole thing out.
 *
 */
class t_const : public t_doc {
 public:
  t_const(t_type* type, std::string name, t_const_value* value) :
    type_(type),
    name_(name),
    value_(value) {}

  t_type* get_type() const {
    return type_;
  }

  std::string get_name() const {
    return name_;
  }

  t_const_value* get_value() const {
    return value_;
  }

 private:
  t_type* type_;
  std::string name_;
  t_const_value* value_;
};

#endif

