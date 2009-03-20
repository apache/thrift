// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_ENUM_VALUE_H
#define T_ENUM_VALUE_H

#include <string>
#include "t_doc.h"

/**
 * A constant. These are used inside of enum definitions. Constants are just
 * symbol identifiers that may or may not have an explicit value associated
 * with them.
 *
 */
class t_enum_value : public t_doc {
 public:
  t_enum_value(std::string name) :
    name_(name),
    has_value_(false),
    value_(0) {}

  t_enum_value(std::string name, int value) :
    name_(name),
    has_value_(true),
    value_(value) {}

  ~t_enum_value() {}

  const std::string& get_name() {
    return name_;
  }

  bool has_value() {
    return has_value_;
  }

  int get_value() {
    return value_;
  }

 private:
  std::string name_;
  bool has_value_;
  int value_;
};

#endif
