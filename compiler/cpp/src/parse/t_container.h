// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_CONTAINER_H
#define T_CONTAINER_H

#include "t_type.h"

class t_container : public t_type {
 public:
  t_container() :
    cpp_name_(),
    has_cpp_name_(false) {}

  virtual ~t_container() {}

  void set_cpp_name(std::string cpp_name) {
    cpp_name_ = cpp_name;
    has_cpp_name_ = true;
  }

  bool has_cpp_name() {
    return has_cpp_name_;
  }

  std::string get_cpp_name() {
    return cpp_name_;
  }

  bool is_container() const {
    return true;
  }

 private:
  std::string cpp_name_;
  bool has_cpp_name_;

};

#endif
