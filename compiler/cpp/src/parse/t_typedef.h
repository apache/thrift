// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

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
