// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_SERVICE_H
#define T_SERVICE_H

#include "t_function.h"
#include <vector>

class t_program;

/**
 * A service consists of a set of functions.
 *
 */
class t_service : public t_type {
 public:
  t_service(t_program* program) :
    t_type(program),
    extends_(NULL) {}

  bool is_service() const {
    return true;
  }

  void set_extends(t_service* extends) {
    extends_ = extends;
  }

  void add_function(t_function* func) {
    functions_.push_back(func);
  }

  const std::vector<t_function*>& get_functions() const {
    return functions_;
  }

  t_service* get_extends() {
    return extends_;
  }

  virtual std::string get_fingerprint_material() const {
    // Services should never be used in fingerprints.
    throw "BUG: Can't get fingerprint material for service.";
  }

 private:
  std::vector<t_function*> functions_;
  t_service* extends_;
};

#endif
