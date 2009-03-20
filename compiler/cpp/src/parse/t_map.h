// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_MAP_H
#define T_MAP_H

#include "t_container.h"

/**
 * A map is a lightweight container type that just wraps another two data
 * types.
 *
 */
class t_map : public t_container {
 public:
  t_map(t_type* key_type, t_type* val_type) :
    key_type_(key_type),
    val_type_(val_type) {}

  t_type* get_key_type() const {
    return key_type_;
  }

  t_type* get_val_type() const {
    return val_type_;
  }

  bool is_map() const {
    return true;
  }

  virtual std::string get_fingerprint_material() const {
    return "map<" + key_type_->get_fingerprint_material() +
      "," + val_type_->get_fingerprint_material() + ">";
  }

  virtual void generate_fingerprint() {
    t_type::generate_fingerprint();
    key_type_->generate_fingerprint();
    val_type_->generate_fingerprint();
  }

 private:
  t_type* key_type_;
  t_type* val_type_;
};

#endif
