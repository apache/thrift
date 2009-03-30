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
