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

#include <set>
#include <string>

#include "thrift/parse/t_program.h"
#include "thrift/parse/t_type.h"
#include "thrift/parse/t_typedef.h"

#include "thrift/main.h"

t_type* t_type::get_true_type() {
  return const_cast<t_type*>(const_cast<const t_type*>(this)->get_true_type());
}

const t_type* t_type::get_true_type() const {
  const t_type* type = this;
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  return type;
}

namespace {

/**
 * Follows typedefs and container elements one step at a time. path holds the
 * types on the way here, so that a typedef reached again through itself,
 * which would send get_true_type() round forever, is reported instead.
 * done holds the types already walked in full.
 */
void resolve_type(const t_type* type,
                  std::set<const t_type*>& path,
                  std::set<const t_type*>& done) {
  if (done.count(type) != 0) {
    return;
  }
  if (!path.insert(type).second) {
    throw std::string("Type \"") + type->get_name() + "\" refers to itself";
  }
  if (type->is_typedef()) {
    resolve_type(((const t_typedef*)type)->get_type(), path, done);
  } else if (type->is_list()) {
    resolve_type(((const t_list*)type)->get_elem_type(), path, done);
  } else if (type->is_set()) {
    resolve_type(((const t_set*)type)->get_elem_type(), path, done);
  } else if (type->is_map()) {
    resolve_type(((const t_map*)type)->get_key_type(), path, done);
    resolve_type(((const t_map*)type)->get_val_type(), path, done);
  }
  path.erase(type);
  done.insert(type);
}

void resolve_type(const t_struct* tstruct,
                  std::set<const t_type*>& path,
                  std::set<const t_type*>& done) {
  for (const t_field* field : tstruct->get_members()) {
    resolve_type(field->get_type(), path, done);
  }
}

} // namespace

void t_program::resolve_types() const {
  std::set<const t_type*> path;
  std::set<const t_type*> done;
  for (const t_typedef* td : typedefs_) {
    resolve_type(td, path, done);
  }
  for (const t_const* c : consts_) {
    resolve_type(c->get_type(), path, done);
  }
  for (const t_struct* ts : objects_) {
    resolve_type(ts, path, done);
  }
  for (const t_service* service : services_) {
    for (const t_function* function : service->get_functions()) {
      resolve_type(function->get_returntype(), path, done);
      resolve_type(function->get_arglist(), path, done);
      resolve_type(function->get_xceptions(), path, done);
    }
  }
}
