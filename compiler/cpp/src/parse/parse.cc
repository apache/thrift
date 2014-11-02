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

#include "t_type.h"
#include "t_typedef.h"

#include "md5.h"
#include "main.h"

void t_type::generate_fingerprint() {
  if (! has_fingerprint()) {
    pdebug("generating fingerprint for %s", get_name().c_str());
    std::string material = get_fingerprint_material();
    md5_state_t ctx;
    md5_init(&ctx);
    md5_append(&ctx, (md5_byte_t*)(material.data()), (int)material.size());
    md5_finish(&ctx, (md5_byte_t*)fingerprint_);
  }
}

t_type* t_type::get_true_type() {
  t_type* type = this;
  while (type->is_typedef()) {
    type = ((t_typedef*)type)->get_type();
  }
  return type;
}
