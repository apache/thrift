/**
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

#include <ruby.h>
#ifdef HAVE_RUBY_ENCODING_H
#include <ruby/encoding.h>
#endif
#include <constants.h>

// Returns a String: callers read the result with RSTRING_PTR and RSTRING_LEN.
VALUE force_binary_encoding(VALUE buffer) {
  StringValue(buffer);

  if (rb_enc_get_index(buffer) == rb_ascii8bit_encindex()) {
    return buffer;
  }

  if (RB_OBJ_FROZEN(buffer)) {
    buffer = rb_obj_dup(buffer);
  }

  rb_enc_associate_index(buffer, rb_ascii8bit_encindex());

  return buffer;
}

// Returns a String: callers read the result with RSTRING_PTR and RSTRING_LEN.
VALUE convert_to_utf8_byte_buffer(VALUE string) {
  VALUE buffer = rb_funcall(thrift_bytes_module, convert_to_utf8_byte_buffer_id, 1, string);
  StringValue(buffer);
  return buffer;
}

VALUE convert_to_string(VALUE utf8_buffer) {
  return rb_funcall(thrift_bytes_module, convert_to_string_id, 1, utf8_buffer);
}
