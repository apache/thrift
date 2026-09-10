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

#define GET_TRANSPORT(obj) rb_ivar_get(obj, transport_ivar_id)
#define GET_STRICT_READ(obj) rb_ivar_get(obj, strict_read_ivar_id)
#define GET_STRICT_WRITE(obj) rb_ivar_get(obj, strict_write_ivar_id)
#define WRITE(obj, data, length) rb_funcall(obj, write_method_id, 1, rb_str_new(data, length))
#define CHECK_NIL(obj) if (NIL_P(obj)) { rb_raise(rb_eStandardError, "nil argument not allowed!");}
#define READ(obj, length) rb_funcall(GET_TRANSPORT(obj), read_all_method_id, 1, INT2FIX(length))

// Refuse a declared string length over the protocol's @max_string_size (nil
// means no limit) before it becomes a read size.
#define CHECK_STRING_SIZE(obj, size) \
  do { \
    VALUE max_string_size_ = rb_ivar_get(obj, max_string_size_ivar_id); \
    if (!NIL_P(max_string_size_) && RB_UNLIKELY((long long)(size) > NUM2LL(max_string_size_))) { \
      rb_exc_raise(get_protocol_exception(INT2FIX(PROTOERR_SIZE_LIMIT), \
        rb_sprintf("String size %lld larger than the maximum %lld", (long long)(size), NUM2LL(max_string_size_)))); \
    } \
  } while (0)

#ifndef RFLOAT_VALUE
#  define RFLOAT_VALUE(v) RFLOAT(rb_Float(v))->value
#endif

#ifndef RSTRING_LEN
#  define RSTRING_LEN(v) RSTRING(rb_String(v))->len
#endif

#ifndef RSTRING_PTR
#  define RSTRING_PTR(v) RSTRING(rb_String(v))->ptr
#endif

#ifndef RARRAY_LEN
#  define RARRAY_LEN(v) RARRAY(rb_Array(v))->len
#endif
