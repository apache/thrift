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

#include <struct.h>
#include <constants.h>
#include "macros.h"

#ifndef HAVE_STRLCPY

static
size_t
strlcpy (char *dst, const char *src, size_t dst_sz)
{
    size_t n;

    for (n = 0; n < dst_sz; n++) {
      if ((*dst++ = *src++) == '\0')
        break;
    }

    if (n < dst_sz)
      return n;
    if (n > 0)
      *(dst - 1) = '\0';
    return n + strlen (src);
}

#endif

static native_proto_method_table *mt;
static native_proto_method_table *default_mt;
// static VALUE last_proto_class = Qnil;

#define IS_CONTAINER(ttype) ((ttype) == TTYPE_MAP || (ttype) == TTYPE_LIST || (ttype) == TTYPE_SET)
#define STRUCT_FIELDS(obj) rb_const_get(CLASS_OF(obj), fields_const_id)

// static void set_native_proto_function_pointers(VALUE protocol) {
//   VALUE method_table_object = rb_const_get(CLASS_OF(protocol), rb_intern("@native_method_table"));
//   // TODO: check nil?
//   Data_Get_Struct(method_table_object, native_proto_method_table, mt);
// }

// static void check_native_proto_method_table(VALUE protocol) {
//   VALUE protoclass = CLASS_OF(protocol);
//   if (protoclass != last_proto_class) {
//     last_proto_class = protoclass;
//     if (rb_funcall(protocol, native_qmark_method_id, 0) == Qtrue) {
//       set_native_proto_function_pointers(protocol);
//     } else {
//       mt = default_mt;
//     }
//   }
// }

//-------------------------------------------
// Writing section
//-------------------------------------------

// default fn pointers for protocol stuff here

VALUE default_write_bool(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_boolean_method_id, 1, value);
  return Qnil;
}

VALUE default_write_byte(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_byte_method_id, 1, value);
  return Qnil;
}

VALUE default_write_i16(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_i16_method_id, 1, value);
  return Qnil;
}

VALUE default_write_i32(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_i32_method_id, 1, value);
  return Qnil;
}

VALUE default_write_i64(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_i64_method_id, 1, value);
  return Qnil;
}

VALUE default_write_double(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_double_method_id, 1, value);
  return Qnil;
}

VALUE default_write_string(VALUE protocol, VALUE value) {
  rb_funcall(protocol, write_string_method_id, 1, value);
  return Qnil;
}

VALUE default_write_list_begin(VALUE protocol, VALUE etype, VALUE length) {
  rb_funcall(protocol, write_list_begin_method_id, 2, etype, length);
  return Qnil;
}

VALUE default_write_list_end(VALUE protocol) {
  rb_funcall(protocol, write_list_end_method_id, 0);
  return Qnil;
}

VALUE default_write_set_begin(VALUE protocol, VALUE etype, VALUE length) {
  rb_funcall(protocol, write_set_begin_method_id, 2, etype, length);
  return Qnil;
}

VALUE default_write_set_end(VALUE protocol) {
  rb_funcall(protocol, write_set_end_method_id, 0);
  return Qnil;
}

VALUE default_write_map_begin(VALUE protocol, VALUE ktype, VALUE vtype, VALUE length) {
  rb_funcall(protocol, write_map_begin_method_id, 3, ktype, vtype, length);
  return Qnil;
}

VALUE default_write_map_end(VALUE protocol) {
  rb_funcall(protocol, write_map_end_method_id, 0);
  return Qnil;
}

VALUE default_write_struct_begin(VALUE protocol, VALUE struct_name) {
  rb_funcall(protocol, write_struct_begin_method_id, 1, struct_name);
  return Qnil;
}

VALUE default_write_struct_end(VALUE protocol) {
  rb_funcall(protocol, write_struct_end_method_id, 0);
  return Qnil;
}

VALUE default_write_field_begin(VALUE protocol, VALUE name, VALUE type, VALUE id) {
  rb_funcall(protocol, write_field_begin_method_id, 3, name, type, id);
  return Qnil;
}

VALUE default_write_field_end(VALUE protocol) {
  rb_funcall(protocol, write_field_end_method_id, 0);
  return Qnil;
}

VALUE default_write_field_stop(VALUE protocol) {
  rb_funcall(protocol, write_field_stop_method_id, 0);
  return Qnil;
}

VALUE default_read_field_begin(VALUE protocol) {
  return rb_funcall(protocol, read_field_begin_method_id, 0);
}

VALUE default_read_field_end(VALUE protocol) {
  return rb_funcall(protocol, read_field_end_method_id, 0);
}

VALUE default_read_map_begin(VALUE protocol) {
  return rb_funcall(protocol, read_map_begin_method_id, 0);
}

VALUE default_read_map_end(VALUE protocol) {
  return rb_funcall(protocol, read_map_end_method_id, 0);
}

VALUE default_read_list_begin(VALUE protocol) {
  return rb_funcall(protocol, read_list_begin_method_id, 0);
}

VALUE default_read_list_end(VALUE protocol) {
  return rb_funcall(protocol, read_list_end_method_id, 0);
}

VALUE default_read_set_begin(VALUE protocol) {
  return rb_funcall(protocol, read_set_begin_method_id, 0);
}

VALUE default_read_set_end(VALUE protocol) {
  return rb_funcall(protocol, read_set_end_method_id, 0);
}

VALUE default_read_byte(VALUE protocol) {
  return rb_funcall(protocol, read_byte_method_id, 0);
}

VALUE default_read_bool(VALUE protocol) {
  return rb_funcall(protocol, read_bool_method_id, 0);
}

VALUE default_read_i16(VALUE protocol) {
  return rb_funcall(protocol, read_i16_method_id, 0);
}

VALUE default_read_i32(VALUE protocol) {
  return rb_funcall(protocol, read_i32_method_id, 0);
}

VALUE default_read_i64(VALUE protocol) {
  return rb_funcall(protocol, read_i64_method_id, 0);
}

VALUE default_read_double(VALUE protocol) {
  return rb_funcall(protocol, read_double_method_id, 0);
}

VALUE default_read_string(VALUE protocol) {
  return rb_funcall(protocol, read_string_method_id, 0);
}

VALUE default_read_struct_begin(VALUE protocol) {
  return rb_funcall(protocol, read_struct_begin_method_id, 0);
}

VALUE default_read_struct_end(VALUE protocol) {
  return rb_funcall(protocol, read_struct_end_method_id, 0);
}

static void set_default_proto_function_pointers() {
  default_mt = ALLOC(native_proto_method_table);

  default_mt->write_field_begin = default_write_field_begin;
  default_mt->write_field_stop = default_write_field_stop;
  default_mt->write_map_begin = default_write_map_begin;
  default_mt->write_map_end = default_write_map_end;
  default_mt->write_list_begin = default_write_list_begin;
  default_mt->write_list_end = default_write_list_end;
  default_mt->write_set_begin = default_write_set_begin;
  default_mt->write_set_end = default_write_set_end;
  default_mt->write_byte = default_write_byte;
  default_mt->write_bool = default_write_bool;
  default_mt->write_i16 = default_write_i16;
  default_mt->write_i32 = default_write_i32;
  default_mt->write_i64 = default_write_i64;
  default_mt->write_double = default_write_double;
  default_mt->write_string = default_write_string;
  default_mt->write_struct_begin = default_write_struct_begin;
  default_mt->write_struct_end = default_write_struct_end;
  default_mt->write_field_end = default_write_field_end;

  default_mt->read_struct_begin = default_read_struct_begin;
  default_mt->read_struct_end = default_read_struct_end;
  default_mt->read_field_begin = default_read_field_begin;
  default_mt->read_field_end = default_read_field_end;
  default_mt->read_map_begin = default_read_map_begin;
  default_mt->read_map_end = default_read_map_end;
  default_mt->read_list_begin = default_read_list_begin;
  default_mt->read_list_end = default_read_list_end;
  default_mt->read_set_begin = default_read_set_begin;
  default_mt->read_set_end = default_read_set_end;
  default_mt->read_byte = default_read_byte;
  default_mt->read_bool = default_read_bool;
  default_mt->read_i16 = default_read_i16;
  default_mt->read_i32 = default_read_i32;
  default_mt->read_i64 = default_read_i64;
  default_mt->read_double = default_read_double;
  default_mt->read_string = default_read_string;
}

// end default protocol methods


static VALUE rb_thrift_struct_write(VALUE self, VALUE protocol);
static void write_anything(int ttype, VALUE value, VALUE protocol, VALUE field_info);

VALUE get_field_value(VALUE obj, VALUE field_name) {
  char name_buf[RSTRING_LEN(field_name) + 1];
  
  name_buf[0] = '@';
  strlcpy(&name_buf[1], RSTRING_PTR(field_name), sizeof(name_buf));

  VALUE value = rb_ivar_get(obj, rb_intern(name_buf));
  
  return value;
}

static void write_container(int ttype, VALUE field_info, VALUE value, VALUE protocol) {
  int sz, i;
  
  if (ttype == TTYPE_MAP) {
    VALUE keys;
    VALUE key;
    VALUE val;

    Check_Type(value, T_HASH);
    
    VALUE key_info = rb_hash_aref(field_info, key_sym);
    VALUE keytype_value = rb_hash_aref(key_info, type_sym);
    int keytype = FIX2INT(keytype_value);
    
    VALUE value_info = rb_hash_aref(field_info, value_sym);
    VALUE valuetype_value = rb_hash_aref(value_info, type_sym);
    int valuetype = FIX2INT(valuetype_value);
    
    keys = rb_funcall(value, keys_method_id, 0);
    
    sz = RARRAY_LEN(keys);
    
    mt->write_map_begin(protocol, keytype_value, valuetype_value, INT2FIX(sz));
    
    for (i = 0; i < sz; i++) {
      key = rb_ary_entry(keys, i);
      val = rb_hash_aref(value, key);
      
      if (IS_CONTAINER(keytype)) {
        write_container(keytype, key_info, key, protocol);
      } else {
        write_anything(keytype, key, protocol, key_info);
      }
      
      if (IS_CONTAINER(valuetype)) {
        write_container(valuetype, value_info, val, protocol);
      } else {
        write_anything(valuetype, val, protocol, value_info);
      }
    }
    
    mt->write_map_end(protocol);
  } else if (ttype == TTYPE_LIST) {
    Check_Type(value, T_ARRAY);

    sz = RARRAY_LEN(value);

    VALUE element_type_info = rb_hash_aref(field_info, element_sym);
    VALUE element_type_value = rb_hash_aref(element_type_info, type_sym);
    int element_type = FIX2INT(element_type_value);
    
    mt->write_list_begin(protocol, element_type_value, INT2FIX(sz));
    for (i = 0; i < sz; ++i) {
      VALUE val = rb_ary_entry(value, i);
      if (IS_CONTAINER(element_type)) {
        write_container(element_type, element_type_info, val, protocol);
      } else {
        write_anything(element_type, val, protocol, element_type_info);
      }
    }
    mt->write_list_end(protocol);
  } else if (ttype == TTYPE_SET) {
    VALUE items;

    if (TYPE(value) == T_ARRAY) {
      items = value;
    } else {        
      if (rb_cSet == CLASS_OF(value)) {
        items = rb_funcall(value, entries_method_id, 0);
      } else {
        Check_Type(value, T_HASH);
        items = rb_funcall(value, keys_method_id, 0);
      }
    }

    sz = RARRAY_LEN(items);

    VALUE element_type_info = rb_hash_aref(field_info, element_sym);
    VALUE element_type_value = rb_hash_aref(element_type_info, type_sym);
    int element_type = FIX2INT(element_type_value);
    
    mt->write_set_begin(protocol, element_type_value, INT2FIX(sz));
    
    for (i = 0; i < sz; i++) {
      VALUE val = rb_ary_entry(items, i);
      if (IS_CONTAINER(element_type)) {
        write_container(element_type, element_type_info, val, protocol);
      } else {
        write_anything(element_type, val, protocol, element_type_info);
      }
    }
    
    mt->write_set_end(protocol);
  } else {
    rb_raise(rb_eNotImpError, "can't write container of type: %d", ttype);
  }
}

static void write_anything(int ttype, VALUE value, VALUE protocol, VALUE field_info) {
  if (ttype == TTYPE_BOOL) {
    mt->write_bool(protocol, value);
  } else if (ttype == TTYPE_BYTE) {
    mt->write_byte(protocol, value);
  } else if (ttype == TTYPE_I16) {
    mt->write_i16(protocol, value);
  } else if (ttype == TTYPE_I32) {
    mt->write_i32(protocol, value);
  } else if (ttype == TTYPE_I64) {
    mt->write_i64(protocol, value);
  } else if (ttype == TTYPE_DOUBLE) {
    mt->write_double(protocol, value);
  } else if (ttype == TTYPE_STRING) {
    mt->write_string(protocol, value);
  } else if (IS_CONTAINER(ttype)) {
    write_container(ttype, field_info, value, protocol);
  } else if (ttype == TTYPE_STRUCT) {
    rb_thrift_struct_write(value, protocol);
  } else {
    rb_raise(rb_eNotImpError, "Unknown type for binary_encoding: %d", ttype);
  }
}

static VALUE rb_thrift_struct_write(VALUE self, VALUE protocol) {
  // call validate
  rb_funcall(self, validate_method_id, 0);

  // check_native_proto_method_table(protocol);

  // write struct begin
  mt->write_struct_begin(protocol, rb_class_name(CLASS_OF(self)));

  // iterate through all the fields here
  VALUE struct_fields = STRUCT_FIELDS(self);
  VALUE struct_field_ids_unordered = rb_funcall(struct_fields, keys_method_id, 0);
  VALUE struct_field_ids_ordered = rb_funcall(struct_field_ids_unordered, sort_method_id, 0);

  int i = 0;
  for (i=0; i < RARRAY_LEN(struct_field_ids_ordered); i++) {
    VALUE field_id = rb_ary_entry(struct_field_ids_ordered, i);
    VALUE field_info = rb_hash_aref(struct_fields, field_id);

    VALUE ttype_value = rb_hash_aref(field_info, type_sym);
    int ttype = FIX2INT(ttype_value);
    VALUE field_name = rb_hash_aref(field_info, name_sym);
    VALUE field_value = get_field_value(self, field_name);

    if (!NIL_P(field_value)) {
      mt->write_field_begin(protocol, field_name, ttype_value, field_id);
      
      write_anything(ttype, field_value, protocol, field_info);
      
      mt->write_field_end(protocol);
    }
  }

  mt->write_field_stop(protocol);

  // write struct end
  mt->write_struct_end(protocol);

  return Qnil;
}

//-------------------------------------------
// Reading section
//-------------------------------------------

static VALUE rb_thrift_struct_read(VALUE self, VALUE protocol);

static void set_field_value(VALUE obj, VALUE field_name, VALUE value) {
  char name_buf[RSTRING_LEN(field_name) + 1];

  name_buf[0] = '@';
  strlcpy(&name_buf[1], RSTRING_PTR(field_name), sizeof(name_buf));

  rb_ivar_set(obj, rb_intern(name_buf), value);
}

static VALUE read_anything(VALUE protocol, int ttype, VALUE field_info) {
  VALUE result = Qnil;

  if (ttype == TTYPE_BOOL) {
    result = mt->read_bool(protocol);
  } else if (ttype == TTYPE_BYTE) {
    result = mt->read_byte(protocol);
  } else if (ttype == TTYPE_I16) {
    result = mt->read_i16(protocol);
  } else if (ttype == TTYPE_I32) {
    result = mt->read_i32(protocol);
  } else if (ttype == TTYPE_I64) {
    result = mt->read_i64(protocol);
  } else if (ttype == TTYPE_STRING) {
    result = mt->read_string(protocol);
  } else if (ttype == TTYPE_DOUBLE) {
    result = mt->read_double(protocol);
  } else if (ttype == TTYPE_STRUCT) {
    VALUE klass = rb_hash_aref(field_info, class_sym);
    result = rb_class_new_instance(0, NULL, klass);
    rb_thrift_struct_read(result, protocol);
  } else if (ttype == TTYPE_MAP) {
    int i;

    VALUE map_header = mt->read_map_begin(protocol);
    int key_ttype = FIX2INT(rb_ary_entry(map_header, 0));
    int value_ttype = FIX2INT(rb_ary_entry(map_header, 1));
    int num_entries = FIX2INT(rb_ary_entry(map_header, 2));

    VALUE key_info = rb_hash_aref(field_info, key_sym);
    VALUE value_info = rb_hash_aref(field_info, value_sym);

    result = rb_hash_new();

    for (i = 0; i < num_entries; ++i) {
      VALUE key, val;

      key = read_anything(protocol, key_ttype, key_info);
      val = read_anything(protocol, value_ttype, value_info);

      rb_hash_aset(result, key, val);
    }

    mt->read_map_end(protocol);
  } else if (ttype == TTYPE_LIST) {
    int i;

    VALUE list_header = mt->read_list_begin(protocol);
    int element_ttype = FIX2INT(rb_ary_entry(list_header, 0));
    int num_elements = FIX2INT(rb_ary_entry(list_header, 1));
    result = rb_ary_new2(num_elements);

    for (i = 0; i < num_elements; ++i) {
      rb_ary_push(result, read_anything(protocol, element_ttype, rb_hash_aref(field_info, element_sym)));
    }


    mt->read_list_end(protocol);
  } else if (ttype == TTYPE_SET) {
    VALUE items;
    int i;

    VALUE set_header = mt->read_set_begin(protocol);
    int element_ttype = FIX2INT(rb_ary_entry(set_header, 0));
    int num_elements = FIX2INT(rb_ary_entry(set_header, 1));
    items = rb_ary_new2(num_elements);

    for (i = 0; i < num_elements; ++i) {
      rb_ary_push(items, read_anything(protocol, element_ttype, rb_hash_aref(field_info, element_sym)));
    }


    mt->read_set_end(protocol);

    result = rb_class_new_instance(1, &items, rb_cSet);
  } else {
    rb_raise(rb_eNotImpError, "read_anything not implemented for type %d!", ttype);
  }

  return result;
}

static VALUE rb_thrift_struct_read(VALUE self, VALUE protocol) {
  // check_native_proto_method_table(protocol);

  // read struct begin
  mt->read_struct_begin(protocol);

  VALUE struct_fields = STRUCT_FIELDS(self);

  // read each field
  while (true) {
    VALUE field_header = mt->read_field_begin(protocol);
    VALUE field_type_value = rb_ary_entry(field_header, 1);
    int field_type = FIX2INT(field_type_value);

    if (field_type == TTYPE_STOP) {
      break;
    }

    // make sure we got a type we expected
    VALUE field_info = rb_hash_aref(struct_fields, rb_ary_entry(field_header, 2));

    if (!NIL_P(field_info)) {
      int specified_type = FIX2INT(rb_hash_aref(field_info, type_sym));
      if (field_type == specified_type) {
        // read the value
        VALUE name = rb_hash_aref(field_info, name_sym);
        set_field_value(self, name, read_anything(protocol, field_type, field_info));
      } else {
        rb_funcall(protocol, skip_method_id, 1, field_type_value);
      }
    } else {
      rb_funcall(protocol, skip_method_id, 1, field_type_value);
    }

    // read field end
    mt->read_field_end(protocol);
  }

  // read struct end
  mt->read_struct_end(protocol);

  // call validate
  rb_funcall(self, validate_method_id, 0);

  return Qnil;
}

void Init_struct() {
  VALUE struct_module = rb_const_get(thrift_module, rb_intern("Struct"));

  rb_define_method(struct_module, "write", rb_thrift_struct_write, 1);
  rb_define_method(struct_module, "read", rb_thrift_struct_read, 1);

  set_default_proto_function_pointers();
  mt = default_mt;
}

