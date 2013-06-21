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

#include "struct.h"
#include "constants.h"
#include "macros.h"
#include "strlcpy.h"

 #define DEBUG 0


 #if SIZEOF_VOIDP <= SIZEOF_LONG
  #define NUM2PTR(x) (void*)NUM2ULONG(x)
  #define PTR2NUM(x) ULONG2NUM((unsigned long)x)
#elif SIZEOF_VOIDP <= SIZEOF_LONG_LONG
  #define NUM2PTR(x) (void*)NUM2ULL(x)
  #define PTR2NUM(x) ULL2NUM((unsigned long long)x)
#else
 #error "Pointer size too large, could not determine a good way to convert a C pointer to a Ruby object"
#endif



#if DEBUG
  #define DEBUG_FUNCTION_ENTRY() printf("%s\n", __FUNCTION__);
  #define DEBUG_FUNCTION_PROGRES() printf("%s, %s:%d\n", __FILE__, __FUNCTION__, __LINE__);
#else
  #define DEBUG_FUNCTION_ENTRY()
  #define DEBUG_FUNCTION_PROGRES() 
#endif





static protocol_method_table default_table;

static ID get_protocol_method_table_ID;







static VALUE thrift_union_class;

static ID setfield_id;
static ID setvalue_id;

static ID to_s_method_id;
static ID name_to_id_method_id;
static ID sorted_field_ids_method_id;

#define IS_CONTAINER(ttype) ((ttype) == TTYPE_MAP || (ttype) == TTYPE_LIST || (ttype) == TTYPE_SET)
#define STRUCT_FIELDS(obj) rb_const_get(CLASS_OF(obj), fields_const_id)

//-------------------------------------------
// Writing section
//-------------------------------------------


static VALUE union_write (VALUE self, VALUE protocol, protocol_method_table *pmt);
static VALUE struct_write(VALUE self, VALUE protocol, protocol_method_table *pmt);
static void write_anything_pmt(int ttype, VALUE value, VALUE protocol, VALUE field_info, protocol_method_table *pmt);

static VALUE get_field_value(VALUE obj, VALUE field_name) {
  char name_buf[RSTRING_LEN(field_name) + 2];

  name_buf[0] = '@';
  strlcpy(&name_buf[1], RSTRING_PTR(field_name), RSTRING_LEN(field_name) + 1);

  VALUE value = rb_ivar_get(obj, rb_intern(name_buf));

  return value;
}

static void write_container(int ttype, VALUE field_info, VALUE value, VALUE protocol, protocol_method_table* pmt) {
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

    fastcall_call(pmt->write_map_begin, protocol, keytype_value, valuetype_value, INT2FIX(sz));

    for (i = 0; i < sz; i++) {
      key = rb_ary_entry(keys, i);
      val = rb_hash_aref(value, key);

      if (IS_CONTAINER(keytype)) {
        write_container(keytype, key_info, key, protocol, pmt);
      } else {
        write_anything_pmt(keytype, key, protocol, key_info, pmt);
      }

      if (IS_CONTAINER(valuetype)) {
        write_container(valuetype, value_info, val, protocol, pmt);
      } else {
        write_anything_pmt(valuetype, val, protocol, value_info, pmt);
      }
    }

    fastcall_call(pmt->write_map_end, protocol, Qnil);
  } else if (ttype == TTYPE_LIST) {
    Check_Type(value, T_ARRAY);

    sz = RARRAY_LEN(value);

    VALUE element_type_info = rb_hash_aref(field_info, element_sym);
    VALUE element_type_value = rb_hash_aref(element_type_info, type_sym);
    int element_type = FIX2INT(element_type_value);

    fastcall_call(pmt->write_list_begin, protocol, element_type_value, INT2FIX(sz));
    for (i = 0; i < sz; ++i) {
      VALUE val = rb_ary_entry(value, i);
      if (IS_CONTAINER(element_type)) {
        write_container(element_type, element_type_info, val, protocol, pmt);
      } else {
        write_anything_pmt(element_type, val, protocol, element_type_info, pmt);
      }
    }
    fastcall_call(pmt->write_list_end, protocol, Qnil);
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

    fastcall_call(pmt->write_set_begin, protocol, element_type_value, INT2FIX(sz));

    for (i = 0; i < sz; i++) {
      VALUE val = rb_ary_entry(items, i);
      if (IS_CONTAINER(element_type)) {
        write_container(element_type, element_type_info, val, protocol, pmt);
      } else {
        write_anything_pmt(element_type, val, protocol, element_type_info, pmt);
      }
    }

    fastcall_call(pmt->write_set_end, protocol, Qnil);
  } else {
    rb_raise(rb_eNotImpError, "can't write container of type: %d", ttype);
  }
}

static void write_anything_pmt(int ttype, VALUE value, VALUE protocol, VALUE field_info, protocol_method_table *pmt) {
  if (ttype == TTYPE_BOOL) {
    fastcall_call(pmt->write_bool, protocol, value);
  } else if (ttype == TTYPE_BYTE) {
    fastcall_call(pmt->write_byte, protocol, value);
  } else if (ttype == TTYPE_I16) {
    fastcall_call(pmt->write_i16, protocol, value);
  } else if (ttype == TTYPE_I32) {
    fastcall_call(pmt->write_i32, protocol, value);
  } else if (ttype == TTYPE_I64) {
    fastcall_call(pmt->write_i64, protocol, value);
  } else if (ttype == TTYPE_DOUBLE) {
    fastcall_call(pmt->write_double, protocol, value);
  } else if (ttype == TTYPE_STRING) {
    fastcall_call(pmt->write_string, protocol, value);
  } else if (IS_CONTAINER(ttype)) {
    write_container(ttype, field_info, value, protocol, pmt);
  } else if (ttype == TTYPE_STRUCT) {
    if (rb_obj_is_kind_of(value, thrift_union_class)) {
      union_write(value, protocol, pmt);
    } else {
      struct_write(value, protocol, pmt);
    }
  } else {
    rb_raise(rb_eNotImpError, "Unknown type for binary_encoding: %d", ttype);
  }
}



static VALUE struct_write(VALUE self, VALUE protocol, protocol_method_table* pmt)
{
  // call validate
  rb_funcall(self, validate_method_id, 0);

  // write struct begin
  fastcall_call(pmt->write_struct_begin, protocol, rb_class_name(CLASS_OF(self)));

  // iterate through all the fields here
  VALUE struct_fields = STRUCT_FIELDS(self);
  VALUE sorted_field_ids = rb_funcall(self, sorted_field_ids_method_id, 0);

  int i = 0;
  for (i=0; i < RARRAY_LEN(sorted_field_ids); i++) {
    VALUE field_id = rb_ary_entry(sorted_field_ids, i);

    VALUE field_info = rb_hash_aref(struct_fields, field_id);

    VALUE ttype_value = rb_hash_aref(field_info, type_sym);
    int ttype = FIX2INT(ttype_value);
    VALUE field_name = rb_hash_aref(field_info, name_sym);

    VALUE field_value = get_field_value(self, field_name);

    if (!NIL_P(field_value)) {
      fastcall_call(pmt->write_field_begin, protocol, field_name, ttype_value, field_id);

      write_anything_pmt(ttype, field_value, protocol, field_info, pmt);

      fastcall_call(pmt->write_field_end, protocol, Qnil);
    }
  }

  fastcall_call(pmt->write_field_stop, protocol, Qnil);

  // write struct end
  fastcall_call(pmt->write_struct_end, protocol, Qnil);

  return Qnil;
}


static VALUE rb_struct_write(VALUE self, VALUE protocol) {
  DEBUG_FUNCTION_ENTRY();
  protocol_method_table* pmt;

  //We haven't been supplied with a method table, try retrieving it...
  int has_gmt = rb_respond_to(protocol, rb_intern("get_protocol_method_table"));

  if (has_gmt)
  {
    pmt = NUM2PTR(rb_funcall(protocol, get_protocol_method_table_ID, 0));
  }
  else
  {
    pmt = &default_table;
  }

  return struct_write(self, protocol, pmt);
}

//-------------------------------------------
// Reading section
//-------------------------------------------

static VALUE rb_union_read(VALUE self, VALUE protocol);
static VALUE rb_struct_read(VALUE self, VALUE protocol);
static VALUE struct_read(VALUE self, VALUE protocol, protocol_method_table *pmt);
static VALUE union_read(VALUE self, VALUE protocol, protocol_method_table *pmt);

static void skip_map_contents(VALUE protocol, VALUE key_type_value, VALUE value_type_value, int size);
static void skip_list_or_set_contents(VALUE protocol, VALUE element_type_value, int size);

static void set_field_value(VALUE obj, VALUE field_name, VALUE value) {
  char name_buf[RSTRING_LEN(field_name) + 2];

  name_buf[0] = '@';
  strlcpy(&name_buf[1], RSTRING_PTR(field_name), RSTRING_LEN(field_name)+1);

  rb_ivar_set(obj, rb_intern(name_buf), value);
}

// Helper method to skip the contents of a map (assumes the map header has been read).
static void skip_map_contents(VALUE protocol, VALUE key_type_value, VALUE value_type_value, int size) {
  int i;
  for (i = 0; i < size; i++) {
    rb_funcall(protocol, skip_method_id, 1, key_type_value);
    rb_funcall(protocol, skip_method_id, 1, value_type_value);
  }
}

// Helper method to skip the contents of a list or set (assumes the list/set header has been read).
static void skip_list_or_set_contents(VALUE protocol, VALUE element_type_value, int size) {
  int i;
  for (i = 0; i < size; i++) {
    rb_funcall(protocol, skip_method_id, 1, element_type_value);
  }
}

static VALUE read_anything_pmt(VALUE protocol, int ttype, VALUE field_info, protocol_method_table *pmt) {
  VALUE result = Qnil;

  if (ttype == TTYPE_BOOL) {
    result = fastcall_call(pmt->read_bool, protocol, Qnil);
  } else if (ttype == TTYPE_BYTE) {
    result = fastcall_call(pmt->read_byte, protocol, Qnil);
  } else if (ttype == TTYPE_I16) {
    result = fastcall_call(pmt->read_i16, protocol, Qnil);
  } else if (ttype == TTYPE_I32) {
    result = fastcall_call(pmt->read_i32, protocol, Qnil);
  } else if (ttype == TTYPE_I64) {
    result = fastcall_call(pmt->read_i64, protocol, Qnil);
  } else if (ttype == TTYPE_STRING) {
    result = fastcall_call(pmt->read_string, protocol, Qnil);
  } else if (ttype == TTYPE_DOUBLE) {
    result = fastcall_call(pmt->read_double, protocol, Qnil);
  } else if (ttype == TTYPE_STRUCT) {
    VALUE klass = rb_hash_aref(field_info, class_sym);
    result = rb_class_new_instance(0, NULL, klass);

    if (rb_obj_is_kind_of(result, thrift_union_class)) {
      union_read(result, protocol, pmt);
    } else {
      struct_read(result, protocol, pmt);
    }
  } else if (ttype == TTYPE_MAP) {
    int i;

    VALUE map_header = fastcall_call(pmt->read_map_begin, protocol, Qnil);
    int key_ttype = FIX2INT(rb_ary_entry(map_header, 0));
    int value_ttype = FIX2INT(rb_ary_entry(map_header, 1));
    int num_entries = FIX2INT(rb_ary_entry(map_header, 2));

    // Check the declared key and value types against the expected ones and skip the map contents
    // if the types don't match.
    VALUE key_info = rb_hash_aref(field_info, key_sym);
    VALUE value_info = rb_hash_aref(field_info, value_sym);

    if (!NIL_P(key_info) && !NIL_P(value_info)) {
      int specified_key_type = FIX2INT(rb_hash_aref(key_info, type_sym));
      int specified_value_type = FIX2INT(rb_hash_aref(value_info, type_sym));
      if (num_entries == 0 || (specified_key_type == key_ttype && specified_value_type == value_ttype)) {
        result = rb_hash_new();

        for (i = 0; i < num_entries; ++i) {
          VALUE key, val;

          key = read_anything_pmt(protocol, key_ttype, key_info, pmt);
          val = read_anything_pmt(protocol, value_ttype, value_info, pmt);

          rb_hash_aset(result, key, val);
        }
      } else {
        skip_map_contents(protocol, INT2FIX(key_ttype), INT2FIX(value_ttype), num_entries);
      }
    } else {
      skip_map_contents(protocol, INT2FIX(key_ttype), INT2FIX(value_ttype), num_entries);
    }

    fastcall_call(pmt->read_map_end, protocol, Qnil);
  } else if (ttype == TTYPE_LIST) {
    int i;

    VALUE list_header = fastcall_call(pmt->read_list_begin, protocol, Qnil);
    int element_ttype = FIX2INT(rb_ary_entry(list_header, 0));
    int num_elements = FIX2INT(rb_ary_entry(list_header, 1));

    // Check the declared element type against the expected one and skip the list contents
    // if the types don't match.
    VALUE element_info = rb_hash_aref(field_info, element_sym);
    if (!NIL_P(element_info)) {
      int specified_element_type = FIX2INT(rb_hash_aref(element_info, type_sym));
      if (specified_element_type == element_ttype) {
        result = rb_ary_new2(num_elements);

        for (i = 0; i < num_elements; ++i) {
          rb_ary_push(result, read_anything_pmt(protocol, element_ttype, rb_hash_aref(field_info, element_sym), pmt));
        }
      } else {
        skip_list_or_set_contents(protocol, INT2FIX(element_ttype), num_elements);
      }
    } else {
      skip_list_or_set_contents(protocol, INT2FIX(element_ttype), num_elements);
    }

    fastcall_call(pmt->read_list_end, protocol, Qnil);
  } else if (ttype == TTYPE_SET) {
    VALUE items;
    int i;

    VALUE set_header = fastcall_call(pmt->read_set_begin, protocol, Qnil);
    int element_ttype = FIX2INT(rb_ary_entry(set_header, 0));
    int num_elements = FIX2INT(rb_ary_entry(set_header, 1));

    // Check the declared element type against the expected one and skip the set contents
    // if the types don't match.
    VALUE element_info = rb_hash_aref(field_info, element_sym);
    if (!NIL_P(element_info)) {
      int specified_element_type = FIX2INT(rb_hash_aref(element_info, type_sym));
      if (specified_element_type == element_ttype) {
        items = rb_ary_new2(num_elements);

        for (i = 0; i < num_elements; ++i) {
          rb_ary_push(items, read_anything_pmt(protocol, element_ttype, rb_hash_aref(field_info, element_sym), pmt));
        }

        result = rb_class_new_instance(1, &items, rb_cSet);
      } else {
        skip_list_or_set_contents(protocol, INT2FIX(element_ttype), num_elements);
      }
    } else {
      skip_list_or_set_contents(protocol, INT2FIX(element_ttype), num_elements);
    }

    fastcall_call(pmt->read_set_end, protocol, Qnil);
  } else {
    rb_raise(rb_eNotImpError, "read_anything not implemented for type %d!", ttype);
  }

  return result;
}

static VALUE struct_read(VALUE self, VALUE protocol, protocol_method_table *pmt)
{
  // read struct begin
  fastcall_call(pmt->read_struct_begin, protocol, Qnil);

  VALUE struct_fields = STRUCT_FIELDS(self);

  // read each field
  while (true) {
    VALUE field_header = fastcall_call(pmt->read_field_begin, protocol, Qnil);
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
        set_field_value(self, name, read_anything_pmt(protocol, field_type, field_info, pmt));
      } else {
        rb_funcall(protocol, skip_method_id, 1, field_type_value);
      }
    } else {
      rb_funcall(protocol, skip_method_id, 1, field_type_value);
    }

    // read field end
    fastcall_call(pmt->read_field_end, protocol, Qnil);
  }

  // read struct end
  fastcall_call(pmt->read_struct_end, protocol, Qnil);

  // call validate
  rb_funcall(self, validate_method_id, 0);

  return Qnil;
}

static VALUE rb_struct_read(VALUE self, VALUE protocol) {
  DEBUG_FUNCTION_ENTRY();
  protocol_method_table *pmt;

  //We haven't been supplied with a method table, try retrieving it...
  int has_gmt = rb_respond_to(protocol, rb_intern("get_protocol_method_table"));

  if (has_gmt)
    pmt = NUM2PTR(rb_funcall(protocol, get_protocol_method_table_ID, 0));
  else
    pmt = &default_table;

  return struct_read(self, protocol, pmt);
}




// --------------------------------
// Union section
// --------------------------------

static VALUE union_read(VALUE self, VALUE protocol, protocol_method_table *pmt)
{
  DEBUG_FUNCTION_ENTRY();

  // read struct begin
  fastcall_call(pmt->read_struct_begin, protocol, Qnil);

  VALUE struct_fields = STRUCT_FIELDS(self);

  VALUE field_header = fastcall_call(pmt->read_field_begin, protocol, Qnil);
  VALUE field_type_value = rb_ary_entry(field_header, 1);
  int field_type = FIX2INT(field_type_value);

  // make sure we got a type we expected
  VALUE field_info = rb_hash_aref(struct_fields, rb_ary_entry(field_header, 2));

  if (!NIL_P(field_info)) {
    int specified_type = FIX2INT(rb_hash_aref(field_info, type_sym));
    if (field_type == specified_type) {
      // read the value
      VALUE name = rb_hash_aref(field_info, name_sym);
      rb_iv_set(self, "@setfield", ID2SYM(rb_intern(RSTRING_PTR(name))));
      rb_iv_set(self, "@value", read_anything_pmt(protocol, field_type, field_info, pmt));
    } else {
      rb_funcall(protocol, skip_method_id, 1, field_type_value);
    }
  } else {
    rb_funcall(protocol, skip_method_id, 1, field_type_value);
  }

  // read field end
  fastcall_call(pmt->read_field_end, protocol, Qnil);

  field_header = fastcall_call(pmt->read_field_begin, protocol, Qnil);
  field_type_value = rb_ary_entry(field_header, 1);
  field_type = FIX2INT(field_type_value);

  if (field_type != TTYPE_STOP) {
    rb_raise(rb_eRuntimeError, "too many fields in union!");
  }

  // read struct end
  fastcall_call(pmt->read_struct_end, protocol, Qnil);

  // call validate
  rb_funcall(self, validate_method_id, 0);

  return Qnil;
}


static VALUE rb_union_read(VALUE self, VALUE protocol) {
  DEBUG_FUNCTION_ENTRY();
  protocol_method_table* pmt;

  //We haven't been supplied with a method table, try retrieving it...
  int has_gmt = rb_respond_to(protocol, rb_intern("get_protocol_method_table"));

  if (has_gmt)
  {
    pmt = NUM2PTR(rb_funcall(protocol, get_protocol_method_table_ID, 0));
  }
  else
  {
    pmt = &default_table;
  }

  return union_read(self, protocol, pmt);
}

static VALUE union_write(VALUE self, VALUE protocol, protocol_method_table *pmt)
{
  // call validate
  rb_funcall(self, validate_method_id, 0);


  // write struct begin
  fastcall_call(pmt->write_struct_begin, protocol, rb_class_name(CLASS_OF(self)));

  VALUE struct_fields = STRUCT_FIELDS(self);

  VALUE setfield = rb_ivar_get(self, setfield_id);
  VALUE setvalue = rb_ivar_get(self, setvalue_id);
  VALUE field_id = rb_funcall(self, name_to_id_method_id, 1, rb_funcall(setfield, to_s_method_id, 0));

  VALUE field_info = rb_hash_aref(struct_fields, field_id);

  VALUE ttype_value = rb_hash_aref(field_info, type_sym);
  int ttype = FIX2INT(ttype_value);

  fastcall_call(pmt->write_field_begin, protocol, setfield, ttype_value, field_id);

  write_anything_pmt(ttype, setvalue, protocol, field_info, pmt);

  fastcall_call(pmt->write_field_end, protocol, Qnil);

  fastcall_call(pmt->write_field_stop, protocol, Qnil);

  // write struct end
  fastcall_call(pmt->write_struct_end, protocol, Qnil);

  return Qnil;
}

static VALUE rb_union_write(VALUE self, VALUE protocol) {
  DEBUG_FUNCTION_ENTRY();
  protocol_method_table* pmt;

  //We haven't been supplied with a method table, try retrieving it...
  int has_gmt = rb_respond_to(protocol, rb_intern("get_protocol_method_table"));

  if (has_gmt)
  {
    pmt = NUM2PTR(rb_funcall(protocol, get_protocol_method_table_ID, 0));
  }
  else
  {
    pmt = &default_table;
  }

  return union_write(self, protocol, pmt);
}

static void Init_default_table()
{
  fastcall_init_ruby(default_table.write_bool, rb_intern("write_bool"), 1);
  fastcall_init_ruby(default_table.write_byte, rb_intern("write_byte"), 1);
  fastcall_init_ruby(default_table.write_double, rb_intern("write_double"), 1);
  fastcall_init_ruby(default_table.write_i16, rb_intern("write_i16"), 1);
  fastcall_init_ruby(default_table.write_i32, rb_intern("write_i32"), 1);
  fastcall_init_ruby(default_table.write_i64, rb_intern("write_i64"), 1);
  fastcall_init_ruby(default_table.write_set_begin, rb_intern("write_set_begin"), 2);
  fastcall_init_ruby(default_table.write_set_end, rb_intern("write_set_end"), 0);
  fastcall_init_ruby(default_table.write_map_begin, rb_intern("write_map_begin"), 3);
  fastcall_init_ruby(default_table.write_map_end, rb_intern("write_map_end"), 0);
  fastcall_init_ruby(default_table.write_list_begin, rb_intern("write_list_begin"), 2);
  fastcall_init_ruby(default_table.write_list_end, rb_intern("write_list_end"), 0);
  fastcall_init_ruby(default_table.write_field_begin, rb_intern("write_field_begin"), 3);
  fastcall_init_ruby(default_table.write_field_end, rb_intern("write_field_end"), 0);
  fastcall_init_ruby(default_table.write_field_stop, rb_intern("write_field_stop"), 0);
  fastcall_init_ruby(default_table.write_struct_begin, rb_intern("write_struct_begin"), 1);
  fastcall_init_ruby(default_table.write_struct_end, rb_intern("write_struct_end"), 0);
  fastcall_init_ruby(default_table.write_string, rb_intern("write_string"), 1);


  fastcall_init_ruby(default_table.read_bool, rb_intern("read_bool"), 0);
  fastcall_init_ruby(default_table.read_byte, rb_intern("read_byte"), 0);
  fastcall_init_ruby(default_table.read_double, rb_intern("read_double"), 0);
  fastcall_init_ruby(default_table.read_i16, rb_intern("read_i16"), 0);
  fastcall_init_ruby(default_table.read_i32, rb_intern("read_i32"), 0);
  fastcall_init_ruby(default_table.read_i64, rb_intern("read_i64"), 0);
  fastcall_init_ruby(default_table.read_set_begin, rb_intern("read_set_begin"), 0);
  fastcall_init_ruby(default_table.read_set_end, rb_intern("read_set_end"), 0);
  fastcall_init_ruby(default_table.read_map_begin, rb_intern("read_map_begin"), 0);
  fastcall_init_ruby(default_table.read_map_end, rb_intern("read_map_end"), 0);
  fastcall_init_ruby(default_table.read_list_begin, rb_intern("read_list_begin"), 0);
  fastcall_init_ruby(default_table.read_list_end, rb_intern("read_list_end"), 0);
  fastcall_init_ruby(default_table.read_field_begin, rb_intern("read_field_begin"), 0);
  fastcall_init_ruby(default_table.read_field_end, rb_intern("read_field_end"), 0);
  fastcall_init_ruby(default_table.read_struct_begin, rb_intern("read_struct_begin"), 0);
  fastcall_init_ruby(default_table.read_struct_end, rb_intern("read_struct_end"), 0);
  fastcall_init_ruby(default_table.read_string, rb_intern("read_string"), 0);
}

void Init_struct() {

  Init_default_table();

  VALUE struct_module = rb_const_get(thrift_module, rb_intern("Struct"));

  rb_define_method(struct_module, "write", rb_struct_write, 1);
  rb_define_method(struct_module, "read", rb_struct_read, 1);

  thrift_union_class = rb_const_get(thrift_module, rb_intern("Union"));

  rb_define_method(thrift_union_class, "write", rb_union_write, 1);
  rb_define_method(thrift_union_class, "read", rb_union_read, 1);

  get_protocol_method_table_ID = rb_intern("get_protocol_method_table");
  
  setfield_id = rb_intern("@setfield");
  setvalue_id = rb_intern("@value");

  to_s_method_id = rb_intern("to_s");
  name_to_id_method_id = rb_intern("name_to_id");
  sorted_field_ids_method_id = rb_intern("sorted_field_ids");
}
