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
#include "ruby_ptr.h"
#include "struct_metadata.h"

#define DEBUG 0

#include "debug.h"







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
static void write_anything(VALUE value, VALUE protocol, field_metadata* fmd, protocol_method_table *pmt);

static void write_container(field_metadata *fmd, VALUE value, VALUE protocol, protocol_method_table* pmt) {
  int sz, i;

  DEBUG_FUNCTION_ENTRY();

  if (fmd->type == TTYPE_MAP) {
  DEBUG_FUNCTION_PROGRESS();

    VALUE keys;
    VALUE key;
    VALUE val;

    Check_Type(value, T_HASH);

    field_metadata* key_md = fmd->key;
    int keytype = key_md->type;

    field_metadata* value_md = fmd->value;
    int valuetype = value_md->type;


    keys = rb_funcall(value, keys_method_id, 0);
    sz = RARRAY_LEN(keys);

    fastcall_call(pmt->write_map_begin, protocol, INT2FIX(keytype), INT2FIX(valuetype), INT2FIX(sz));

    for (i = 0; i < sz; i++) {
      key = rb_ary_entry(keys, i);
      val = rb_hash_aref(value, key);

      write_anything(key, protocol, key_md, pmt);
      write_anything(val, protocol, value_md, pmt);
    }

    fastcall_call(pmt->write_map_end, protocol, Qnil);
  } else if (fmd->type == TTYPE_LIST) {
  DEBUG_FUNCTION_PROGRESS();
    Check_Type(value, T_ARRAY);
  DEBUG_FUNCTION_PROGRESS();

    sz = RARRAY_LEN(value);

  DEBUG_FUNCTION_PROGRESS();
    field_metadata* element_md = fmd->element;
    int elementtype = element_md->type;
  DEBUG_FUNCTION_PROGRESS();

    fastcall_call(pmt->write_list_begin, protocol, INT2FIX(elementtype), INT2FIX(sz));
  DEBUG_FUNCTION_PROGRESS();
    for (i = 0; i < sz; ++i) {
  DEBUG_FUNCTION_PROGRESS();
      VALUE val = rb_ary_entry(value, i);
      
  DEBUG_FUNCTION_PROGRESS();
      write_anything(val, protocol, element_md, pmt);
  DEBUG_FUNCTION_PROGRESS();
      
    }
  DEBUG_FUNCTION_PROGRESS();
    fastcall_call(pmt->write_list_end, protocol, Qnil);
  } else if (fmd->type == TTYPE_SET) {
      DEBUG_FUNCTION_PROGRESS();
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

    field_metadata* element_md = fmd->element;
    int elementtype = element_md->type;

    fastcall_call(pmt->write_set_begin, protocol, INT2FIX(elementtype), INT2FIX(sz));

    for (i = 0; i < sz; i++) {
      VALUE val = rb_ary_entry(items, i);

      write_anything(val, protocol, element_md, pmt);
    }

    fastcall_call(pmt->write_set_end, protocol, Qnil);
  } else {
    rb_raise(rb_eNotImpError, "can't write container of type: %d", fmd->type);
  }

  DEBUG_FUNCTION_EXIT();
}

static void write_anything(VALUE value, VALUE protocol, field_metadata* fmd, protocol_method_table *pmt) {
  DEBUG_FUNCTION_ENTRY();

  if (fmd->type == TTYPE_BOOL) {
    fastcall_call(pmt->write_bool, protocol, value);
  } else if (fmd->type == TTYPE_BYTE) {
    fastcall_call(pmt->write_byte, protocol, value);
  } else if (fmd->type == TTYPE_I16) {
    fastcall_call(pmt->write_i16, protocol, value);
  } else if (fmd->type == TTYPE_I32) {
    fastcall_call(pmt->write_i32, protocol, value);
  } else if (fmd->type == TTYPE_I64) {
    fastcall_call(pmt->write_i64, protocol, value);
  } else if (fmd->type == TTYPE_DOUBLE) {
    fastcall_call(pmt->write_double, protocol, value);
  } else if (fmd->type == TTYPE_STRING) {
    fastcall_call(pmt->write_string, protocol, value);
  } else if (IS_CONTAINER(fmd->type)) {
    write_container(fmd, value, protocol, pmt);
  } else if (fmd->type == TTYPE_STRUCT) {
    if (rb_obj_is_kind_of(value, thrift_union_class)) {
      union_write(value, protocol, pmt);
    } else {
      struct_write(value, protocol, pmt);
    }
  } else {
    rb_raise(rb_eNotImpError, "Unknown type for binary_encoding: %d", fmd->type);
  }

  DEBUG_FUNCTION_EXIT();
}



static VALUE struct_write(VALUE self, VALUE protocol, protocol_method_table* pmt)
{
  DEBUG_FUNCTION_ENTRY();
  // call validate
  rb_funcall(self, validate_method_id, 0);

  // write struct begin
  fastcall_call(pmt->write_struct_begin, protocol, rb_class_name(CLASS_OF(self)));


  // iterate through all the fields here
  struct_metadata* md = getStructMetadata(CLASS_OF(self));

  int i = 0;
  for (i=0; i < getMetadataFieldCount(md); i++) {

    field_metadata* fmd = getFieldMetadataByIndex(md, i);

    DEBUGF("name=%s", fmd->name);
    VALUE field_value = rb_ivar_get(self, fmd->name_id);
    VALUE field_name = rb_str_new_cstr(fmd->name);
    DEBUGF("type=%d", TYPE(field_value));
    DEBUG_FUNCTION_PROGRESS();

    if (!NIL_P(field_value)) {
    DEBUG_FUNCTION_PROGRESS();
      fastcall_call(pmt->write_field_begin, protocol, field_name, INT2NUM(fmd->type), INT2NUM(fmd->id));
    DEBUG_FUNCTION_PROGRESS();

      write_anything(field_value, protocol, fmd, pmt);
    DEBUG_FUNCTION_PROGRESS();

      fastcall_call(pmt->write_field_end, protocol, Qnil);
    DEBUG_FUNCTION_PROGRESS();
    }
  }

    DEBUG_FUNCTION_PROGRESS();
  fastcall_call(pmt->write_field_stop, protocol, Qnil);
    DEBUG_FUNCTION_PROGRESS();

  // write struct end
  fastcall_call(pmt->write_struct_end, protocol, Qnil);
    DEBUG_FUNCTION_PROGRESS();

  fastcall_call(pmt->flush, protocol, Qnil);

  DEBUG_FUNCTION_EXIT();
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

  struct_write(self, protocol, pmt);
  DEBUG_FUNCTION_EXIT();

  return Qnil;
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

static VALUE read_anything(VALUE protocol, field_metadata* fmd, protocol_method_table *pmt) {
  VALUE result = Qnil;

  if (fmd->type == TTYPE_BOOL) {
    result = fastcall_call(pmt->read_bool, protocol, Qnil);
  } else if (fmd->type == TTYPE_BYTE) {
    result = fastcall_call(pmt->read_byte, protocol, Qnil);
  } else if (fmd->type == TTYPE_I16) {
    result = fastcall_call(pmt->read_i16, protocol, Qnil);
  } else if (fmd->type == TTYPE_I32) {
    result = fastcall_call(pmt->read_i32, protocol, Qnil);
  } else if (fmd->type == TTYPE_I64) {
    result = fastcall_call(pmt->read_i64, protocol, Qnil);
  } else if (fmd->type == TTYPE_STRING) {
    result = fastcall_call(pmt->read_string, protocol, Qnil);
  } else if (fmd->type == TTYPE_DOUBLE) {
    result = fastcall_call(pmt->read_double, protocol, Qnil);
  } else if (fmd->type == TTYPE_STRUCT) {

    result = rb_class_new_instance(0, NULL, fmd->klass_v);

    if (rb_obj_is_kind_of(result, thrift_union_class)) {
      union_read(result, protocol, pmt);
    } else {
      struct_read(result, protocol, pmt);
    }
  } else if (fmd->type == TTYPE_MAP) {
    int i;

    VALUE map_header = fastcall_call(pmt->read_map_begin, protocol, Qnil);
    int key_ttype = FIX2INT(rb_ary_entry(map_header, 0));
    int value_ttype = FIX2INT(rb_ary_entry(map_header, 1));
    int num_entries = FIX2INT(rb_ary_entry(map_header, 2));

    // Check the declared key and value types against the expected ones and skip the map contents
    // if the types don't match.
    field_metadata* key_md = fmd->key;
    field_metadata* value_md = fmd->value;

    if (key_md && value_md) {
      int specified_key_type = key_md->type;
      int specified_value_type = value_md->type;
      if (num_entries == 0 || (specified_key_type == key_ttype && specified_value_type == value_ttype)) {
        result = rb_hash_new();

        for (i = 0; i < num_entries; ++i) {
          VALUE key, val;

          key = read_anything(protocol, key_md, pmt);
          val = read_anything(protocol, value_md, pmt);

          rb_hash_aset(result, key, val);
        }
      } else {
        skip_map_contents(protocol, INT2FIX(key_ttype), INT2FIX(value_ttype), num_entries);
      }
    } else {
      skip_map_contents(protocol, INT2FIX(key_ttype), INT2FIX(value_ttype), num_entries);
    }

    fastcall_call(pmt->read_map_end, protocol, Qnil);
  } else if (fmd->type == TTYPE_LIST) {
    int i;

    VALUE list_header = fastcall_call(pmt->read_list_begin, protocol, Qnil);
    int element_ttype = FIX2INT(rb_ary_entry(list_header, 0));
    int num_elements = FIX2INT(rb_ary_entry(list_header, 1));

    // Check the declared element type against the expected one and skip the list contents
    // if the types don't match.
    field_metadata* element_md = fmd->element;

    if (element_md) {
      int specified_element_type = element_md->type;
      if (specified_element_type == element_ttype) {
        result = rb_ary_new2(num_elements);

        for (i = 0; i < num_elements; ++i) {
          rb_ary_push(result, read_anything(protocol, element_md, pmt));
        }
      } else {
        skip_list_or_set_contents(protocol, INT2FIX(element_ttype), num_elements);
      }
    } else {
      skip_list_or_set_contents(protocol, INT2FIX(element_ttype), num_elements);
    }

    fastcall_call(pmt->read_list_end, protocol, Qnil);
  } else if (fmd->type == TTYPE_SET) {
    VALUE items;
    int i;

    VALUE set_header = fastcall_call(pmt->read_set_begin, protocol, Qnil);
    int element_ttype = FIX2INT(rb_ary_entry(set_header, 0));
    int num_elements = FIX2INT(rb_ary_entry(set_header, 1));

    // Check the declared element type against the expected one and skip the set contents
    // if the types don't match.
    field_metadata* element_md = fmd->element;
    if (element_md) {
      int specified_element_type = element_md->type;
      if (specified_element_type == element_ttype) {
        items = rb_ary_new2(num_elements);

        for (i = 0; i < num_elements; ++i) {
          rb_ary_push(items, read_anything(protocol, element_md, pmt));
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
    rb_raise(rb_eNotImpError, "read_anything not implemented for type %d!", fmd->type);
  }

  return result;
}

static VALUE struct_read(VALUE self, VALUE protocol, protocol_method_table *pmt)
{
  // read struct begin
  fastcall_call(pmt->read_struct_begin, protocol, Qnil);

  struct_metadata* md = getStructMetadata(CLASS_OF(self));

  // read each field
  while (true) {
    VALUE field_header = fastcall_call(pmt->read_field_begin, protocol, Qnil);

    VALUE field_type_value = rb_ary_entry(field_header, 1);
    VALUE field_id_value = rb_ary_entry(field_header, 2);

    int field_type = FIX2INT(field_type_value);
    if (field_type == TTYPE_STOP) {
      break;
    }


    // make sure we got a type we expected
    int field_id = FIX2INT(field_id_value);
    field_metadata *fmd = getFieldMetadataByID(md, field_id);

    if (fmd) {
      int specified_type = fmd->type;
      if (field_type == specified_type) {
        // read the value
        rb_ivar_set(self, fmd->name_id, read_anything(protocol, fmd, pmt));
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

  VALUE ret = struct_read(self, protocol, pmt);

  DEBUG_FUNCTION_EXIT();
  return ret;
}




// --------------------------------
// Union section
// --------------------------------

static VALUE union_read(VALUE self, VALUE protocol, protocol_method_table *pmt)
{
  DEBUG_FUNCTION_ENTRY();

  // read struct begin
  fastcall_call(pmt->read_struct_begin, protocol, Qnil);

  struct_metadata* md = getStructMetadata(CLASS_OF(self));

  VALUE field_header = fastcall_call(pmt->read_field_begin, protocol, Qnil);

  VALUE field_type_value = rb_ary_entry(field_header, 1);
  VALUE field_id_value = rb_ary_entry(field_header, 2);

  int field_type = FIX2INT(field_type_value);
  int field_id = FIX2INT(field_id_value);


  field_metadata *fmd = getFieldMetadataByID(md, field_id);

  // make sure we got a type we expected
  if (fmd) {
    int specified_type = fmd->type;
    if (field_type == specified_type) {
      // read the value
      char* name = fmd->name;
      rb_iv_set(self, "@setfield", ID2SYM(rb_intern(name)));
      rb_iv_set(self, "@value", read_anything(protocol, fmd, pmt));
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

  DEBUG_FUNCTION_EXIT();
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

  VALUE ret = union_read(self, protocol, pmt);

  DEBUG_FUNCTION_EXIT();
  return ret;
}

static VALUE union_write(VALUE self, VALUE protocol, protocol_method_table *pmt)
{
  DEBUG_FUNCTION_ENTRY();
  // call validate
  rb_funcall(self, validate_method_id, 0);


  // write struct begin
  fastcall_call(pmt->write_struct_begin, protocol, rb_class_name(CLASS_OF(self)));

  struct_metadata* md = getStructMetadata(CLASS_OF(self));

  VALUE setfield = rb_ivar_get(self, setfield_id);
  VALUE setvalue = rb_ivar_get(self, setvalue_id);

  field_metadata* fmd = getFieldMetadataByName(md, RSTRING_PTR(rb_funcall(setfield, to_s_method_id, 0)));

  int ttype = fmd->type;
  int field_id = fmd->id;

  fastcall_call(pmt->write_field_begin, protocol, setfield, INT2NUM(ttype), INT2NUM(field_id));

  write_anything(setvalue, protocol, fmd, pmt);

  fastcall_call(pmt->write_field_end, protocol, Qnil);

  fastcall_call(pmt->write_field_stop, protocol, Qnil);

  // write struct end
  fastcall_call(pmt->write_struct_end, protocol, Qnil);

  fastcall_call(pmt->flush, protocol, Qnil);
  DEBUG_FUNCTION_EXIT();
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

  VALUE ret = union_write(self, protocol, pmt);
  DEBUG_FUNCTION_EXIT();
  return ret;
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

  fastcall_init_ruby(default_table.flush, rb_intern("flush"), 0);
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
