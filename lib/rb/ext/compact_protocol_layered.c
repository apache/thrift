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
#include <stdbool.h>
#include <stdint.h>
#include <constants.h>
#include <struct.h>
#include <bytes.h>
#include "fastcall.h"
#include "struct.h"
#include "ruby_ptr.h"

#define DEBUG 0
#include "debug.h"

#include "protocol_transfer.h"
#include "compact_protocol_layered.h"


#define CHECK_NIL(obj) if (NIL_P(obj)) { rb_raise(rb_eStandardError, "nil argument not allowed!");}


static VALUE rb_native_qmark(VALUE self) {
  return Qtrue;
}

static protocol_method_table method_table;

static int VERSION;
static int VERSION_MASK;
static int TYPE_MASK;
static int TYPE_SHIFT_AMOUNT;
static int PROTOCOL_ID;

#define CTYPE_BOOLEAN_TRUE 0x01
#define CTYPE_BOOLEAN_FALSE 0x02
#define CTYPE_BYTE 0x03
#define CTYPE_I16 0x04
#define CTYPE_I32 0x05
#define CTYPE_I64 0x06
#define CTYPE_DOUBLE 0x07
#define CTYPE_BINARY 0x08
#define CTYPE_LIST 0x09
#define CTYPE_SET 0x0A
#define CTYPE_MAP 0x0B
#define CTYPE_STRUCT 0x0C

static VALUE cdata_id;



static VALUE write_i16(compact_data *cdata, VALUE i16);
static VALUE write_i32(compact_data *cdata, VALUE i32);


/* Implements a stack to keep track of the last sent ID */
void push_last_field_id(compact_data* cdata, int id)
{
  DEBUG_FUNCTION_ENTRY();
  if (cdata->last_field_index >= MAX_STRUCT_DEPTH) rb_raise(rb_eStandardError, "Structure nesting is too deep! Increase value of MAX_STRUCT_DEPTH in layered.h!");

  cdata->last_field_id[cdata->last_field_index] = id;
  cdata->last_field_index++;

  DEBUG_FUNCTION_EXIT();
}

/* Implements a stack to keep track of the last sent ID */
int pop_last_field_id(compact_data* cdata)
{
        int lid;
        DEBUG_FUNCTION_ENTRY();

        if (cdata->last_field_index <= 0) rb_bug("Structure nesting corrupt!");

        cdata->last_field_index--;
        lid = cdata->last_field_id[cdata->last_field_index];

        DEBUG_FUNCTION_EXIT();        
        return lid;
}

/* Getter / Setter for the C data structure tied to the ruby object. May be easily swapped to a more efficient implementation later */
compact_data* get_cdata(VALUE self)
{
    compact_data* cdata = (compact_data*)NUM2PTR(rb_ivar_get(self, cdata_id));
    return cdata;
}

/* Getter / Setter for the C data structure tied to the ruby object. May be easily swapped to a more efficient implementation later */
void set_cdata(VALUE self, compact_data* cdata)
{
  rb_ivar_set(self, cdata_id, PTR2NUM(cdata));
}

static int get_compact_type(VALUE type_value) {
  int type = FIX2INT(type_value);
  if (type == TTYPE_BOOL) {
    return CTYPE_BOOLEAN_TRUE;
  } else if (type == TTYPE_BYTE) {
    return CTYPE_BYTE;
  } else if (type == TTYPE_I16) {
    return CTYPE_I16;
  } else if (type == TTYPE_I32) {
    return CTYPE_I32;
  } else if (type == TTYPE_I64) {
    return CTYPE_I64;
  } else if (type == TTYPE_DOUBLE) {
    return CTYPE_DOUBLE;
  } else if (type == TTYPE_STRING) {
    return CTYPE_BINARY;
  } else if (type == TTYPE_LIST) {
    return CTYPE_LIST;
  } else if (type == TTYPE_SET) {
    return CTYPE_SET;
  } else if (type == TTYPE_MAP) {
    return CTYPE_MAP;
  } else if (type == TTYPE_STRUCT) {
    return CTYPE_STRUCT;
  } else {
    rb_raise(rb_eStandardError, "don't know what type: %d", type);
    return 0;
  }
}

static void transfer_write_byte(compact_data* data, int8_t b)
{ 
   DEBUG_FUNCTION_ENTRY();
   protocol_transfer *pt = data->pt;
   pt->write(pt, (char*)&b, 1);
   DEBUG_FUNCTION_EXIT();
}

static void transfer_write_bytes(compact_data* data, char* ba, int sz)
{
   DEBUG_FUNCTION_ENTRY();
   protocol_transfer *pt = data->pt;
   pt->write(pt, ba, sz); 
   DEBUG_FUNCTION_EXIT();
}

static void transfer_flush(compact_data* data)
{
  DEBUG_FUNCTION_ENTRY();
  protocol_transfer *pt = data->pt; 
  pt->flush(pt);
  DEBUG_FUNCTION_EXIT();
}

static char transfer_read_byte(compact_data* data)
{
  char b;

  DEBUG_FUNCTION_ENTRY();
  protocol_transfer *pt = data->pt; 
  pt->read(pt, &b, 1);

  DEBUG_FUNCTION_EXIT();
  return b;
}

static int transfer_read_bytes(compact_data* data, char* buf, int len)
{
  DEBUG_FUNCTION_ENTRY();
  protocol_transfer *pt = data->pt; 

  int r = pt->read(pt, buf, len);
  DEBUG_FUNCTION_EXIT();

  return r;
}

static VALUE rb_flush(VALUE self)
{
   DEBUG_FUNCTION_ENTRY();
   compact_data* cd = get_cdata(self);
   transfer_flush(cd);

   DEBUG_FUNCTION_EXIT();
   return Qnil;
}



static void write_field_begin_internal(compact_data* cd, VALUE type, VALUE id_value, VALUE type_override) {
  DEBUG_FUNCTION_ENTRY();
  
  int id = FIX2INT(id_value);
  int last_id = pop_last_field_id(cd);

  // if there's a type override, use that.
  int8_t type_to_write = RTEST(type_override) ? FIX2INT(type_override) : get_compact_type(type);
  // check if we can use delta encoding for the field id
  int diff = id - last_id;
  if (diff > 0 && diff <= 15) {
    // write them together
    transfer_write_byte(cd, diff << 4 | (type_to_write & 0x0f));
  } else {
    // write them separate
    transfer_write_byte(cd, type_to_write & 0x0f);
    write_i16(cd, id_value);
  }

  push_last_field_id(cd, id);

  DEBUG_FUNCTION_EXIT();
}

static int32_t int_to_zig_zag(int32_t n) {
  DEBUG_FUNCTION_ENTRY();
  int r = (n << 1) ^ (n >> 31);
  DEBUG_FUNCTION_EXIT();

  return r;
}

static uint64_t ll_to_zig_zag(int64_t n) {
  DEBUG_FUNCTION_ENTRY();
  int64_t r = (n << 1) ^ (n >> 63);
  DEBUG_FUNCTION_EXIT();
  return r;
}

static void write_varint32(compact_data* cdata, uint32_t n) {
  DEBUG_FUNCTION_ENTRY();
  while (true) {
    if ((n & ~0x7F) == 0) {
      transfer_write_byte(cdata, n & 0x7f);
      break;
    } else {
      transfer_write_byte(cdata, (n & 0x7F) | 0x80);
      n = n >> 7;
    }
  }
  DEBUG_FUNCTION_EXIT();
}

static void write_varint64(compact_data* cd, uint64_t n) {
   DEBUG_FUNCTION_ENTRY();
 while (true) {
    if ((n & ~0x7F) == 0) {
      transfer_write_byte(cd, n & 0x7f);
      break;
    } else {
      transfer_write_byte(cd, (n & 0x7F) | 0x80);
      n = n >> 7;
    }
  }
  DEBUG_FUNCTION_EXIT();
}
static void write_collection_begin(compact_data* cdata, VALUE elem_type, VALUE size_value) {
   DEBUG_FUNCTION_ENTRY();
 int size = FIX2INT(size_value);
  if (size <= 14) {
    transfer_write_byte(cdata, size << 4 | get_compact_type(elem_type));
  } else {
    transfer_write_byte(cdata, 0xf0 | get_compact_type(elem_type));
    write_varint32(cdata, size);
  }

  DEBUG_FUNCTION_EXIT();
}


//--------------------------------
// interface writing methods
//--------------------------------

static VALUE rb_write_i32(VALUE self, VALUE i32);
static VALUE rb_write_string(VALUE self, VALUE str);
static VALUE rb_write_binary(VALUE self, VALUE buf);

static VALUE write_i32(compact_data* cdata, VALUE i32);
static VALUE write_string(compact_data *cdata, VALUE str);
static VALUE write_binary(compact_data *cdata, VALUE buf);


static VALUE rb_write_message_end(VALUE self) {
  return Qnil;
}


static VALUE rb_write_struct_begin(VALUE self, VALUE name) 
{
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);

  push_last_field_id(cd, 0);

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}


static VALUE rb_write_struct_end(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);

  (void)pop_last_field_id(cd);

  DEBUG_FUNCTION_EXIT();

  return Qnil;
}

static VALUE rb_write_field_end(VALUE self) {
  return Qnil;
}

static VALUE rb_write_map_end(VALUE self) {
  return Qnil;
}

static VALUE rb_write_list_end(VALUE self) {
  return Qnil;
}

static VALUE rb_write_set_end(VALUE self) {
  return Qnil;
}

static VALUE rb_write_message_begin(VALUE self, VALUE name, VALUE type, VALUE seqid) {
  DEBUG_FUNCTION_ENTRY();



  compact_data* cd = get_cdata(self);

  transfer_write_byte(cd, PROTOCOL_ID);
  transfer_write_byte(cd, (VERSION & VERSION_MASK) | ((FIX2INT(type) << TYPE_SHIFT_AMOUNT) & TYPE_MASK));
  write_varint32(cd, FIX2INT(seqid));
  write_string(cd, name);

  DEBUG_FUNCTION_EXIT();

  return Qnil;
}



static VALUE rb_write_field_begin(VALUE self, VALUE name, VALUE type, VALUE id) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);

  if (FIX2INT(type) == TTYPE_BOOL) {
    // we want to possibly include the value, so we'll wait.
    cd->current_field_active = 1;
    cd->current_field_type = type;
    cd->current_field_id = id;
  } else {
    write_field_begin_internal(cd, type, id, Qnil);
  }

  DEBUG_FUNCTION_EXIT();

  return Qnil;
}

static VALUE rb_write_field_stop(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
    compact_data* cd = get_cdata(self);
  transfer_write_byte(cd, TTYPE_STOP);
 // transfer_flush(cd);

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_map_begin(VALUE self, VALUE ktype, VALUE vtype, VALUE size_value) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);      

  int size = FIX2INT(size_value);
  if (size == 0) {
    transfer_write_byte(cd, 0);
  } else {
    write_varint32(cd, size);
    transfer_write_byte(cd, get_compact_type(ktype) << 4 | get_compact_type(vtype));
  }

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_list_begin(VALUE self, VALUE etype, VALUE size) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self); 
  write_collection_begin(cd, etype, size);

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_set_begin(VALUE self, VALUE etype, VALUE size) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self); 
  write_collection_begin(cd, etype, size);

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_bool(VALUE self, VALUE b) {
   DEBUG_FUNCTION_ENTRY();
 compact_data* cd = get_cdata(self);      
  int8_t type = b == Qtrue ? CTYPE_BOOLEAN_TRUE : CTYPE_BOOLEAN_FALSE;

  if (!cd->current_field_active) {
    // we're not part of a field, so just write the value.
    transfer_write_byte(cd, type);
  } else {
    // we haven't written the field header yet
    write_field_begin_internal(cd, cd->current_field_type, cd->current_field_id, INT2FIX(type));
    cd->current_field_active = 0;
  }

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_byte(VALUE self, VALUE byte) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   
  CHECK_NIL(byte);
  transfer_write_byte(cd, FIX2INT(byte));
  DEBUG_FUNCTION_EXIT();
  return Qnil;
}



static VALUE write_i16(compact_data* cdata, VALUE i16) {
  DEBUG_FUNCTION_ENTRY();
  write_i32(cdata, i16);
  DEBUG_FUNCTION_EXIT();

  return Qnil;
}

static VALUE write_i32(compact_data *cdata, VALUE i32) {
  DEBUG_FUNCTION_ENTRY();
  CHECK_NIL(i32);
  write_varint32(cdata, int_to_zig_zag(NUM2INT(i32)));
  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_i16(VALUE self, VALUE i16) {
  DEBUG_FUNCTION_ENTRY();
  
  rb_write_i32(self, i16);

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_i32(VALUE self, VALUE i32) {
  DEBUG_FUNCTION_ENTRY();
     compact_data* cd = get_cdata(self);   

  CHECK_NIL(i32);
  write_varint32(cd, int_to_zig_zag(NUM2INT(i32)));
  DEBUG_FUNCTION_EXIT();

  return Qnil;
}

static VALUE rb_write_i64(VALUE self, VALUE i64) {
   DEBUG_FUNCTION_ENTRY();
 compact_data* cd = get_cdata(self);   

  CHECK_NIL(i64);
  write_varint64(cd, ll_to_zig_zag(NUM2LL(i64)));
  DEBUG_FUNCTION_EXIT();

  return Qnil;
}

static VALUE rb_write_double(VALUE self, VALUE dub) {
   DEBUG_FUNCTION_ENTRY();
 CHECK_NIL(dub);
  compact_data* cd = get_cdata(self);   

  // Unfortunately, bitwise_cast doesn't work in C.  Bad C!
  union {
    double f;
    int64_t l;
  } transfer;
  transfer.f = RFLOAT_VALUE(rb_Float(dub));
  char buf[8];
  buf[0] = transfer.l & 0xff;
  buf[1] = (transfer.l >> 8) & 0xff;
  buf[2] = (transfer.l >> 16) & 0xff;
  buf[3] = (transfer.l >> 24) & 0xff;
  buf[4] = (transfer.l >> 32) & 0xff;
  buf[5] = (transfer.l >> 40) & 0xff;
  buf[6] = (transfer.l >> 48) & 0xff;
  buf[7] = (transfer.l >> 56) & 0xff;
  transfer_write_bytes(cd, buf, 8);

  DEBUG_FUNCTION_EXIT();

  return Qnil;
}


static VALUE write_string(compact_data* cdata, VALUE str) {
  DEBUG_FUNCTION_ENTRY();
  str = convert_to_utf8_byte_buffer(str);
  write_binary(cdata, str);
  DEBUG_FUNCTION_EXIT();
  return Qnil;
}



static VALUE write_binary(compact_data* cdata, VALUE buf) {
   DEBUG_FUNCTION_ENTRY();
 buf = force_binary_encoding(buf);
  write_varint32(cdata, RSTRING_LEN(buf));
  transfer_write_bytes(cdata, RSTRING_PTR(buf), RSTRING_LEN(buf));
  DEBUG_FUNCTION_EXIT();
  return Qnil;
}

static VALUE rb_write_string(VALUE self, VALUE str) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   

  write_string(cd, str);

  DEBUG_FUNCTION_EXIT();
  return Qnil;
}


static VALUE rb_write_binary(VALUE self, VALUE buf) {
   DEBUG_FUNCTION_ENTRY();
 compact_data* cd = get_cdata(self);   
  write_binary(cd, buf);

  DEBUG_FUNCTION_EXIT();
  return Qnil;

}
/*
//---------------------------------------
// interface reading methods
//---------------------------------------
*/
#define is_bool_type(ctype) (((ctype) & 0x0F) == CTYPE_BOOLEAN_TRUE || ((ctype) & 0x0F) == CTYPE_BOOLEAN_FALSE)

static VALUE read_string(compact_data* cd);
static VALUE read_binary(compact_data* cd);
//static VALUE rb_thrift_compact_proto_read_byte(VALUE self);
//static VALUE rb_thrift_compact_proto_read_i32(VALUE self);
//static VALUE rb_thrift_compact_proto_read_i16(VALUE self);
/*
static VALUE rb_thrift_compact_proto_read_string(VALUE self);
static VALUE rb_thrift_compact_proto_read_binary(VALUE self);
static VALUE rb_thrift_compact_proto_read_byte(VALUE self);
static VALUE rb_thrift_compact_proto_read_i32(VALUE self);
static VALUE rb_thrift_compact_proto_read_i16(VALUE self);
*/

static int8_t get_ttype(int8_t ctype) {

  if (ctype == TTYPE_STOP) return TTYPE_STOP;

  switch(ctype)
  {
    case CTYPE_BOOLEAN_TRUE:  // break omitted
    case CTYPE_BOOLEAN_FALSE: return TTYPE_BOOL;
    case CTYPE_BYTE: return TTYPE_BYTE;
    case CTYPE_I16: return TTYPE_I16;
    case CTYPE_I32: return TTYPE_I32;
    case CTYPE_I64: return TTYPE_I64;
    case CTYPE_DOUBLE: return TTYPE_DOUBLE;
    case CTYPE_BINARY: return TTYPE_STRING;
    case CTYPE_LIST: return TTYPE_LIST;
    case CTYPE_SET: return TTYPE_SET;
    case CTYPE_MAP: return TTYPE_MAP;
    case CTYPE_STRUCT: return TTYPE_STRUCT;

    default:
        rb_raise(rb_eStandardError, "get_ttype: don't know what type: %d", ctype);
        return 0;
  }
}


static int64_t zig_zag_to_ll(int64_t n) {
  return (((uint64_t)n) >> 1) ^ -(n & 1);
}
 
static int32_t zig_zag_to_int(int32_t n) {
  return (((uint32_t)n) >> 1) ^ -(n & 1);
}


static int64_t read_varint64(compact_data* cd) {
  DEBUG_FUNCTION_ENTRY();
  int shift = 0;
  int64_t result = 0;
  while (true) {
    int8_t b = transfer_read_byte(cd);
    result = result | ((uint64_t)(b & 0x7f) << shift);
    if ((b & 0x80) != 0x80) {
      break;
    }
    shift += 7;
  }

  DEBUGF("%llu", (long long unsigned int)result);
  DEBUG_FUNCTION_EXIT();
  return result;
}

static int16_t read_i16(compact_data* cd) {
  return zig_zag_to_int((int32_t)read_varint64(cd));
}

static VALUE get_protocol_exception(VALUE code, VALUE message) {
  DEBUG_FUNCTION_ENTRY();
  VALUE args[2];
  args[0] = code;
  args[1] = message;

  VALUE o = rb_class_new_instance(2, (VALUE*)&args, protocol_exception_class);

  DEBUG_FUNCTION_EXIT();  
  return o;
}

static VALUE rb_read_struct_begin(VALUE self) {
        DEBUG_FUNCTION_ENTRY();
        compact_data* cd = get_cdata(self);   
                
        push_last_field_id(cd, 0);

        DEBUG_FUNCTION_EXIT();  
        return Qnil;
}

static VALUE rb_read_struct_end(VALUE self) {
        DEBUG_FUNCTION_ENTRY();
        compact_data* cd = get_cdata(self);   
                
        pop_last_field_id(cd);
        
        DEBUG_FUNCTION_EXIT();  
        return Qnil;
}



static VALUE rb_read_message_end(VALUE self) {
  return Qnil;
}

static VALUE rb_read_field_end(VALUE self) {
  return Qnil;
}

static VALUE rb_read_map_end(VALUE self) {
  return Qnil;
}

static VALUE rb_read_list_end(VALUE self) {
  return Qnil;
}

static VALUE rb_read_set_end(VALUE self) {
  return Qnil;
}

static VALUE rb_read_message_begin(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   

  int8_t protocol_id = transfer_read_byte(cd);

  DEBUG_FUNCTION_PROGRESS();

  if (protocol_id != PROTOCOL_ID) {
    char buf[100];
    int len = sprintf(buf, "Expected protocol id %d but got %d", PROTOCOL_ID, protocol_id);
    buf[len] = 0;
    rb_exc_raise(get_protocol_exception(INT2FIX(-1), rb_str_new2(buf)));
  }
  
  int8_t version_and_type = transfer_read_byte(cd);
  int8_t version = version_and_type & VERSION_MASK;
  if (version != VERSION) {
    char buf[100];
    int len = sprintf(buf, "Expected version id %d but got %d", version, VERSION);
    buf[len] = 0;
    rb_exc_raise(get_protocol_exception(INT2FIX(-1), rb_str_new2(buf)));
  }
  
  int8_t type = (version_and_type >> TYPE_SHIFT_AMOUNT) & 0x03;
  int32_t seqid = read_varint64(cd);

  VALUE messageName = read_string(cd);

  VALUE arr = rb_ary_new3(3, messageName, INT2FIX(type), INT2NUM(seqid));
  DEBUG_FUNCTION_EXIT();  
  return arr;
}

static VALUE rb_read_field_begin(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   

  int8_t type = transfer_read_byte(cd);
  // if it's a stop, then we can return immediately, as the struct is over.
  if ((type & 0x0f) == TTYPE_STOP) {
    return rb_ary_new3(3, Qnil, INT2FIX(0), INT2FIX(0));
  } else {
    int field_id = 0;

    // mask off the 4 MSB of the type header. it could contain a field id delta.
    uint8_t modifier = ((type & 0xf0) >> 4);

    if (modifier == 0) {
      // not a delta. look ahead for the zigzag varint field id.
      (void) pop_last_field_id(cd);
      field_id = read_i16(cd);
    } else {
      // has a delta. add the delta to the last read field id.
      field_id = pop_last_field_id(cd) + modifier;
    }

    // if this happens to be a boolean field, the value is encoded in the type
    if (is_bool_type(type)) {
      // save the boolean value in a special instance variable.
      cd->current_field_value = (type & 0x0f) == CTYPE_BOOLEAN_TRUE ? Qtrue : Qfalse;
      cd->current_field_active = 1;
    }

    // push the new field onto the field stack so we can keep the deltas going.
    push_last_field_id(cd, field_id);
    VALUE arr = rb_ary_new3(3, Qnil, INT2FIX(get_ttype(type & 0x0f)), INT2FIX(field_id));
    DEBUG_FUNCTION_EXIT();  
    return arr;
  }
}

static VALUE rb_read_map_begin(VALUE self) {
  compact_data* cd = get_cdata(self);   
  DEBUG_FUNCTION_ENTRY();

  int32_t size = read_varint64(cd);
  uint8_t key_and_value_type = size == 0 ? 0 : transfer_read_byte(cd);
  VALUE arr = rb_ary_new3(3, INT2FIX(get_ttype(key_and_value_type >> 4)), INT2FIX(get_ttype(key_and_value_type & 0xf)), INT2FIX(size));
  
  DEBUG_FUNCTION_EXIT();  
  return arr;
}

static VALUE rb_read_list_begin(VALUE self) {
  compact_data* cd = get_cdata(self);   
  DEBUG_FUNCTION_ENTRY();

  uint8_t size_and_type = transfer_read_byte(cd);
  int32_t size = (size_and_type >> 4) & 0x0f;
  if (size == 15) {
    size = read_varint64(cd);
  }
  uint8_t type = get_ttype(size_and_type & 0x0f);
  VALUE arr = rb_ary_new3(2, INT2FIX(type), INT2FIX(size));

  DEBUG_FUNCTION_EXIT();  
  return arr;
}

static VALUE rb_read_set_begin(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  VALUE v = rb_read_list_begin(self);
  
  DEBUG_FUNCTION_EXIT();  
  return v;
}

static VALUE rb_read_bool(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   


  VALUE bool_value = cd->current_field_value;
  if (cd->current_field_active == 0) {
    VALUE r = transfer_read_byte(cd) == CTYPE_BOOLEAN_TRUE ? Qtrue : Qfalse;
    DEBUG_FUNCTION_EXIT();
    return r;
  } else {
    cd->current_field_active = 0;

    DEBUG_FUNCTION_ENTRY();
    return bool_value;
  }
}



static VALUE rb_read_byte(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   
  VALUE v = INT2FIX(transfer_read_byte(cd));
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE rb_read_i16(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   
  VALUE v = INT2FIX(read_i16(cd));
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE rb_read_i32(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   
  VALUE v = INT2NUM(zig_zag_to_int(read_varint64(cd)));
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE rb_read_i64(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   
  VALUE v = LL2NUM(zig_zag_to_ll(read_varint64(cd)));
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE rb_read_double(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   

  double f = 0;  // Avoid unitialized warning.
  transfer_read_bytes(cd, (char*)&f, 8);
  VALUE v = rb_float_new(f);
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE read_string(compact_data* cd) {
  DEBUG_FUNCTION_ENTRY();
  VALUE buffer = read_binary(cd);
  VALUE v = convert_to_string(buffer);
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE read_binary(compact_data* cd) {
  DEBUG_FUNCTION_ENTRY();
  int64_t size = read_varint64(cd);

  //Try allocating on stack if size is reasonable.
  char b[4096];
  char *buf = b;

  if (size > 4096)
        buf = malloc(size);

  int l = transfer_read_bytes(cd, buf, size);

  VALUE str = rb_str_new(buf, l);

  if (size > 4096)
        free(buf);

  DEBUG_FUNCTION_EXIT();
  return str;
}

static VALUE rb_read_string(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   

  VALUE v = read_string(cd);
  DEBUG_FUNCTION_EXIT();
  return v;
}

static VALUE rb_read_binary(VALUE self) {
  DEBUG_FUNCTION_ENTRY();
  compact_data* cd = get_cdata(self);   

  VALUE v = read_binary(cd);
  DEBUG_FUNCTION_EXIT();
  return v;
}



 compact_data* compact_data_allocate()
 {
  return calloc(sizeof(compact_data), 1);
 }

 void compact_data_free(void *cd)
 {
   compact_data* cdata = (compact_data*)cd;

   if (cdata)
   {
      if (cdata->pt)
        cdata->pt->free(cdata->pt);
   }
   free(cdata);
 }



static VALUE rb_initialize(VALUE self, VALUE transport)
{
  rb_call_super(1, &transport);



  compact_data* cd = compact_data_allocate();
  set_cdata(self, cd);

//  cd->pt = bypass_transfer_create(transport);
//  cd->pt = buffer_transfer_create(transport);

  return self;
} 

VALUE rb_get_protocol_method_table(VALUE self)
{
  return PTR2NUM(&method_table);
}

void Init_protocol_method_table()
{
  fastcall_init_c(method_table.write_bool, (rfunc)rb_write_bool);
  fastcall_init_c(method_table.write_byte, (rfunc)rb_write_byte);
  fastcall_init_c(method_table.write_double, (rfunc)rb_write_double);
  fastcall_init_c(method_table.write_i16, (rfunc)rb_write_i16);
  fastcall_init_c(method_table.write_i32, (rfunc)rb_write_i32);
  fastcall_init_c(method_table.write_i64, (rfunc)rb_write_i64);
  fastcall_init_c(method_table.write_set_begin, (rfunc)rb_write_set_begin);
  fastcall_init_c(method_table.write_set_end, (rfunc)rb_write_set_end);
  fastcall_init_c(method_table.write_map_begin, (rfunc)rb_write_map_begin);
  fastcall_init_c(method_table.write_map_end, (rfunc)rb_write_map_end);
  fastcall_init_c(method_table.write_list_begin, (rfunc)rb_write_list_begin);
  fastcall_init_c(method_table.write_list_end, (rfunc)rb_write_list_end);
  fastcall_init_c(method_table.write_field_begin, (rfunc)rb_write_field_begin);
  fastcall_init_c(method_table.write_field_end, (rfunc)rb_write_field_end);
  fastcall_init_c(method_table.write_field_stop, (rfunc)rb_write_field_stop);
  fastcall_init_c(method_table.write_struct_begin, (rfunc)rb_write_struct_begin);
  fastcall_init_c(method_table.write_struct_end, (rfunc)rb_write_struct_end);
  fastcall_init_c(method_table.write_string, (rfunc)rb_write_string);

  fastcall_init_c(method_table.read_bool, (rfunc)rb_read_bool);
  fastcall_init_c(method_table.read_byte, (rfunc)rb_read_byte);
  fastcall_init_c(method_table.read_double, (rfunc)rb_read_double);
  fastcall_init_c(method_table.read_i16, (rfunc)rb_read_i16);
  fastcall_init_c(method_table.read_i32, (rfunc)rb_read_i32);
  fastcall_init_c(method_table.read_i64, (rfunc)rb_read_i64);
  fastcall_init_c(method_table.read_set_begin, (rfunc)rb_read_set_begin);
  fastcall_init_c(method_table.read_set_end, (rfunc)rb_read_set_end);
  fastcall_init_c(method_table.read_map_begin, (rfunc)rb_read_map_begin);
  fastcall_init_c(method_table.read_map_end, (rfunc)rb_read_map_end);
  fastcall_init_c(method_table.read_list_begin, (rfunc)rb_read_list_begin);
  fastcall_init_c(method_table.read_list_end, (rfunc)rb_read_list_end);
  fastcall_init_c(method_table.read_field_begin, (rfunc)rb_read_field_begin);
  fastcall_init_c(method_table.read_field_end, (rfunc)rb_read_field_end);
  fastcall_init_c(method_table.read_struct_begin, (rfunc)rb_read_struct_begin);
  fastcall_init_c(method_table.read_struct_end, (rfunc)rb_read_struct_end);
  fastcall_init_c(method_table.read_string, (rfunc)rb_read_string);

  fastcall_init_c(method_table.flush, (rfunc)rb_flush);
}  

void Init_compact_protocol_layered() {
  Init_protocol_method_table();

  VALUE thrift_compact_protocol_class = rb_const_get(thrift_module, rb_intern("CompactProtocol"));
  VALUE bpa_class = rb_define_class_under(thrift_module, "LayeredCompactProtocol", thrift_compact_protocol_class);

  VERSION = rb_num2ll(rb_const_get(bpa_class, rb_intern("VERSION")));
  VERSION_MASK = rb_num2ll(rb_const_get(bpa_class, rb_intern("VERSION_MASK")));
  TYPE_MASK = rb_num2ll(rb_const_get(bpa_class, rb_intern("TYPE_MASK")));
  TYPE_SHIFT_AMOUNT = FIX2INT(rb_const_get(bpa_class, rb_intern("TYPE_SHIFT_AMOUNT")));
  PROTOCOL_ID = FIX2INT(rb_const_get(bpa_class, rb_intern("PROTOCOL_ID")));

  rb_define_method(bpa_class, "initialize", rb_initialize, 1);

  rb_define_method(bpa_class, "native?", rb_native_qmark, 0);
  rb_define_method(bpa_class, "flush", rb_flush, 0);

  rb_define_method(bpa_class, "write_message_begin", rb_write_message_begin, 3);
  rb_define_method(bpa_class, "write_field_begin",   rb_write_field_begin, 3);
  rb_define_method(bpa_class, "write_field_stop",    rb_write_field_stop, 0);
  rb_define_method(bpa_class, "write_map_begin",     rb_write_map_begin, 3);
  rb_define_method(bpa_class, "write_list_begin",    rb_write_list_begin, 2);
  rb_define_method(bpa_class, "write_set_begin",     rb_write_set_begin, 2);
  rb_define_method(bpa_class, "write_byte",          rb_write_byte, 1);
  rb_define_method(bpa_class, "write_bool",          rb_write_bool, 1);
  rb_define_method(bpa_class, "write_i16",           rb_write_i16, 1);
  rb_define_method(bpa_class, "write_i32",           rb_write_i32, 1);
  rb_define_method(bpa_class, "write_i64",           rb_write_i64, 1);
  rb_define_method(bpa_class, "write_double",        rb_write_double, 1);
  rb_define_method(bpa_class, "write_string",        rb_write_string, 1);
  rb_define_method(bpa_class, "write_binary",        rb_write_binary, 1);

  rb_define_method(bpa_class, "write_struct_begin", rb_write_struct_begin, 1);
  rb_define_method(bpa_class, "write_struct_end", rb_write_struct_end, 0);

  rb_define_method(bpa_class, "write_message_end", rb_write_message_end, 0);

  // These are empty.
  rb_define_method(bpa_class, "write_field_end", rb_write_field_end, 0);
  rb_define_method(bpa_class, "write_map_end", rb_write_map_end, 0);
  rb_define_method(bpa_class, "write_list_end", rb_write_list_end, 0);
  rb_define_method(bpa_class, "write_set_end", rb_write_set_end, 0);

  rb_define_method(bpa_class, "read_message_begin",  rb_read_message_begin, 0);
  rb_define_method(bpa_class, "read_field_begin",    rb_read_field_begin, 0);
  rb_define_method(bpa_class, "read_map_begin",      rb_read_map_begin, 0);
  rb_define_method(bpa_class, "read_list_begin",     rb_read_list_begin, 0);
  rb_define_method(bpa_class, "read_set_begin",      rb_read_set_begin, 0);
  rb_define_method(bpa_class, "read_byte",           rb_read_byte, 0);
  rb_define_method(bpa_class, "read_bool",           rb_read_bool, 0);
  rb_define_method(bpa_class, "read_i16",            rb_read_i16, 0);
  rb_define_method(bpa_class, "read_i32",            rb_read_i32, 0);
  rb_define_method(bpa_class, "read_i64",            rb_read_i64, 0);
  rb_define_method(bpa_class, "read_double",         rb_read_double, 0);
  rb_define_method(bpa_class, "read_string",         rb_read_string, 0);
  rb_define_method(bpa_class, "read_binary",         rb_read_binary, 0);

  rb_define_method(bpa_class, "read_struct_begin",  rb_read_struct_begin, 0);
  rb_define_method(bpa_class, "read_struct_end",    rb_read_struct_end, 0);

  //These are empty.
  rb_define_method(bpa_class, "read_message_end", rb_read_message_end, 0);
  rb_define_method(bpa_class, "read_field_end",     rb_read_field_end, 0);
  rb_define_method(bpa_class, "read_map_end",       rb_read_map_end, 0);
  rb_define_method(bpa_class, "read_list_end",      rb_read_list_end, 0);
  rb_define_method(bpa_class, "read_set_end",       rb_read_set_end, 0);

  rb_define_method(bpa_class, "get_protocol_method_table", rb_get_protocol_method_table, 0);
}
