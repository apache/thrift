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
#include <constants.h>
#include <bytes.h>
#include <macros.h>

#define DEBUG 0

#if DEBUG
  #define DEBUG_FUNCTION_ENTRY() printf("FMEM %s\n", __FUNCTION__);
  #define DEBUG_FUNCTION_PROGRES() printf("%s, %s:%d\n", __FILE__, __FUNCTION__, __LINE__);
#else
  #define DEBUG_FUNCTION_ENTRY()
  #define DEBUG_FUNCTION_PROGRES() 
#endif

#define GARBAGE_BUFFER_SIZE 32768 //32k

//#define GARBAGE_BUFFER_SIZE 16 //32k

static ID cdata_id;

struct _fmem_data;
typedef struct _fmem_data fmem_data;


struct _fmem_data {
  char *buffer;
  int buf_sz;
  int buf_rd_idx;
  int buf_wr_idx;
 };

static VALUE rb_write(VALUE self, VALUE str);




 /* Getter / Setter for the C data structure tied to the ruby object. May be easily swapped to a more efficient implementation later */
static fmem_data* get_cdata(VALUE self)
{
    fmem_data* cdata = (fmem_data*)NUM2ULONG(rb_ivar_get(self, cdata_id));
    return cdata;
}

/* Getter / Setter for the C data structure tied to the ruby object. May be easily swapped to a more efficient implementation later */
static void set_cdata(VALUE self, fmem_data* cdata)
{
  rb_ivar_set(self, cdata_id, ULONG2NUM((long)cdata));
}

//Move data to the beginning of the array and reduce size to minimum
static void collect_garbage(fmem_data* fm)
{
  if (fm->buf_rd_idx > GARBAGE_BUFFER_SIZE)
  {
    memmove(fm->buffer, fm->buffer + fm->buf_rd_idx, fm->buf_sz - fm->buf_rd_idx);
    fm->buf_wr_idx -= fm->buf_rd_idx;
    fm->buf_rd_idx = 0;

    fm->buffer = realloc(fm->buffer, fm->buf_wr_idx);
    fm->buf_sz = fm->buf_wr_idx;
  }
}

static VALUE rb_initialize(VALUE self)
{
  DEBUG_FUNCTION_ENTRY();
  fmem_data* fm = calloc(sizeof(fmem_data), 1);

  set_cdata(self, fm);
  return self;
}

static VALUE rb_reset_buffer(int argc, VALUE* argv, VALUE self)
{
    DEBUG_FUNCTION_ENTRY();

    fmem_data* fm = get_cdata(self);

    fm->buf_rd_idx = 0;
    fm->buf_wr_idx = 0;

    collect_garbage(fm);

    if (argc > 1)
      rb_raise(rb_eArgError, "wrong number of arguments");

    if (argc == 1)
      rb_write(self, argv[0]);

    return Qnil;
}

static VALUE rb_available(VALUE self)
{
  DEBUG_FUNCTION_ENTRY();

  fmem_data* fm = get_cdata(self);
  return INT2FIX(fm->buf_wr_idx - fm->buf_rd_idx);
}

static VALUE rb_write(VALUE self, VALUE str) {
  DEBUG_FUNCTION_ENTRY();

  fmem_data* fm = get_cdata(self);

  str = force_binary_encoding(str);

  //Check if the allocated buffer is large enouh
  if (fm->buf_wr_idx + RSTRING_LEN(str) >= fm->buf_sz)
  {
    //No, make it bigger
    int inc = fm->buf_sz - fm->buf_wr_idx + RSTRING_LEN(str);
    if (inc < GARBAGE_BUFFER_SIZE) inc = GARBAGE_BUFFER_SIZE;

    fm->buf_sz += inc;
    fm->buffer = realloc(fm->buffer, fm->buf_sz);
  }


  memcpy(fm->buffer + fm->buf_wr_idx, RSTRING_PTR(str), RSTRING_LEN(str));
  fm->buf_wr_idx += RSTRING_LEN(str);
  return Qnil;
}

static VALUE rb_read(VALUE self, VALUE length_value) {
  DEBUG_FUNCTION_ENTRY();

  fmem_data* fm = get_cdata(self);

  int len = FIX2INT(length_value);

  if (fm->buf_sz - fm->buf_rd_idx < len)
  {
    rb_raise(rb_eEOFError, "Not enough bytes remain in memory buffer");
  }

  VALUE ret = rb_str_new(fm->buffer + fm->buf_rd_idx, len);

  fm->buf_rd_idx += len;

  collect_garbage(fm);

  return ret;
}

static VALUE rb_read_byte(VALUE self) {
  DEBUG_FUNCTION_ENTRY();

  fmem_data* fm = get_cdata(self);

  if (fm->buf_sz - fm->buf_rd_idx < 1)
  {
    rb_raise(rb_eEOFError, "Not enough bytes remain in memory buffer");
    return Qnil;
  }

  VALUE ret = INT2FIX(fm->buffer[fm->buf_rd_idx++]);

  collect_garbage(fm);

  return ret;
}

static VALUE rb_read_into_buffer(VALUE self, VALUE buffer_value, VALUE size_value) {
  DEBUG_FUNCTION_ENTRY();

  fmem_data* fm = get_cdata(self);


  int len = FIX2INT(size_value);

  if (fm->buf_sz - fm->buf_rd_idx < len)
  {
    rb_raise(rb_eEOFError, "Not enough bytes remain in memory buffer");
    return Qnil;
  }

  int buflen = RSTRING_LEN(buffer_value);
  char* buf = RSTRING_PTR(buffer_value);

  if (buflen < len)
  {
    rb_raise(rb_eIndexError, "read of %d bytes requested, but buffer is only %d long", len, buflen);
    return Qnil;
  }

  memcpy(buf, fm->buffer + fm->buf_rd_idx, len);

  fm->buf_rd_idx += len;

  collect_garbage(fm);

  return INT2FIX(len);
}

void Init_fast_memory_buffer() {

  VALUE thrift_base_protocol_class = rb_const_get(thrift_module, rb_intern("MemoryBufferTransport"));
  VALUE thrift_memory_buffer_class = rb_define_class_under(thrift_module, "FastMemoryBufferTransport", thrift_base_protocol_class);

  rb_define_method(thrift_memory_buffer_class, "initialize", rb_initialize, 0);
  rb_define_method(thrift_memory_buffer_class, "available", rb_available, 0);
  rb_define_method(thrift_memory_buffer_class, "reset_buffer", rb_reset_buffer, -1);
  rb_define_method(thrift_memory_buffer_class, "write", rb_write, 1);
  rb_define_method(thrift_memory_buffer_class, "read", rb_read, 1);
  rb_define_method(thrift_memory_buffer_class, "read_byte", rb_read_byte, 0);
  rb_define_method(thrift_memory_buffer_class, "read_into_buffer", rb_read_into_buffer, 2);
  
  cdata_id = rb_intern("@cdata");
}
