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
#include <protocol.h>
#include <stdbool.h>
#include <constants.h>
#include <struct.h>

static VALUE skip(VALUE self, int ttype) {
  if (ttype == TTYPE_STOP) {
    return Qnil;
  } else if (ttype == TTYPE_BOOL) {
    rb_funcall(self, read_bool_method_id, 0);
  } else if (ttype == TTYPE_BYTE) {
    rb_funcall(self, read_byte_method_id, 0);
  } else if (ttype == TTYPE_I16) {
    rb_funcall(self, read_i16_method_id, 0);
  } else if (ttype == TTYPE_I32) {
    rb_funcall(self, read_i32_method_id, 0);
  } else if (ttype == TTYPE_I64) {
    rb_funcall(self, read_i64_method_id, 0);
  } else if (ttype == TTYPE_DOUBLE) {
    rb_funcall(self, read_double_method_id, 0);
  } else if (ttype == TTYPE_STRING) {
    rb_funcall(self, read_string_method_id, 0);
  } else if (ttype == TTYPE_STRUCT) {
    rb_funcall(self, read_struct_begin_method_id, 0);
    while (true) {
      VALUE field_header = rb_funcall(self, read_field_begin_method_id, 0);
      if (NIL_P(field_header) || FIX2INT(rb_ary_entry(field_header, 1)) == TTYPE_STOP ) {
        break;
      } 
      skip(self, FIX2INT(rb_ary_entry(field_header, 1)));
      rb_funcall(self, read_field_end_method_id, 0);
    }
    rb_funcall(self, read_struct_end_method_id, 0);
  } else if (ttype == TTYPE_MAP) {
    int i;
    VALUE map_header = rb_funcall(self, read_map_begin_method_id, 0);
    int ktype = FIX2INT(rb_ary_entry(map_header, 0));
    int vtype = FIX2INT(rb_ary_entry(map_header, 1));
    int size = FIX2INT(rb_ary_entry(map_header, 2));
    
    for (i = 0; i < size; i++) {
      skip(self, ktype);
      skip(self, vtype);
    }
    rb_funcall(self, read_map_end_method_id, 0);
  } else if (ttype == TTYPE_LIST || ttype == TTYPE_SET) {
    int i;
    VALUE collection_header = rb_funcall(self, ttype == TTYPE_LIST ? read_list_begin_method_id : read_set_begin_method_id, 0);
    int etype = FIX2INT(rb_ary_entry(collection_header, 0));
    int size = FIX2INT(rb_ary_entry(collection_header, 1));
    for (i = 0; i < size; i++) {
      skip(self, etype);
    }
    rb_funcall(self, ttype == TTYPE_LIST ? read_list_end_method_id : read_set_end_method_id, 0);
  } else {
    rb_raise(rb_eNotImpError, "don't know how to skip type %d", ttype);
  }

  return Qnil;
}

VALUE rb_thrift_protocol_native_qmark(VALUE self) {
  return Qfalse;
}

VALUE rb_thrift_protocol_skip(VALUE protocol, VALUE ttype) {
  return skip(protocol, FIX2INT(ttype));
}

VALUE rb_thrift_write_message_end(VALUE self) {
  return Qnil;
}

VALUE rb_thrift_write_struct_begin(VALUE self, VALUE name) {
  return Qnil;
}

VALUE rb_thrift_write_struct_end(VALUE self) {
  return Qnil;
}

VALUE rb_thrift_write_field_end(VALUE self) {
  return Qnil;
}

VALUE rb_thrift_write_map_end(VALUE self) {
  return Qnil;
}

VALUE rb_thrift_write_list_end(VALUE self) {
  return Qnil;
}

VALUE rb_thrift_write_set_end(VALUE self) {
  return Qnil;
}

VALUE rb_thrift_read_message_end(VALUE self) {
  return Qnil;
}

VALUE rb_thift_read_struct_begin(VALUE self) {
  return Qnil;
}

VALUE rb_thift_read_struct_end(VALUE self) {
  return Qnil;
}

VALUE rb_thift_read_field_end(VALUE self) {
  return Qnil;
}

VALUE rb_thift_read_map_end(VALUE self) {
  return Qnil;
}

VALUE rb_thift_read_list_end(VALUE self) {
  return Qnil;
}

VALUE rb_thift_read_set_end(VALUE self) {
  return Qnil;
}

void Init_protocol() {
  VALUE c_protocol = rb_const_get(thrift_module, rb_intern("BaseProtocol"));
  
  rb_define_method(c_protocol, "skip", rb_thrift_protocol_skip, 1);
  rb_define_method(c_protocol, "write_message_end", rb_thrift_write_message_end, 0);
  rb_define_method(c_protocol, "write_struct_begin", rb_thrift_write_struct_begin, 1);
  rb_define_method(c_protocol, "write_struct_end", rb_thrift_write_struct_end, 0);
  rb_define_method(c_protocol, "write_field_end", rb_thrift_write_field_end, 0);
  rb_define_method(c_protocol, "write_map_end", rb_thrift_write_map_end, 0);
  rb_define_method(c_protocol, "write_list_end", rb_thrift_write_list_end, 0);
  rb_define_method(c_protocol, "write_set_end", rb_thrift_write_set_end, 0);
  rb_define_method(c_protocol, "read_message_end", rb_thrift_read_message_end, 0);
  rb_define_method(c_protocol, "read_struct_begin", rb_thift_read_struct_begin, 0);
  rb_define_method(c_protocol, "read_struct_end", rb_thift_read_struct_end, 0);
  rb_define_method(c_protocol, "read_field_end", rb_thift_read_field_end, 0);
  rb_define_method(c_protocol, "read_map_end", rb_thift_read_map_end, 0);
  rb_define_method(c_protocol, "read_list_end", rb_thift_read_list_end, 0);
  rb_define_method(c_protocol, "read_set_end", rb_thift_read_set_end, 0);
  rb_define_method(c_protocol, "native?", rb_thrift_protocol_native_qmark, 0);
  
  // native_proto_method_table *npmt;
  // npmt = ALLOC(native_proto_method_table);
  // npmt->write_message_end = rb_thrift_write_message_end;
  // npmt->write_struct_begin = rb_thrift_write_struct_begin;
  // npmt->write_struct_end = rb_thrift_write_struct_end;
  // npmt->write_field_end = rb_thrift_write_field_end;
  // npmt->write_map_end = rb_thrift_write_map_end;
  // npmt->write_list_end = rb_thrift_write_list_end;
  // npmt->write_set_end = rb_thrift_write_set_end;
  // npmt->read_message_end = rb_thrift_read_message_end;
  // npmt->read_struct_begin = rb_thift_read_struct_begin;
  // npmt->read_struct_end = rb_thift_read_struct_end;
  // npmt->read_field_end = rb_thift_read_field_end;
  // npmt->read_map_end = rb_thift_read_map_end;
  // npmt->read_list_end = rb_thift_read_list_end;
  // npmt->read_set_end = rb_thift_read_set_end;
  // 
  // VALUE method_table_object = Data_Wrap_Struct(rb_cObject, 0, free, npmt);
  // rb_const_set(c_protocol, rb_intern("@native_method_table"), method_table_object);
}
