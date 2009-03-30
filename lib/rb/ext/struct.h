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

#include <stdbool.h>
#include <ruby.h>

typedef struct native_proto_method_table {
  VALUE (*write_bool)(VALUE, VALUE);
  VALUE (*write_byte)(VALUE, VALUE);
  VALUE (*write_i16)(VALUE, VALUE);
  VALUE (*write_i32)(VALUE, VALUE);
  VALUE (*write_i64)(VALUE, VALUE);
  VALUE (*write_double)(VALUE, VALUE);
  VALUE (*write_string)(VALUE, VALUE);
  VALUE (*write_list_begin)(VALUE, VALUE, VALUE);
  VALUE (*write_list_end)(VALUE);
  VALUE (*write_set_begin)(VALUE, VALUE, VALUE);
  VALUE (*write_set_end)(VALUE);
  VALUE (*write_map_begin)(VALUE, VALUE, VALUE, VALUE);
  VALUE (*write_map_end)(VALUE);
  VALUE (*write_struct_begin)(VALUE, VALUE);
  VALUE (*write_struct_end)(VALUE);
  VALUE (*write_field_begin)(VALUE, VALUE, VALUE, VALUE);
  VALUE (*write_field_end)(VALUE);
  VALUE (*write_field_stop)(VALUE);
  VALUE (*write_message_begin)(VALUE, VALUE, VALUE, VALUE);
  VALUE (*write_message_end)(VALUE);
  
  VALUE (*read_message_begin)(VALUE);
  VALUE (*read_message_end)(VALUE);
  VALUE (*read_field_begin)(VALUE);
  VALUE (*read_field_end)(VALUE);
  VALUE (*read_map_begin)(VALUE);
  VALUE (*read_map_end)(VALUE);
  VALUE (*read_list_begin)(VALUE);
  VALUE (*read_list_end)(VALUE);
  VALUE (*read_set_begin)(VALUE);
  VALUE (*read_set_end)(VALUE);
  VALUE (*read_byte)(VALUE);
  VALUE (*read_bool)(VALUE);
  VALUE (*read_i16)(VALUE);
  VALUE (*read_i32)(VALUE);
  VALUE (*read_i64)(VALUE);
  VALUE (*read_double)(VALUE);
  VALUE (*read_string)(VALUE);
  VALUE (*read_struct_begin)(VALUE);
  VALUE (*read_struct_end)(VALUE);
  
} native_proto_method_table;

void Init_struct();
