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

 #ifndef __STRUCT_H_
 #define __STRUCT_H_


#include <stdbool.h>
#include <ruby.h>
#include "fastcall.h"

struct _protcol_method_table;
typedef struct _protocol_method_table protocol_method_table;
struct _protocol_method_table
{
	fastcall write_bool;
	fastcall write_byte;
	fastcall write_double;
	fastcall write_i16;
	fastcall write_i32;
	fastcall write_i64;
	fastcall write_set_begin;
	fastcall write_set_end;
	fastcall write_map_begin;
	fastcall write_map_end;
	fastcall write_list_begin;
	fastcall write_list_end;
	fastcall write_field_begin;
	fastcall write_field_end;
	fastcall write_field_stop;
	fastcall write_struct_begin;
	fastcall write_struct_end;
	fastcall write_string;

	fastcall read_bool;
	fastcall read_byte;
	fastcall read_double;
	fastcall read_i16;
	fastcall read_i32;
	fastcall read_i64;
	fastcall read_set_begin;
	fastcall read_set_end;
	fastcall read_map_begin;
	fastcall read_map_end;
	fastcall read_list_begin;
	fastcall read_list_end;
	fastcall read_field_begin;
	fastcall read_field_end;
	fastcall read_struct_begin;
	fastcall read_struct_end;
	fastcall read_string;

	fastcall flush;
};

void Init_struct();
void Init_union();

#endif