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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

#include <cassert>

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <cctype>

#include <stdlib.h>
#include <sys/stat.h>
#include <sstream>

#include "thrift/platform.h"
#include "thrift/generate/t_oop_generator.h"

using std::map;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const string endl = "\n"; // avoid ostream << std::endl flushes

static const string DEEP_COPY_METHOD_NAME = "DeepCopy";

class t_netstd_generator : public t_oop_generator
{

  struct member_mapping_scope
  {
    public:
      member_mapping_scope() : scope_member(0) { }
      void* scope_member;
      map<string, string> mapping_table;
  };

public:
  t_netstd_generator(t_program* program, const map<string, string>& parsed_options, const string& option_string);

  bool is_wcf_enabled() const;
  bool is_hashcode_enabled() const;
  bool is_serialize_enabled() const;
  bool is_union_enabled() const;
  map<string, int> get_keywords_list() const;

  // overrides
  void init_generator();
  void close_generator();
  void generate_consts(vector<t_const*> consts);
  void generate_consts(ostream& out, vector<t_const*> consts);
  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum);
  void generate_enum(ostream& out, t_enum* tenum);
  void generate_struct(t_struct* tstruct);
  void generate_xception(t_struct* txception);
  void generate_service(t_service* tservice);

  // additional files
  void generate_extensions_file();

  void generate_property(ostream& out, t_field* tfield, bool isPublic, bool generateIsset);
  void generate_netstd_property(ostream& out, t_field* tfield, bool isPublic, bool includeIsset = true, string fieldPrefix = "");
  bool print_const_value(ostream& out, string name, t_type* type, t_const_value* value, bool in_static, bool defval = false, bool needtype = false);
  string render_const_value(ostream& out, string name, t_type* type, t_const_value* value);
  void print_const_constructor(ostream& out, vector<t_const*> consts);
  void print_const_def_value(ostream& out, string name, t_type* type, t_const_value* value);
  void generate_netstd_struct(t_struct* tstruct, bool is_exception);
  void generate_netstd_union(t_struct* tunion);
  void generate_netstd_struct_definition(ostream& out, t_struct* tstruct, bool is_xception = false, bool in_class = false, bool is_result = false);
  void generate_netstd_union_definition(ostream& out, t_struct* tunion);
  void generate_netstd_union_class(ostream& out, t_struct* tunion, t_field* tfield);
  void generate_netstd_wcffault(ostream& out, t_struct* tstruct);
  void generate_netstd_deepcopy_method(ostream& out, t_struct* tstruct, std::string sharp_struct_name);
  void generate_netstd_struct_reader(ostream& out, t_struct* tstruct);
  void generate_netstd_struct_result_writer(ostream& out, t_struct* tstruct);
  void generate_netstd_struct_writer(ostream& out, t_struct* tstruct);
  void generate_netstd_struct_tostring(ostream& out, t_struct* tstruct);
  void generate_netstd_struct_equals(ostream& out, t_struct* tstruct);
  void generate_netstd_struct_hashcode(ostream& out, t_struct* tstruct);
  void generate_netstd_union_reader(ostream& out, t_struct* tunion);
  void generate_function_helpers(ostream& out, t_function* tfunction);
  void generate_service_interface(ostream& out, t_service* tservice);
  void generate_service_helpers(ostream& out, t_service* tservice);
  void generate_service_client(ostream& out, t_service* tservice);
  void generate_service_server(ostream& out, t_service* tservice);
  void generate_process_function_async(ostream& out, t_service* tservice, t_function* function);
  void generate_deserialize_field(ostream& out, t_field* tfield, string prefix = "", bool is_propertyless = false);
  void generate_deserialize_struct(ostream& out, t_struct* tstruct, string prefix = "");
  void generate_deserialize_container(ostream& out, t_type* ttype, string prefix = "");
  void generate_deserialize_set_element(ostream& out, t_set* tset, string prefix = "");
  void generate_deserialize_map_element(ostream& out, t_map* tmap, string prefix = "");
  void generate_deserialize_list_element(ostream& out, t_list* list, string prefix = "");
  void generate_serialize_field(ostream& out, t_field* tfield, string prefix = "", bool is_propertyless = false);
  void generate_serialize_struct(ostream& out, t_struct* tstruct, string prefix = "");
  void generate_serialize_container(ostream& out, t_type* ttype, string prefix = "");
  void generate_serialize_map_element(ostream& out, t_map* tmap, string iter, string map);
  void generate_serialize_set_element(ostream& out, t_set* tmap, string iter);
  void generate_serialize_list_element(ostream& out, t_list* tlist, string iter);
  void generate_netstd_doc(ostream& out, t_field* field);
  void generate_netstd_doc(ostream& out, t_doc* tdoc);
  void generate_netstd_doc(ostream& out, t_function* tdoc);
  void generate_netstd_docstring_comment(ostream& out, string contents);
  void docstring_comment(ostream& out, const string& comment_start, const string& line_prefix, const string& contents, const string& comment_end);
  void start_netstd_namespace(ostream& out);
  void end_netstd_namespace(ostream& out);

  string netstd_type_usings() const;
  string netstd_thrift_usings() const;

  string type_name(t_type* ttype);
  string base_type_name(t_base_type* tbase);
  string declare_field(t_field* tfield, bool init = false, string prefix = "");
  string function_signature_async(t_function* tfunction, string prefix = "");
  string function_signature(t_function* tfunction, string prefix = "");
  string argument_list(t_struct* tstruct);
  string type_to_enum(t_type* ttype);
  string prop_name(t_field* tfield, bool suppress_mapping = false);
  string convert_to_pascal_case(const string& str);
  string get_enum_class_name(t_type* type);

private:
  string namespace_name_;
  string namespace_dir_;

  bool union_;
  bool hashcode_;
  bool serialize_;
  bool wcf_;
  bool use_pascal_case_properties;
  bool suppress_deepcopy;

  string wcf_namespace_;
  map<string, int> netstd_keywords;
  vector<member_mapping_scope> member_mapping_scopes;
  map<string, t_type*> collected_extension_types;
  map<string, t_type*> checked_extension_types;
  
  void init_keywords();
  string normalize_name(string name);
  string make_valid_csharp_identifier(string const& fromName);
  void prepare_member_name_mapping(t_struct* tstruct);
  void prepare_member_name_mapping(void* scope, const vector<t_field*>& members, const string& structname);
  void cleanup_member_name_mapping(void* scope);
  string get_mapped_member_name(string oldname);
  string get_isset_name(const string& str);
  string get_deep_copy_method_call(t_type* ttype, bool& needs_typecast);
  void collect_extensions_types(t_struct* tstruct);
  void collect_extensions_types(t_type* ttype);
  void generate_extensions(ostream& out, map<string, t_type*> types);
  void reset_indent();
  void generate_null_check_begin(ostream& out, t_field* tfield);
  void generate_null_check_end(ostream& out, t_field* tfield);
};
