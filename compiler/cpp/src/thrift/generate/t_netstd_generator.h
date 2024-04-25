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
using std::set;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const string DEEP_COPY_METHOD_NAME = "DeepCopy";
static const string CANCELLATION_TOKEN_NAME = "cancellationToken";

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

  // overrides
  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;
  void generate_consts(vector<t_const*> consts) override;
  void generate_consts(ostream& out, vector<t_const*> consts);
  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_enum(ostream& out, t_enum* tenum);
  void generate_struct(t_struct* tstruct) override;
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;

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
  void generate_deprecation_attribute(ostream& out, std::map<std::string, std::vector<std::string>>& annotations);
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
  void generate_serialize_field(ostream& out, t_field* tfield, string prefix = "", bool is_propertyless = false, bool allow_nullable = true);
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

  static const int MODE_FULL_DECL = 0x00;
  static const int MODE_NO_RETURN = 0x01;
  static const int MODE_NO_ARGS   = 0x02;

  string type_name(t_type* ttype, bool with_namespace = true);
  string base_type_name(t_base_type* tbase);
  string declare_field(t_field* tfield, bool init = false, bool allow_nullable = true, string prefix = "");
  string function_signature_async(t_function* tfunction, string prefix = "", int mode = MODE_FULL_DECL);
  string function_signature(t_function* tfunction, string prefix = "");
  string argument_list(t_struct* tstruct, bool with_types = true);
  string type_to_enum(t_type* ttype);
  string prop_name(t_field* tfield, bool suppress_mapping = false);
  string func_name(t_function* tfunc, bool suppress_mapping = false);
  string func_name(std::string fname, bool suppress_mapping = false);
  string convert_to_pascal_case(const string& str);
  string get_enum_class_name(t_type* type) override;

protected:
  std::string autogen_comment() override {
    string comment = "/**\n";
    if( target_net_version < 6) {
        comment += " * <auto-generated>\n";
    }
    comment += " * " + autogen_summary() + "\n";
    comment += " * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n";
    if( target_net_version < 6) {
        comment += " * </auto-generated>\n";
    }
    comment += " */\n";
    return comment;
  }


private:
  string namespace_name_;
  string namespace_dir_;

  bool union_;
  bool hashcode_;
  bool serialize_;
  bool wcf_;
  bool use_pascal_case_properties;
  bool suppress_deepcopy;
  int  target_net_version;  // 0 = any, 6 = net6, 8 = net8
  bool add_async_postfix;

  const std::string CSHARP_KEYWORDS[101] = {
    // C# keywords
    "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const", "continue",
    "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern", "false", "finally",
    "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface", "internal", "is", "lock",
    "long", "namespace", "new", "null", "object", "operator", "out", "override", "params", "private", "protected",
    "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc", "static", "string",
    "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked", "unsafe", "ushort",
    "using", "virtual", "void", "volatile", "while",
    // C# contextual keywords
    "add", "alias", "ascending", "async", "await", "descending", "dynamic", "from", "get", "global", "group", "into",
    "join", "let", "orderby", "partial", "remove", "select", "set", "value", "var", "when", "where", "yield"
  };

  string wcf_namespace_;
  std::set<string> netstd_keywords = std::set<string>(CSHARP_KEYWORDS, CSHARP_KEYWORDS + sizeof(CSHARP_KEYWORDS) / sizeof(CSHARP_KEYWORDS[0]));
  vector<member_mapping_scope> member_mapping_scopes;
  map<string, t_type*> collected_extension_types;
  map<string, t_type*> checked_extension_types;

  string normalize_name(string name, bool is_arg_name = false);
  string make_valid_csharp_identifier(string const& fromName);
  string make_csharp_string_literal( string const& value);
  void prepare_member_name_mapping(t_service* tservice);
  void prepare_member_name_mapping(t_struct* tstruct);
  void prepare_member_name_mapping(t_struct* scope, const vector<t_field*>& members, const string& structname);
  void prepare_member_name_mapping(t_service* scope, const vector<t_function*>& members, const string& structname);
  void cleanup_member_name_mapping(void* scope);
  string get_mapped_member_name(string oldname);
  string get_isset_name(const string& str);
  string get_deep_copy_method_call(t_type* ttype, bool is_not_null, bool& needs_typecast, string& suffix);
  void collect_extensions_types(t_struct* tstruct);
  void collect_extensions_types(t_type* ttype);
  void generate_extensions(ostream& out, map<string, t_type*> types);
  void reset_indent();
  void generate_null_check_begin(ostream& out, t_field* tfield);
  void generate_null_check_end(ostream& out, t_field* tfield);
  string initialize_field(t_field* tfield);

  void pragmas_and_directives(ostream& out);
  bool any_deprecations();
  bool is_deprecated(std::map<std::string, std::vector<std::string>>& annotations);
  bool is_nullable_type(t_type* ttype);
  bool force_member_nullable(t_field* tfield);  // see there
  string nullable_suffix();                     // unconditionally
  string nullable_field_suffix(t_field* tfield);  // depends on field type
  string nullable_field_suffix(t_type* ttype);  // depends on field type
  string nullable_value_access(t_type* ttype);  // depends on field type
};
