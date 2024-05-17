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

#ifndef T_GO_GENERATOR_H
#define T_GO_GENERATOR_H

#include <fstream>
#include <iostream>
#include <limits>
#include <string>
#include <unordered_map>
#include <vector>

#include "thrift/generate/t_generator.h"
#include "thrift/platform.h"
#include "thrift/version.h"
#include <algorithm>
#include <clocale>
#include <sstream>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

using std::map;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

const string DEFAULT_THRIFT_IMPORT = "github.com/apache/thrift/lib/go/thrift";
static std::string package_flag;

/**
 * Go code generator.
 */
class t_go_generator : public t_generator {
public:
  t_go_generator(t_program* program,
                 const std::map<std::string, std::string>& parsed_options,
                 const std::string& option_string)
    : t_generator(program) {
    (void)option_string;
    std::map<std::string, std::string>::const_iterator iter;

    gen_thrift_import_ = DEFAULT_THRIFT_IMPORT;
    gen_package_prefix_ = "";
    package_flag = "";
    read_write_private_ = false;
    ignore_initialisms_ = false;
    skip_remote_ = false;
    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if (iter->first.compare("package_prefix") == 0) {
        gen_package_prefix_ = (iter->second);
      } else if (iter->first.compare("thrift_import") == 0) {
        gen_thrift_import_ = (iter->second);
      } else if (iter->first.compare("package") == 0) {
        package_flag = (iter->second);
      } else if (iter->first.compare("read_write_private") == 0) {
        read_write_private_ = true;
      } else if (iter->first.compare("ignore_initialisms") == 0) {
        ignore_initialisms_ = true;
      } else if( iter->first.compare("skip_remote") == 0) {
        skip_remote_ =  true;
      } else {
        throw "unknown option go:" + iter->first;
      }
    }

    out_dir_base_ = "gen-go";
  }

  /**
   * Init and close methods
   */

  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_const(t_const* tconst) override;
  void generate_struct(t_struct* tstruct) override;
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;

  std::string render_const_value(t_type* type,
                                 t_const_value* value,
                                 const string& name,
                                 bool opt = false);

  /**
   * Struct generation code
   */

  void generate_go_struct(t_struct* tstruct, bool is_exception);
  void generate_go_struct_definition(std::ostream& out,
                                     t_struct* tstruct,
                                     bool is_xception = false,
                                     bool is_result = false,
                                     bool is_args = false);
  void generate_go_struct_initializer(std::ostream& out,
                                      t_struct* tstruct,
                                      bool is_args_or_result = false);
  void generate_isset_helpers(std::ostream& out,
                              t_struct* tstruct,
                              const string& tstruct_name,
                              bool is_result = false);
  void generate_countsetfields_helper(std::ostream& out,
                                      t_struct* tstruct,
                                      const string& tstruct_name,
                                      bool is_result = false);
  void generate_go_struct_reader(std::ostream& out,
                                 t_struct* tstruct,
                                 const string& tstruct_name,
                                 bool is_result = false);
  void generate_go_struct_writer(std::ostream& out,
                                 t_struct* tstruct,
                                 const string& tstruct_name,
                                 bool is_result = false,
                                 bool uses_countsetfields = false);
  void generate_go_struct_equals(std::ostream& out, t_struct* tstruct, const string& tstruct_name);
  void generate_go_function_helpers(t_function* tfunction);
  void get_publicized_name_and_def_value(t_field* tfield,
                                         string* OUT_pub_name,
                                         t_const_value** OUT_def_value) const;

  /**
   * Service-level generation functions
   */

  void generate_service_helpers(t_service* tservice);
  void generate_service_interface(t_service* tservice);
  void generate_service_client(t_service* tservice);
  void generate_service_remote(t_service* tservice);
  void generate_service_server(t_service* tservice);
  void generate_process_function(t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field(std::ostream& out,
                                  t_field* tfield,
                                  bool declare,
                                  std::string prefix = "",
                                  bool inclass = false,
                                  bool coerceData = false,
                                  bool inkey = false,
                                  bool in_container = false);

  void generate_deserialize_struct(std::ostream& out,
                                   t_struct* tstruct,
                                   bool is_pointer_field,
                                   bool declare,
                                   std::string prefix = "");

  void generate_deserialize_container(std::ostream& out,
                                      t_type* ttype,
                                      bool pointer_field,
                                      bool declare,
                                      std::string prefix = "");

  void generate_deserialize_set_element(std::ostream& out,
                                        t_set* tset,
                                        bool declare,
                                        std::string prefix = "");

  void generate_deserialize_map_element(std::ostream& out,
                                        t_map* tmap,
                                        bool declare,
                                        std::string prefix = "");

  void generate_deserialize_list_element(std::ostream& out,
                                         t_list* tlist,
                                         bool declare,
                                         std::string prefix = "");

  void generate_serialize_field(std::ostream& out,
                                t_field* tfield,
                                std::string prefix = "",
                                bool inkey = false);

  void generate_serialize_struct(std::ostream& out, t_struct* tstruct, std::string prefix = "");

  void generate_serialize_container(std::ostream& out,
                                    t_type* ttype,
                                    bool pointer_field,
                                    std::string prefix = "");

  void generate_serialize_map_element(std::ostream& out,
                                      t_map* tmap,
                                      std::string kiter,
                                      std::string viter);

  void generate_serialize_set_element(std::ostream& out, t_set* tmap, std::string iter);

  void generate_serialize_list_element(std::ostream& out, t_list* tlist, std::string iter);

  void generate_go_equals(std::ostream& out, t_type* ttype, string tgt, string src);

  void generate_go_equals_struct(std::ostream& out, t_type* ttype, string tgt, string src);

  void generate_go_equals_container(std::ostream& out, t_type* ttype, string tgt, string src);

  void generate_go_docstring(std::ostream& out, t_struct* tstruct);

  void generate_go_docstring(std::ostream& out, t_function* tfunction);

  void generate_go_docstring(std::ostream& out,
                             t_doc* tdoc,
                             t_struct* tstruct,
                             const char* subheader);

  void generate_go_docstring(std::ostream& out, t_doc* tdoc);

  void parse_go_tags(map<string, string>* tags, const string in);

  /**
   * Helper rendering functions
   */

  std::string go_autogen_comment();
  std::string go_package();
  std::string go_imports_begin(bool consts);
  std::string go_imports_end();
  std::string render_includes(bool consts);
  std::string render_included_programs(string& unused_protection);
  std::string render_program_import(const t_program* program, string& unused_protection);
  std::string render_system_packages(std::vector<string>& system_packages);
  std::string render_import_protection();
  std::string render_fastbinary_includes();
  std::string declare_argument(t_field* tfield);
  std::string render_field_initial_value(t_field* tfield, const string& name, bool optional_field);
  std::string type_name(t_type* ttype);
  std::string module_name(t_type* ttype);
  std::string function_signature(t_function* tfunction, std::string prefix = "");
  std::string function_signature_if(t_function* tfunction,
                                    std::string prefix = "",
                                    bool addError = false);
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);
  std::string type_to_go_type(t_type* ttype);
  std::string type_to_go_type_with_opt(t_type* ttype, bool optional_field);
  std::string type_to_go_key_type(t_type* ttype);
  std::string type_to_spec_args(t_type* ttype);

  void generate_deprecation_comment(std::ostream& os, const std::map<std::string, std::vector<std::string>>& annotations);

  void indent_up() { t_generator::indent_up(); }
  void indent_down() { t_generator::indent_down(); }
  std::string indent() { return t_generator::indent(); }
  std::ostream& indent(std::ostream& os) { return t_generator::indent(os); }

  static std::string get_real_go_module(const t_program* program) {

    if (!package_flag.empty()) {
      return package_flag;
    }
    std::string real_module = program->get_namespace("go");
    if (!real_module.empty()) {
      return real_module;
    }

    return lowercase(program->get_name());
  }

  static bool is_pointer_field(t_field* tfield, bool in_container = false);

  std::string indent_str() const {
    return "\t";
  }

private:
  std::string gen_package_prefix_;
  std::string gen_thrift_import_;
  bool read_write_private_;
  bool ignore_initialisms_;
  bool skip_remote_;

  /**
   * File streams
   */

  ofstream_with_content_based_conditional_update f_types_;
  std::string f_types_name_;
  ofstream_with_content_based_conditional_update f_consts_;
  std::string f_consts_name_;
  std::stringstream f_const_values_;

  std::string package_name_;
  std::string package_dir_;
  std::unordered_map<std::string, std::string> package_identifiers_;
  std::set<std::string> package_identifiers_set_;
  std::string read_method_name_;
  std::string write_method_name_;
  std::string equals_method_name_;

  std::set<std::string> commonInitialisms;

  std::string camelcase(const std::string& value) const;
  void fix_common_initialism(std::string& value, int i) const;
  std::string publicize(const std::string& value, bool is_args_or_result = false) const;
  std::string publicize(const std::string& value,
                        bool is_args_or_result,
                        const std::string& service_name) const;
  std::string privatize(const std::string& value) const;
  std::string new_prefix(const std::string& value) const;
  static std::string variable_name_to_go_name(const std::string& value);
  static bool omit_initialization(t_field* tfield);
};

#endif
