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

#include <cassert>
#include <ctime>

#include <cctype>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <limits>
#include <sstream>
#include <string>
#include <vector>

#include <stdexcept>
#include <sys/stat.h>

#include "thrift/generate/t_oop_generator.h"
#include "thrift/platform.h"

using std::map;
using std::ostream;
using std::ostringstream;
using std::setfill;
using std::set;
using std::setw;
using std::string;
using std::stringstream;
using std::vector;

static const string thrift_option_class = "org.apache.thrift.Option";
static const string jdk_option_class = "java.util.Optional";

/**
 * Java code generator.
 *
 */
class t_java_generator : public t_oop_generator {
public:
  t_java_generator(t_program* program,
                   const std::map<std::string, std::string>& parsed_options,
                   const std::string& option_string)
    : t_oop_generator(program) {
    (void)option_string;
    std::map<std::string, std::string>::const_iterator iter;

    bean_style_ = false;
    android_style_ = false;
    private_members_ = false;
    nocamel_style_ = false;
    fullcamel_style_ = false;
    android_legacy_ = false;
    sorted_containers_ = false;
    java5_ = false;
    reuse_objects_ = false;
    use_option_type_ = false;
    generate_future_iface_ = false;
    use_jdk8_option_type_ = false;
    undated_generated_annotations_ = false;
    suppress_generated_annotations_ = false;
    rethrow_unhandled_exceptions_ = false;
    unsafe_binaries_ = false;
    annotations_as_metadata_ = false;
    jakarta_annotations_ = false;
    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if (iter->first.compare("beans") == 0) {
        bean_style_ = true;
      } else if (iter->first.compare("android") == 0) {
        android_style_ = true;
      } else if (iter->first.compare("private_members") == 0
                 || iter->first.compare("private-members") == 0) {
        // keep both private_members and private-members (legacy) for backwards compatibility
        private_members_ = true;
      } else if (iter->first.compare("nocamel") == 0) {
        nocamel_style_ = true;
      } else if (iter->first.compare("fullcamel") == 0) {
        fullcamel_style_ = true;
      } else if (iter->first.compare("android_legacy") == 0) {
        android_legacy_ = true;
      } else if (iter->first.compare("sorted_containers") == 0) {
        sorted_containers_ = true;
      } else if (iter->first.compare("java5") == 0) {
        java5_ = true;
      } else if (iter->first.compare("future_iface") == 0) {
        generate_future_iface_ = true;
      } else if (iter->first.compare("reuse_objects") == 0
                 || iter->first.compare("reuse-objects") == 0) {
        // keep both reuse_objects and reuse-objects (legacy) for backwards compatibility
        reuse_objects_ = true;
      } else if (iter->first.compare("option_type") == 0) {
        use_option_type_ = true;
        if (iter->second.compare("jdk8") == 0) {
          use_jdk8_option_type_ = true;
        } else if (iter->second.compare("thrift") == 0 || iter->second.compare("") == 0) {
          use_jdk8_option_type_ = false;
        } else {
          throw "option_type must be 'jdk8' or 'thrift'";
        }
      } else if (iter->first.compare("rethrow_unhandled_exceptions") == 0) {
        rethrow_unhandled_exceptions_ = true;
      } else if (iter->first.compare("generated_annotations") == 0) {
        if (iter->second.compare("undated") == 0) {
          undated_generated_annotations_ = true;
        } else if (iter->second.compare("suppress") == 0) {
          suppress_generated_annotations_ = true;
        } else {
          throw "unknown option java:" + iter->first + "=" + iter->second;
        }
      } else if (iter->first.compare("unsafe_binaries") == 0) {
        unsafe_binaries_ = true;
      } else if (iter->first.compare("annotations_as_metadata") == 0) {
        annotations_as_metadata_ = true;
      } else if (iter->first.compare("jakarta_annotations") == 0) {
        jakarta_annotations_ = true;
      } else {
        throw "unknown option java:" + iter->first;
      }
    }

    if (java5_) {
      android_legacy_ = true;
    }

    out_dir_base_ = (bean_style_ ? "gen-javabean" : "gen-java");
  }

  /**
   * Init and close methods
   */

  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;

  void generate_consts(std::vector<t_const*> consts) override;

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_struct(t_struct* tstruct) override;
  void generate_union(t_struct* tunion);
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;

  void print_const_value(std::ostream& out,
                         std::string name,
                         t_type* type,
                         t_const_value* value,
                         bool in_static,
                         bool defval = false);
  std::string render_const_value(std::ostream& out, t_type* type, t_const_value* value);

  /**
   * Service-level generation functions
   */

  void generate_java_struct(t_struct* tstruct, bool is_exception);

  void generate_java_struct_definition(std::ostream& out,
                                       t_struct* tstruct,
                                       bool is_xception = false,
                                       bool in_class = false,
                                       bool is_result = false);
  void generate_java_struct_parcelable(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_equality(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_compare_to(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_reader(std::ostream& out, t_struct* tstruct);
  void generate_java_validator(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_result_writer(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_writer(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_tostring(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_clear(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_write_object(std::ostream& out, t_struct* tstruct);
  void generate_java_struct_read_object(std::ostream& out, t_struct* tstruct);
  void generate_java_meta_data_map(std::ostream& out, t_struct* tstruct);
  void generate_field_value_meta_data(std::ostream& out, t_type* type);
  void generate_metadata_for_field_annotations(std::ostream& out, t_field* field);
  std::string get_java_type_string(t_type* type);
  void generate_java_struct_field_by_id(ostream& out, t_struct* tstruct);
  void generate_reflection_setters(std::ostringstream& out,
                                   t_type* type,
                                   std::string field_name,
                                   std::string cap_name);
  void generate_reflection_getters(std::ostringstream& out,
                                   t_type* type,
                                   std::string field_name,
                                   std::string cap_name);
  void generate_generic_field_getters_setters(std::ostream& out, t_struct* tstruct);
  void generate_generic_isset_method(std::ostream& out, t_struct* tstruct);
  void generate_java_bean_boilerplate(std::ostream& out, t_struct* tstruct);

  void generate_function_helpers(t_function* tfunction);
  std::string as_camel_case(std::string name, bool ucfirst = true);
  std::string get_rpc_method_name(std::string name);
  std::string get_cap_name(std::string name);
  std::string generate_isset_check(t_field* field);
  std::string generate_isset_check(std::string field);
  void generate_isset_set(ostream& out, t_field* field, std::string prefix);
  std::string isset_field_id(t_field* field);

  void generate_service_interface(t_service* tservice);
  void generate_service_async_interface(t_service* tservice);
  void generate_service_future_interface(t_service* tservice);
  void generate_service_helpers(t_service* tservice);
  void generate_service_client(t_service* tservice);
  void generate_service_async_client(t_service* tservice);
  void generate_service_future_client(t_service* tservice);
  void generate_service_server(t_service* tservice);
  void generate_service_async_server(t_service* tservice);
  void generate_process_function(t_service* tservice, t_function* tfunction);
  void generate_process_async_function(t_service* tservice, t_function* tfunction);

  void generate_java_union(t_struct* tstruct);
  void generate_union_constructor(ostream& out, t_struct* tstruct);
  void generate_union_getters_and_setters(ostream& out, t_struct* tstruct);
  void generate_union_is_set_methods(ostream& out, t_struct* tstruct);
  void generate_union_abstract_methods(ostream& out, t_struct* tstruct);
  void generate_check_type(ostream& out, t_struct* tstruct);
  void generate_standard_scheme_read_value(ostream& out, t_struct* tstruct);
  void generate_standard_scheme_write_value(ostream& out, t_struct* tstruct);
  void generate_tuple_scheme_read_value(ostream& out, t_struct* tstruct);
  void generate_tuple_scheme_write_value(ostream& out, t_struct* tstruct);
  void generate_get_field_desc(ostream& out, t_struct* tstruct);
  void generate_get_struct_desc(ostream& out, t_struct* tstruct);
  void generate_get_field_name(ostream& out, t_struct* tstruct);

  void generate_union_comparisons(ostream& out, t_struct* tstruct);
  void generate_union_hashcode(ostream& out, t_struct* tstruct);

  void generate_scheme_map(ostream& out, t_struct* tstruct);
  void generate_standard_writer(ostream& out, t_struct* tstruct, bool is_result);
  void generate_standard_reader(ostream& out, t_struct* tstruct);
  void generate_java_struct_standard_scheme(ostream& out, t_struct* tstruct, bool is_result);

  void generate_java_struct_tuple_scheme(ostream& out, t_struct* tstruct);
  void generate_java_struct_tuple_reader(ostream& out, t_struct* tstruct);
  void generate_java_struct_tuple_writer(ostream& out, t_struct* tstruct);

  void generate_java_scheme_lookup(ostream& out);

  void generate_javax_generated_annotation(ostream& out);
  /**
   * Serialization constructs
   */

  void generate_deserialize_field(std::ostream& out,
                                  t_field* tfield,
                                  std::string prefix = "",
                                  bool has_metadata = true);

  void generate_deserialize_struct(std::ostream& out, t_struct* tstruct, std::string prefix = "");

  void generate_deserialize_container(std::ostream& out,
                                      t_type* ttype,
                                      std::string prefix = "",
                                      bool has_metadata = true);

  void generate_deserialize_set_element(std::ostream& out,
                                        t_set* tset,
                                        std::string prefix = "",
                                        std::string obj = "",
                                        bool has_metadata = true);

  void generate_deserialize_map_element(std::ostream& out,
                                        t_map* tmap,
                                        std::string prefix = "",
                                        std::string obj = "",
                                        bool has_metadata = true);

  void generate_deserialize_list_element(std::ostream& out,
                                         t_list* tlist,
                                         std::string prefix = "",
                                         std::string obj = "",
                                         bool has_metadata = true);

  void generate_serialize_field(std::ostream& out,
                                t_field* tfield,
                                std::string prefix = "",
                                std::string postfix = "",
                                bool has_metadata = true);

  void generate_serialize_struct(std::ostream& out, t_struct* tstruct, std::string prefix = "");

  void generate_serialize_container(std::ostream& out,
                                    t_type* ttype,
                                    std::string prefix = "",
                                    bool has_metadata = true);

  void generate_serialize_map_element(std::ostream& out,
                                      t_map* tmap,
                                      std::string iter,
                                      std::string map,
                                      bool has_metadata = true);

  void generate_serialize_set_element(std::ostream& out,
                                      t_set* tmap,
                                      std::string iter,
                                      bool has_metadata = true);

  void generate_serialize_list_element(std::ostream& out,
                                       t_list* tlist,
                                       std::string iter,
                                       bool has_metadata = true);

  void generate_deep_copy_container(std::ostream& out,
                                    std::string source_name_p1,
                                    std::string source_name_p2,
                                    std::string result_name,
                                    t_type* type);
  void generate_deep_copy_non_container(std::ostream& out,
                                        std::string source_name,
                                        std::string dest_name,
                                        t_type* type);

  enum isset_type { ISSET_NONE, ISSET_PRIMITIVE, ISSET_BITSET };
  isset_type needs_isset(t_struct* tstruct, std::string* outPrimitiveType = nullptr);

  /**
   * Helper rendering functions
   */

  std::string java_package();
  std::string java_suppressions();
  std::string java_nullable_annotation();
  std::string java_override_annotation();
  std::string type_name(t_type* ttype,
                        bool in_container = false,
                        bool in_init = false,
                        bool skip_generic = false,
                        bool force_namespace = false);
  std::string base_type_name(t_base_type* tbase, bool in_container = false);
  std::string declare_field(t_field* tfield, bool init = false, bool comment = false);
  std::string function_signature(t_function* tfunction, std::string prefix = "");
  std::string function_signature_async(t_function* tfunction,
                                       bool use_base_method = false,
                                       std::string prefix = "");
  std::string function_signature_future(t_function* tfunction, std::string prefix = "");
  std::string argument_list(t_struct* tstruct, bool include_types = true);
  std::string async_function_call_arglist(t_function* tfunc,
                                          bool use_base_method = true,
                                          bool include_types = true);
  std::string async_argument_list(t_function* tfunct,
                                  t_struct* tstruct,
                                  t_type* ttype,
                                  bool include_types = false);
  std::string type_to_enum(t_type* ttype);
  void generate_struct_desc(ostream& out, t_struct* tstruct);
  void generate_field_descs(ostream& out, t_struct* tstruct);
  void generate_field_name_constants(ostream& out, t_struct* tstruct);

  std::string make_valid_java_filename(std::string const& fromName);
  std::string make_valid_java_identifier(std::string const& fromName);

  string normalize_name(string name);

  bool type_can_be_null(t_type* ttype) {
    ttype = get_true_type(ttype);

    return ttype->is_container() || ttype->is_struct() || ttype->is_xception() || ttype->is_string()
           || ttype->is_uuid() || ttype->is_enum();
  }

  bool is_deprecated(const std::map<std::string, std::vector<std::string>>& annotations) {
    return annotations.find("deprecated") != annotations.end();
  }

  bool is_deprecated(const std::map<std::string, std::string>& annotations) {
    return annotations.find("deprecated") != annotations.end();
  }

  bool is_enum_set(t_type* ttype) {
    if (!sorted_containers_) {
      ttype = get_true_type(ttype);
      if (ttype->is_set()) {
        t_set* tset = (t_set*)ttype;
        t_type* elem_type = get_true_type(tset->get_elem_type());
        return elem_type->is_enum();
      }
    }
    return false;
  }

  bool is_enum_map(t_type* ttype) {
    if (!sorted_containers_) {
      ttype = get_true_type(ttype);
      if (ttype->is_map()) {
        t_map* tmap = (t_map*)ttype;
        t_type* key_type = get_true_type(tmap->get_key_type());
        return key_type->is_enum();
      }
    }
    return false;
  }

  std::string inner_enum_type_name(t_type* ttype) {
    ttype = get_true_type(ttype);
    if (ttype->is_map()) {
      t_map* tmap = (t_map*)ttype;
      t_type* key_type = get_true_type(tmap->get_key_type());
      return type_name(key_type, true) + ".class";
    } else if (ttype->is_set()) {
      t_set* tset = (t_set*)ttype;
      t_type* elem_type = get_true_type(tset->get_elem_type());
      return type_name(elem_type, true) + ".class";
    }
    return "";
  }

  std::string constant_name(std::string name);

private:
  /**
   * File streams
   */

  std::string package_name_;
  ofstream_with_content_based_conditional_update f_service_;
  std::string package_dir_;

  // keywords (according to Oracle docs)
  // true, false, and null might seem like keywords, but they are actually literals;
  // you cannot use them as identifiers in your programs.
  const std::string JAVA_KEYWORDS[53] = {
    "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
    "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if",
    "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private",
    "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this",
    "throw", "throws", "transient", "try", "void", "volatile", "while", "true", "false", "null"
  };
  std::set<string> java_keywords = std::set<string>(JAVA_KEYWORDS, JAVA_KEYWORDS + sizeof(JAVA_KEYWORDS) / sizeof(JAVA_KEYWORDS[0]));

  bool bean_style_;
  bool android_style_;
  bool private_members_;
  bool nocamel_style_;
  bool fullcamel_style_;
  bool android_legacy_;
  bool java5_;
  bool sorted_containers_;
  bool reuse_objects_;
  bool generate_future_iface_;
  bool use_option_type_;
  bool use_jdk8_option_type_;
  bool undated_generated_annotations_;
  bool suppress_generated_annotations_;
  bool rethrow_unhandled_exceptions_;
  bool unsafe_binaries_;
  bool annotations_as_metadata_;
  bool jakarta_annotations_;
};

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_java_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());
  package_name_ = program_->get_namespace("java");

  string dir = package_name_;
  string subdir = get_out_dir();
  string::size_type loc;
  while ((loc = dir.find(".")) != string::npos) {
    subdir = subdir + "/" + dir.substr(0, loc);
    MKDIR(subdir.c_str());
    dir = dir.substr(loc + 1);
  }
  if (dir.size() > 0) {
    subdir = subdir + "/" + dir;
    MKDIR(subdir.c_str());
  }

  package_dir_ = subdir;
}

/**
 * Packages the generated file
 *
 * @return String of the package, i.e. "package org.apache.thriftdemo;"
 */
string t_java_generator::java_package() {
  if (!package_name_.empty()) {
    return string("package ") + package_name_ + ";\n\n";
  }
  return "";
}

string t_java_generator::java_suppressions() {
  return "@SuppressWarnings({\"cast\", \"rawtypes\", \"serial\", \"unchecked\", \"unused\"})\n";
}

string t_java_generator::java_nullable_annotation() {
  return "@org.apache.thrift.annotation.Nullable";
}

string t_java_generator::java_override_annotation() {
  return "@Override";
}

string t_java_generator::normalize_name(string name)
{
    string tmp(name);
    //transform(tmp.begin(), tmp.end(), tmp.begin(), static_cast<int(*)(int)>(tolower));

    // un-conflict keywords by prefixing with "$"
    if (java_keywords.find(tmp) != java_keywords.end())
    {
        return "$" + name;
    }

    // no changes necessary
    return name;
}

/**
 * Nothing in Java
 */
void t_java_generator::close_generator() {}

/**
 * Generates a typedef. This is not done in Java, since it does
 * not support arbitrary name replacements, and it'd be a wacky waste
 * of overhead to make wrapper classes.
 *
 * @param ttypedef The type definition
 */
void t_java_generator::generate_typedef(t_typedef* ttypedef) {
  (void)ttypedef;
}

/**
 * Enums are a class with a set of static constants.
 *
 * @param tenum The enumeration
 */
void t_java_generator::generate_enum(t_enum* tenum) {
  bool is_deprecated = this->is_deprecated(tenum->annotations_);
  // Make output file
  string f_enum_name = package_dir_ + "/" + make_valid_java_filename(tenum->get_name()) + ".java";
  ofstream_with_content_based_conditional_update f_enum;
  f_enum.open(f_enum_name.c_str());

  // Comment and package it
  f_enum << autogen_comment() << java_package() << '\n';

  generate_java_doc(f_enum, tenum);

  if (!suppress_generated_annotations_) {
    generate_javax_generated_annotation(f_enum);
  }

  if (is_deprecated) {
    indent(f_enum) << "@Deprecated" << '\n';
  }
  indent(f_enum) << "public enum " << tenum->get_name() << " implements org.apache.thrift.TEnum ";
  scope_up(f_enum);

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  bool first = true;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();

    if (first) {
      first = false;
    } else {
      f_enum << "," << '\n';
    }

    generate_java_doc(f_enum, *c_iter);
    if (this->is_deprecated((*c_iter)->annotations_)) {
      indent(f_enum) << "@Deprecated" << '\n';
    }
    indent(f_enum) << (*c_iter)->get_name() << "(" << value << ")";
  }
  f_enum << ";" << '\n' << '\n';

  // Field for thriftCode
  indent(f_enum) << "private final int value;" << '\n' << '\n';

  indent(f_enum) << "private " << tenum->get_name() << "(int value) {" << '\n';
  indent(f_enum) << "  this.value = value;" << '\n';
  indent(f_enum) << "}" << '\n' << '\n';

  indent(f_enum) << "/**" << '\n';
  indent(f_enum) << " * Get the integer value of this enum value, as defined in the Thrift IDL."
                 << '\n';
  indent(f_enum) << " */" << '\n';
  indent(f_enum) << java_override_annotation() << '\n';
  indent(f_enum) << "public int getValue() {" << '\n';
  indent(f_enum) << "  return value;" << '\n';
  indent(f_enum) << "}" << '\n' << '\n';

  indent(f_enum) << "/**" << '\n';
  indent(f_enum) << " * Find a the enum type by its integer value, as defined in the Thrift IDL."
                 << '\n';
  indent(f_enum) << " * @return null if the value is not found." << '\n';
  indent(f_enum) << " */" << '\n';
  indent(f_enum) << java_nullable_annotation() << '\n';
  indent(f_enum) << "public static " + tenum->get_name() + " findByValue(int value) { " << '\n';

  indent_up();

  indent(f_enum) << "switch (value) {" << '\n';
  indent_up();

  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    indent(f_enum) << "case " << value << ":" << '\n';
    indent(f_enum) << "  return " << (*c_iter)->get_name() << ";" << '\n';
  }

  indent(f_enum) << "default:" << '\n';
  indent(f_enum) << "  return null;" << '\n';

  indent_down();

  indent(f_enum) << "}" << '\n';

  indent_down();

  indent(f_enum) << "}" << '\n';

  scope_down(f_enum);

  f_enum.close();
}

/**
 * Generates a class that holds all the constants.
 */
void t_java_generator::generate_consts(std::vector<t_const*> consts) {
  if (consts.empty()) {
    return;
  }

  string f_consts_name
      = package_dir_ + '/' + make_valid_java_filename(program_name_) + "Constants.java";
  ofstream_with_content_based_conditional_update f_consts;
  f_consts.open(f_consts_name.c_str());

  // Print header
  f_consts << autogen_comment() << java_package() << java_suppressions();
  f_consts << "public class " << make_valid_java_identifier(program_name_) << "Constants {" << '\n'
           << '\n';
  indent_up();
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    generate_java_doc(f_consts, (*c_iter));
    print_const_value(f_consts, (*c_iter)->get_name(), (*c_iter)->get_type(),
                      (*c_iter)->get_value(), false);
  }
  indent_down();
  indent(f_consts) << "}" << '\n';
  f_consts.close();
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
void t_java_generator::print_const_value(std::ostream& out,
                                         string name,
                                         t_type* type,
                                         t_const_value* value,
                                         bool in_static,
                                         bool defval) {
  type = get_true_type(type);

  indent(out);
  if (!defval) {
    out << (in_static ? "" : "public static final ") << type_name(type) << " ";
  }
  if (type->is_base_type()) {
    string v2 = render_const_value(out, type, value);
    out << name << " = " << v2 << ";" << '\n' << '\n';
  } else if (type->is_enum()) {
    out << name << " = " << render_const_value(out, type, value) << ";" << '\n' << '\n';
  } else if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& unsorted_fields = ((t_struct*)type)->get_members();
    vector<t_field*> fields = unsorted_fields;
    std::sort(fields.begin(), fields.end(), t_field::key_compare());
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
    out << name << " = new " << type_name(type, false, true) << "();" << '\n';
    if (!in_static) {
      indent(out) << "static {" << '\n';
      indent_up();
    }
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = nullptr;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
        }
      }
      if (field_type == nullptr) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      string val = render_const_value(out, field_type, v_iter->second);
      indent(out) << name << ".";
      std::string cap_name = get_cap_name(v_iter->first->get_string());
      out << "set" << cap_name << "(" << val << ");" << '\n';
    }
    if (!in_static) {
      indent_down();
      indent(out) << "}" << '\n';
    }
    out << '\n';
  } else if (type->is_map()) {
    std::string constructor_args;
    if (is_enum_map(type)) {
      constructor_args = inner_enum_type_name(type);
    }
    out << name << " = new " << type_name(type, false, true) << "(" << constructor_args << ");"
        << '\n';
    if (!in_static) {
      indent(out) << "static {" << '\n';
      indent_up();
    }
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(out, ktype, v_iter->first);
      string val = render_const_value(out, vtype, v_iter->second);
      indent(out) << name << ".put(" << key << ", " << val << ");" << '\n';
    }
    if (!in_static) {
      indent_down();
      indent(out) << "}" << '\n';
    }
    out << '\n';
  } else if (type->is_list() || type->is_set()) {
    if (is_enum_set(type)) {
      out << name << " = " << type_name(type, false, true, true) << ".noneOf("
          << inner_enum_type_name(type) << ");" << '\n';
    } else {
      out << name << " = new " << type_name(type, false, true) << "();" << '\n';
    }
    if (!in_static) {
      indent(out) << "static {" << '\n';
      indent_up();
    }
    t_type* etype;
    if (type->is_list()) {
      etype = ((t_list*)type)->get_elem_type();
    } else {
      etype = ((t_set*)type)->get_elem_type();
    }
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(out, etype, *v_iter);
      indent(out) << name << ".add(" << val << ");" << '\n';
    }
    if (!in_static) {
      indent_down();
      indent(out) << "}" << '\n';
    }
    out << '\n';
  } else {
    throw "compiler error: no const of type " + type->get_name();
  }
}

string t_java_generator::render_const_value(ostream& out, t_type* type, t_const_value* value) {
  type = get_true_type(type);
  std::ostringstream render;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      if (((t_base_type*)type)->is_binary()) {
        render << "java.nio.ByteBuffer.wrap(\"" << get_escaped_string(value) << "\".getBytes())";
      } else {
        render << '"' << get_escaped_string(value) << '"';
      }
      break;
    case t_base_type::TYPE_UUID:
      render << "java.util.UUID.fromString(\"" << get_escaped_string(value) << "\")";
      break;
    case t_base_type::TYPE_BOOL:
      render << ((value->get_integer() > 0) ? "true" : "false");
      break;
    case t_base_type::TYPE_I8:
      render << "(byte)" << value->get_integer();
      break;
    case t_base_type::TYPE_I16:
      render << "(short)" << value->get_integer();
      break;
    case t_base_type::TYPE_I32:
      render << value->get_integer();
      break;
    case t_base_type::TYPE_I64:
      render << value->get_integer() << "L";
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        render << value->get_integer() << "d";
      } else {
        render << emit_double_as_string(value->get_double());
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    std::string namespace_prefix = type->get_program()->get_namespace("java");
    if (namespace_prefix.length() > 0) {
      namespace_prefix += ".";
    }
    render << namespace_prefix << value->get_identifier_with_parent();
  } else {
    string t = tmp("tmp");
    print_const_value(out, t, type, value, true);
    render << t;
  }

  return render.str();
}

/**
 * Generates a struct definition for a thrift data type. This will be a org.apache.thrift.TBase
 * implementor.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_struct(t_struct* tstruct) {
  if (tstruct->is_union()) {
    generate_java_union(tstruct);
  } else {
    generate_java_struct(tstruct, false);
  }
}

/**
 * Exceptions are structs, but they inherit from Exception
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_xception(t_struct* txception) {
  generate_java_struct(txception, true);
}

/**
 * Java struct definition.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct(t_struct* tstruct, bool is_exception) {
  // Make output file
  string f_struct_name
      = package_dir_ + "/" + make_valid_java_filename(tstruct->get_name()) + ".java";
  ofstream_with_content_based_conditional_update f_struct;
  f_struct.open(f_struct_name.c_str());

  f_struct << autogen_comment() << java_package();

  generate_java_struct_definition(f_struct, tstruct, is_exception);
  f_struct.close();
}

/**
 * Java union definition.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_union(t_struct* tstruct) {
  // Make output file
  string f_struct_name
      = package_dir_ + "/" + make_valid_java_filename(tstruct->get_name()) + ".java";
  ofstream_with_content_based_conditional_update f_struct;
  f_struct.open(f_struct_name.c_str());

  f_struct << autogen_comment() << java_package();

  generate_java_doc(f_struct, tstruct);
  f_struct << java_suppressions();

  bool is_final = (tstruct->annotations_.find("final") != tstruct->annotations_.end());
  bool is_deprecated = this->is_deprecated(tstruct->annotations_);

  if (!suppress_generated_annotations_) {
    generate_javax_generated_annotation(f_struct);
  }

  if (is_deprecated) {
    indent(f_struct) << "@Deprecated" << '\n';
  }
  indent(f_struct) << "public " << (is_final ? "final " : "") << "class " << make_valid_java_identifier(tstruct->get_name())
                   << " extends org.apache.thrift.TUnion<" << make_valid_java_identifier(tstruct->get_name()) << ", "
                   << make_valid_java_identifier(tstruct->get_name()) << "._Fields> ";

  scope_up(f_struct);

  generate_struct_desc(f_struct, tstruct);
  generate_field_descs(f_struct, tstruct);

  f_struct << '\n';

  generate_field_name_constants(f_struct, tstruct);

  f_struct << '\n';

  generate_java_meta_data_map(f_struct, tstruct);

  generate_union_constructor(f_struct, tstruct);

  f_struct << '\n';

  generate_union_abstract_methods(f_struct, tstruct);

  f_struct << '\n';

  generate_java_struct_field_by_id(f_struct, tstruct);

  f_struct << '\n';

  generate_union_getters_and_setters(f_struct, tstruct);

  f_struct << '\n';

  generate_union_is_set_methods(f_struct, tstruct);

  f_struct << '\n';

  generate_union_comparisons(f_struct, tstruct);

  f_struct << '\n';

  generate_union_hashcode(f_struct, tstruct);

  f_struct << '\n';

  generate_java_struct_write_object(f_struct, tstruct);

  f_struct << '\n';

  generate_java_struct_read_object(f_struct, tstruct);

  f_struct << '\n';

  scope_down(f_struct);

  f_struct.close();
}

void t_java_generator::generate_union_constructor(ostream& out, t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent(out) << "public " << type_name(tstruct) << "() {" << '\n';
  indent_up();
  bool default_value = false;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* type = get_true_type((*m_iter)->get_type());
    if ((*m_iter)->get_value() != nullptr) {
      indent(out) << "super(_Fields." << constant_name((*m_iter)->get_name()) << ", "
                  << render_const_value(out, type, (*m_iter)->get_value()) << ");" << '\n';
      default_value = true;
      break;
    }
  }
  if (default_value == false) {
    indent(out) << "super();" << '\n';
  }
  indent_down();
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "public " << type_name(tstruct) << "(_Fields setField, java.lang.Object value) {"
              << '\n';
  indent(out) << "  super(setField, value);" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "public " << type_name(tstruct)
              << "(" << type_name(tstruct) << " other) {"
              << '\n';
  indent(out) << "  super(other);" << '\n';
  indent(out) << "}" << '\n';

  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public " << make_valid_java_identifier(tstruct->get_name()) << " deepCopy() {" << '\n';
  indent(out) << "  return new " << tstruct->get_name() << "(this);" << '\n';
  indent(out) << "}" << '\n' << '\n';

  // generate "constructors" for each field
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* type = (*m_iter)->get_type();
    indent(out) << "public static " << type_name(tstruct) << " " << (*m_iter)->get_name() << "("
                << type_name(type) << " value) {" << '\n';
    indent(out) << "  " << type_name(tstruct) << " x = new " << type_name(tstruct) << "();" << '\n';
    indent(out) << "  x.set" << get_cap_name((*m_iter)->get_name()) << "(value);" << '\n';
    indent(out) << "  return x;" << '\n';
    indent(out) << "}" << '\n' << '\n';

    if (type->is_binary()) {
      indent(out) << "public static " << type_name(tstruct) << " " << (*m_iter)->get_name()
                  << "(byte[] value) {" << '\n';
      indent(out) << "  " << type_name(tstruct) << " x = new " << type_name(tstruct) << "();"
                  << '\n';
      indent(out) << "  x.set" << get_cap_name((*m_iter)->get_name());
      if (unsafe_binaries_) {
        indent(out) << "(java.nio.ByteBuffer.wrap(value));" << '\n';
      } else {
        indent(out) << "(java.nio.ByteBuffer.wrap(value.clone()));" << '\n';
      }
      indent(out) << "  return x;" << '\n';
      indent(out) << "}" << '\n' << '\n';
    }
  }
}

void t_java_generator::generate_union_getters_and_setters(ostream& out, t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  bool first = true;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (first) {
      first = false;
    } else {
      out << '\n';
    }

    t_field* field = (*m_iter);
    t_type* type = field->get_type();
    std::string cap_name = get_cap_name(field->get_name());
    bool is_deprecated = this->is_deprecated(field->annotations_);

    generate_java_doc(out, field);
    if (type->is_binary()) {
      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public byte[] get" << cap_name << "() {" << '\n';
      indent(out) << "  set" << cap_name << "(org.apache.thrift.TBaseHelper.rightSize(buffer"
                  << get_cap_name("for") << cap_name << "()));" << '\n';
      indent(out) << "  java.nio.ByteBuffer b = buffer" << get_cap_name("for") << cap_name << "();"
                  << '\n';
      indent(out) << "  return b == null ? null : b.array();" << '\n';
      indent(out) << "}" << '\n';

      out << '\n';

      indent(out) << "public java.nio.ByteBuffer buffer" << get_cap_name("for")
                  << get_cap_name(field->get_name()) << "() {" << '\n';
      indent(out) << "  if (getSetField() == _Fields." << constant_name(field->get_name()) << ") {"
                  << '\n';

      if (unsafe_binaries_) {
        indent(out) << "    return (java.nio.ByteBuffer)getFieldValue();" << '\n';
      } else {
        indent(out)
            << "    return "
               "org.apache.thrift.TBaseHelper.copyBinary((java.nio.ByteBuffer)getFieldValue());"
            << '\n';
      }

      indent(out) << "  } else {" << '\n';
      indent(out) << "    throw new java.lang.RuntimeException(\"Cannot get field '"
                  << field->get_name()
                  << "' because union is currently set to \" + getFieldDesc(getSetField()).name);"
                  << '\n';
      indent(out) << "  }" << '\n';
      indent(out) << "}" << '\n';
    } else {
      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public " << type_name(field->get_type()) << " get"
                  << get_cap_name(field->get_name()) << "() {" << '\n';
      indent(out) << "  if (getSetField() == _Fields." << constant_name(field->get_name()) << ") {"
                  << '\n';
      indent(out) << "    return (" << type_name(field->get_type(), true) << ")getFieldValue();"
                  << '\n';
      indent(out) << "  } else {" << '\n';
      indent(out) << "    throw new java.lang.RuntimeException(\"Cannot get field '"
                  << field->get_name()
                  << "' because union is currently set to \" + getFieldDesc(getSetField()).name);"
                  << '\n';
      indent(out) << "  }" << '\n';
      indent(out) << "}" << '\n';
    }

    out << '\n';

    generate_java_doc(out, field);
    if (type->is_binary()) {
      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public void set" << get_cap_name(field->get_name()) << "(byte[] value) {"
                  << '\n';
      indent(out) << "  set" << get_cap_name(field->get_name());

      if (unsafe_binaries_) {
        indent(out) << "(java.nio.ByteBuffer.wrap(value));" << '\n';
      } else {
        indent(out) << "(java.nio.ByteBuffer.wrap(value.clone()));" << '\n';
      }

      indent(out) << "}" << '\n';

      out << '\n';
    }
    if (is_deprecated) {
      indent(out) << "@Deprecated" << '\n';
    }
    indent(out) << "public void set" << get_cap_name(field->get_name()) << "("
                << type_name(field->get_type()) << " value) {" << '\n';

    indent(out) << "  setField_ = _Fields." << constant_name(field->get_name()) << ";" << '\n';

    if (type_can_be_null(field->get_type())) {
      indent(out) << "  value_ = java.util.Objects.requireNonNull(value,\""
                  << "_Fields." << constant_name(field->get_name()) << "\");" << '\n';
    } else {
      indent(out) << "  value_ = value;" << '\n';
    }

    indent(out) << "}" << '\n';
  }
}

void t_java_generator::generate_union_is_set_methods(ostream& out, t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  bool first = true;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (first) {
      first = false;
    } else {
      out << '\n';
    }

    std::string field_name = (*m_iter)->get_name();

    indent(out) << "public boolean is" << get_cap_name("set") << get_cap_name(field_name) << "() {"
                << '\n';
    indent_up();
    indent(out) << "return setField_ == _Fields." << constant_name(field_name) << ";" << '\n';
    indent_down();
    indent(out) << "}" << '\n' << '\n';
  }
}

void t_java_generator::generate_union_abstract_methods(ostream& out, t_struct* tstruct) {
  generate_check_type(out, tstruct);
  out << '\n';
  generate_standard_scheme_read_value(out, tstruct);
  out << '\n';
  generate_standard_scheme_write_value(out, tstruct);
  out << '\n';
  generate_tuple_scheme_read_value(out, tstruct);
  out << '\n';
  generate_tuple_scheme_write_value(out, tstruct);
  out << '\n';
  generate_get_field_desc(out, tstruct);
  out << '\n';
  generate_get_struct_desc(out, tstruct);
  out << '\n';
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "protected _Fields enumForId(short id) {" << '\n';
  indent(out) << "  return _Fields.findByThriftIdOrThrow(id);" << '\n';
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_check_type(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "protected void checkType(_Fields setField, java.lang.Object value) throws "
                 "java.lang.ClassCastException {"
              << '\n';
  indent_up();

  indent(out) << "switch (setField) {" << '\n';
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);

    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent(out) << "  if (value instanceof " << type_name(field->get_type(), true, false, true)
                << ") {" << '\n';
    indent(out) << "    break;" << '\n';
    indent(out) << "  }" << '\n';
    indent(out) << "  throw new java.lang.ClassCastException(\"Was expecting value of type "
                << type_name(field->get_type(), true, false) << " for field '" << field->get_name()
                << "', but got \" + value.getClass().getSimpleName());" << '\n';
    // do the real check here
  }

  indent(out) << "default:" << '\n';
  indent(out) << "  throw new java.lang.IllegalArgumentException(\"Unknown field id \" + setField);"
              << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_standard_scheme_read_value(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out)
      << "protected java.lang.Object standardSchemeReadValue(org.apache.thrift.protocol.TProtocol "
         "iprot, org.apache.thrift.protocol.TField field) throws "
         "org.apache.thrift.TException {"
      << '\n';

  indent_up();

  indent(out) << "_Fields setField = _Fields.findByThriftId(field.id);" << '\n';
  indent(out) << "if (setField != null) {" << '\n';
  indent_up();
  indent(out) << "switch (setField) {" << '\n';
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);

    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent_up();
    indent(out) << "if (field.type == " << constant_name(field->get_name()) << "_FIELD_DESC.type) {"
                << '\n';
    indent_up();
    indent(out) << type_name(field->get_type(), true, false) << " " << field->get_name() << ";"
                << '\n';
    generate_deserialize_field(out, field, "");
    indent(out) << "return " << field->get_name() << ";" << '\n';
    indent_down();
    indent(out) << "} else {" << '\n';
    indent(out) << "  org.apache.thrift.protocol.TProtocolUtil.skip(iprot, field.type);" << '\n';
    indent(out) << "  return null;" << '\n';
    indent(out) << "}" << '\n';
    indent_down();
  }

  indent(out) << "default:" << '\n';
  indent(out)
      << "  throw new java.lang.IllegalStateException(\"setField wasn't null, but didn't match any "
         "of the case statements!\");"
      << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();
  indent(out) << "} else {" << '\n';
  indent_up();
  indent(out) << "org.apache.thrift.protocol.TProtocolUtil.skip(iprot, field.type);" << '\n';
  indent(out) << "return null;" << '\n';
  indent_down();
  indent(out) << "}" << '\n';

  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_standard_scheme_write_value(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "protected void standardSchemeWriteValue(org.apache.thrift.protocol.TProtocol "
                 "oprot) throws org.apache.thrift.TException {"
              << '\n';

  indent_up();

  indent(out) << "switch (setField_) {" << '\n';
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);

    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent_up();
    indent(out) << type_name(field->get_type(), true, false) << " " << field->get_name() << " = ("
                << type_name(field->get_type(), true, false) << ")value_;" << '\n';
    generate_serialize_field(out, field);
    indent(out) << "return;" << '\n';
    indent_down();
  }

  indent(out) << "default:" << '\n';
  indent(out)
      << "  throw new java.lang.IllegalStateException(\"Cannot write union with unknown field \" + "
         "setField_);"
      << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();

  indent(out) << "}" << '\n';
}

void t_java_generator::generate_tuple_scheme_read_value(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out)
      << "protected java.lang.Object tupleSchemeReadValue(org.apache.thrift.protocol.TProtocol "
         "iprot, short fieldID) throws org.apache.thrift.TException {"
      << '\n';

  indent_up();

  indent(out) << "_Fields setField = _Fields.findByThriftId(fieldID);" << '\n';
  indent(out) << "if (setField != null) {" << '\n';
  indent_up();
  indent(out) << "switch (setField) {" << '\n';
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);

    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent_up();
    indent(out) << type_name(field->get_type(), true, false) << " " << field->get_name() << ";"
                << '\n';
    generate_deserialize_field(out, field, "");
    indent(out) << "return " << field->get_name() << ";" << '\n';
    indent_down();
  }

  indent(out) << "default:" << '\n';
  indent(out)
      << "  throw new java.lang.IllegalStateException(\"setField wasn't null, but didn't match any "
         "of the case statements!\");"
      << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();
  indent(out) << "} else {" << '\n';
  indent_up();
  indent(out) << "throw new org.apache.thrift.protocol.TProtocolException(\"Couldn't find a field "
                 "with field id \" + fieldID);"
              << '\n';
  indent_down();
  indent(out) << "}" << '\n';
  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_tuple_scheme_write_value(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "protected void tupleSchemeWriteValue(org.apache.thrift.protocol.TProtocol oprot) "
                 "throws org.apache.thrift.TException {"
              << '\n';

  indent_up();

  indent(out) << "switch (setField_) {" << '\n';
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);

    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent_up();
    indent(out) << type_name(field->get_type(), true, false) << " " << field->get_name() << " = ("
                << type_name(field->get_type(), true, false) << ")value_;" << '\n';
    generate_serialize_field(out, field);
    indent(out) << "return;" << '\n';
    indent_down();
  }

  indent(out) << "default:" << '\n';
  indent(out)
      << "  throw new java.lang.IllegalStateException(\"Cannot write union with unknown field \" + "
         "setField_);"
      << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();

  indent(out) << "}" << '\n';
}

void t_java_generator::generate_get_field_desc(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "protected org.apache.thrift.protocol.TField getFieldDesc(_Fields setField) {"
              << '\n';
  indent_up();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent(out) << "switch (setField) {" << '\n';
  indent_up();

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);
    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent(out) << "  return " << constant_name(field->get_name()) << "_FIELD_DESC;" << '\n';
  }

  indent(out) << "default:" << '\n';
  indent(out) << "  throw new java.lang.IllegalArgumentException(\"Unknown field id \" + setField);"
              << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_get_struct_desc(ostream& out, t_struct* tstruct) {
  (void)tstruct;
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "protected org.apache.thrift.protocol.TStruct getStructDesc() {" << '\n';
  indent(out) << "  return STRUCT_DESC;" << '\n';
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_union_comparisons(ostream& out, t_struct* tstruct) {
  // equality
  indent(out) << "public boolean equals(java.lang.Object other) {" << '\n';
  indent(out) << "  if (other instanceof " << make_valid_java_identifier(tstruct->get_name()) << ") {" << '\n';
  indent(out) << "    return equals((" << make_valid_java_identifier(tstruct->get_name()) << ")other);" << '\n';
  indent(out) << "  } else {" << '\n';
  indent(out) << "    return false;" << '\n';
  indent(out) << "  }" << '\n';
  indent(out) << "}" << '\n';

  out << '\n';

  indent(out) << "public boolean equals(" << make_valid_java_identifier(tstruct->get_name()) << " other) {" << '\n';
  indent(out) << "  return other != null && getSetField() == other.getSetField() && "
                 "getFieldValue().equals(other.getFieldValue());"
              << '\n';
  indent(out) << "}" << '\n';
  out << '\n';

  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public int compareTo(" << type_name(tstruct) << " other) {" << '\n';
  indent(out) << "  int lastComparison = org.apache.thrift.TBaseHelper.compareTo(getSetField(), "
                 "other.getSetField());"
              << '\n';
  indent(out) << "  if (lastComparison == 0) {" << '\n';
  indent(out) << "    return org.apache.thrift.TBaseHelper.compareTo(getFieldValue(), "
                 "other.getFieldValue());"
              << '\n';
  indent(out) << "  }" << '\n';
  indent(out) << "  return lastComparison;" << '\n';
  indent(out) << "}" << '\n';
  out << '\n';
}

void t_java_generator::generate_union_hashcode(ostream& out, t_struct* tstruct) {
  (void)tstruct;
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public int hashCode() {" << '\n';
  indent(out)
      << "  java.util.List<java.lang.Object> list = new java.util.ArrayList<java.lang.Object>();"
      << '\n';
  indent(out) << "  list.add(this.getClass().getName());" << '\n';
  indent(out) << "  org.apache.thrift.TFieldIdEnum setField = getSetField();" << '\n';
  indent(out) << "  if (setField != null) {" << '\n';
  indent(out) << "    list.add(setField.getThriftFieldId());" << '\n';
  indent(out) << "    java.lang.Object value = getFieldValue();" << '\n';
  indent(out) << "    if (value instanceof org.apache.thrift.TEnum) {" << '\n';
  indent(out) << "      list.add(((org.apache.thrift.TEnum)getFieldValue()).getValue());" << '\n';
  indent(out) << "    } else {" << '\n';
  indent(out) << "      list.add(value);" << '\n';
  indent(out) << "    }" << '\n';
  indent(out) << "  }" << '\n';
  indent(out) << "  return list.hashCode();" << '\n';
  indent(out) << "}";
}

/**
 * Java struct definition. This has various parameters, as it could be
 * generated standalone or inside another class as a helper. If it
 * is a helper than it is a static class.
 *
 * @param tstruct      The struct definition
 * @param is_exception Is this an exception?
 * @param in_class     If inside a class, needs to be static class
 * @param is_result    If this is a result it needs a different writer
 */
void t_java_generator::generate_java_struct_definition(ostream& out,
                                                       t_struct* tstruct,
                                                       bool is_exception,
                                                       bool in_class,
                                                       bool is_result) {
  generate_java_doc(out, tstruct);
  indent(out) << java_suppressions();

  bool is_final = (tstruct->annotations_.find("final") != tstruct->annotations_.end());
  bool is_deprecated = this->is_deprecated(tstruct->annotations_);

  if (!in_class && !suppress_generated_annotations_) {
    generate_javax_generated_annotation(out);
  }

  if (is_deprecated) {
    indent(out) << "@Deprecated" << '\n';
  }
  indent(out) << "public " << (is_final ? "final " : "") << (in_class ? "static " : "") << "class "
              << make_valid_java_identifier(tstruct->get_name()) << " ";

  if (is_exception) {
    out << "extends org.apache.thrift.TException ";
  }
  out << "implements org.apache.thrift.TBase<" << make_valid_java_identifier(tstruct->get_name())
      << ", " << make_valid_java_identifier(tstruct->get_name())
      << "._Fields>, java.io.Serializable, Cloneable, Comparable<" << make_valid_java_identifier(tstruct->get_name()) << ">";

  if (android_style_) {
    out << ", android.os.Parcelable";
  }

  out << " ";

  scope_up(out);

  generate_struct_desc(out, tstruct);

  // Members are public for -java, private for -javabean
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  out << '\n';

  generate_field_descs(out, tstruct);

  out << '\n';

  generate_scheme_map(out, tstruct);

  out << '\n';

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (bean_style_ || private_members_) {
      indent(out) << "private ";
    } else {
      generate_java_doc(out, *m_iter);
      indent(out) << "public ";
    }
    out << declare_field(*m_iter, false, true) << '\n';
  }

  out << '\n';

  if (android_style_) {
    generate_java_struct_parcelable(out, tstruct);
  }

  generate_field_name_constants(out, tstruct);

  // isset data
  if (members.size() > 0) {
    out << '\n';

    indent(out) << "// isset id assignments" << '\n';

    int i = 0;
    int optionals = 0;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() == t_field::T_OPTIONAL) {
        optionals++;
      }
      if (!type_can_be_null((*m_iter)->get_type())) {
        indent(out) << "private static final int " << isset_field_id(*m_iter) << " = " << i << ";"
                    << '\n';
        i++;
      }
    }

    std::string primitiveType;
    switch (needs_isset(tstruct, &primitiveType)) {
    case ISSET_NONE:
      break;
    case ISSET_PRIMITIVE:
      indent(out) << "private " << primitiveType << " __isset_bitfield = 0;" << '\n';
      break;
    case ISSET_BITSET:
      indent(out) << "private java.util.BitSet __isset_bit_vector = new java.util.BitSet(" << i
                  << ");" << '\n';
      break;
    }

    if (optionals > 0) {
      std::string output_string = "private static final _Fields optionals[] = {";
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        if ((*m_iter)->get_req() == t_field::T_OPTIONAL) {
          output_string = output_string + "_Fields." + constant_name((*m_iter)->get_name()) + ",";
        }
      }
      indent(out) << output_string.substr(0, output_string.length() - 1) << "};" << '\n';
    }
  }

  generate_java_meta_data_map(out, tstruct);

  bool all_optional_members = true;

  // Default constructor
  indent(out) << "public " << make_valid_java_identifier(tstruct->get_name()) << "() {" << '\n';
  indent_up();
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type((*m_iter)->get_type());
    if ((*m_iter)->get_value() != nullptr) {
      print_const_value(out, "this." + (*m_iter)->get_name(), t, (*m_iter)->get_value(), true,
                        true);
    }
    if ((*m_iter)->get_req() != t_field::T_OPTIONAL) {
      all_optional_members = false;
    }
  }
  indent_down();
  indent(out) << "}" << '\n' << '\n';

  if (!members.empty() && !all_optional_members) {
    // Full constructor for all fields
    indent(out) << "public " << make_valid_java_identifier(tstruct->get_name()) << "(" << '\n';
    indent_up();
    bool first = true;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_OPTIONAL) {
        if (!first) {
          out << "," << '\n';
        }
        first = false;
        indent(out) << type_name((*m_iter)->get_type()) << " " << make_valid_java_identifier((*m_iter)->get_name());
      }
    }
    out << ")" << '\n';
    indent_down();
    indent(out) << "{" << '\n';
    indent_up();
    indent(out) << "this();" << '\n';
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_OPTIONAL) {
        t_type* type = get_true_type((*m_iter)->get_type());
        if (type->is_binary()) {
          if (unsafe_binaries_) {
            indent(out) << "this." << make_valid_java_identifier((*m_iter)->get_name())
                        << " = " << make_valid_java_identifier((*m_iter)->get_name()) << ";"
                        << '\n';
          } else {
            indent(out) << "this." << make_valid_java_identifier((*m_iter)->get_name())
                        << " = org.apache.thrift.TBaseHelper.copyBinary("
                        << make_valid_java_identifier((*m_iter)->get_name())
                        << ");" << '\n';
          }
        } else {
          indent(out) << "this." << make_valid_java_identifier((*m_iter)->get_name()) << " = "
                      << make_valid_java_identifier((*m_iter)->get_name()) << ";"
                      << '\n';
        }
        generate_isset_set(out, (*m_iter), "");
      }
    }

    indent_down();
    indent(out) << "}" << '\n' << '\n';
  }

  // copy constructor
  indent(out) << "/**" << '\n';
  indent(out) << " * Performs a deep copy on <i>other</i>." << '\n';
  indent(out) << " */" << '\n';
  indent(out) << "public " << make_valid_java_identifier(tstruct->get_name())
              << "(" << make_valid_java_identifier(tstruct->get_name()) << " other) {"
              << '\n';
  indent_up();

  switch (needs_isset(tstruct)) {
  case ISSET_NONE:
    break;
  case ISSET_PRIMITIVE:
    indent(out) << "__isset_bitfield = other.__isset_bitfield;" << '\n';
    break;
  case ISSET_BITSET:
    indent(out) << "__isset_bit_vector.clear();" << '\n';
    indent(out) << "__isset_bit_vector.or(other.__isset_bit_vector);" << '\n';
    break;
  }

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = (*m_iter);
    std::string field_name = field->get_name();
    t_type* type = field->get_type()->get_true_type();
    bool can_be_null = type_can_be_null(type);

    if (can_be_null) {
      indent(out) << "if (other." << generate_isset_check(field) << ") {" << '\n';
      indent_up();
    }

    if (type->is_container()) {
      generate_deep_copy_container(out, "other", field_name, "__this__" + field_name, type);
      indent(out) << "this." << make_valid_java_identifier(field_name) << " = __this__" << field_name << ";" << '\n';
    } else {
      indent(out) << "this." << make_valid_java_identifier(field_name) << " = ";
      generate_deep_copy_non_container(out, "other." + make_valid_java_identifier(field_name), field_name, type);
      out << ";" << '\n';
    }

    if (can_be_null) {
      indent_down();
      indent(out) << "}" << '\n';
    }
  }

  indent_down();
  indent(out) << "}" << '\n' << '\n';

  // clone method, so that you can deep copy an object when you don't know its class.
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public " << make_valid_java_identifier(tstruct->get_name()) << " deepCopy() {" << '\n';
  indent(out) << "  return new " << make_valid_java_identifier(tstruct->get_name()) << "(this);" << '\n';
  indent(out) << "}" << '\n' << '\n';

  generate_java_struct_clear(out, tstruct);

  generate_java_bean_boilerplate(out, tstruct);
  generate_generic_field_getters_setters(out, tstruct);
  generate_generic_isset_method(out, tstruct);

  generate_java_struct_equality(out, tstruct);
  generate_java_struct_compare_to(out, tstruct);
  generate_java_struct_field_by_id(out, tstruct);

  generate_java_struct_reader(out, tstruct);
  if (is_result) {
    generate_java_struct_result_writer(out, tstruct);
  } else {
    generate_java_struct_writer(out, tstruct);
  }
  generate_java_struct_tostring(out, tstruct);
  generate_java_validator(out, tstruct);

  generate_java_struct_write_object(out, tstruct);
  generate_java_struct_read_object(out, tstruct);

  generate_java_struct_standard_scheme(out, tstruct, is_result);
  generate_java_struct_tuple_scheme(out, tstruct);
  generate_java_scheme_lookup(out);

  scope_down(out);
  out << '\n';
}

/**
 * generates parcelable interface implementation
 */
void t_java_generator::generate_java_struct_parcelable(ostream& out, t_struct* tstruct) {
  string tname = tstruct->get_name();

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  out << indent() << java_override_annotation() << '\n'
      << indent() << "public void writeToParcel(android.os.Parcel out, int flags) {" << '\n';
  indent_up();
  string bitsetPrimitiveType = "";
  switch (needs_isset(tstruct, &bitsetPrimitiveType)) {
  case ISSET_NONE:
    break;
  case ISSET_PRIMITIVE:
    indent(out) << "//primitive bitfield of type: " << bitsetPrimitiveType << '\n';
    if (bitsetPrimitiveType == "byte") {
      indent(out) << "out.writeByte(__isset_bitfield);" << '\n';
    } else if (bitsetPrimitiveType == "short") {
      indent(out) << "out.writeInt(new Short(__isset_bitfield).intValue());" << '\n';
    } else if (bitsetPrimitiveType == "int") {
      indent(out) << "out.writeInt(__isset_bitfield);" << '\n';
    } else if (bitsetPrimitiveType == "long") {
      indent(out) << "out.writeLong(__isset_bitfield);" << '\n';
    }
    out << '\n';
    break;
  case ISSET_BITSET:
    indent(out) << "//BitSet" << '\n';
    indent(out) << "out.writeSerializable(__isset_bit_vector);" << '\n';
    out << '\n';
    break;
  }
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type((*m_iter)->get_type());
    string name = (*m_iter)->get_name();

    if (t->is_struct()) {
      indent(out) << "out.writeParcelable(" << name << ", flags);" << '\n';
    } else if (type_name(t) == "float") {
      indent(out) << "out.writeFloat(" << name << ");" << '\n';
    } else if (t->is_enum()) {
      indent(out) << "out.writeInt(" << name << " != null ? " << name << ".getValue() : -1);"
                  << '\n';
    } else if (t->is_list()) {
      if (((t_list*)t)->get_elem_type()->get_true_type()->is_struct()) {
        indent(out) << "out.writeTypedList(" << name << ");" << '\n';
      } else {
        indent(out) << "out.writeList(" << name << ");" << '\n';
      }
    } else if (t->is_map()) {
      indent(out) << "out.writeMap(" << name << ");" << '\n';
    } else if (t->is_base_type()) {
      if (t->is_binary()) {
        indent(out) << "out.writeInt(" << name << "!=null ? 1 : 0);" << '\n';
        indent(out) << "if(" << name << " != null) { " << '\n';
        indent_up();
        indent(out) << "out.writeByteArray(" << name << ".array(), " << name << ".position() + "
                    << name << ".arrayOffset(), " << name << ".limit() - " << name
                    << ".position() );" << '\n';
        scope_down(out);
      } else {
        switch (((t_base_type*)t)->get_base()) {
        case t_base_type::TYPE_I16:
          indent(out) << "out.writeInt(new Short(" << name << ").intValue());" << '\n';
          break;
        case t_base_type::TYPE_UUID:
          indent(out) << "out.writeUuid(" << name << ");" << '\n';
          break;
        case t_base_type::TYPE_I32:
          indent(out) << "out.writeInt(" << name << ");" << '\n';
          break;
        case t_base_type::TYPE_I64:
          indent(out) << "out.writeLong(" << name << ");" << '\n';
          break;
        case t_base_type::TYPE_BOOL:
          indent(out) << "out.writeInt(" << name << " ? 1 : 0);" << '\n';
          break;
        case t_base_type::TYPE_I8:
          indent(out) << "out.writeByte(" << name << ");" << '\n';
          break;
        case t_base_type::TYPE_DOUBLE:
          indent(out) << "out.writeDouble(" << name << ");" << '\n';
          break;
        case t_base_type::TYPE_STRING:
          indent(out) << "out.writeString(" << name << ");" << '\n';
          break;
        case t_base_type::TYPE_VOID:
          break;
        default:
          throw "compiler error: unhandled type";
        }
      }
    }
  }
  scope_down(out);
  out << '\n';

  out << indent() << java_override_annotation() << '\n'
      << indent() << "public int describeContents() {" << '\n';
  indent_up();
  out << indent() << "return 0;" << '\n';
  scope_down(out);
  out << '\n';

  indent(out) << "public " << tname << "(android.os.Parcel in) {" << '\n';
  indent_up();
  // read in the required bitfield
  switch (needs_isset(tstruct, &bitsetPrimitiveType)) {
  case ISSET_NONE:
    break;
  case ISSET_PRIMITIVE:
    indent(out) << "//primitive bitfield of type: " << bitsetPrimitiveType << '\n';
    if (bitsetPrimitiveType == "byte") {
      indent(out) << "__isset_bitfield = in.readByte();" << '\n';
    } else if (bitsetPrimitiveType == "short") {
      indent(out) << "__isset_bitfield = (short) in.readInt();" << '\n';
    } else if (bitsetPrimitiveType == "int") {
      indent(out) << "__isset_bitfield = in.readInt();" << '\n';
    } else if (bitsetPrimitiveType == "long") {
      indent(out) << "__isset_bitfield = in.readLong();" << '\n';
    }
    out << '\n';
    break;
  case ISSET_BITSET:
    indent(out) << "//BitSet" << '\n';
    indent(out) << "__isset_bit_vector = (java.util.BitSet) in.readSerializable();" << '\n';
    out << '\n';
    break;
  }
  // read all the fields
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type((*m_iter)->get_type());
    string name = (*m_iter)->get_name();
    string prefix = "this." + name;

    if (t->is_struct()) {
      indent(out) << prefix << "= in.readParcelable(" << tname << ".class.getClassLoader());"
                  << '\n';
    } else if (t->is_enum()) {
      indent(out) << prefix << " = " << type_name(t) << ".findByValue(in.readInt());" << '\n';
    } else if (t->is_list()) {
      t_list* list = (t_list*)t;
      indent(out) << prefix << " = new " << type_name(t, false, true) << "();" << '\n';
      if (list->get_elem_type()->get_true_type()->is_struct()) {
        indent(out) << "in.readTypedList(" << prefix << ", " << type_name(list->get_elem_type())
                    << ".CREATOR);" << '\n';
      } else {
        indent(out) << "in.readList(" << prefix << ", " << tname << ".class.getClassLoader());"
                    << '\n';
      }
    } else if (t->is_map()) {
      indent(out) << prefix << " = new " << type_name(t, false, true) << "();" << '\n';
      indent(out) << " in.readMap(" << prefix << ", " << tname << ".class.getClassLoader());"
                  << '\n';
    } else if (type_name(t) == "float") {
      indent(out) << prefix << " = in.readFloat();" << '\n';
    } else if (t->is_base_type()) {
      t_base_type* bt = (t_base_type*)t;
      if (bt->is_binary()) {
        indent(out) << "if(in.readInt()==1) {" << '\n';
        indent_up();
        indent(out) << prefix << " = java.nio.ByteBuffer.wrap(in.createByteArray());" << '\n';
        scope_down(out);
      } else {
        switch (bt->get_base()) {
        case t_base_type::TYPE_I8:
          indent(out) << prefix << " = in.readByte();" << '\n';
          break;
        case t_base_type::TYPE_I16:
          indent(out) << prefix << " = (short) in.readInt();" << '\n';
          break;
        case t_base_type::TYPE_I32:
          indent(out) << prefix << " = in.readInt();" << '\n';
          break;
        case t_base_type::TYPE_I64:
          indent(out) << prefix << " = in.readLong();" << '\n';
          break;
        case t_base_type::TYPE_UUID:
          indent(out) << prefix << " = in.readUuid();" << '\n';
          break;
        case t_base_type::TYPE_BOOL:
          indent(out) << prefix << " = (in.readInt()==1);" << '\n';
          break;
        case t_base_type::TYPE_DOUBLE:
          indent(out) << prefix << " = in.readDouble();" << '\n';
          break;
        case t_base_type::TYPE_STRING:
          indent(out) << prefix << "= in.readString();" << '\n';
          break;
        case t_base_type::TYPE_VOID:
          break;
        default:
          throw "compiler error: unhandled type";
        }
      }
    }
  }

  scope_down(out);
  out << '\n';

  indent(out) << "public static final android.os.Parcelable.Creator<" << tname
              << "> CREATOR = new android.os.Parcelable.Creator<" << tname << ">() {" << '\n';
  indent_up();

  indent(out) << java_override_annotation() << '\n'
              << indent() << "public " << tname << "[] newArray(int size) {" << '\n';
  indent_up();
  indent(out) << "return new " << tname << "[size];" << '\n';
  scope_down(out);
  out << '\n';

  indent(out) << java_override_annotation() << '\n'
              << indent() << "public " << tname << " createFromParcel(android.os.Parcel in) {"
              << '\n';
  indent_up();
  indent(out) << "return new " << tname << "(in);" << '\n';
  scope_down(out);

  indent_down();
  indent(out) << "};" << '\n';
  out << '\n';
}

/**
 * Generates equals methods and a hashCode method for a structure.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_equality(ostream& out, t_struct* tstruct) {
  out << indent() << java_override_annotation() << '\n'
      << indent() << "public boolean equals(java.lang.Object that) {" << '\n';
  indent_up();
  out << indent() << "if (that instanceof " << make_valid_java_identifier(tstruct->get_name()) << ")" << '\n'
      << indent() << "  return this.equals((" << make_valid_java_identifier(tstruct->get_name()) << ")that);" << '\n'
      << indent() << "return false;" << '\n';
  scope_down(out);
  out << '\n';

  out << indent() << "public boolean equals(" << make_valid_java_identifier(tstruct->get_name()) << " that) {" << '\n';
  indent_up();
  out << indent() << "if (that == null)" << '\n'
      << indent() << "  return false;" << '\n'
      << indent() << "if (this == that)" << '\n'
      << indent() << "  return true;" << '\n';

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    out << '\n';

    t_type* t = get_true_type((*m_iter)->get_type());
    // Most existing Thrift code does not use isset or optional/required,
    // so we treat "default" fields as required.
    bool is_optional = (*m_iter)->get_req() == t_field::T_OPTIONAL;
    bool can_be_null = type_can_be_null(t);
    string name = (*m_iter)->get_name();

    string this_present = "true";
    string that_present = "true";
    string unequal;

    if (is_optional || can_be_null) {
      this_present += " && this." + generate_isset_check(*m_iter);
      that_present += " && that." + generate_isset_check(*m_iter);
    }

    out << indent() << "boolean this_present_" << name << " = " << this_present << ";" << '\n'
        << indent() << "boolean that_present_" << name << " = " << that_present << ";" << '\n'
        << indent() << "if ("
        << "this_present_" << name << " || that_present_" << name << ") {" << '\n';
    indent_up();
    out << indent() << "if (!("
        << "this_present_" << name << " && that_present_" << name << "))" << '\n'
        << indent() << "  return false;" << '\n';

    if (t->is_binary()) {
      unequal = "!this." + make_valid_java_identifier(name) + ".equals(that." + make_valid_java_identifier(name) + ")";
    } else if (can_be_null) {
      unequal = "!this." + make_valid_java_identifier(name) + ".equals(that." + make_valid_java_identifier(name) + ")";
    } else {
      unequal = "this." + make_valid_java_identifier(name) + " != that." + make_valid_java_identifier(name);
    }

    out << indent() << "if (" << unequal << ")" << '\n' << indent() << "  return false;" << '\n';

    scope_down(out);
  }
  out << '\n';
  indent(out) << "return true;" << '\n';
  scope_down(out);
  out << '\n';

  const int MUL = 8191; // HashCode multiplier
  const int B_YES = 131071;
  const int B_NO = 524287;
  out << indent() << java_override_annotation() << '\n'
      << indent() << "public int hashCode() {" << '\n';
  indent_up();
  indent(out) << "int hashCode = 1;" << '\n';

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    out << '\n';

    t_type* t = get_true_type((*m_iter)->get_type());
    bool is_optional = (*m_iter)->get_req() == t_field::T_OPTIONAL;
    bool can_be_null = type_can_be_null(t);
    string name = make_valid_java_identifier((*m_iter)->get_name());

    if (is_optional || can_be_null) {
      indent(out) << "hashCode = hashCode * " << MUL << " + ((" << generate_isset_check(*m_iter)
                  << ") ? " << B_YES << " : " << B_NO << ");" << '\n';
    }

    if (is_optional || can_be_null) {
      indent(out) << "if (" + generate_isset_check(*m_iter) + ")" << '\n';
      indent_up();
    }

    if (t->is_enum()) {
      indent(out) << "hashCode = hashCode * " << MUL << " + " << name << ".getValue();" << '\n';
    } else if (t->is_base_type()) {
      switch (((t_base_type*)t)->get_base()) {
      case t_base_type::TYPE_STRING:
      case t_base_type::TYPE_UUID:
        indent(out) << "hashCode = hashCode * " << MUL << " + " << name << ".hashCode();" << '\n';
        break;
      case t_base_type::TYPE_BOOL:
        indent(out) << "hashCode = hashCode * " << MUL << " + ((" << name << ") ? " << B_YES
                    << " : " << B_NO << ");" << '\n';
        break;
      case t_base_type::TYPE_I8:
        indent(out) << "hashCode = hashCode * " << MUL << " + (int) (" << name << ");" << '\n';
        break;
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
        indent(out) << "hashCode = hashCode * " << MUL << " + " << name << ";" << '\n';
        break;
      case t_base_type::TYPE_I64:
      case t_base_type::TYPE_DOUBLE:
        indent(out) << "hashCode = hashCode * " << MUL
                    << " + org.apache.thrift.TBaseHelper.hashCode(" << name << ");" << '\n';
        break;
      case t_base_type::TYPE_VOID:
        throw std::logic_error("compiler error: a struct field cannot be void");
      default:
        throw std::logic_error("compiler error: the following base type has no hashcode generator: "
                               + t_base_type::t_base_name(((t_base_type*)t)->get_base()));
      }
    } else {
      indent(out) << "hashCode = hashCode * " << MUL << " + " << name << ".hashCode();" << '\n';
    }

    if (is_optional || can_be_null) {
      indent_down();
    }
  }

  out << '\n';
  indent(out) << "return hashCode;" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

void t_java_generator::generate_java_struct_compare_to(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public int compareTo(" << type_name(tstruct) << " other) {" << '\n';
  indent_up();

  indent(out) << "if (!getClass().equals(other.getClass())) {" << '\n';
  indent(out) << "  return getClass().getName().compareTo(other.getClass().getName());" << '\n';
  indent(out) << "}" << '\n';
  out << '\n';

  indent(out) << "int lastComparison = 0;" << '\n';
  out << '\n';

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_field* field = *m_iter;
    indent(out) << "lastComparison = java.lang.Boolean.compare(" << generate_isset_check(field)
                << ", other." << generate_isset_check(field) << ");" << '\n';
    indent(out) << "if (lastComparison != 0) {" << '\n';
    indent(out) << "  return lastComparison;" << '\n';
    indent(out) << "}" << '\n';

    indent(out) << "if (" << generate_isset_check(field) << ") {" << '\n';
    indent(out) << "  lastComparison = org.apache.thrift.TBaseHelper.compareTo(this."
                << make_valid_java_identifier(field->get_name())
                << ", other." << make_valid_java_identifier(field->get_name()) << ");" << '\n';
    indent(out) << "  if (lastComparison != 0) {" << '\n';
    indent(out) << "    return lastComparison;" << '\n';
    indent(out) << "  }" << '\n';
    indent(out) << "}" << '\n';
  }

  indent(out) << "return 0;" << '\n';

  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

/**
 * Generates a function to read all the fields of the struct.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_reader(ostream& out, t_struct* /*tstruct*/) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void read(org.apache.thrift.protocol.TProtocol iprot) throws "
                 "org.apache.thrift.TException {"
              << '\n';
  indent_up();
  indent(out) << "scheme(iprot).read(iprot, this);" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

// generates java method to perform various checks
// (e.g. check that all required fields are set)
void t_java_generator::generate_java_validator(ostream& out, t_struct* tstruct) {
  indent(out) << "public void validate() throws org.apache.thrift.TException {" << '\n';
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out << indent() << "// check for required fields" << '\n';
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      if (bean_style_) {
        out << indent() << "if (!" << generate_isset_check(*f_iter) << ") {" << '\n'
            << indent()
            << "  throw new org.apache.thrift.protocol.TProtocolException(\"Required field '"
            << (*f_iter)->get_name() << "' is unset! Struct:\" + toString());" << '\n'
            << indent() << "}" << '\n'
            << '\n';
      } else {
        if (type_can_be_null((*f_iter)->get_type())) {
          indent(out) << "if (" << (*f_iter)->get_name() << " == null) {" << '\n';
          indent(out)
              << "  throw new org.apache.thrift.protocol.TProtocolException(\"Required field '"
              << (*f_iter)->get_name() << "' was not present! Struct: \" + toString());" << '\n';
          indent(out) << "}" << '\n';
        } else {
          indent(out) << "// alas, we cannot check '" << (*f_iter)->get_name()
                      << "' because it's a primitive and you chose the non-beans generator."
                      << '\n';
        }
      }
    }
  }

  out << indent() << "// check for sub-struct validity" << '\n';
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_type* type = get_true_type((*f_iter)->get_type());
    if (type->is_struct() && !((t_struct*)type)->is_union()) {
      out << indent() << "if (" << make_valid_java_identifier((*f_iter)->get_name()) << " != null) {" << '\n';
      out << indent() << "  " << make_valid_java_identifier((*f_iter)->get_name()) << ".validate();" << '\n';
      out << indent() << "}" << '\n';
    }
  }

  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

/**
 * Generates a function to write all the fields of the struct
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_writer(ostream& out, t_struct* /*tstruct*/) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void write(org.apache.thrift.protocol.TProtocol oprot) throws "
                 "org.apache.thrift.TException {"
              << '\n';
  indent_up();
  indent(out) << "scheme(oprot).write(oprot, this);" << '\n';

  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

/**
 * Generates a function to write all the fields of the struct,
 * which is a function result. These fields are only written
 * if they are set in the Isset array, and only one of them
 * can be set at a time.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_result_writer(ostream& out, t_struct* tstruct) {
  (void)tstruct;
  indent(out) << "public void write(org.apache.thrift.protocol.TProtocol oprot) throws "
                 "org.apache.thrift.TException {"
              << '\n';
  indent_up();
  indent(out) << "scheme(oprot).write(oprot, this);" << '\n';

  indent_down();
  indent(out) << "  }" << '\n' << '\n';
}

void t_java_generator::generate_java_struct_field_by_id(ostream& out, t_struct* tstruct) {
  (void)tstruct;
  indent(out) << java_nullable_annotation() << '\n';
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public _Fields fieldForId(int fieldId) {" << '\n';
  indent(out) << "  return _Fields.findByThriftId(fieldId);" << '\n';
  indent(out) << "}" << '\n' << '\n';
}

void t_java_generator::generate_reflection_getters(ostringstream& out,
                                                   t_type* type,
                                                   string field_name,
                                                   string cap_name) {
  indent(out) << "case " << constant_name(field_name) << ":" << '\n';
  indent_up();
  indent(out) << "return " << (type->is_bool() ? "is" : "get") << cap_name << "();" << '\n' << '\n';
  indent_down();
}

void t_java_generator::generate_reflection_setters(ostringstream& out,
                                                   t_type* type,
                                                   string field_name,
                                                   string cap_name) {
  const bool is_binary = type->is_binary();
  indent(out) << "case " << constant_name(field_name) << ":" << '\n';
  indent_up();
  indent(out) << "if (value == null) {" << '\n';
  indent(out) << "  unset" << get_cap_name(field_name) << "();" << '\n';
  indent(out) << "} else {" << '\n';
  if (is_binary) {
    indent_up();
    indent(out) << "if (value instanceof byte[]) {" << '\n';
    indent(out) << "  set" << cap_name << "((byte[])value);" << '\n';
    indent(out) << "} else {" << '\n';
  }
  indent(out) << "  set" << cap_name << "((" << type_name(type, true, false) << ")value);" << '\n';
  if (is_binary) {
    indent(out) << "}" << '\n';
    indent_down();
  }
  indent(out) << "}" << '\n';
  indent(out) << "break;" << '\n' << '\n';

  indent_down();
}

void t_java_generator::generate_generic_field_getters_setters(std::ostream& out,
                                                              t_struct* tstruct) {
  std::ostringstream getter_stream;
  std::ostringstream setter_stream;

  // build up the bodies of both the getter and setter at once
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_field* field = *f_iter;
    t_type* type = get_true_type(field->get_type());
    std::string field_name = field->get_name();
    std::string cap_name = get_cap_name(field_name);

    indent_up();
    generate_reflection_setters(setter_stream, type, field_name, cap_name);
    generate_reflection_getters(getter_stream, type, field_name, cap_name);
    indent_down();
  }

  // create the setter

  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void setFieldValue(_Fields field, " << java_nullable_annotation()
              << " java.lang.Object value) {" << '\n';
  indent(out) << "  switch (field) {" << '\n';
  out << setter_stream.str();
  indent(out) << "  }" << '\n';
  indent(out) << "}" << '\n' << '\n';

  // create the getter
  indent(out) << java_nullable_annotation() << '\n';
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public java.lang.Object getFieldValue(_Fields field) {" << '\n';
  indent_up();
  indent(out) << "switch (field) {" << '\n';
  out << getter_stream.str();
  indent(out) << "}" << '\n';
  indent(out) << "throw new java.lang.IllegalStateException();" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

// Creates a generic isSet method that takes the field number as argument
void t_java_generator::generate_generic_isset_method(std::ostream& out, t_struct* tstruct) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // create the isSet method
  indent(out) << "/** Returns true if field corresponding to fieldID is set (has been assigned a "
                 "value) and false otherwise */"
              << '\n';
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public boolean isSet(_Fields field) {" << '\n';
  indent_up();
  indent(out) << "if (field == null) {" << '\n';
  indent(out) << "  throw new java.lang.IllegalArgumentException();" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "switch (field) {" << '\n';

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_field* field = *f_iter;
    indent(out) << "case " << constant_name(field->get_name()) << ":" << '\n';
    indent_up();
    indent(out) << "return " << generate_isset_check(field) << ";" << '\n';
    indent_down();
  }

  indent(out) << "}" << '\n';
  indent(out) << "throw new java.lang.IllegalStateException();" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

/**
 * Generates a set of Java Bean boilerplate functions (setters, getters, etc.)
 * for the given struct.
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_bean_boilerplate(ostream& out, t_struct* tstruct) {
  isset_type issetType = needs_isset(tstruct);
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_field* field = *f_iter;
    t_type* type = get_true_type(field->get_type());
    std::string field_name = field->get_name();
    std::string cap_name = get_cap_name(field_name);
    bool optional = use_option_type_ && field->get_req() == t_field::T_OPTIONAL;
    bool is_deprecated = this->is_deprecated(field->annotations_);

    if (type->is_container()) {
      // Method to return the size of the collection
      if (optional) {
        if (is_deprecated) {
          indent(out) << "@Deprecated" << '\n';
        }

        if (use_jdk8_option_type_) {
          indent(out) << "public " << jdk_option_class << "<Integer> get" << cap_name;
        } else {
          indent(out) << "public " << thrift_option_class << "<Integer> get" << cap_name;
        }

        out << get_cap_name("size() {") << '\n';

        indent_up();
        indent(out) << "if (this." << field_name << " == null) {" << '\n';
        indent_up();

        if (use_jdk8_option_type_) {
          indent(out) << "return " << jdk_option_class << ".empty();" << '\n';
        } else {
          indent(out) << "return " << thrift_option_class << ".none();" << '\n';
        }

        indent_down();
        indent(out) << "} else {" << '\n';
        indent_up();

        if (use_jdk8_option_type_) {
          indent(out) << "return " << jdk_option_class << ".of(this.";
        } else {
          indent(out) << "return " << thrift_option_class << ".some(this.";
        }
        out << field_name << ".size());" << '\n';

        indent_down();
        indent(out) << "}" << '\n';
        indent_down();
        indent(out) << "}" << '\n' << '\n';
      } else {
        if (is_deprecated) {
          indent(out) << "@Deprecated" << '\n';
        }
        indent(out) << "public int get" << cap_name;
        out << get_cap_name("size() {") << '\n';

        indent_up();
        indent(out) << "return (this." << field_name << " == null) ? 0 : "
                    << "this." << field_name << ".size();" << '\n';
        indent_down();
        indent(out) << "}" << '\n' << '\n';
      }
    }

    if (type->is_set() || type->is_list()) {
      t_type* element_type;
      if (type->is_set()) {
        element_type = ((t_set*)type)->get_elem_type();
      } else {
        element_type = ((t_list*)type)->get_elem_type();
      }

      // Iterator getter for sets and lists
      if (optional) {
        if (is_deprecated) {
          indent(out) << "@Deprecated" << '\n';
        }

        if (use_jdk8_option_type_) {
          indent(out) << "public " << jdk_option_class << "<";
        } else {
          indent(out) << "public " << thrift_option_class << "<";
        }
        out << "java.util.Iterator<" << type_name(element_type, true, false) << ">> get"
            << cap_name;

        out << get_cap_name("iterator() {") << '\n';

        indent_up();
        indent(out) << "if (this." << field_name << " == null) {" << '\n';
        indent_up();

        if (use_jdk8_option_type_) {
          indent(out) << "return " << jdk_option_class << ".empty();" << '\n';
        } else {
          indent(out) << "return " << thrift_option_class << ".none();" << '\n';
        }

        indent_down();
        indent(out) << "} else {" << '\n';
        indent_up();

        if (use_jdk8_option_type_) {
          indent(out) << "return " << jdk_option_class << ".of(this.";
        } else {
          indent(out) << "return " << thrift_option_class << ".some(this.";
        }
        out << field_name << ".iterator());" << '\n';

        indent_down();
        indent(out) << "}" << '\n';
        indent_down();
        indent(out) << "}" << '\n' << '\n';
      } else {
        if (is_deprecated) {
          indent(out) << "@Deprecated" << '\n';
        }
        indent(out) << java_nullable_annotation() << '\n';
        indent(out) << "public java.util.Iterator<" << type_name(element_type, true, false)
                    << "> get" << cap_name;
        out << get_cap_name("iterator() {") << '\n';

        indent_up();
        indent(out) << "return (this." << field_name << " == null) ? null : "
                    << "this." << field_name << ".iterator();" << '\n';
        indent_down();
        indent(out) << "}" << '\n' << '\n';
      }

      // Add to set or list, create if the set/list is null
      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public void add" << get_cap_name("to");
      out << cap_name << "(" << type_name(element_type) << " elem) {" << '\n';

      indent_up();
      indent(out) << "if (this." << field_name << " == null) {" << '\n';
      indent_up();
      indent(out) << "this." << field_name;
      if (is_enum_set(type)) {
        out << " = " << type_name(type, false, true, true) << ".noneOf("
            << inner_enum_type_name(type) << ");" << '\n';
      } else {
        out << " = new " << type_name(type, false, true) << "();" << '\n';
      }
      indent_down();
      indent(out) << "}" << '\n';
      indent(out) << "this." << field_name << ".add(elem);" << '\n';
      indent_down();
      indent(out) << "}" << '\n' << '\n';
    } else if (type->is_map()) {
      // Put to map
      t_type* key_type = ((t_map*)type)->get_key_type();
      t_type* val_type = ((t_map*)type)->get_val_type();

      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public void put" << get_cap_name("to");
      out << cap_name << "(" << type_name(key_type) << " key, " << type_name(val_type) << " val) {"
          << '\n';

      indent_up();
      indent(out) << "if (this." << field_name << " == null) {" << '\n';
      indent_up();
      std::string constructor_args;
      if (is_enum_map(type)) {
        constructor_args = inner_enum_type_name(type);
      }
      indent(out) << "this." << field_name << " = new " << type_name(type, false, true) << "("
                  << constructor_args << ");" << '\n';
      indent_down();
      indent(out) << "}" << '\n';
      indent(out) << "this." << field_name << ".put(key, val);" << '\n';
      indent_down();
      indent(out) << "}" << '\n' << '\n';
    }

    // Simple getter
    generate_java_doc(out, field);
    if (type->is_binary()) {
      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public byte[] get" << cap_name << "() {" << '\n';
      indent(out) << "  set" << cap_name << "(org.apache.thrift.TBaseHelper.rightSize("
                  << field_name << "));" << '\n';
      indent(out) << "  return " << field_name << " == null ? null : " << field_name << ".array();"
                  << '\n';
      indent(out) << "}" << '\n' << '\n';

      indent(out) << "public java.nio.ByteBuffer buffer" << get_cap_name("for") << cap_name
                  << "() {" << '\n';
      if (unsafe_binaries_) {
        indent(out) << "  return " << field_name << ";" << '\n';
      } else {
        indent(out) << "  return org.apache.thrift.TBaseHelper.copyBinary(" << field_name << ");"
                    << '\n';
      }
      indent(out) << "}" << '\n' << '\n';
    } else {
      if (optional) {
        if (is_deprecated) {
          indent(out) << "@Deprecated" << '\n';
        }

        if (use_jdk8_option_type_) {
          indent(out) << "public " << jdk_option_class << "<" << type_name(type, true) << ">";
        } else {
          indent(out) << "public " << thrift_option_class << "<" << type_name(type, true) << ">";
        }

        if (type->is_base_type() && ((t_base_type*)type)->get_base() == t_base_type::TYPE_BOOL) {
          out << " is";
        } else {
          out << " get";
        }
        out << cap_name << "() {" << '\n';
        indent_up();

        indent(out) << "if (this.isSet" << cap_name << "()) {" << '\n';
        indent_up();

        if (use_jdk8_option_type_) {
          indent(out) << "return " << jdk_option_class << ".of(this.";
        } else {
          indent(out) << "return " << thrift_option_class << ".some(this.";
        }
        out << field_name << ");" << '\n';

        indent_down();
        indent(out) << "} else {" << '\n';
        indent_up();

        if (use_jdk8_option_type_) {
          indent(out) << "return " << jdk_option_class << ".empty();" << '\n';
        } else {
          indent(out) << "return " << thrift_option_class << ".none();" << '\n';
        }

        indent_down();
        indent(out) << "}" << '\n';
        indent_down();
        indent(out) << "}" << '\n' << '\n';
      } else {
        if (is_deprecated) {
          indent(out) << "@Deprecated" << '\n';
        }
        if (type_can_be_null(type)) {
          indent(out) << java_nullable_annotation() << '\n';
        }
        indent(out) << "public " << type_name(type);
        if (type->is_base_type() && ((t_base_type*)type)->get_base() == t_base_type::TYPE_BOOL) {
          out << " is";
        } else {
          out << " get";
        }
        out << cap_name << "() {" << '\n';
        indent_up();
        indent(out) << "return this." << make_valid_java_identifier(field_name) << ";" << '\n';
        indent_down();
        indent(out) << "}" << '\n' << '\n';
      }
    }

    // Simple setter
    generate_java_doc(out, field);
    if (type->is_binary()) {
      if (is_deprecated) {
        indent(out) << "@Deprecated" << '\n';
      }
      indent(out) << "public ";
      if (bean_style_) {
        out << "void";
      } else {
        out << type_name(tstruct);
      }
      out << " set" << cap_name << "(byte[] " << make_valid_java_identifier(field_name) << ") {" << '\n';
      indent(out) << "  this." << make_valid_java_identifier(field_name) << " = " << make_valid_java_identifier(field_name)
                  << " == null ? (java.nio.ByteBuffer)null";

      if (unsafe_binaries_) {
        indent(out) << " : java.nio.ByteBuffer.wrap(" << make_valid_java_identifier(field_name) << ");" << '\n';
      } else {
        indent(out) << " : java.nio.ByteBuffer.wrap(" << make_valid_java_identifier(field_name) << ".clone());" << '\n';
      }

      if (!bean_style_) {
        indent(out) << "  return this;" << '\n';
      }
      indent(out) << "}" << '\n' << '\n';
    }
    if (is_deprecated) {
      indent(out) << "@Deprecated" << '\n';
    }
    indent(out) << "public ";
    if (bean_style_) {
      out << "void";
    } else {
      out << type_name(tstruct);
    }
    out << " set" << cap_name << "("
        << (type_can_be_null(type) ? (java_nullable_annotation() + " ") : "")
        << type_name(type)
        << " " << make_valid_java_identifier(field_name) << ") {" << '\n';
    indent_up();
    indent(out) << "this." << make_valid_java_identifier(field_name) << " = ";
    if (type->is_binary() && !unsafe_binaries_) {
      out << "org.apache.thrift.TBaseHelper.copyBinary(" << make_valid_java_identifier(field_name) << ")";
    } else {
      out << make_valid_java_identifier(field_name);
    }
    out << ";" << '\n';
    generate_isset_set(out, field, "");
    if (!bean_style_) {
      indent(out) << "return this;" << '\n';
    }

    indent_down();
    indent(out) << "}" << '\n' << '\n';

    // Unsetter
    if (is_deprecated) {
      indent(out) << "@Deprecated" << '\n';
    }
    indent(out) << "public void unset" << cap_name << "() {" << '\n';
    indent_up();
    if (type_can_be_null(type)) {
      indent(out) << "this." << make_valid_java_identifier(field_name) << " = null;" << '\n';
    } else if (issetType == ISSET_PRIMITIVE) {
      indent(out)
          << "__isset_bitfield = org.apache.thrift.EncodingUtils.clearBit(__isset_bitfield, "
          << isset_field_id(field) << ");" << '\n';
    } else {
      indent(out) << "__isset_bit_vector.clear(" << isset_field_id(field) << ");" << '\n';
    }
    indent_down();
    indent(out) << "}" << '\n' << '\n';

    // isSet method
    indent(out) << "/** Returns true if field " << field_name
                << " is set (has been assigned a value) and false otherwise */" << '\n';
    if (is_deprecated) {
      indent(out) << "@Deprecated" << '\n';
    }
    indent(out) << "public boolean is" << get_cap_name("set") << cap_name << "() {" << '\n';
    indent_up();
    if (type_can_be_null(type)) {
      indent(out) << "return this." << make_valid_java_identifier(field_name) << " != null;" << '\n';
    } else if (issetType == ISSET_PRIMITIVE) {
      indent(out) << "return org.apache.thrift.EncodingUtils.testBit(__isset_bitfield, "
                  << isset_field_id(field) << ");" << '\n';
    } else {
      indent(out) << "return __isset_bit_vector.get(" << isset_field_id(field) << ");" << '\n';
    }
    indent_down();
    indent(out) << "}" << '\n' << '\n';

    if (is_deprecated) {
      indent(out) << "@Deprecated" << '\n';
    }
    indent(out) << "public void set" << cap_name << get_cap_name("isSet") << "(boolean value) {"
                << '\n';
    indent_up();
    if (type_can_be_null(type)) {
      indent(out) << "if (!value) {" << '\n';
      indent(out) << "  this." << make_valid_java_identifier(field_name) << " = null;" << '\n';
      indent(out) << "}" << '\n';
    } else if (issetType == ISSET_PRIMITIVE) {
      indent(out) << "__isset_bitfield = org.apache.thrift.EncodingUtils.setBit(__isset_bitfield, "
                  << isset_field_id(field) << ", value);" << '\n';
    } else {
      indent(out) << "__isset_bit_vector.set(" << isset_field_id(field) << ", value);" << '\n';
    }
    indent_down();
    indent(out) << "}" << '\n' << '\n';
  }
}

/**
 * Generates a toString() method for the given struct
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_struct_tostring(ostream& out, t_struct* tstruct) {
  out << indent() << java_override_annotation() << '\n'
      << indent() << "public java.lang.String toString() {" << '\n';
  indent_up();

  out << indent() << "java.lang.StringBuilder sb = new java.lang.StringBuilder(\""
      << tstruct->get_name() << "(\");" << '\n';
  out << indent() << "boolean first = true;" << '\n' << '\n';

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    bool could_be_unset = (*f_iter)->get_req() == t_field::T_OPTIONAL;
    if (could_be_unset) {
      indent(out) << "if (" << generate_isset_check(*f_iter) << ") {" << '\n';
      indent_up();
    }

    t_field* field = (*f_iter);

    if (!first) {
      indent(out) << "if (!first) sb.append(\", \");" << '\n';
    }
    indent(out) << "sb.append(\"" << (*f_iter)->get_name() << ":\");" << '\n';
    bool can_be_null = type_can_be_null(field->get_type());
    if (can_be_null) {
      indent(out) << "if (this." << make_valid_java_identifier((*f_iter)->get_name()) << " == null) {" << '\n';
      indent(out) << "  sb.append(\"null\");" << '\n';
      indent(out) << "} else {" << '\n';
      indent_up();
    }

    if (get_true_type(field->get_type())->is_binary()) {
      indent(out) << "org.apache.thrift.TBaseHelper.toString(this." << make_valid_java_identifier(field->get_name()) << ", sb);"
                  << '\n';
    } else if ((field->get_type()->is_set())
               && (get_true_type(((t_set*)field->get_type())->get_elem_type())->is_binary())) {
      indent(out) << "org.apache.thrift.TBaseHelper.toString(this." << make_valid_java_identifier(field->get_name()) << ", sb);"
                  << '\n';
    } else if ((field->get_type()->is_list())
               && (get_true_type(((t_list*)field->get_type())->get_elem_type())->is_binary())) {
      indent(out) << "org.apache.thrift.TBaseHelper.toString(this." << make_valid_java_identifier(field->get_name()) << ", sb);"
                  << '\n';
    } else {
      indent(out) << "sb.append(this." << make_valid_java_identifier((*f_iter)->get_name()) << ");" << '\n';
    }

    if (can_be_null) {
      indent_down();
      indent(out) << "}" << '\n';
    }
    indent(out) << "first = false;" << '\n';

    if (could_be_unset) {
      indent_down();
      indent(out) << "}" << '\n';
    }
    first = false;
  }
  out << indent() << "sb.append(\")\");" << '\n' << indent() << "return sb.toString();" << '\n';

  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

/**
 * Generates a static map with meta data to store information such as fieldID to
 * fieldName mapping
 *
 * @param tstruct The struct definition
 */
void t_java_generator::generate_java_meta_data_map(ostream& out, t_struct* tstruct) {
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Static Map with fieldID -> org.apache.thrift.meta_data.FieldMetaData mappings
  indent(out) << "public static final java.util.Map<_Fields, "
                 "org.apache.thrift.meta_data.FieldMetaData> metaDataMap;"
              << '\n';
  indent(out) << "static {" << '\n';
  indent_up();

  indent(out)
      << "java.util.Map<_Fields, org.apache.thrift.meta_data.FieldMetaData> tmpMap = new "
         "java.util.EnumMap<_Fields, org.apache.thrift.meta_data.FieldMetaData>(_Fields.class);"
      << '\n';

  // Populate map
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_field* field = *f_iter;
    std::string field_name = field->get_name();
    indent(out) << "tmpMap.put(_Fields." << constant_name(field_name)
                << ", new org.apache.thrift.meta_data.FieldMetaData(\"" << field_name << "\", ";

    // Set field requirement type (required, optional, etc.)
    if (field->get_req() == t_field::T_REQUIRED) {
      out << "org.apache.thrift.TFieldRequirementType.REQUIRED, ";
    } else if (field->get_req() == t_field::T_OPTIONAL) {
      out << "org.apache.thrift.TFieldRequirementType.OPTIONAL, ";
    } else {
      out << "org.apache.thrift.TFieldRequirementType.DEFAULT, ";
    }

    // Create value meta data
    generate_field_value_meta_data(out, field->get_type());

    // Include the annotation into metadata when asked
    if (annotations_as_metadata_) {
      generate_metadata_for_field_annotations(out, field);
    }
    out << "));" << '\n';
  }

  indent(out) << "metaDataMap = java.util.Collections.unmodifiableMap(tmpMap);" << '\n';

  indent(out) << "org.apache.thrift.meta_data.FieldMetaData.addStructMetaDataMap("
              << type_name(tstruct) << ".class, metaDataMap);" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';
}

/**
 * Returns a string with the java representation of the given thrift type
 * (e.g. for the type struct it returns "org.apache.thrift.protocol.TType.STRUCT")
 */
std::string t_java_generator::get_java_type_string(t_type* type) {
  if (type->is_list()) {
    return "org.apache.thrift.protocol.TType.LIST";
  } else if (type->is_map()) {
    return "org.apache.thrift.protocol.TType.MAP";
  } else if (type->is_set()) {
    return "org.apache.thrift.protocol.TType.SET";
  } else if (type->is_struct() || type->is_xception()) {
    return "org.apache.thrift.protocol.TType.STRUCT";
  } else if (type->is_enum()) {
    return "org.apache.thrift.protocol.TType.ENUM";
  } else if (type->is_typedef()) {
    return get_java_type_string(((t_typedef*)type)->get_type());
  } else if (type->is_base_type()) {
    switch (((t_base_type*)type)->get_base()) {
    case t_base_type::TYPE_VOID:
      return "org.apache.thrift.protocol.TType.VOID";
      break;
    case t_base_type::TYPE_STRING:
      return "org.apache.thrift.protocol.TType.STRING";
      break;
    case t_base_type::TYPE_UUID:
      return "org.apache.thrift.protocol.TType.UUID";
      break;
    case t_base_type::TYPE_BOOL:
      return "org.apache.thrift.protocol.TType.BOOL";
      break;
    case t_base_type::TYPE_I8:
      return "org.apache.thrift.protocol.TType.BYTE";
      break;
    case t_base_type::TYPE_I16:
      return "org.apache.thrift.protocol.TType.I16";
      break;
    case t_base_type::TYPE_I32:
      return "org.apache.thrift.protocol.TType.I32";
      break;
    case t_base_type::TYPE_I64:
      return "org.apache.thrift.protocol.TType.I64";
      break;
    case t_base_type::TYPE_DOUBLE:
      return "org.apache.thrift.protocol.TType.DOUBLE";
      break;
    default:
      throw std::runtime_error("Unknown thrift type \"" + type->get_name()
                               + "\" passed to t_java_generator::get_java_type_string!");
      return "Unknown thrift type \"" + type->get_name()
             + "\" passed to t_java_generator::get_java_type_string!";
      break; // This should never happen!
    }
  } else {
    throw std::runtime_error("Unknown thrift type \"" + type->get_name()
                             + "\" passed to t_java_generator::get_java_type_string!");
    // This should never happen!
  }
}

void t_java_generator::generate_metadata_for_field_annotations(std::ostream& out, t_field* field) {
  if (field->annotations_.size() == 0) {
    return;
  }
  out << ", " << '\n';
  indent_up();
  indent_up();
  indent(out) << "java.util.stream.Stream.<java.util.Map.Entry<java.lang.String, "
                 "java.lang.String>>builder()"
              << '\n';

  indent_up();
  indent_up();
  for (auto& annotation : field->annotations_) {
    indent(out) << ".add(new java.util.AbstractMap.SimpleImmutableEntry<>(\"" + annotation.first
                       + "\", \"" + annotation.second.back() + "\"))"
                << '\n';
  }
  indent(out) << ".build().collect(java.util.stream.Collectors.toMap(java.util.Map.Entry::getKey, "
                 "java.util.Map.Entry::getValue))";
  indent_down();
  indent_down();

  indent_down();
  indent_down();
}

void t_java_generator::generate_field_value_meta_data(std::ostream& out, t_type* type) {
  t_type* ttype = get_true_type(type);
  out << '\n';
  indent_up();
  indent_up();
  if (ttype->is_struct() || ttype->is_xception()) {
    indent(out) << "new "
                   "org.apache.thrift.meta_data.StructMetaData(org.apache.thrift.protocol.TType."
                   "STRUCT, "
                << type_name(ttype) << ".class";
  } else if (ttype->is_container()) {
    if (ttype->is_list()) {
      indent(out)
          << "new org.apache.thrift.meta_data.ListMetaData(org.apache.thrift.protocol.TType.LIST, ";
      t_type* elem_type = ((t_list*)ttype)->get_elem_type();
      generate_field_value_meta_data(out, elem_type);
    } else if (ttype->is_set()) {
      indent(out)
          << "new org.apache.thrift.meta_data.SetMetaData(org.apache.thrift.protocol.TType.SET, ";
      t_type* elem_type = ((t_set*)ttype)->get_elem_type();
      generate_field_value_meta_data(out, elem_type);
    } else { // map
      indent(out)
          << "new org.apache.thrift.meta_data.MapMetaData(org.apache.thrift.protocol.TType.MAP, ";
      t_type* key_type = ((t_map*)ttype)->get_key_type();
      t_type* val_type = ((t_map*)ttype)->get_val_type();
      generate_field_value_meta_data(out, key_type);
      out << ", ";
      generate_field_value_meta_data(out, val_type);
    }
  } else if (ttype->is_enum()) {
    indent(out)
        << "new org.apache.thrift.meta_data.EnumMetaData(org.apache.thrift.protocol.TType.ENUM, "
        << type_name(ttype) << ".class";
  } else {
    indent(out) << "new org.apache.thrift.meta_data.FieldValueMetaData("
                << get_java_type_string(ttype);
    if (ttype->is_binary()) {
      indent(out) << ", true";
    } else if (type->is_typedef()) {
      indent(out) << ", \"" << ((t_typedef*)type)->get_symbolic() << "\"";
    }
  }
  out << ")";
  indent_down();
  indent_down();
}

/**
 * Generates a thrift service. In C++, this comprises an entirely separate
 * header and source file. The header file defines the methods and includes
 * the data types defined in the main header file, and the implementation
 * file contains implementations of the basic printer and default interfaces.
 *
 * @param tservice The service definition
 */
void t_java_generator::generate_service(t_service* tservice) {
  // Make output file
  string f_service_name = package_dir_ + "/" + make_valid_java_filename(service_name_) + ".java";
  f_service_.open(f_service_name.c_str());

  f_service_ << autogen_comment() << java_package();

  if (!suppress_generated_annotations_) {
    generate_javax_generated_annotation(f_service_);
  }
  f_service_ << java_suppressions();
  f_service_ << "public class " << make_valid_java_identifier(service_name_) << " {" << '\n' << '\n';
  indent_up();

  // Generate the three main parts of the service
  generate_service_interface(tservice);
  generate_service_async_interface(tservice);
  if (generate_future_iface_) {
    generate_service_future_interface(tservice);
  }
  generate_service_client(tservice);
  generate_service_async_client(tservice);
  if (generate_future_iface_) {
    generate_service_future_client(tservice);
  }
  generate_service_server(tservice);
  generate_service_async_server(tservice);
  generate_service_helpers(tservice);

  indent_down();
  f_service_ << "}" << '\n';
  f_service_.close();
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_java_generator::generate_service_interface(t_service* tservice) {
  string extends = "";
  string extends_iface = "";
  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends());
    extends_iface = " extends " + extends + ".Iface";
  }

  generate_java_doc(f_service_, tservice);
  f_service_ << indent() << "public interface Iface" << extends_iface << " {" << '\n' << '\n';
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_java_doc(f_service_, *f_iter);
    indent(f_service_) << "public " << function_signature(*f_iter) << ";" << '\n' << '\n';
  }
  indent_down();
  f_service_ << indent() << "}" << '\n' << '\n';
}

void t_java_generator::generate_service_async_interface(t_service* tservice) {
  string extends = "";
  string extends_iface = "";
  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends());
    extends_iface = " extends " + extends + ".AsyncIface";
  }

  f_service_ << indent() << "public interface AsyncIface" << extends_iface << " {" << '\n' << '\n';
  indent_up();
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) << "public " << function_signature_async(*f_iter, true)
                       << " throws org.apache.thrift.TException;" << '\n'
                       << '\n';
  }
  indent_down();
  f_service_ << indent() << "}" << '\n' << '\n';
}

void t_java_generator::generate_service_future_interface(t_service* tservice) {
  string extends = "";
  string extends_iface = "";
  if (tservice->get_extends() != nullptr) {
    extends = type_name(tservice->get_extends());
    extends_iface = " extends " + extends + " .FutureIface";
  }

  f_service_ << indent() << "public interface FutureIface" << extends_iface << " {" << '\n' << '\n';
  indent_up();
  for (auto tfunc : tservice->get_functions()) {
    indent(f_service_) << "public " << function_signature_future(tfunc)
                       << " throws org.apache.thrift.TException;" << '\n'
                       << '\n';
  }
  scope_down(f_service_);
  f_service_ << '\n' << '\n';
}

/**
 * Generates structs for all the service args and return types
 *
 * @param tservice The service
 */
void t_java_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_java_struct_definition(f_service_, ts, false, true);
    generate_function_helpers(*f_iter);
  }
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_client(t_service* tservice) {
  string extends_client = "org.apache.thrift.TServiceClient";
  if (tservice->get_extends() != nullptr) {
    extends_client = type_name(tservice->get_extends()) + ".Client";
  }

  indent(f_service_) << "public static class Client extends " << extends_client
                     << " implements Iface {" << '\n';
  indent_up();

  indent(f_service_)
      << "public static class Factory implements org.apache.thrift.TServiceClientFactory<Client> {"
      << '\n';
  indent_up();
  indent(f_service_) << "public Factory() {}" << '\n';
  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public Client getClient(org.apache.thrift.protocol.TProtocol prot) {"
                     << '\n';
  indent_up();
  indent(f_service_) << "return new Client(prot);" << '\n';
  indent_down();
  indent(f_service_) << "}" << '\n';
  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public Client getClient(org.apache.thrift.protocol.TProtocol iprot, "
                        "org.apache.thrift.protocol.TProtocol oprot) {"
                     << '\n';
  indent_up();
  indent(f_service_) << "return new Client(iprot, oprot);" << '\n';
  indent_down();
  indent(f_service_) << "}" << '\n';
  indent_down();
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << "public Client(org.apache.thrift.protocol.TProtocol prot)" << '\n';
  scope_up(f_service_);
  indent(f_service_) << "super(prot, prot);" << '\n';
  scope_down(f_service_);
  f_service_ << '\n';

  indent(f_service_) << "public Client(org.apache.thrift.protocol.TProtocol iprot, "
                        "org.apache.thrift.protocol.TProtocol oprot) {"
                     << '\n';
  indent(f_service_) << "  super(iprot, oprot);" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();
    string sep = "_";
    string javaname = funname;
    if (fullcamel_style_) {
      sep = "";
      javaname = as_camel_case(funname);
    }

    // Open function
    indent(f_service_) << java_override_annotation() << '\n';
    indent(f_service_) << "public " << function_signature(*f_iter) << '\n';
    scope_up(f_service_);
    indent(f_service_) << "send" << sep << javaname << "(";

    // Get the struct of function call params
    t_struct* arg_struct = (*f_iter)->get_arglist();

    // Declare the function arguments
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      if (first) {
        first = false;
      } else {
        f_service_ << ", ";
      }
      f_service_ << make_valid_java_identifier((*fld_iter)->get_name());
    }
    f_service_ << ");" << '\n';

    if (!(*f_iter)->is_oneway()) {
      f_service_ << indent();
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ << "return ";
      }
      f_service_ << "recv" << sep << javaname << "();" << '\n';
    }
    scope_down(f_service_);
    f_service_ << '\n';

    t_function send_function(g_type_void, string("send") + sep + javaname,
                             (*f_iter)->get_arglist());

    string argsname = (*f_iter)->get_name() + "_args";

    // Open function
    indent(f_service_) << "public " << function_signature(&send_function) << '\n';
    scope_up(f_service_);

    // Serialize the request
    indent(f_service_) << argsname << " args = new " << argsname << "();" << '\n';

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent(f_service_) << "args.set" << get_cap_name((*fld_iter)->get_name()) << "("
                         << make_valid_java_identifier((*fld_iter)->get_name()) << ");" << '\n';
    }

    const string sendBaseName = (*f_iter)->is_oneway() ? "sendBaseOneway" : "sendBase";
    indent(f_service_) << sendBaseName << "(\"" << funname << "\", args);" << '\n';

    scope_down(f_service_);
    f_service_ << '\n';

    if (!(*f_iter)->is_oneway()) {
      string resultname = (*f_iter)->get_name() + "_result";

      t_struct noargs(program_);
      t_function recv_function((*f_iter)->get_returntype(), string("recv") + sep + javaname,
                               &noargs, (*f_iter)->get_xceptions());
      // Open function
      indent(f_service_) << "public " << function_signature(&recv_function) << '\n';
      scope_up(f_service_);

      f_service_ << indent() << resultname << " result = new " << resultname << "();" << '\n'
                 << indent() << "receiveBase(result, \"" << funname << "\");" << '\n';

      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ << indent() << "if (result." << generate_isset_check("success") << ") {" << '\n'
                   << indent() << "  return result.success;" << '\n'
                   << indent() << "}" << '\n';
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        f_service_ << indent() << "if (result." << make_valid_java_identifier((*x_iter)->get_name()) << " != null) {" << '\n'
                   << indent() << "  throw result." << make_valid_java_identifier((*x_iter)->get_name()) << ";" << '\n'
                   << indent() << "}" << '\n';
      }

      // If you get here it's an exception, unless a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) << "return;" << '\n';
      } else {
        f_service_ << indent()
                   << "throw new "
                      "org.apache.thrift.TApplicationException(org.apache.thrift."
                      "TApplicationException.MISSING_RESULT, \""
                   << (*f_iter)->get_name() << " failed: unknown result\");" << '\n';
      }

      // Close function
      scope_down(f_service_);
      f_service_ << '\n';
    }
  }

  indent_down();
  indent(f_service_) << "}" << '\n';
}

void t_java_generator::generate_service_future_client(t_service* tservice) {
  string extends_client = "";
  if (tservice->get_extends() != nullptr) {
    extends_client = "extends " + type_name(tservice->get_extends()) + ".FutureClient ";
  }

  static string adapter_class = "org.apache.thrift.async.AsyncMethodFutureAdapter";
  indent(f_service_) << "public static class FutureClient " << extends_client
                     << "implements FutureIface {" << '\n';
  indent_up();
  indent(f_service_) << "public FutureClient(AsyncIface delegate) {" << '\n';
  indent_up();
  indent(f_service_) << "this.delegate = delegate;" << '\n';
  scope_down(f_service_);
  indent(f_service_) << "private final AsyncIface delegate;" << '\n';
  for (auto tfunc : tservice->get_functions()) {
    string funname = tfunc->get_name();
    string sep = "_";
    string javaname = funname;
    if (fullcamel_style_) {
      sep = "";
      javaname = as_camel_case(javaname);
    }
    auto ret_type_name = type_name(tfunc->get_returntype(), /*in_container=*/true);
    t_struct* arg_struct = tfunc->get_arglist();
    string funclassname = funname + "_call";
    auto fields = arg_struct->get_members();

    string args_name = funname + "_args";
    string result_name = funname + "_result";

    indent(f_service_) << "@Override" << '\n';
    indent(f_service_) << "public " << function_signature_future(tfunc)
                       << " throws org.apache.thrift.TException {" << '\n';
    indent_up();
    auto adapter = tmp("asyncMethodFutureAdapter");
    indent(f_service_) << adapter_class << "<" << ret_type_name << "> " << adapter << " = "
                       << adapter_class << ".<" << ret_type_name << ">create();" << '\n';
    bool empty_args = tfunc->get_arglist()->get_members().empty();
    indent(f_service_) << "delegate." << get_rpc_method_name(funname) << "("
                       << argument_list(tfunc->get_arglist(), false) << (empty_args ? "" : ", ")
                       << adapter << ");" << '\n';
    indent(f_service_) << "return " << adapter << ".getFuture();" << '\n';
    scope_down(f_service_);
    f_service_ << '\n';
  }
  scope_down(f_service_);
  f_service_ << '\n';
}

void t_java_generator::generate_service_async_client(t_service* tservice) {
  string extends_client = "org.apache.thrift.async.TAsyncClient";
  if (tservice->get_extends() != nullptr) {
    extends_client = type_name(tservice->get_extends()) + ".AsyncClient";
  }

  indent(f_service_) << "public static class AsyncClient extends " << extends_client
                     << " implements AsyncIface {" << '\n';
  indent_up();

  // Factory method
  indent(f_service_) << "public static class Factory implements "
                        "org.apache.thrift.async.TAsyncClientFactory<AsyncClient> {"
                     << '\n';
  indent(f_service_) << "  private org.apache.thrift.async.TAsyncClientManager clientManager;"
                     << '\n';
  indent(f_service_) << "  private org.apache.thrift.protocol.TProtocolFactory protocolFactory;"
                     << '\n';
  indent(f_service_) << "  public Factory(org.apache.thrift.async.TAsyncClientManager "
                        "clientManager, org.apache.thrift.protocol.TProtocolFactory "
                        "protocolFactory) {"
                     << '\n';
  indent(f_service_) << "    this.clientManager = clientManager;" << '\n';
  indent(f_service_) << "    this.protocolFactory = protocolFactory;" << '\n';
  indent(f_service_) << "  }" << '\n';
  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "  public AsyncClient "
                        "getAsyncClient(org.apache.thrift.transport.TNonblockingTransport "
                        "transport) {"
                     << '\n';
  indent(f_service_) << "    return new AsyncClient(protocolFactory, clientManager, transport);"
                     << '\n';
  indent(f_service_) << "  }" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << "public AsyncClient(org.apache.thrift.protocol.TProtocolFactory "
                        "protocolFactory, org.apache.thrift.async.TAsyncClientManager "
                        "clientManager, org.apache.thrift.transport.TNonblockingTransport "
                        "transport) {"
                     << '\n';
  indent(f_service_) << "  super(protocolFactory, clientManager, transport);" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();
    string sep = "_";
    string javaname = funname;
    if (fullcamel_style_) {
      sep = "";
      javaname = as_camel_case(javaname);
    }
    t_type* ret_type = (*f_iter)->get_returntype();
    t_struct* arg_struct = (*f_iter)->get_arglist();
    string funclassname = funname + "_call";
    const vector<t_field*>& fields = arg_struct->get_members();
    const std::vector<t_field*>& xceptions = (*f_iter)->get_xceptions()->get_members();
    vector<t_field*>::const_iterator fld_iter;
    string args_name = (*f_iter)->get_name() + "_args";
    string result_name = (*f_iter)->get_name() + "_result";

    // Main method body
    indent(f_service_) << java_override_annotation() << '\n';
    indent(f_service_) << "public " << function_signature_async(*f_iter, false)
                       << " throws org.apache.thrift.TException {" << '\n';
    indent(f_service_) << "  checkReady();" << '\n';
    indent(f_service_) << "  " << funclassname << " method_call = new " + funclassname + "("
                       << async_argument_list(*f_iter, arg_struct, ret_type)
                       << ", this, ___protocolFactory, ___transport);" << '\n';
    indent(f_service_) << "  this.___currentMethod = method_call;" << '\n';
    indent(f_service_) << "  ___manager.call(method_call);" << '\n';
    indent(f_service_) << "}" << '\n';

    f_service_ << '\n';

    // TAsyncMethod object for this function call
    indent(f_service_) << "public static class " + funclassname
                              + " extends org.apache.thrift.async.TAsyncMethodCall<"
                              + type_name((*f_iter)->get_returntype(), true) + "> {"
                       << '\n';
    indent_up();

    // Member variables
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent(f_service_) << "private " + type_name((*fld_iter)->get_type()) + " "
                                + make_valid_java_identifier((*fld_iter)->get_name()) + ";"
                         << '\n';
    }

    // NOTE since we use a new Client instance to deserialize, let's keep seqid to 0 for now
    // indent(f_service_) << "private int seqid;" << '\n' << '\n';

    // Constructor
    indent(f_service_) << "public " + funclassname + "("
                              + async_argument_list(*f_iter, arg_struct, ret_type, true)
                       << ", org.apache.thrift.async.TAsyncClient client, "
                          "org.apache.thrift.protocol.TProtocolFactory protocolFactory, "
                          "org.apache.thrift.transport.TNonblockingTransport transport) throws "
                          "org.apache.thrift.TException {"
                       << '\n';
    indent(f_service_) << "  super(client, protocolFactory, transport, resultHandler, "
                       << ((*f_iter)->is_oneway() ? "true" : "false") << ");" << '\n';

    // Assign member variables
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      indent(f_service_) << "  this." + make_valid_java_identifier((*fld_iter)->get_name()) + " = " + make_valid_java_identifier((*fld_iter)->get_name())
                                + ";"
                         << '\n';
    }

    indent(f_service_) << "}" << '\n' << '\n';
    indent(f_service_) << java_override_annotation() << '\n';
    indent(f_service_) << "public void write_args(org.apache.thrift.protocol.TProtocol prot) "
                          "throws org.apache.thrift.TException {"
                       << '\n';
    indent_up();

    // Serialize request
    // NOTE we are leaving seqid as 0, for now (see above)
    f_service_ << indent() << "prot.writeMessageBegin(new org.apache.thrift.protocol.TMessage(\""
               << funname << "\", org.apache.thrift.protocol."
               << ((*f_iter)->is_oneway() ? "TMessageType.ONEWAY" : "TMessageType.CALL") << ", 0));"
               << '\n'
               << indent() << args_name << " args = new " << args_name << "();" << '\n';

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ << indent() << "args.set" << get_cap_name((*fld_iter)->get_name()) << "("
                 << make_valid_java_identifier((*fld_iter)->get_name()) << ");" << '\n';
    }

    f_service_ << indent() << "args.write(prot);" << '\n'
               << indent() << "prot.writeMessageEnd();" << '\n';

    indent_down();
    indent(f_service_) << "}" << '\n' << '\n';

    // Return method
    indent(f_service_) << java_override_annotation() << '\n';
    indent(f_service_) << "public " + type_name(ret_type, true) + " getResult() throws ";
    vector<t_field*>::const_iterator x_iter;
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      f_service_ << type_name((*x_iter)->get_type(), false, false) + ", ";
    }
    f_service_ << "org.apache.thrift.TException {" << '\n';

    indent_up();
    f_service_
        << indent()
        << "if (getState() != org.apache.thrift.async.TAsyncMethodCall.State.RESPONSE_READ) {"
        << '\n'
        << indent() << "  throw new java.lang.IllegalStateException(\"Method call not finished!\");"
        << '\n'
        << indent() << "}" << '\n'
        << indent()
        << "org.apache.thrift.transport.TMemoryInputTransport memoryTransport = new "
           "org.apache.thrift.transport.TMemoryInputTransport(getFrameBuffer().array());"
        << '\n'
        << indent()
        << "org.apache.thrift.protocol.TProtocol prot = "
           "client.getProtocolFactory().getProtocol(memoryTransport);"
        << '\n';
    indent(f_service_);
    if (ret_type->is_void()) { // NB: Includes oneways which always return void.
      if (!(*f_iter)->is_oneway()) {
        f_service_ << "(new Client(prot)).recv" + sep + javaname + "();" << '\n';
        indent(f_service_);
      }
      f_service_ << "return null;" << '\n';
    } else {
      f_service_ << "return (new Client(prot)).recv" + sep + javaname + "();" << '\n';
    }

    // Close function
    indent_down();
    indent(f_service_) << "}" << '\n';

    // Close class
    indent_down();
    indent(f_service_) << "}" << '\n' << '\n';
  }

  // Close AsyncClient
  scope_down(f_service_);
  f_service_ << '\n';
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  // Extends stuff
  string extends = "";
  string extends_processor = "";
  if (tservice->get_extends() == nullptr) {
    extends_processor = "org.apache.thrift.TBaseProcessor<I>";
  } else {
    extends = type_name(tservice->get_extends());
    extends_processor = extends + ".Processor<I>";
  }

  // Generate the header portion
  indent(f_service_) << "public static class Processor<I extends Iface> extends "
                     << extends_processor << " implements org.apache.thrift.TProcessor {" << '\n';
  indent_up();

  indent(f_service_) << "private static final org.slf4j.Logger _LOGGER = "
                        "org.slf4j.LoggerFactory.getLogger(Processor.class.getName());"
                     << '\n';

  indent(f_service_) << "public Processor(I iface) {" << '\n';
  indent(f_service_) << "  super(iface, getProcessMap(new java.util.HashMap<java.lang.String, "
                        "org.apache.thrift.ProcessFunction<I, ? extends "
                        "org.apache.thrift.TBase, ? extends org.apache.thrift.TBase>>()));"
                     << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << "protected Processor(I iface, java.util.Map<java.lang.String, "
                        "org.apache.thrift.ProcessFunction<I, ? extends org.apache.thrift.TBase, ? "
                        "extends org.apache.thrift.TBase>> processMap) {"
                     << '\n';
  indent(f_service_) << "  super(iface, getProcessMap(processMap));" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << "private static <I extends Iface> java.util.Map<java.lang.String, "
                        "org.apache.thrift.ProcessFunction<I, ? extends org.apache.thrift.TBase, "
                        "? extends org.apache.thrift.TBase>> "
                        "getProcessMap(java.util.Map<java.lang.String, "
                        "org.apache.thrift.ProcessFunction<I, ? extends "
                        " org.apache.thrift.TBase, ? extends org.apache.thrift.TBase>> processMap) {"
                     << '\n';
  indent_up();
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) << "processMap.put(\"" << (*f_iter)->get_name() << "\", new "
                       << make_valid_java_identifier((*f_iter)->get_name()) << "());" << '\n';
  }
  indent(f_service_) << "return processMap;" << '\n';
  indent_down();
  indent(f_service_) << "}" << '\n' << '\n';

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  indent(f_service_) << "}" << '\n' << '\n';
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_java_generator::generate_service_async_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  // Extends stuff
  string extends = "";
  string extends_processor = "";
  if (tservice->get_extends() == nullptr) {
    extends_processor = "org.apache.thrift.TBaseAsyncProcessor<I>";
  } else {
    extends = type_name(tservice->get_extends());
    extends_processor = extends + ".AsyncProcessor<I>";
  }

  // Generate the header portion
  indent(f_service_) << "public static class AsyncProcessor<I extends AsyncIface> extends "
                     << extends_processor << " {" << '\n';
  indent_up();

  indent(f_service_) << "private static final org.slf4j.Logger _LOGGER = "
                        "org.slf4j.LoggerFactory.getLogger(AsyncProcessor.class.getName());"
                     << '\n';

  indent(f_service_) << "public AsyncProcessor(I iface) {" << '\n';
  indent(f_service_) << "  super(iface, getProcessMap(new java.util.HashMap<java.lang.String, "
                        "org.apache.thrift.AsyncProcessFunction<I, ? extends "
                        "org.apache.thrift.TBase, ?, ? extends org.apache.thrift.TBase>>()));"
                     << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << "protected AsyncProcessor(I iface, java.util.Map<java.lang.String,  "
                        "org.apache.thrift.AsyncProcessFunction<I, ? extends  "
                        "org.apache.thrift.TBase, ?, ? extends org.apache.thrift.TBase>> processMap) {"
                     << '\n';
  indent(f_service_) << "  super(iface, getProcessMap(processMap));" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_)
      << "private static <I extends AsyncIface> java.util.Map<java.lang.String,  "
         "org.apache.thrift.AsyncProcessFunction<I, ? extends  "
         "org.apache.thrift.TBase, ?, ? extends org.apache.thrift.TBase>> getProcessMap(java.util.Map<java.lang.String,  "
         "org.apache.thrift.AsyncProcessFunction<I, ? extends  "
         "org.apache.thrift.TBase, ?, ? extends org.apache.thrift.TBase>> processMap) {"
      << '\n';
  indent_up();
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_service_) << "processMap.put(\"" << (*f_iter)->get_name() << "\", new "
                       << make_valid_java_identifier((*f_iter)->get_name()) << "());" << '\n';
  }
  indent(f_service_) << "return processMap;" << '\n';
  indent_down();
  indent(f_service_) << "}" << '\n' << '\n';

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_process_async_function(tservice, *f_iter);
  }

  indent_down();
  indent(f_service_) << "}" << '\n' << '\n';
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_java_generator::generate_function_helpers(t_function* tfunction) {
  if (tfunction->is_oneway()) {
    return;
  }

  t_struct result(program_, tfunction->get_name() + "_result");
  t_field success(tfunction->get_returntype(), "success", 0);
  if (!tfunction->get_returntype()->is_void()) {
    result.append(&success);
  }

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    result.append(*f_iter);
  }

  generate_java_struct_definition(f_service_, &result, false, true, true);
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_java_generator::generate_process_async_function(t_service* tservice, t_function* tfunction) {
  string argsname = tfunction->get_name() + "_args";

  string resultname = tfunction->get_name() + "_result";
  if (tfunction->is_oneway()) {
    resultname = "org.apache.thrift.TBase";
  }

  string resulttype = type_name(tfunction->get_returntype(), true);

  (void)tservice;
  // Open class
  indent(f_service_) << "public static class " << make_valid_java_identifier(tfunction->get_name())
                     << "<I extends AsyncIface> extends org.apache.thrift.AsyncProcessFunction<I, "
                     << argsname << ", " << resulttype << ", " << resultname << "> {" << '\n';
  indent_up();

  indent(f_service_) << "public " << make_valid_java_identifier(tfunction->get_name()) << "() {" << '\n';
  indent(f_service_) << "  super(\"" << tfunction->get_name() << "\");" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public " << resultname << " getEmptyResultInstance() {" << '\n';
  if (tfunction->is_oneway()) {
    indent(f_service_) << "  return null;" << '\n';
  }
  else {
    indent(f_service_) << "  return new " << resultname << "();" << '\n';
  }
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public " << argsname << " getEmptyArgsInstance() {" << '\n';
  indent(f_service_) << "  return new " << argsname << "();" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public org.apache.thrift.async.AsyncMethodCallback<" << resulttype
                     << "> getResultHandler(final "
                        "org.apache.thrift.server.AbstractNonblockingServer.AsyncFrameBuffer fb, "
                        "final int seqid) {"
                     << '\n';
  indent_up();
  indent(f_service_) << "final org.apache.thrift.AsyncProcessFunction fcall = this;" << '\n';
  indent(f_service_) << "return new org.apache.thrift.async.AsyncMethodCallback<" << resulttype
                     << ">() { " << '\n';
  indent_up();
  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public void onComplete(" << resulttype << " o) {" << '\n';

  indent_up();
  if (!tfunction->is_oneway()) {
    indent(f_service_) << resultname << " result = new " << resultname << "();" << '\n';

    if (!tfunction->get_returntype()->is_void()) {
      indent(f_service_) << "result.success = o;" << '\n';
      // Set isset on success field
      if (!type_can_be_null(tfunction->get_returntype())) {
        indent(f_service_) << "result.set" << get_cap_name("success") << get_cap_name("isSet")
                           << "(true);" << '\n';
      }
    }

    indent(f_service_) << "try {" << '\n';
    indent(f_service_)
        << "  fcall.sendResponse(fb, result, org.apache.thrift.protocol.TMessageType.REPLY,seqid);"
        << '\n';
    indent(f_service_) << "} catch (org.apache.thrift.transport.TTransportException e) {" << '\n';
    indent_up();
    f_service_ << indent()
               << "_LOGGER.error(\"TTransportException writing to internal frame buffer\", e);"
               << '\n'
               << indent() << "fb.close();" << '\n';
    indent_down();
    indent(f_service_) << "} catch (java.lang.Exception e) {" << '\n';
    indent_up();
    f_service_ << indent() << "_LOGGER.error(\"Exception writing to internal frame buffer\", e);"
               << '\n'
               << indent() << "onError(e);" << '\n';
    indent_down();
    indent(f_service_) << "}" << '\n';
  }
  indent_down();
  indent(f_service_) << "}" << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public void onError(java.lang.Exception e) {" << '\n';
  indent_up();

  if (tfunction->is_oneway()) {
    indent(f_service_) << "if (e instanceof org.apache.thrift.transport.TTransportException) {"
                       << '\n';
    indent_up();

    f_service_ << indent() << "_LOGGER.error(\"TTransportException inside handler\", e);" << '\n'
               << indent() << "fb.close();" << '\n';

    indent_down();
    indent(f_service_) << "} else {" << '\n';
    indent_up();

    f_service_ << indent() << "_LOGGER.error(\"Exception inside oneway handler\", e);" << '\n';

    indent_down();
    indent(f_service_) << "}" << '\n';
  } else {
    indent(f_service_) << "byte msgType = org.apache.thrift.protocol.TMessageType.REPLY;" << '\n';
    indent(f_service_) << "org.apache.thrift.TSerializable msg;" << '\n';
    indent(f_service_) << resultname << " result = new " << resultname << "();" << '\n';

    t_struct* xs = tfunction->get_xceptions();
    const std::vector<t_field*>& xceptions = xs->get_members();

    vector<t_field*>::const_iterator x_iter;
    if (xceptions.size() > 0) {
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        if (x_iter == xceptions.begin())
          f_service_ << indent();
        string type = type_name((*x_iter)->get_type(), false, false);
        string name = (*x_iter)->get_name();
        f_service_ << "if (e instanceof " << type << ") {" << '\n';
        indent_up();
        f_service_ << indent() << "result." << make_valid_java_identifier(name) << " = (" << type << ") e;" << '\n'
                   << indent() << "result.set" << get_cap_name(name) << get_cap_name("isSet")
                   << "(true);" << '\n'
                   << indent() << "msg = result;" << '\n';
        indent_down();
        indent(f_service_) << "} else ";
      }
    } else {
      indent(f_service_);
    }
    f_service_ << "if (e instanceof org.apache.thrift.transport.TTransportException) {" << '\n';
    indent_up();
    f_service_ << indent() << "_LOGGER.error(\"TTransportException inside handler\", e);" << '\n'
               << indent() << "fb.close();" << '\n'
               << indent() << "return;" << '\n';
    indent_down();
    indent(f_service_) << "} else if (e instanceof org.apache.thrift.TApplicationException) {"
                       << '\n';
    indent_up();
    f_service_ << indent() << "_LOGGER.error(\"TApplicationException inside handler\", e);" << '\n'
               << indent() << "msgType = org.apache.thrift.protocol.TMessageType.EXCEPTION;" << '\n'
               << indent() << "msg = (org.apache.thrift.TApplicationException)e;" << '\n';
    indent_down();
    indent(f_service_) << "} else {" << '\n';
    indent_up();
    f_service_ << indent() << "_LOGGER.error(\"Exception inside handler\", e);" << '\n'
               << indent() << "msgType = org.apache.thrift.protocol.TMessageType.EXCEPTION;" << '\n'
               << indent()
               << "msg = new "
                  "org.apache.thrift.TApplicationException(org.apache.thrift."
                  "TApplicationException.INTERNAL_ERROR, e.getMessage());"
               << '\n';
    indent_down();
    f_service_ << indent() << "}" << '\n'
               << indent() << "try {" << '\n'
               << indent() << "  fcall.sendResponse(fb,msg,msgType,seqid);" << '\n'
               << indent() << "} catch (java.lang.Exception ex) {" << '\n'
               << indent() << "  _LOGGER.error(\"Exception writing to internal frame buffer\", ex);"
               << '\n'
               << indent() << "  fb.close();" << '\n'
               << indent() << "}" << '\n';
  }
  indent_down();
  indent(f_service_) << "}" << '\n';
  indent_down();
  indent(f_service_) << "};" << '\n';
  indent_down();
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public boolean isOneway() {" << '\n';
  indent(f_service_) << "  return " << ((tfunction->is_oneway()) ? "true" : "false") << ";" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public void start(I iface, " << argsname
                     << " args, org.apache.thrift.async.AsyncMethodCallback<" << resulttype
                     << "> resultHandler) throws org.apache.thrift.TException {" << '\n';
  indent_up();

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;
  f_service_ << indent();

  f_service_ << "iface." << get_rpc_method_name(tfunction->get_name()) << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "args." << make_valid_java_identifier((*f_iter)->get_name());
  }
  if (!first)
    f_service_ << ",";
  f_service_ << "resultHandler";
  f_service_ << ");" << '\n';

  indent_down();
  indent(f_service_) << "}";

  // Close function
  f_service_ << '\n';

  // Close class
  indent_down();
  f_service_ << indent() << "}" << '\n' << '\n';
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_java_generator::generate_process_function(t_service* tservice, t_function* tfunction) {
  string argsname = tfunction->get_name() + "_args";
  string resultname = tfunction->get_name() + "_result";
  if (tfunction->is_oneway()) {
    resultname = "org.apache.thrift.TBase";
  }

  (void)tservice;
  // Open class
  indent(f_service_) << "public static class " << make_valid_java_identifier(tfunction->get_name())
                     << "<I extends Iface> extends org.apache.thrift.ProcessFunction<I, "
                     << argsname << ", " << resultname << "> {" << '\n';
  indent_up();

  indent(f_service_) << "public " << make_valid_java_identifier(tfunction->get_name()) << "() {" << '\n';
  indent(f_service_) << "  super(\"" << tfunction->get_name() << "\");" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public " << argsname << " getEmptyArgsInstance() {" << '\n';
  indent(f_service_) << "  return new " << argsname << "();" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public boolean isOneway() {" << '\n';
  indent(f_service_) << "  return " << ((tfunction->is_oneway()) ? "true" : "false") << ";" << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "protected boolean rethrowUnhandledExceptions() {" << '\n';
  indent(f_service_) << "  return " << ((rethrow_unhandled_exceptions_) ? "true" : "false") << ";"
                     << '\n';
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public " << resultname << " getEmptyResultInstance() {" << '\n';
  if (tfunction->is_oneway()) {
    indent(f_service_) << "  return null;" << '\n';
  }
  else {
    indent(f_service_) << "  return new " << resultname << "();" << '\n';
  }
  indent(f_service_) << "}" << '\n' << '\n';

  indent(f_service_) << java_override_annotation() << '\n';
  indent(f_service_) << "public " << resultname << " getResult(I iface, " << argsname
                     << " args) throws org.apache.thrift.TException {" << '\n';
  indent_up();
  if (!tfunction->is_oneway()) {
    indent(f_service_) << resultname << " result = getEmptyResultInstance();" << '\n';
  }

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  // Try block for a function with exceptions
  if (xceptions.size() > 0) {
    f_service_ << indent() << "try {" << '\n';
    indent_up();
  }

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;
  f_service_ << indent();

  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
    f_service_ << "result.success = ";
  }
  f_service_ << "iface." << get_rpc_method_name(tfunction->get_name()) << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "args." << make_valid_java_identifier((*f_iter)->get_name());
  }
  f_service_ << ");" << '\n';

  // Set isset on success field
  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()
      && !type_can_be_null(tfunction->get_returntype())) {
    indent(f_service_) << "result.set" << get_cap_name("success") << get_cap_name("isSet")
                       << "(true);" << '\n';
  }

  if (!tfunction->is_oneway() && xceptions.size() > 0) {
    indent_down();
    f_service_ << indent() << "}";
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      f_service_ << " catch (" << type_name((*x_iter)->get_type(), false, false) << " "
                 << make_valid_java_identifier((*x_iter)->get_name()) << ") {" << '\n';
      if (!tfunction->is_oneway()) {
        indent_up();
        f_service_ << indent() << "result." << make_valid_java_identifier((*x_iter)->get_name()) << " = "
                   << make_valid_java_identifier((*x_iter)->get_name()) << ";" << '\n';
        indent_down();
        f_service_ << indent() << "}";
      } else {
        f_service_ << "}";
      }
    }
    f_service_ << '\n';
  }

  if (tfunction->is_oneway()) {
    indent(f_service_) << "return null;" << '\n';
  } else {
    indent(f_service_) << "return result;" << '\n';
  }
  indent_down();
  indent(f_service_) << "}";

  // Close function
  f_service_ << '\n';

  // Close class
  indent_down();
  f_service_ << indent() << "}" << '\n' << '\n';
}

/**
 * Deserializes a field of any type.
 *
 * @param tfield The field
 * @param prefix The variable name or container for this field
 */
void t_java_generator::generate_deserialize_field(ostream& out,
                                                  t_field* tfield,
                                                  string prefix,
                                                  bool has_metadata) {
  t_type* type = get_true_type(tfield->get_type());

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " + prefix + tfield->get_name();
  }

  string name = prefix + make_valid_java_identifier(tfield->get_name());

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out, (t_struct*)type, name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type, name, has_metadata);
  } else if (type->is_base_type()) {
    indent(out) << name << " = iprot.";

    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "compiler error: cannot serialize void field in a struct: " + name;
      break;
    case t_base_type::TYPE_STRING:
      if (type->is_binary()) {
        out << "readBinary();";
      } else {
        out << "readString();";
      }
      break;
    case t_base_type::TYPE_BOOL:
      out << "readBool();";
      break;
    case t_base_type::TYPE_I8:
      out << "readByte();";
      break;
    case t_base_type::TYPE_I16:
      out << "readI16();";
      break;
    case t_base_type::TYPE_I32:
      out << "readI32();";
      break;
    case t_base_type::TYPE_I64:
      out << "readI64();";
      break;
    case t_base_type::TYPE_UUID:
      out << "readUuid();";
      break;
    case t_base_type::TYPE_DOUBLE:
      out << "readDouble();";
      break;
    default:
      throw "compiler error: no Java name for base type " + t_base_type::t_base_name(tbase);
    }
    out << '\n';
  } else if (type->is_enum()) {
    indent(out) << name << " = "
                << type_name(tfield->get_type(), true, false, false, true)
                       + ".findByValue(iprot.readI32());"
                << '\n';
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n", tfield->get_name().c_str(),
           type_name(type).c_str());
  }
}

/**
 * Generates an unserializer for a struct, invokes read()
 */
void t_java_generator::generate_deserialize_struct(ostream& out, t_struct* tstruct, string prefix) {

  if (reuse_objects_) {
    indent(out) << "if (" << prefix << " == null) {" << '\n';
    indent_up();
  }
  indent(out) << prefix << " = new " << type_name(tstruct) << "();" << '\n';
  if (reuse_objects_) {
    indent_down();
    indent(out) << "}" << '\n';
  }
  indent(out) << prefix << ".read(iprot);" << '\n';
}

/**
 * Deserializes a container by reading its size and then iterating
 */
void t_java_generator::generate_deserialize_container(ostream& out,
                                                      t_type* ttype,
                                                      string prefix,
                                                      bool has_metadata) {

  scope_up(out);

  string obj;

  if (ttype->is_map()) {
    obj = tmp("_map");
  } else if (ttype->is_set()) {
    obj = tmp("_set");
  } else if (ttype->is_list()) {
    obj = tmp("_list");
  }

  if (has_metadata) {
    // Declare variables, read header
    if (ttype->is_map()) {
      indent(out) << "org.apache.thrift.protocol.TMap " << obj << " = iprot.readMapBegin();"
                  << '\n';
    } else if (ttype->is_set()) {
      indent(out) << "org.apache.thrift.protocol.TSet " << obj << " = iprot.readSetBegin();"
                  << '\n';
    } else if (ttype->is_list()) {
      indent(out) << "org.apache.thrift.protocol.TList " << obj << " = iprot.readListBegin();"
                  << '\n';
    }
  } else {
    // Declare variables, read header
    if (ttype->is_map()) {
      indent(out) << "org.apache.thrift.protocol.TMap " << obj << " = iprot.readMapBegin("
                  << type_to_enum(((t_map*)ttype)->get_key_type()) << ", "
                  << type_to_enum(((t_map*)ttype)->get_val_type()) << "); " << '\n';
    } else if (ttype->is_set()) {
      indent(out) << "org.apache.thrift.protocol.TSet " << obj << " = iprot.readSetBegin("
                  << type_to_enum(((t_set*)ttype)->get_elem_type()) << ");" << '\n';
    } else if (ttype->is_list()) {
      indent(out) << "org.apache.thrift.protocol.TList " << obj << " = iprot.readListBegin("
                  << type_to_enum(((t_list*)ttype)->get_elem_type()) << ");" << '\n';
    }
  }

  if (reuse_objects_) {
    indent(out) << "if (" << prefix << " == null) {" << '\n';
    indent_up();
  }

  if (is_enum_set(ttype)) {
    out << indent() << prefix << " = " << type_name(ttype, false, true, true) << ".noneOf";
  } else {
    out << indent() << prefix << " = new " << type_name(ttype, false, true);
  }

  // construct the collection correctly i.e. with appropriate size/type
  if (is_enum_set(ttype) || is_enum_map(ttype)) {
    out << "(" << inner_enum_type_name(ttype) << ");" << '\n';
  } else if (sorted_containers_ && (ttype->is_map() || ttype->is_set())) {
    // TreeSet and TreeMap don't have any constructor which takes a capacity as an argument
    out << "();" << '\n';
  } else {
    out << "(" << (ttype->is_list() ? "" : "2*") << obj << ".size"
        << ");" << '\n';
  }

  if (reuse_objects_) {
    indent_down();
    indent(out) << "}" << '\n';
  }

  if (ttype->is_map()) {
    generate_deserialize_map_element(out, (t_map*)ttype, prefix, obj, has_metadata);
  } else if (ttype->is_set()) {
    generate_deserialize_set_element(out, (t_set*)ttype, prefix, obj, has_metadata);
  } else if (ttype->is_list()) {
    generate_deserialize_list_element(out, (t_list*)ttype, prefix, obj, has_metadata);
  }

  scope_down(out);

  if (has_metadata) {
    // Read container end
    if (ttype->is_map()) {
      indent(out) << "iprot.readMapEnd();" << '\n';
    } else if (ttype->is_set()) {
      indent(out) << "iprot.readSetEnd();" << '\n';
    } else if (ttype->is_list()) {
      indent(out) << "iprot.readListEnd();" << '\n';
    }
  }
  scope_down(out);
}

/**
 * Generates code to deserialize a map
 */
void t_java_generator::generate_deserialize_map_element(ostream& out,
                                                        t_map* tmap,
                                                        string prefix,
                                                        string obj,
                                                        bool has_metadata) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  indent(out) << declare_field(&fkey, reuse_objects_, false) << '\n';
  indent(out) << declare_field(&fval, reuse_objects_, false) << '\n';

  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) << "for (int " << i << " = 0; " << i << " < " << obj << ".size"
              << "; "
              << "++" << i << ")" << '\n';

  scope_up(out);

  generate_deserialize_field(out, &fkey, "", has_metadata);
  generate_deserialize_field(out, &fval, "", has_metadata);

  if (get_true_type(fkey.get_type())->is_enum()) {
    indent(out) << "if (" << key << " != null)" << '\n';
    scope_up(out);
  }

  indent(out) << prefix << ".put(" << key << ", " << val << ");" << '\n';

  if (get_true_type(fkey.get_type())->is_enum()) {
    scope_down(out);
  }

  if (reuse_objects_ && !get_true_type(fkey.get_type())->is_base_type()) {
    indent(out) << key << " = null;" << '\n';
  }

  if (reuse_objects_ && !get_true_type(fval.get_type())->is_base_type()) {
    indent(out) << val << " = null;" << '\n';
  }
}

/**
 * Deserializes a set element
 */
void t_java_generator::generate_deserialize_set_element(ostream& out,
                                                        t_set* tset,
                                                        string prefix,
                                                        string obj,
                                                        bool has_metadata) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(out) << declare_field(&felem, reuse_objects_, false) << '\n';

  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) << "for (int " << i << " = 0; " << i << " < " << obj << ".size"
              << "; "
              << "++" << i << ")" << '\n';
  scope_up(out);

  generate_deserialize_field(out, &felem, "", has_metadata);

  if (get_true_type(felem.get_type())->is_enum()) {
    indent(out) << "if (" << elem << " != null)" << '\n';
    scope_up(out);
  }

  indent(out) << prefix << ".add(" << elem << ");" << '\n';

  if (get_true_type(felem.get_type())->is_enum()) {
    scope_down(out);
  }

  if (reuse_objects_ && !get_true_type(felem.get_type())->is_base_type()) {
    indent(out) << elem << " = null;" << '\n';
  }
}

/**
 * Deserializes a list element
 */
void t_java_generator::generate_deserialize_list_element(ostream& out,
                                                         t_list* tlist,
                                                         string prefix,
                                                         string obj,
                                                         bool has_metadata) {
  string elem = tmp("_elem");
  t_field felem(tlist->get_elem_type(), elem);

  indent(out) << declare_field(&felem, reuse_objects_, false) << '\n';

  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) << "for (int " << i << " = 0; " << i << " < " << obj << ".size"
              << "; "
              << "++" << i << ")" << '\n';
  scope_up(out);

  generate_deserialize_field(out, &felem, "", has_metadata);

  if (get_true_type(felem.get_type())->is_enum()) {
    indent(out) << "if (" << elem << " != null)" << '\n';
    scope_up(out);
  }

  indent(out) << prefix << ".add(" << elem << ");" << '\n';

  if (get_true_type(felem.get_type())->is_enum()) {
    scope_down(out);
  }

  if (reuse_objects_ && !get_true_type(felem.get_type())->is_base_type()) {
    indent(out) << elem << " = null;" << '\n';
  }
}

/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_java_generator::generate_serialize_field(ostream& out,
                                                t_field* tfield,
                                                string prefix, string postfix,
                                                bool has_metadata) {
  t_type* type = get_true_type(tfield->get_type());

  // Do nothing for void types
  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " + prefix + tfield->get_name() + postfix;
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out, (t_struct*)type, prefix + make_valid_java_identifier(tfield->get_name()) + postfix);
  } else if (type->is_container()) {
    generate_serialize_container(out, type, prefix + make_valid_java_identifier(tfield->get_name()) + postfix, has_metadata);
  } else if (type->is_enum()) {
    indent(out) << "oprot.writeI32(" << prefix + make_valid_java_identifier(tfield->get_name()) + postfix << ".getValue());" << '\n';
  } else if (type->is_base_type()) {
    string name = prefix + make_valid_java_identifier(tfield->get_name()) + postfix;
    indent(out) << "oprot.";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        if (type->is_binary()) {
          out << "writeBinary(" << name << ");";
        } else {
          out << "writeString(" << name << ");";
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool(" << name << ");";
        break;
      case t_base_type::TYPE_I8:
        out << "writeByte(" << name << ");";
        break;
      case t_base_type::TYPE_I16:
        out << "writeI16(" << name << ");";
        break;
      case t_base_type::TYPE_I32:
        out << "writeI32(" << name << ");";
        break;
      case t_base_type::TYPE_I64:
        out << "writeI64(" << name << ");";
        break;
      case t_base_type::TYPE_UUID:
        out << "writeUuid(" << name << ");";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble(" << name << ");";
        break;
      default:
        throw "compiler error: no Java name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "writeI32(struct." << name << ");";
    }
    out << '\n';
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s%s%s' TYPE '%s'\n", prefix.c_str(),
           tfield->get_name().c_str(), postfix.c_str(), type_name(type).c_str());
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_java_generator::generate_serialize_struct(ostream& out, t_struct* tstruct, string prefix) {
  (void)tstruct;
  out << indent() << prefix << ".write(oprot);" << '\n';
}

/**
 * Serializes a container by writing its size then the elements.
 *
 * @param ttype  The type of container
 * @param prefix String prefix for fields
 */
void t_java_generator::generate_serialize_container(ostream& out,
                                                    t_type* ttype,
                                                    string prefix,
                                                    bool has_metadata) {
  scope_up(out);

  if (has_metadata) {
    if (ttype->is_map()) {
      indent(out) << "oprot.writeMapBegin(new org.apache.thrift.protocol.TMap("
                  << type_to_enum(((t_map*)ttype)->get_key_type()) << ", "
                  << type_to_enum(((t_map*)ttype)->get_val_type()) << ", " << prefix << ".size()));"
                  << '\n';
    } else if (ttype->is_set()) {
      indent(out) << "oprot.writeSetBegin(new org.apache.thrift.protocol.TSet("
                  << type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " << prefix
                  << ".size()));" << '\n';
    } else if (ttype->is_list()) {
      indent(out) << "oprot.writeListBegin(new org.apache.thrift.protocol.TList("
                  << type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " << prefix
                  << ".size()));" << '\n';
    }
  } else {
    indent(out) << "oprot.writeI32(" << prefix << ".size());" << '\n';
  }

  string iter = tmp("_iter");
  if (ttype->is_map()) {
    indent(out) << "for (java.util.Map.Entry<"
                << type_name(((t_map*)ttype)->get_key_type(), true, false) << ", "
                << type_name(((t_map*)ttype)->get_val_type(), true, false) << "> " << iter << " : "
                << prefix << ".entrySet())";
  } else if (ttype->is_set()) {
    indent(out) << "for (" << type_name(((t_set*)ttype)->get_elem_type()) << " " << iter << " : "
                << prefix << ")";
  } else if (ttype->is_list()) {
    indent(out) << "for (" << type_name(((t_list*)ttype)->get_elem_type()) << " " << iter << " : "
                << prefix << ")";
  }

  out << '\n';
  scope_up(out);
  if (ttype->is_map()) {
    generate_serialize_map_element(out, (t_map*)ttype, iter, prefix, has_metadata);
  } else if (ttype->is_set()) {
    generate_serialize_set_element(out, (t_set*)ttype, iter, has_metadata);
  } else if (ttype->is_list()) {
    generate_serialize_list_element(out, (t_list*)ttype, iter, has_metadata);
  }
  scope_down(out);

  if (has_metadata) {
    if (ttype->is_map()) {
      indent(out) << "oprot.writeMapEnd();" << '\n';
    } else if (ttype->is_set()) {
      indent(out) << "oprot.writeSetEnd();" << '\n';
    } else if (ttype->is_list()) {
      indent(out) << "oprot.writeListEnd();" << '\n';
    }
  }

  scope_down(out);
}

/**
 * Serializes the members of a map.
 */
void t_java_generator::generate_serialize_map_element(ostream& out,
                                                      t_map* tmap,
                                                      string iter,
                                                      string map,
                                                      bool has_metadata) {
  (void)map;
  t_field kfield(tmap->get_key_type(), iter);
  generate_serialize_field(out, &kfield, "", ".getKey()", has_metadata);
  t_field vfield(tmap->get_val_type(), iter);
  generate_serialize_field(out, &vfield, "", ".getValue()", has_metadata);
}

/**
 * Serializes the members of a set.
 */
void t_java_generator::generate_serialize_set_element(ostream& out,
                                                      t_set* tset,
                                                      string iter,
                                                      bool has_metadata) {
  t_field efield(tset->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "", "", has_metadata);
}

/**
 * Serializes the members of a list.
 */
void t_java_generator::generate_serialize_list_element(ostream& out,
                                                       t_list* tlist,
                                                       string iter,
                                                       bool has_metadata) {
  t_field efield(tlist->get_elem_type(), iter);
  generate_serialize_field(out, &efield, "", "", has_metadata);
}

/**
 * Returns a Java type name
 *
 * @param ttype The type
 * @param container Is the type going inside a container?
 * @return Java type name, i.e. java.util.HashMap<Key,Value>
 */
string t_java_generator::type_name(t_type* ttype,
                                   bool in_container,
                                   bool in_init,
                                   bool skip_generic,
                                   bool force_namespace) {
  // In Java typedefs are just resolved to their real type
  ttype = get_true_type(ttype);
  string prefix;

  if (ttype->is_base_type()) {
    return base_type_name((t_base_type*)ttype, in_container);
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    if (in_init) {
      if (is_enum_map(tmap)) {
        prefix = "java.util.EnumMap";
      } else if (sorted_containers_) {
        prefix = "java.util.TreeMap";
      } else {
        prefix = "java.util.HashMap";
      }
    } else {
      prefix = "java.util.Map";
    }
    return prefix
           + (skip_generic ? ""
                           : "<" + type_name(tmap->get_key_type(), true) + ","
                                 + type_name(tmap->get_val_type(), true) + ">");
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    if (in_init) {
      if (is_enum_set(tset)) {
        prefix = "java.util.EnumSet";
      } else if (sorted_containers_) {
        prefix = "java.util.TreeSet";
      } else {
        prefix = "java.util.HashSet";
      }
    } else {
      prefix = "java.util.Set";
    }
    return prefix + (skip_generic ? "" : "<" + type_name(tset->get_elem_type(), true) + ">");
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    if (in_init) {
      prefix = "java.util.ArrayList";
    } else {
      prefix = "java.util.List";
    }
    return prefix + (skip_generic ? "" : "<" + type_name(tlist->get_elem_type(), true) + ">");
  }

  // Check for namespacing
  t_program* program = ttype->get_program();
  if ((program != nullptr) && ((program != program_) || force_namespace)) {
    string package = program->get_namespace("java");
    if (!package.empty()) {
      return package + "." + make_valid_java_identifier(ttype->get_name());
    }
  }

  return make_valid_java_identifier(ttype->get_name());
}

/**
 * Returns the Java type that corresponds to the thrift type.
 *
 * @param tbase The base type
 * @param container Is it going in a Java container?
 */
string t_java_generator::base_type_name(t_base_type* type, bool in_container) {
  t_base_type::t_base tbase = type->get_base();

  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return (in_container ? "Void" : "void");
  case t_base_type::TYPE_STRING:
    if (type->is_binary()) {
      return "java.nio.ByteBuffer";
    } else {
      return "java.lang.String";
    }
  case t_base_type::TYPE_UUID:
    return "java.util.UUID";
  case t_base_type::TYPE_BOOL:
    return (in_container ? "java.lang.Boolean" : "boolean");
  case t_base_type::TYPE_I8:
    return (in_container ? "java.lang.Byte" : "byte");
  case t_base_type::TYPE_I16:
    return (in_container ? "java.lang.Short" : "short");
  case t_base_type::TYPE_I32:
    return (in_container ? "java.lang.Integer" : "int");
  case t_base_type::TYPE_I64:
    return (in_container ? "java.lang.Long" : "long");
  case t_base_type::TYPE_DOUBLE:
    return (in_container ? "java.lang.Double" : "double");
  default:
    throw "compiler error: no Java name for base type " + t_base_type::t_base_name(tbase);
  }
}

/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param tfield The field
 * @param init Whether to initialize the field
 */
string t_java_generator::declare_field(t_field* tfield, bool init, bool comment) {
  // TODO(mcslee): do we ever need to initialize the field?
  string result = "";
  t_type* ttype = get_true_type(tfield->get_type());
  if (type_can_be_null(ttype)) {
    result += java_nullable_annotation() + " ";
  }
  result += type_name(tfield->get_type()) + " " + make_valid_java_identifier(tfield->get_name());
  if (init) {
    if (ttype->is_base_type() && tfield->get_value() != nullptr) {
      std::ofstream dummy;
      result += " = " + render_const_value(dummy, ttype, tfield->get_value());
    } else if (ttype->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "NO T_VOID CONSTRUCT";
      case t_base_type::TYPE_STRING:
      case t_base_type::TYPE_UUID:
        result += " = null";
        break;
      case t_base_type::TYPE_BOOL:
        result += " = false";
        break;
      case t_base_type::TYPE_I8:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        result += " = 0";
        break;
      case t_base_type::TYPE_DOUBLE:
        result += " = (double)0";
        break;
      default:
        throw "compiler error: unhandled type";
      }
    } else if (ttype->is_enum()) {
      result += " = null";
    } else if (ttype->is_container()) {
      result += " = new " + type_name(ttype, false, true) + "()";
    } else {
      result += " = new " + type_name(ttype, false, true) + "()";
      ;
    }
  }
  result += ";";
  if (comment) {
    result += " // ";
    if (tfield->get_req() == t_field::T_OPTIONAL) {
      result += "optional";
    } else {
      result += "required";
    }
  }
  return result;
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_java_generator::function_signature(t_function* tfunction, string prefix) {
  t_type* ttype = tfunction->get_returntype();
  std::string fn_name = get_rpc_method_name(tfunction->get_name());
  std::string result = type_name(ttype) + " " + prefix + fn_name + "("
                       + argument_list(tfunction->get_arglist()) + ") throws ";
  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;
  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    result += type_name((*x_iter)->get_type(), false, false) + ", ";
  }
  result += "org.apache.thrift.TException";
  return result;
}

/**
 * Renders a function signature of the form 'void name(args, resultHandler)'
 *
 * @params tfunction Function definition
 * @return String of rendered function definition
 */
string t_java_generator::function_signature_async(t_function* tfunction,
                                                  bool use_base_method,
                                                  string prefix) {
  std::string arglist = async_function_call_arglist(tfunction, use_base_method, true);

  std::string ret_type = "";
  if (use_base_method) {
    ret_type += "AsyncClient.";
  }
  ret_type += tfunction->get_name() + "_call";

  std::string fn_name = get_rpc_method_name(tfunction->get_name());

  std::string result = prefix + "void " + fn_name + "(" + arglist + ")";
  return result;
}

/**
 * Renders a function signature of the form 'CompletableFuture<R> name(args)'
 *
 * @params tfunction Function definition
 * @return String of rendered function definition
 */
string t_java_generator::function_signature_future(t_function* tfunction, string prefix) {
  t_type* ttype = tfunction->get_returntype();
  std::string fn_name = get_rpc_method_name(tfunction->get_name());
  return "java.util.concurrent.CompletableFuture<" + type_name(ttype, /*in_container=*/true) + "> "
         + prefix + fn_name + "(" + argument_list(tfunction->get_arglist()) + ")";
}

string t_java_generator::async_function_call_arglist(t_function* tfunc,
                                                     bool use_base_method,
                                                     bool include_types) {
  (void)use_base_method;
  std::string arglist = "";
  if (tfunc->get_arglist()->get_members().size() > 0) {
    arglist = argument_list(tfunc->get_arglist(), include_types) + ", ";
  }

  if (include_types) {
    arglist += "org.apache.thrift.async.AsyncMethodCallback<";
    arglist += type_name(tfunc->get_returntype(), true) + "> ";
  }
  arglist += "resultHandler";

  return arglist;
}

/**
 * Renders a comma separated field list, with type names
 */
string t_java_generator::argument_list(t_struct* tstruct, bool include_types) {
  string result = "";

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += ", ";
    }
    if (include_types) {
      result += type_name((*f_iter)->get_type()) + " ";
    }
    result += make_valid_java_identifier((*f_iter)->get_name());
  }
  return result;
}

string t_java_generator::async_argument_list(t_function* tfunct,
                                             t_struct* tstruct,
                                             t_type* ttype,
                                             bool include_types) {
  (void)tfunct;
  (void)ttype;
  string result = "";
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += ", ";
    }
    if (include_types) {
      result += type_name((*f_iter)->get_type()) + " ";
    }
    result += make_valid_java_identifier((*f_iter)->get_name());
  }
  if (!first) {
    result += ", ";
  }
  if (include_types) {
    result += "org.apache.thrift.async.AsyncMethodCallback<";
    result += type_name(tfunct->get_returntype(), true) + "> ";
  }
  result += "resultHandler";
  return result;
}

/**
 * Converts the parse type to a Java enum string for the given type.
 */
string t_java_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "org.apache.thrift.protocol.TType.STRING";
    case t_base_type::TYPE_BOOL:
      return "org.apache.thrift.protocol.TType.BOOL";
    case t_base_type::TYPE_I8:
      return "org.apache.thrift.protocol.TType.BYTE";
    case t_base_type::TYPE_I16:
      return "org.apache.thrift.protocol.TType.I16";
    case t_base_type::TYPE_I32:
      return "org.apache.thrift.protocol.TType.I32";
    case t_base_type::TYPE_I64:
      return "org.apache.thrift.protocol.TType.I64";
    case t_base_type::TYPE_UUID:
      return "org.apache.thrift.protocol.TType.UUID";
    case t_base_type::TYPE_DOUBLE:
      return "org.apache.thrift.protocol.TType.DOUBLE";
    default:
      throw "compiler error: unhandled type";
    }
  } else if (type->is_enum()) {
    return "org.apache.thrift.protocol.TType.I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "org.apache.thrift.protocol.TType.STRUCT";
  } else if (type->is_map()) {
    return "org.apache.thrift.protocol.TType.MAP";
  } else if (type->is_set()) {
    return "org.apache.thrift.protocol.TType.SET";
  } else if (type->is_list()) {
    return "org.apache.thrift.protocol.TType.LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

/**
 * Takes a name and produes a valid Java source file name from it
 *
 * @param fromName The name which shall become a valid Java source file name
 * @return The produced identifier
 */
std::string t_java_generator::make_valid_java_filename(std::string const& fromName) {
  // if any further rules apply to source file names in Java, modify as necessary
  return make_valid_java_identifier(fromName);
}

/**
 * Takes a name and produes a valid Java identifier from it
 *
 * @param fromName The name which shall become a valid Java identifier
 * @return The produced identifier
 */
std::string t_java_generator::make_valid_java_identifier(std::string const& fromName) {
  std::string str = fromName;
  if (str.empty()) {
    return str;
  }

  // tests rely on this
  assert(('A' < 'Z') && ('a' < 'z') && ('0' < '9'));

  // if the first letter is a number, we add an additional underscore in front of it
  char c = str.at(0);
  if (('0' <= c) && (c <= '9')) {
    str = "_" + str;
  }

  // following chars: letter, number or underscore
  for (size_t i = 0; i < str.size(); ++i) {
    c = str.at(i);
    if ((('A' > c) || (c > 'Z')) && (('a' > c) || (c > 'z')) && (('0' > c) || (c > '9'))
        && ('_' != c)) {
      str.replace(i, 1, "_");
    }
  }

  return normalize_name(str);
}

std::string t_java_generator::as_camel_case(std::string name, bool ucfirst) {
  std::string new_name;
  size_t i = 0;
  for (i = 0; i < name.size(); i++) {
    if (name[i] != '_')
      break;
  }
  if (ucfirst) {
    new_name += toupper(name[i++]);
  } else {
    new_name += tolower(name[i++]);
  }
  for (; i < name.size(); i++) {
    if (name[i] == '_') {
      if (i < name.size() - 1) {
        i++;
        new_name += toupper(name[i]);
      }
    } else {
      new_name += name[i];
    }
  }
  return new_name;
}

std::string t_java_generator::get_rpc_method_name(std::string name) {
  if (fullcamel_style_) {
    return make_valid_java_identifier(as_camel_case(name, false));
  } else {
    return make_valid_java_identifier(name);
  }
}

/**
 * Applies the correct style to a string based on the value of nocamel_style_
 * and/or fullcamel_style_
 */
std::string t_java_generator::get_cap_name(std::string name) {
  if (nocamel_style_) {
    return "_" + name;
  } else if (fullcamel_style_) {
    return as_camel_case(name);
  } else {
    name[0] = toupper(name[0]);
    return name;
  }
}

string t_java_generator::constant_name(string name) {
  string constant_name;

  bool is_first = true;
  bool was_previous_char_upper = false;
  for (char character : name) {
    bool is_upper = isupper(character);

    if (is_upper && !is_first && !was_previous_char_upper) {
      constant_name += '_';
    }
    constant_name += toupper(character);

    is_first = false;
    was_previous_char_upper = is_upper;
  }

  return constant_name;
}

void t_java_generator::generate_deep_copy_container(ostream& out,
                                                    std::string source_name_p1,
                                                    std::string source_name_p2,
                                                    std::string result_name,
                                                    t_type* type) {

  t_container* container = (t_container*)type;
  std::string source_name;
  if (source_name_p2 == "")
    source_name = source_name_p1;
  else
    source_name = source_name_p1 + "." + source_name_p2;

  bool copy_construct_container;
  if (container->is_map()) {
    t_map* tmap = (t_map*)container;
    copy_construct_container
        = tmap->get_key_type()->is_base_type() && tmap->get_val_type()->is_base_type();
  } else {
    t_type* elem_type = container->is_list() ? ((t_list*)container)->get_elem_type()
                                             : ((t_set*)container)->get_elem_type();
    copy_construct_container = elem_type->is_base_type();
  }

  if (copy_construct_container) {
    // deep copy of base types can be done much more efficiently than iterating over all the
    // elements manually
    indent(out) << type_name(type, true, false) << " " << result_name << " = new "
                << type_name(container, false, true) << "(" << source_name << ");" << '\n';
    return;
  }

  std::string constructor_args;
  if (is_enum_set(container) || is_enum_map(container)) {
    constructor_args = inner_enum_type_name(container);
  } else if (!(sorted_containers_ && (container->is_map() || container->is_set()))) {
    // unsorted containers accept a capacity value
    constructor_args = source_name + ".size()";
  }

  if (is_enum_set(container)) {
    indent(out) << type_name(type, true, false) << " " << result_name << " = "
                << type_name(container, false, true, true) << ".noneOf(" << constructor_args << ");"
                << '\n';
  } else {
    indent(out) << type_name(type, true, false) << " " << result_name << " = new "
                << type_name(container, false, true) << "(" << constructor_args << ");" << '\n';
  }

  std::string iterator_element_name = source_name_p1 + "_element";
  std::string result_element_name = result_name + "_copy";

  if (container->is_map()) {
    t_type* key_type = ((t_map*)container)->get_key_type();
    t_type* val_type = ((t_map*)container)->get_val_type();

    indent(out) << "for (java.util.Map.Entry<" << type_name(key_type, true, false) << ", "
                << type_name(val_type, true, false) << "> " << iterator_element_name << " : "
                << source_name << ".entrySet()) {" << '\n';
    indent_up();

    out << '\n';

    indent(out) << type_name(key_type, true, false) << " " << iterator_element_name
                << "_key = " << iterator_element_name << ".getKey();" << '\n';
    indent(out) << type_name(val_type, true, false) << " " << iterator_element_name
                << "_value = " << iterator_element_name << ".getValue();" << '\n';

    out << '\n';

    if (key_type->is_container()) {
      generate_deep_copy_container(out, iterator_element_name + "_key", "",
                                   result_element_name + "_key", key_type);
    } else {
      indent(out) << type_name(key_type, true, false) << " " << result_element_name << "_key = ";
      generate_deep_copy_non_container(out, iterator_element_name + "_key",
                                       result_element_name + "_key", key_type);
      out << ";" << '\n';
    }

    out << '\n';

    if (val_type->is_container()) {
      generate_deep_copy_container(out, iterator_element_name + "_value", "",
                                   result_element_name + "_value", val_type);
    } else {
      indent(out) << type_name(val_type, true, false) << " " << result_element_name << "_value = ";
      generate_deep_copy_non_container(out, iterator_element_name + "_value",
                                       result_element_name + "_value", val_type);
      out << ";" << '\n';
    }

    out << '\n';

    indent(out) << result_name << ".put(" << result_element_name << "_key, " << result_element_name
                << "_value);" << '\n';

    indent_down();
    indent(out) << "}" << '\n';

  } else {
    t_type* elem_type;

    if (container->is_set()) {
      elem_type = ((t_set*)container)->get_elem_type();
    } else {
      elem_type = ((t_list*)container)->get_elem_type();
    }

    indent(out) << "for (" << type_name(elem_type, true, false) << " " << iterator_element_name
                << " : " << source_name << ") {" << '\n';

    indent_up();

    if (elem_type->is_container()) {
      // recursive deep copy
      generate_deep_copy_container(out, iterator_element_name, "", result_element_name, elem_type);
      indent(out) << result_name << ".add(" << result_element_name << ");" << '\n';
    } else {
      // iterative copy
      if (elem_type->is_binary()) {
        indent(out) << "java.nio.ByteBuffer temp_binary_element = ";
        generate_deep_copy_non_container(out, iterator_element_name, "temp_binary_element",
                                         elem_type);
        out << ";" << '\n';
        indent(out) << result_name << ".add(temp_binary_element);" << '\n';
      } else {
        indent(out) << result_name << ".add(";
        generate_deep_copy_non_container(out, iterator_element_name, result_name, elem_type);
        out << ");" << '\n';
      }
    }

    indent_down();

    indent(out) << "}" << '\n';
  }
}

void t_java_generator::generate_deep_copy_non_container(ostream& out,
                                                        std::string source_name,
                                                        std::string dest_name,
                                                        t_type* type) {
  (void)dest_name;
  type = get_true_type(type);
  if (type->is_base_type() || type->is_enum() || type->is_typedef()) {
    if (type->is_binary()) {
      out << "org.apache.thrift.TBaseHelper.copyBinary(" << source_name << ")";
    } else {
      // everything else can be copied directly
      out << source_name;
    }
  } else {
    out << "new " << type_name(type, true, true) << "(" << source_name << ")";
  }
}

std::string t_java_generator::generate_isset_check(t_field* field) {
  return generate_isset_check(field->get_name());
}

std::string t_java_generator::isset_field_id(t_field* field) {
  return "__" + upcase_string(field->get_name() + "_isset_id");
}

std::string t_java_generator::generate_isset_check(std::string field_name) {
  return "is" + get_cap_name("set") + get_cap_name(field_name) + "()";
}

void t_java_generator::generate_isset_set(ostream& out, t_field* field, string prefix) {
  if (!type_can_be_null(field->get_type())) {
    indent(out) << prefix << "set" << get_cap_name(field->get_name()) << get_cap_name("isSet")
                << "(true);" << '\n';
  }
}

void t_java_generator::generate_struct_desc(ostream& out, t_struct* tstruct) {
  indent(out) << "private static final org.apache.thrift.protocol.TStruct STRUCT_DESC = new "
                 "org.apache.thrift.protocol.TStruct(\""
              << tstruct->get_name() << "\");" << '\n';
}

void t_java_generator::generate_field_descs(ostream& out, t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) << "private static final org.apache.thrift.protocol.TField "
                << constant_name((*m_iter)->get_name())
                << "_FIELD_DESC = new org.apache.thrift.protocol.TField(\"" << (*m_iter)->get_name()
                << "\", " << type_to_enum((*m_iter)->get_type()) << ", "
                << "(short)" << (*m_iter)->get_key() << ");" << '\n';
  }
}

void t_java_generator::generate_scheme_map(ostream& out, t_struct* tstruct) {
  indent(out) << "private static final org.apache.thrift.scheme.SchemeFactory "
                 "STANDARD_SCHEME_FACTORY = new "
              << tstruct->get_name() << "StandardSchemeFactory();" << '\n';
  indent(out)
      << "private static final org.apache.thrift.scheme.SchemeFactory TUPLE_SCHEME_FACTORY = new "
      << tstruct->get_name() << "TupleSchemeFactory();" << '\n';
}

void t_java_generator::generate_field_name_constants(ostream& out, t_struct* tstruct) {
  indent(out) << "/** The set of fields this struct contains, along with convenience methods for "
                 "finding and manipulating them. */"
              << '\n';
  indent(out) << "public enum _Fields implements org.apache.thrift.TFieldIdEnum {" << '\n';

  indent_up();
  bool first = true;
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (!first) {
      out << "," << '\n';
    }
    first = false;
    generate_java_doc(out, *m_iter);
    indent(out) << constant_name((*m_iter)->get_name()) << "((short)" << (*m_iter)->get_key()
                << ", \"" << (*m_iter)->get_name() << "\")";
  }

  out << ";" << '\n' << '\n';

  indent(out) << "private static final java.util.Map<java.lang.String, _Fields> byName = new "
                 "java.util.HashMap<java.lang.String, _Fields>();"
              << '\n';
  out << '\n';

  indent(out) << "static {" << '\n';
  indent(out) << "  for (_Fields field : java.util.EnumSet.allOf(_Fields.class)) {" << '\n';
  indent(out) << "    byName.put(field.getFieldName(), field);" << '\n';
  indent(out) << "  }" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "/**" << '\n';
  indent(out) << " * Find the _Fields constant that matches fieldId, or null if its not found."
              << '\n';
  indent(out) << " */" << '\n';
  indent(out) << java_nullable_annotation() << '\n';
  indent(out) << "public static _Fields findByThriftId(int fieldId) {" << '\n';
  indent_up();
  indent(out) << "switch(fieldId) {" << '\n';
  indent_up();

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) << "case " << (*m_iter)->get_key() << ": // "
                << constant_name((*m_iter)->get_name()) << '\n';
    indent(out) << "  return " << constant_name((*m_iter)->get_name()) << ";" << '\n';
  }

  indent(out) << "default:" << '\n';
  indent(out) << "  return null;" << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  indent_down();
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "/**" << '\n';
  indent(out) << " * Find the _Fields constant that matches fieldId, throwing an exception" << '\n';
  indent(out) << " * if it is not found." << '\n';
  indent(out) << " */" << '\n';
  indent(out) << "public static _Fields findByThriftIdOrThrow(int fieldId) {" << '\n';
  indent(out) << "  _Fields fields = findByThriftId(fieldId);" << '\n';
  indent(out) << "  if (fields == null) throw new java.lang.IllegalArgumentException(\"Field \" + "
                 "fieldId + "
                 "\" doesn't exist!\");"
              << '\n';
  indent(out) << "  return fields;" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "/**" << '\n';
  indent(out) << " * Find the _Fields constant that matches name, or null if its not found."
              << '\n';
  indent(out) << " */" << '\n';
  indent(out) << java_nullable_annotation() << '\n';
  indent(out) << "public static _Fields findByName(java.lang.String name) {" << '\n';
  indent(out) << "  return byName.get(name);" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << "private final short _thriftId;" << '\n';
  indent(out) << "private final java.lang.String _fieldName;" << '\n' << '\n';

  indent(out) << "_Fields(short thriftId, java.lang.String fieldName) {" << '\n';
  indent(out) << "  _thriftId = thriftId;" << '\n';
  indent(out) << "  _fieldName = fieldName;" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public short getThriftFieldId() {" << '\n';
  indent(out) << "  return _thriftId;" << '\n';
  indent(out) << "}" << '\n' << '\n';

  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public java.lang.String getFieldName() {" << '\n';
  indent(out) << "  return _fieldName;" << '\n';
  indent(out) << "}" << '\n';

  indent_down();

  indent(out) << "}" << '\n';
}

t_java_generator::isset_type t_java_generator::needs_isset(t_struct* tstruct,
                                                           std::string* outPrimitiveType) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  int count = 0;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (!type_can_be_null(get_true_type((*m_iter)->get_type()))) {
      count++;
    }
  }
  if (count == 0) {
    return ISSET_NONE;
  } else if (count <= 64) {
    if (outPrimitiveType != nullptr) {
      if (count <= 8)
        *outPrimitiveType = "byte";
      else if (count <= 16)
        *outPrimitiveType = "short";
      else if (count <= 32)
        *outPrimitiveType = "int";
      else if (count <= 64)
        *outPrimitiveType = "long";
    }
    return ISSET_PRIMITIVE;
  } else {
    return ISSET_BITSET;
  }
}

void t_java_generator::generate_java_struct_clear(std::ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void clear() {" << '\n';

  indent_up();
  for (auto field : tstruct->get_members()) {
    t_type* t = get_true_type(field->get_type());

    if (field->get_value() != nullptr) {
      print_const_value(out, "this." + make_valid_java_identifier(field->get_name()), t, field->get_value(), true, true);
      continue;
    }

    if (type_can_be_null(t)) {
      if (reuse_objects_ && (t->is_container() || t->is_struct())) {
        indent(out) << "if (this." << make_valid_java_identifier(field->get_name()) << " != null) {" << '\n';
        indent_up();
        indent(out) << "this." << make_valid_java_identifier(field->get_name()) << ".clear();" << '\n';
        indent_down();
        indent(out) << "}" << '\n';
      } else {
        indent(out) << "this." << make_valid_java_identifier(field->get_name()) << " = null;" << '\n';
      }
      continue;
    }

    // must be a base type
    // means it also needs to be explicitly unset
    indent(out) << "set" << get_cap_name(field->get_name()) << get_cap_name("isSet") << "(false);"
                << '\n';
    t_base_type* base_type = (t_base_type*)t;

    switch (base_type->get_base()) {
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      indent(out) << "this." << make_valid_java_identifier(field->get_name()) << " = 0;" << '\n';
      break;
    case t_base_type::TYPE_DOUBLE:
      indent(out) << "this." << make_valid_java_identifier(field->get_name()) << " = 0.0;" << '\n';
      break;
    case t_base_type::TYPE_BOOL:
      indent(out) << "this." << make_valid_java_identifier(field->get_name()) << " = false;" << '\n';
      break;
    default:
      throw "unsupported type: " + base_type->get_name() + " for field " + field->get_name();
    }
  }
  indent_down();

  indent(out) << "}" << '\n' << '\n';
}

// generates java method to serialize (in the Java sense) the object
void t_java_generator::generate_java_struct_write_object(ostream& out, t_struct* tstruct) {
  (void)tstruct;
  indent(out)
      << "private void writeObject(java.io.ObjectOutputStream out) throws java.io.IOException {"
      << '\n';
  indent(out) << "  try {" << '\n';
  indent(out) << "    write(new org.apache.thrift.protocol.TCompactProtocol(new "
                 "org.apache.thrift.transport.TIOStreamTransport(out)));"
              << '\n';
  indent(out) << "  } catch (org.apache.thrift.TException te) {" << '\n';
  indent(out) << "    throw new java.io.IOException(te" << (android_legacy_ ? ".getMessage()" : "")
              << ");" << '\n';
  indent(out) << "  }" << '\n';
  indent(out) << "}" << '\n' << '\n';
}

// generates java method to serialize (in the Java sense) the object
void t_java_generator::generate_java_struct_read_object(ostream& out, t_struct* tstruct) {
  indent(out) << "private void readObject(java.io.ObjectInputStream in) throws "
                 "java.io.IOException, java.lang.ClassNotFoundException {"
              << '\n';
  indent(out) << "  try {" << '\n';
  if (!tstruct->is_union()) {
    switch (needs_isset(tstruct)) {
    case ISSET_NONE:
      break;
    case ISSET_PRIMITIVE:
      indent(out) << "    // it doesn't seem like you should have to do this, but java "
                     "serialization is wacky, and doesn't call the default constructor."
                  << '\n';
      indent(out) << "    __isset_bitfield = 0;" << '\n';
      break;
    case ISSET_BITSET:
      indent(out) << "    // it doesn't seem like you should have to do this, but java "
                     "serialization is wacky, and doesn't call the default constructor."
                  << '\n';
      indent(out) << "    __isset_bit_vector = new java.util.BitSet(1);" << '\n';
      break;
    }
  }
  indent(out) << "    read(new org.apache.thrift.protocol.TCompactProtocol(new "
                 "org.apache.thrift.transport.TIOStreamTransport(in)));"
              << '\n';
  indent(out) << "  } catch (org.apache.thrift.TException te) {" << '\n';
  indent(out) << "    throw new java.io.IOException(te" << (android_legacy_ ? ".getMessage()" : "")
              << ");" << '\n';
  indent(out) << "  }" << '\n';
  indent(out) << "}" << '\n' << '\n';
}

void t_java_generator::generate_standard_reader(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void read(org.apache.thrift.protocol.TProtocol iprot, "
              << make_valid_java_identifier(tstruct->get_name()) << " struct) throws org.apache.thrift.TException {" << '\n';
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Declare stack tmp variables and read struct header
  out << indent() << "org.apache.thrift.protocol.TField schemeField;" << '\n'
      << indent() << "iprot.readStructBegin();" << '\n';

  // Loop over reading in fields
  indent(out) << "while (true)" << '\n';
  scope_up(out);

  // Read beginning field marker
  indent(out) << "schemeField = iprot.readFieldBegin();" << '\n';

  // Check for field STOP marker and break
  indent(out) << "if (schemeField.type == org.apache.thrift.protocol.TType.STOP) { " << '\n';
  indent_up();
  indent(out) << "break;" << '\n';
  indent_down();
  indent(out) << "}" << '\n';

  // Switch statement on the field we are reading
  indent(out) << "switch (schemeField.id) {" << '\n';

  indent_up();

  // Generate deserialization code for known cases
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent(out) << "case " << (*f_iter)->get_key() << ": // "
                << constant_name((*f_iter)->get_name()) << '\n';
    indent_up();
    indent(out) << "if (schemeField.type == " << type_to_enum((*f_iter)->get_type()) << ") {"
                << '\n';
    indent_up();

    generate_deserialize_field(out, *f_iter, "struct.", true);
    indent(out) << "struct."
                << "set" << get_cap_name((*f_iter)->get_name()) << get_cap_name("isSet")
                << "(true);" << '\n';
    indent_down();
    out << indent() << "} else { " << '\n'
        << indent() << "  org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);"
        << '\n'
        << indent() << "}" << '\n'
        << indent() << "break;" << '\n';
    indent_down();
  }

  indent(out) << "default:" << '\n';
  indent(out) << "  org.apache.thrift.protocol.TProtocolUtil.skip(iprot, schemeField.type);"
              << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  // Read field end marker
  indent(out) << "iprot.readFieldEnd();" << '\n';

  indent_down();
  indent(out) << "}" << '\n';

  out << indent() << "iprot.readStructEnd();" << '\n';

  // in non-beans style, check for required fields of primitive type
  // (which can be checked here but not in the general validate method)
  if (!bean_style_) {
    out << '\n'
        << indent()
        << "// check for required fields of primitive type, which can't be "
           "checked in the validate method"
        << '\n';
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      if ((*f_iter)->get_req() == t_field::T_REQUIRED && !type_can_be_null((*f_iter)->get_type())) {
        out << indent() << "if (!struct." << generate_isset_check(*f_iter) << ") {" << '\n'
            << indent()
            << "  throw new org.apache.thrift.protocol.TProtocolException(\"Required field '"
            << (*f_iter)->get_name()
            << "' was not found in serialized data! Struct: \" + toString());" << '\n'
            << indent() << "}" << '\n';
      }
    }
  }

  // performs various checks (e.g. check that all required fields are set)
  indent(out) << "struct.validate();" << '\n';

  indent_down();
  out << indent() << "}" << '\n';
}

void t_java_generator::generate_standard_writer(ostream& out, t_struct* tstruct, bool is_result) {
  indent_up();
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void write(org.apache.thrift.protocol.TProtocol oprot, "
              << make_valid_java_identifier(tstruct->get_name()) << " struct) throws org.apache.thrift.TException {" << '\n';
  indent_up();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  // performs various checks (e.g. check that all required fields are set)
  indent(out) << "struct.validate();" << '\n' << '\n';

  indent(out) << "oprot.writeStructBegin(STRUCT_DESC);" << '\n';

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    if (null_allowed) {
      out << indent() << "if (struct." << make_valid_java_identifier((*f_iter)->get_name()) << " != null) {" << '\n';
      indent_up();
    }
    bool optional = ((*f_iter)->get_req() == t_field::T_OPTIONAL) || (is_result && !null_allowed);
    if (optional) {
      indent(out) << "if ("
                  << "struct." << generate_isset_check((*f_iter)) << ") {" << '\n';
      indent_up();
    }

    indent(out) << "oprot.writeFieldBegin(" << constant_name((*f_iter)->get_name())
                << "_FIELD_DESC);" << '\n';

    // Write field contents
    generate_serialize_field(out, *f_iter, "struct.", "", true);

    // Write field closer
    indent(out) << "oprot.writeFieldEnd();" << '\n';

    if (optional) {
      indent_down();
      indent(out) << "}" << '\n';
    }
    if (null_allowed) {
      indent_down();
      indent(out) << "}" << '\n';
    }
  }
  // Write the struct map
  out << indent() << "oprot.writeFieldStop();" << '\n'
      << indent() << "oprot.writeStructEnd();" << '\n';

  indent_down();
  out << indent() << "}" << '\n' << '\n';
  indent_down();
}

void t_java_generator::generate_java_struct_standard_scheme(ostream& out,
                                                            t_struct* tstruct,
                                                            bool is_result) {
  indent(out) << "private static class " << tstruct->get_name()
              << "StandardSchemeFactory implements org.apache.thrift.scheme.SchemeFactory {"
              << '\n';
  indent_up();
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public " << tstruct->get_name() << "StandardScheme getScheme() {" << '\n';
  indent_up();
  indent(out) << "return new " << tstruct->get_name() << "StandardScheme();" << '\n';
  indent_down();
  indent(out) << "}" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';

  out << indent() << "private static class " << tstruct->get_name()
      << "StandardScheme extends org.apache.thrift.scheme.StandardScheme<" << make_valid_java_identifier(tstruct->get_name())
      << "> {" << '\n'
      << '\n';
  indent_up();
  generate_standard_reader(out, tstruct);
  indent_down();
  out << '\n';
  generate_standard_writer(out, tstruct, is_result);

  out << indent() << "}" << '\n' << '\n';
}

void t_java_generator::generate_java_struct_tuple_reader(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void read(org.apache.thrift.protocol.TProtocol prot, "
              << make_valid_java_identifier(tstruct->get_name()) << " struct) throws org.apache.thrift.TException {" << '\n';
  indent_up();
  indent(out) << "org.apache.thrift.protocol.TTupleProtocol iprot = "
                 "(org.apache.thrift.protocol.TTupleProtocol) prot;"
              << '\n';
  int optional_count = 0;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_OPTIONAL
        || (*f_iter)->get_req() == t_field::T_OPT_IN_REQ_OUT) {
      optional_count++;
    }
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      generate_deserialize_field(out, (*f_iter), "struct.", false);
      indent(out) << "struct.set" << get_cap_name((*f_iter)->get_name()) << get_cap_name("isSet")
                  << "(true);" << '\n';
    }
  }
  if (optional_count > 0) {
    indent(out) << "java.util.BitSet incoming = iprot.readBitSet(" << optional_count << ");"
                << '\n';
    int i = 0;
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      if ((*f_iter)->get_req() == t_field::T_OPTIONAL
          || (*f_iter)->get_req() == t_field::T_OPT_IN_REQ_OUT) {
        indent(out) << "if (incoming.get(" << i << ")) {" << '\n';
        indent_up();
        generate_deserialize_field(out, (*f_iter), "struct.", false);
        indent(out) << "struct.set" << get_cap_name((*f_iter)->get_name()) << get_cap_name("isSet")
                    << "(true);" << '\n';
        indent_down();
        indent(out) << "}" << '\n';
        i++;
      }
    }
  }
  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_java_struct_tuple_writer(ostream& out, t_struct* tstruct) {
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public void write(org.apache.thrift.protocol.TProtocol prot, "
              << make_valid_java_identifier(tstruct->get_name()) << " struct) throws org.apache.thrift.TException {" << '\n';
  indent_up();
  indent(out) << "org.apache.thrift.protocol.TTupleProtocol oprot = "
                 "(org.apache.thrift.protocol.TTupleProtocol) prot;"
              << '\n';

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool has_optional = false;
  int optional_count = 0;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_OPTIONAL
        || (*f_iter)->get_req() == t_field::T_OPT_IN_REQ_OUT) {
      optional_count++;
      has_optional = true;
    }
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      generate_serialize_field(out, (*f_iter), "struct.", "", false);
    }
  }
  if (has_optional) {
    indent(out) << "java.util.BitSet optionals = new java.util.BitSet();" << '\n';
    int i = 0;
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      if ((*f_iter)->get_req() == t_field::T_OPTIONAL
          || (*f_iter)->get_req() == t_field::T_OPT_IN_REQ_OUT) {
        indent(out) << "if (struct." << generate_isset_check((*f_iter)) << ") {" << '\n';
        indent_up();
        indent(out) << "optionals.set(" << i << ");" << '\n';
        indent_down();
        indent(out) << "}" << '\n';
        i++;
      }
    }

    indent(out) << "oprot.writeBitSet(optionals, " << optional_count << ");" << '\n';
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      if ((*f_iter)->get_req() == t_field::T_OPTIONAL
          || (*f_iter)->get_req() == t_field::T_OPT_IN_REQ_OUT) {
        indent(out) << "if (struct." << generate_isset_check(*f_iter) << ") {" << '\n';
        indent_up();
        generate_serialize_field(out, (*f_iter), "struct.", "", false);
        indent_down();
        indent(out) << "}" << '\n';
      }
    }
  }
  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_java_struct_tuple_scheme(ostream& out, t_struct* tstruct) {
  indent(out) << "private static class " << tstruct->get_name()
              << "TupleSchemeFactory implements org.apache.thrift.scheme.SchemeFactory {" << '\n';
  indent_up();
  indent(out) << java_override_annotation() << '\n';
  indent(out) << "public " << tstruct->get_name() << "TupleScheme getScheme() {" << '\n';
  indent_up();
  indent(out) << "return new " << tstruct->get_name() << "TupleScheme();" << '\n';
  indent_down();
  indent(out) << "}" << '\n';
  indent_down();
  indent(out) << "}" << '\n' << '\n';
  out << indent() << "private static class " << tstruct->get_name()
      << "TupleScheme extends org.apache.thrift.scheme.TupleScheme<" << make_valid_java_identifier(tstruct->get_name()) << "> {"
      << '\n'
      << '\n';
  indent_up();
  generate_java_struct_tuple_writer(out, tstruct);
  out << '\n';
  generate_java_struct_tuple_reader(out, tstruct);
  indent_down();
  out << indent() << "}" << '\n' << '\n';
}

void t_java_generator::generate_java_scheme_lookup(ostream& out) {
  indent(out) << "private static <S extends org.apache.thrift.scheme.IScheme> S scheme("
              << "org.apache.thrift.protocol.TProtocol proto) {" << '\n';
  indent_up();
  indent(out) << "return (org.apache.thrift.scheme.StandardScheme.class.equals(proto.getScheme()) "
              << "? STANDARD_SCHEME_FACTORY "
              << ": TUPLE_SCHEME_FACTORY"
              << ").getScheme();" << '\n';
  indent_down();
  indent(out) << "}" << '\n';
}

void t_java_generator::generate_javax_generated_annotation(ostream& out) {
  time_t seconds = time(nullptr);
  struct tm* now = localtime(&seconds);
  if (jakarta_annotations_) {
    indent(out) << "@jakarta.annotation.Generated(value = \"" << autogen_summary() << "\"";
  } else {
    indent(out) << "@javax.annotation.Generated(value = \"" << autogen_summary() << "\"";
  }

  if (undated_generated_annotations_) {
    out << ")" << '\n';
  } else {
    indent(out) << ", date = \"" << (now->tm_year + 1900) << "-" << setfill('0') << setw(2)
                << (now->tm_mon + 1) << "-" << setfill('0') << setw(2) << now->tm_mday << "\")"
                << '\n';
  }
}

std::string t_java_generator::display_name() const {
  return "Java";
}


THRIFT_REGISTER_GENERATOR(
    java,
    "Java",
    "    beans:           Members will be private, and setter methods will return void.\n"
    "    private_members: Members will be private, but setter methods will return 'this' like usual.\n"
    "    private-members: Same as 'private_members' (deprecated).\n"
    "    nocamel:         Do not use CamelCase field accessors with beans.\n"
    "    fullcamel:       Convert underscored_accessor_or_service_names to camelCase.\n"
    "    android:         Generated structures are Parcelable.\n"
    "    android_legacy:  Do not use java.io.IOException(throwable) (available for Android 2.3 and above).\n"
    "    option_type=[thrift|jdk8]:\n"
    "                     thrift: wrap optional fields in thrift Option type.\n"
    "                     jdk8: Wrap optional fields in JDK8+ Option type.\n"
    "                     If the Option type is not specified, 'thrift' is used.\n"
    "    rethrow_unhandled_exceptions:\n"
    "                     Enable rethrow of unhandled exceptions and let them propagate further. (Default behavior is to catch and log it.)\n"
    "    java5:           Generate Java 1.5 compliant code (includes android_legacy flag).\n"
    "    future_iface:    Generate CompletableFuture based iface based on async client.\n"
    "    reuse_objects:   Data objects will not be allocated, but existing instances will be used (read and write).\n"
    "    reuse-objects:   Same as 'reuse_objects' (deprecated).\n"
    "    sorted_containers:\n"
    "                     Use TreeSet/TreeMap instead of HashSet/HashMap as a implementation of set/map.\n"
    "    generated_annotations=[undated|suppress]:\n"
    "                     undated: suppress the date at @Generated annotations\n"
    "                     suppress: suppress @Generated annotations entirely\n"
    "    unsafe_binaries: Do not copy ByteBuffers in constructors, getters, and setters.\n"
    "    jakarta_annotations: generate jakarta annotations (javax by default)\n"
    "    annotations_as_metadata:\n"
    "                     Include Thrift field annotations as metadata in the generated code.\n")
