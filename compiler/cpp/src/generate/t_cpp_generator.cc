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

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <sys/stat.h>

#include "platform.h"
#include "t_oop_generator.h"
using namespace std;


/**
 * C++ code generator. This is legitimacy incarnate.
 *
 */
class t_cpp_generator : public t_oop_generator {
 public:
  t_cpp_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_oop_generator(program)
  {
    (void) option_string;
    std::map<std::string, std::string>::const_iterator iter;

    iter = parsed_options.find("pure_enums");
    gen_pure_enums_ = (iter != parsed_options.end());

    iter = parsed_options.find("dense");
    gen_dense_ = (iter != parsed_options.end());

    iter = parsed_options.find("include_prefix");
    use_include_prefix_ = (iter != parsed_options.end());

    iter = parsed_options.find("cob_style");
    gen_cob_style_ = (iter != parsed_options.end());

    iter = parsed_options.find("no_client_completion");
    gen_no_client_completion_ = (iter != parsed_options.end());

    iter = parsed_options.find("templates");
    gen_templates_ = (iter != parsed_options.end());

    out_dir_base_ = "gen-cpp";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  void generate_consts(std::vector<t_const*> consts);

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum);
  void generate_struct(t_struct* tstruct) {
    generate_cpp_struct(tstruct, false);
  }
  void generate_xception(t_struct* txception) {
    generate_cpp_struct(txception, true);
  }
  void generate_cpp_struct(t_struct* tstruct, bool is_exception);

  void generate_service(t_service* tservice);

  void print_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value);
  std::string render_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value);

  void generate_struct_definition    (std::ofstream& out, t_struct* tstruct, bool is_exception=false, bool pointers=false, bool read=true, bool write=true);
  void generate_struct_fingerprint   (std::ofstream& out, t_struct* tstruct, bool is_definition);
  void generate_struct_reader        (std::ofstream& out, t_struct* tstruct, bool pointers=false);
  void generate_struct_writer        (std::ofstream& out, t_struct* tstruct, bool pointers=false);
  void generate_struct_result_writer (std::ofstream& out, t_struct* tstruct, bool pointers=false);

  /**
   * Service-level generation functions
   */

  void generate_service_interface (t_service* tservice, string style);
  void generate_service_null      (t_service* tservice, string style);
  void generate_service_multiface (t_service* tservice);
  void generate_service_helpers   (t_service* tservice);
  void generate_service_client    (t_service* tservice, string style);
  void generate_service_processor (t_service* tservice, string style);
  void generate_service_skeleton  (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction,
                                   string style, bool specialized=false);
  void generate_function_helpers  (t_service* tservice, t_function* tfunction);
  void generate_service_async_skeleton (t_service* tservice);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string prefix="",
                                          std::string suffix="");

  void generate_deserialize_struct       (std::ofstream& out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_deserialize_container    (std::ofstream& out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_deserialize_set_element  (std::ofstream& out,
                                          t_set*      tset,
                                          std::string prefix="");

  void generate_deserialize_map_element  (std::ofstream& out,
                                          t_map*      tmap,
                                          std::string prefix="");

  void generate_deserialize_list_element (std::ofstream& out,
                                          t_list*     tlist,
                                          std::string prefix,
                                          bool push_back,
                                          std::string index);

  void generate_serialize_field          (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string prefix="",
                                          std::string suffix="");

  void generate_serialize_struct         (std::ofstream& out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_serialize_container      (std::ofstream& out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (std::ofstream& out,
                                          t_map*      tmap,
                                          std::string iter);

  void generate_serialize_set_element    (std::ofstream& out,
                                          t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (std::ofstream& out,
                                          t_list*     tlist,
                                          std::string iter);

  void generate_function_call            (ostream& out,
                                          t_function* tfunction,
                                          string target,
                                          string iface,
                                          string arg_prefix);
  /*
   * Helper rendering functions
   */

  std::string namespace_prefix(std::string ns);
  std::string namespace_open(std::string ns);
  std::string namespace_close(std::string ns);
  std::string type_name(t_type* ttype, bool in_typedef=false, bool arg=false);
  std::string base_type_name(t_base_type::t_base tbase);
  std::string declare_field(t_field* tfield, bool init=false, bool pointer=false, bool constant=false, bool reference=false);
  std::string function_signature(t_function* tfunction, std::string style, std::string prefix="", bool name_params=true);
  std::string cob_function_signature(t_function* tfunction, std::string prefix="", bool name_params=true);
  std::string argument_list(t_struct* tstruct, bool name_params=true, bool start_comma=false);
  std::string type_to_enum(t_type* ttype);
  std::string local_reflection_name(const char*, t_type* ttype, bool external=false);

  void generate_enum_constant_list(std::ofstream& f,
                                   const vector<t_enum_value*>& constants,
                                   const char* prefix,
                                   const char* suffix,
                                   bool include_values);

  // These handles checking gen_dense_ and checking for duplicates.
  void generate_local_reflection(std::ofstream& out, t_type* ttype, bool is_definition);
  void generate_local_reflection_pointer(std::ofstream& out, t_type* ttype);

  bool is_complex_type(t_type* ttype) {
    ttype = get_true_type(ttype);

    return
      ttype->is_container() ||
      ttype->is_struct() ||
      ttype->is_xception() ||
      (ttype->is_base_type() && (((t_base_type*)ttype)->get_base() == t_base_type::TYPE_STRING));
  }

  void set_use_include_prefix(bool use_include_prefix) {
    use_include_prefix_ = use_include_prefix;
  }

 private:
  /**
   * Returns the include prefix to use for a file generated by program, or the
   * empty string if no include prefix should be used.
   */
  std::string get_include_prefix(const t_program& program) const;

  /**
   * True if we should generate pure enums for Thrift enums, instead of wrapper classes.
   */
  bool gen_pure_enums_;

  /**
   * True if we should generate local reflection metadata for TDenseProtocol.
   */
  bool gen_dense_;

  /**
   * True if we should generate templatized reader/writer methods.
   */
  bool gen_templates_;

  /**
   * True if we should use a path prefix in our #include statements for other
   * thrift-generated header files.
   */
  bool use_include_prefix_;

  /**
   * True if we should generate "Continuation OBject"-style classes as well.
   */
  bool gen_cob_style_;

  /**
   * True if we should omit calls to completion__() in CobClient class.
   */
  bool gen_no_client_completion_;

  /**
   * Strings for namespace, computed once up front then used directly
   */

  std::string ns_open_;
  std::string ns_close_;

  /**
   * File streams, stored here to avoid passing them as parameters to every
   * function.
   */

  std::ofstream f_types_;
  std::ofstream f_types_impl_;
  std::ofstream f_types_tcc_;
  std::ofstream f_header_;
  std::ofstream f_service_;
  std::ofstream f_service_tcc_;

  /**
   * When generating local reflections, make sure we don't generate duplicates.
   */
  std::set<std::string> reflected_fingerprints_;
};

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 */
void t_cpp_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());

  // Make output file
  string f_types_name = get_out_dir()+program_name_+"_types.h";
  f_types_.open(f_types_name.c_str());

  string f_types_impl_name = get_out_dir()+program_name_+"_types.cpp";
  f_types_impl_.open(f_types_impl_name.c_str());

  if (gen_templates_) {
    // If we don't open the stream, it appears to just discard data,
    // which is fine.
    string f_types_tcc_name = get_out_dir()+program_name_+"_types.tcc";
    f_types_tcc_.open(f_types_tcc_name.c_str());
  }

  // Print header
  f_types_ <<
    autogen_comment();
  f_types_impl_ <<
    autogen_comment();
  f_types_tcc_ <<
    autogen_comment();

  // Start ifndef
  f_types_ <<
    "#ifndef " << program_name_ << "_TYPES_H" << endl <<
    "#define " << program_name_ << "_TYPES_H" << endl <<
    endl;
  f_types_tcc_ <<
    "#ifndef " << program_name_ << "_TYPES_TCC" << endl <<
    "#define " << program_name_ << "_TYPES_TCC" << endl <<
    endl;

  // Include base types
  f_types_ <<
    "#include <Thrift.h>" << endl <<
    "#include <TApplicationException.h>" << endl <<
    "#include <protocol/TProtocol.h>" << endl <<
    "#include <transport/TTransport.h>" << endl <<
    endl;

  // Include other Thrift includes
  const vector<t_program*>& includes = program_->get_includes();
  for (size_t i = 0; i < includes.size(); ++i) {
    f_types_ <<
      "#include \"" << get_include_prefix(*(includes[i])) <<
      includes[i]->get_name() << "_types.h\"" << endl;

    // XXX(simpkins): If gen_templates_ is enabled, we currently assume all
    // included files were also generated with templates enabled.
    f_types_tcc_ <<
      "#include \"" << get_include_prefix(*(includes[i])) <<
      includes[i]->get_name() << "_types.tcc\"" << endl;
  }
  f_types_ << endl;

  // Include custom headers
  const vector<string>& cpp_includes = program_->get_cpp_includes();
  for (size_t i = 0; i < cpp_includes.size(); ++i) {
    if (cpp_includes[i][0] == '<') {
      f_types_ <<
        "#include " << cpp_includes[i] << endl;
    } else {
      f_types_ <<
        "#include \"" << cpp_includes[i] << "\"" << endl;
    }
  }
  f_types_ <<
    endl;

  // Include the types file
  f_types_impl_ <<
    "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
    "_types.h\"" << endl <<
    endl;
  f_types_tcc_ <<
    "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
    "_types.h\"" << endl <<
    endl;

  // If we are generating local reflection metadata, we need to include
  // the definition of TypeSpec.
  if (gen_dense_) {
    f_types_impl_ <<
      "#include <TReflectionLocal.h>" << endl <<
      endl;
  }

  // Open namespace
  ns_open_ = namespace_open(program_->get_namespace("cpp"));
  ns_close_ = namespace_close(program_->get_namespace("cpp"));

  f_types_ <<
    ns_open_ << endl <<
    endl;

  f_types_impl_ <<
    ns_open_ << endl <<
    endl;

  f_types_tcc_ <<
    ns_open_ << endl <<
    endl;
}

/**
 * Closes the output files.
 */
void t_cpp_generator::close_generator() {
  // Close namespace
  f_types_ <<
    ns_close_ << endl <<
    endl;
  f_types_impl_ <<
    ns_close_ << endl;
  f_types_tcc_ <<
    ns_close_ << endl <<
    endl;

  // Include the types.tcc file from the types header file,
  // so clients don't have to explicitly include the tcc file.
  // TODO(simpkins): Make this a separate option.
  if (gen_templates_) {
    f_types_ <<
      "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
      "_types.tcc\"" << endl <<
      endl;
  }

  // Close ifndef
  f_types_ <<
    "#endif" << endl;
  f_types_tcc_ <<
    "#endif" << endl;

  // Close output file
  f_types_.close();
  f_types_impl_.close();
  f_types_tcc_.close();
}

/**
 * Generates a typedef. This is just a simple 1-liner in C++
 *
 * @param ttypedef The type definition
 */
void t_cpp_generator::generate_typedef(t_typedef* ttypedef) {
  f_types_ <<
    indent() << "typedef " << type_name(ttypedef->get_type(), true) << " " << ttypedef->get_symbolic() << ";" << endl <<
    endl;
}


void t_cpp_generator::generate_enum_constant_list(std::ofstream& f,
                                                  const vector<t_enum_value*>& constants,
                                                  const char* prefix,
                                                  const char* suffix,
                                                  bool include_values) {
  f << " {" << endl;
  indent_up();

  vector<t_enum_value*>::const_iterator c_iter;
  bool first = true;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if (first) {
      first = false;
    } else {
      f << "," << endl;
    }
    indent(f)
      << prefix << (*c_iter)->get_name() << suffix;
    if (include_values && (*c_iter)->has_value()) {
      f << " = " << (*c_iter)->get_value();
    }
  }

  f << endl;
  indent_down();
  indent(f) << "};" << endl;
}

/**
 * Generates code for an enumerated type. In C++, this is essentially the same
 * as the thrift definition itself, using the enum keyword in C++.
 *
 * @param tenum The enumeration
 */
void t_cpp_generator::generate_enum(t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();

  std::string enum_name = tenum->get_name();
  if (!gen_pure_enums_) {
    enum_name = "type";
    f_types_ <<
      indent() << "struct " << tenum->get_name() << " {" << endl;
    indent_up();
  }
  f_types_ <<
    indent() << "enum " << enum_name;

  generate_enum_constant_list(f_types_, constants, "", "", true);

  if (!gen_pure_enums_) {
    indent_down();
    f_types_ << "};" << endl;
  }

  f_types_ << endl;

  /**
     Generate a character array of enum names for debugging purposes.
  */
  std::string prefix = "";
  if (!gen_pure_enums_) {
    prefix = tenum->get_name() + "::";
  }

  f_types_impl_ <<
    indent() << "int _k" << tenum->get_name() << "Values[] =";
  generate_enum_constant_list(f_types_impl_, constants, prefix.c_str(), "", false);

  f_types_impl_ <<
    indent() << "const char* _k" << tenum->get_name() << "Names[] =";
  generate_enum_constant_list(f_types_impl_, constants, "\"", "\"", false);

  f_types_ <<
    indent() << "extern const std::map<int, const char*> _" <<
    tenum->get_name() << "_VALUES_TO_NAMES;" << endl << endl;

  f_types_impl_ <<
    indent() << "const std::map<int, const char*> _" << tenum->get_name() <<
    "_VALUES_TO_NAMES(::apache::thrift::TEnumIterator(" << constants.size() <<
    ", _k" << tenum->get_name() << "Values" <<
    ", _k" << tenum->get_name() << "Names), " <<
    "::apache::thrift::TEnumIterator(-1, NULL, NULL));" << endl << endl;

  generate_local_reflection(f_types_, tenum, false);
  generate_local_reflection(f_types_impl_, tenum, true);
}

/**
 * Generates a class that holds all the constants.
 */
void t_cpp_generator::generate_consts(std::vector<t_const*> consts) {
  string f_consts_name = get_out_dir()+program_name_+"_constants.h";
  ofstream f_consts;
  f_consts.open(f_consts_name.c_str());

  string f_consts_impl_name = get_out_dir()+program_name_+"_constants.cpp";
  ofstream f_consts_impl;
  f_consts_impl.open(f_consts_impl_name.c_str());

  // Print header
  f_consts <<
    autogen_comment();
  f_consts_impl <<
    autogen_comment();

  // Start ifndef
  f_consts <<
    "#ifndef " << program_name_ << "_CONSTANTS_H" << endl <<
    "#define " << program_name_ << "_CONSTANTS_H" << endl <<
    endl <<
    "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
    "_types.h\"" << endl <<
    endl <<
    ns_open_ << endl <<
    endl;

  f_consts_impl <<
    "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
    "_constants.h\"" << endl <<
    endl <<
    ns_open_ << endl <<
    endl;

  f_consts <<
    "class " << program_name_ << "Constants {" << endl <<
    " public:" << endl <<
    "  " << program_name_ << "Constants();" << endl <<
    endl;
  indent_up();
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    string name = (*c_iter)->get_name();
    t_type* type = (*c_iter)->get_type();
    f_consts <<
      indent() << type_name(type) << " " << name << ";" << endl;
  }
  indent_down();
  f_consts <<
    "};" << endl;

  f_consts_impl <<
    "const " << program_name_ << "Constants g_" << program_name_ << "_constants;" << endl <<
    endl <<
    program_name_ << "Constants::" << program_name_ << "Constants() {" << endl;
  indent_up();
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    print_const_value(f_consts_impl,
                      (*c_iter)->get_name(),
                      (*c_iter)->get_type(),
                      (*c_iter)->get_value());
  }
  indent_down();
  indent(f_consts_impl) <<
    "}" << endl;

  f_consts <<
    endl <<
    "extern const " << program_name_ << "Constants g_" << program_name_ << "_constants;" << endl <<
    endl <<
    ns_close_ << endl <<
    endl <<
    "#endif" << endl;
  f_consts.close();

  f_consts_impl <<
    endl <<
    ns_close_ << endl <<
    endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
void t_cpp_generator::print_const_value(ofstream& out, string name, t_type* type, t_const_value* value) {
  type = get_true_type(type);
  if (type->is_base_type()) {
    string v2 = render_const_value(out, name, type, value);
    indent(out) << name << " = " << v2 << ";" << endl <<
      endl;
  } else if (type->is_enum()) {
    indent(out) << name << " = (" << type_name(type) << ")" << value->get_integer() << ";" << endl <<
      endl;
  } else if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      t_type* field_type = NULL;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
        }
      }
      if (field_type == NULL) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      string val = render_const_value(out, name, field_type, v_iter->second);
      indent(out) << name << "." << v_iter->first->get_string() << " = " << val << ";" << endl;
      indent(out) << name << ".__isset." << v_iter->first->get_string() << " = true;" << endl;
    }
    out << endl;
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(out, name, ktype, v_iter->first);
      string val = render_const_value(out, name, vtype, v_iter->second);
      indent(out) << name << ".insert(std::make_pair(" << key << ", " << val << "));" << endl;
    }
    out << endl;
  } else if (type->is_list()) {
    t_type* etype = ((t_list*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(out, name, etype, *v_iter);
      indent(out) << name << ".push_back(" << val << ");" << endl;
    }
    out << endl;
  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(out, name, etype, *v_iter);
      indent(out) << name << ".insert(" << val << ");" << endl;
    }
    out << endl;
  } else {
    throw "INVALID TYPE IN print_const_value: " + type->get_name();
  }
}

/**
 *
 */
string t_cpp_generator::render_const_value(ofstream& out, string name, t_type* type, t_const_value* value) {
  (void) name;
  std::ostringstream render;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      render << '"' << get_escaped_string(value) << '"';
      break;
    case t_base_type::TYPE_BOOL:
      render << ((value->get_integer() > 0) ? "true" : "false");
      break;
    case t_base_type::TYPE_BYTE:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
      render << value->get_integer();
      break;
    case t_base_type::TYPE_I64:
      render << value->get_integer() << "LL";
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        render << value->get_integer();
      } else {
        render << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    render << "(" << type_name(type) << ")" << value->get_integer();
  } else {
    string t = tmp("tmp");
    indent(out) << type_name(type) << " " << t << ";" << endl;
    print_const_value(out, t, type, value);
    render << t;
  }

  return render.str();
}

/**
 * Generates a struct definition for a thrift data type. This is a class
 * with data members and a read/write() function, plus a mirroring isset
 * inner class.
 *
 * @param tstruct The struct definition
 */
void t_cpp_generator::generate_cpp_struct(t_struct* tstruct, bool is_exception) {
  generate_struct_definition(f_types_, tstruct, is_exception);
  generate_struct_fingerprint(f_types_impl_, tstruct, true);
  generate_local_reflection(f_types_, tstruct, false);
  generate_local_reflection(f_types_impl_, tstruct, true);
  generate_local_reflection_pointer(f_types_impl_, tstruct);

  std::ofstream& out = (gen_templates_ ? f_types_tcc_ : f_types_impl_);
  generate_struct_reader(out, tstruct);
  generate_struct_writer(out, tstruct);
}

/**
 * Writes the struct definition into the header file
 *
 * @param out Output stream
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_definition(ofstream& out,
                                                 t_struct* tstruct,
                                                 bool is_exception,
                                                 bool pointers,
                                                 bool read,
                                                 bool write) {
  string extends = "";
  if (is_exception) {
    extends = " : public ::apache::thrift::TException";
  }

  // Get members
  vector<t_field*>::const_iterator m_iter;
  const vector<t_field*>& members = tstruct->get_members();

  // Write the isset structure declaration outside the class. This makes
  // the generated code amenable to processing by SWIG.
  // We only declare the struct if it gets used in the class.

  // Isset struct has boolean fields, but only for non-required fields.
  bool has_nonrequired_fields = false;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if ((*m_iter)->get_req() != t_field::T_REQUIRED)
      has_nonrequired_fields = true;
  }

  if (has_nonrequired_fields && (!pointers || read)) {

    out <<
      indent() << "typedef struct _" << tstruct->get_name() << "__isset {" << endl;
    indent_up();

    indent(out) <<
      "_" << tstruct->get_name() << "__isset() ";
    bool first = true;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() == t_field::T_REQUIRED) {
        continue;
      }
      if (first) {
        first = false;
        out <<
          ": " << (*m_iter)->get_name() << "(false)";
      } else {
        out <<
          ", " << (*m_iter)->get_name() << "(false)";
      }
    }
    out << " {}" << endl;

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if ((*m_iter)->get_req() != t_field::T_REQUIRED) {
        indent(out) <<
          "bool " << (*m_iter)->get_name() << ";" << endl;
        }
      }

      indent_down();
      indent(out) <<
        "} _" << tstruct->get_name() << "__isset;" << endl;
    }

  out << endl;

  // Open struct def
  out <<
    indent() << "class " << tstruct->get_name() << extends << " {" << endl <<
    indent() << " public:" << endl <<
    endl;
  indent_up();

  // Put the fingerprint up top for all to see.
  generate_struct_fingerprint(out, tstruct, false);

  if (!pointers) {
    // Default constructor
    indent(out) <<
      tstruct->get_name() << "()";

    bool init_ctor = false;

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      t_type* t = get_true_type((*m_iter)->get_type());
      if (t->is_base_type()) {
        string dval;
        if (t->is_enum()) {
          dval += "(" + type_name(t) + ")";
        }
        dval += t->is_string() ? "\"\"" : "0";
        t_const_value* cv = (*m_iter)->get_value();
        if (cv != NULL) {
          dval = render_const_value(out, (*m_iter)->get_name(), t, cv);
        }
        if (!init_ctor) {
          init_ctor = true;
          out << " : ";
          out << (*m_iter)->get_name() << "(" << dval << ")";
        } else {
          out << ", " << (*m_iter)->get_name() << "(" << dval << ")";
        }
      }
    }
    out << " {" << endl;
    indent_up();
    // TODO(dreiss): When everything else in Thrift is perfect,
    // do more of these in the initializer list.
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      t_type* t = get_true_type((*m_iter)->get_type());

      if (!t->is_base_type()) {
        t_const_value* cv = (*m_iter)->get_value();
        if (cv != NULL) {
          print_const_value(out, (*m_iter)->get_name(), t, cv);
        }
      }
    }
    scope_down(out);
  }

  if (tstruct->annotations_.find("final") == tstruct->annotations_.end()) {
    out <<
      endl <<
      indent() << "virtual ~" << tstruct->get_name() << "() throw() {}" << endl << endl;
  }

  // Pointer to this structure's reflection local typespec.
  if (gen_dense_) {
    indent(out) <<
      "static ::apache::thrift::reflection::local::TypeSpec* local_reflection;" <<
      endl << endl;
  }

  // Declare all fields
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    indent(out) <<
      declare_field(*m_iter, false, pointers && !(*m_iter)->get_type()->is_xception(), !read) << endl;
  }

  // Add the __isset data member if we need it, using the definition from above
  if (has_nonrequired_fields && (!pointers || read)) {
    out <<
      endl <<
      indent() << "_" << tstruct->get_name() << "__isset __isset;" << endl;
  }

  // Create a setter function for each field
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (pointers) {
      continue;
    }
    out <<
      endl <<
      indent() << "void __set_" << (*m_iter)->get_name() <<
        "(" << type_name((*m_iter)->get_type(), false, true);
    out << " val) {" << endl << indent() <<
      indent() << (*m_iter)->get_name() << " = val;" << endl;

    // assume all fields are required except optional fields.
    // for optional fields change __isset.name to true
    bool is_optional = (*m_iter)->get_req() == t_field::T_OPTIONAL;
    if (is_optional) {
      out <<
        indent() <<
        indent() << "__isset." << (*m_iter)->get_name() << " = true;" << endl;
    }
    out <<
      indent()<< "}" << endl;
  }
  out << endl;

  if (!pointers) {
    // Generate an equality testing operator.  Make it inline since the compiler
    // will do a better job than we would when deciding whether to inline it.
    out <<
      indent() << "bool operator == (const " << tstruct->get_name() << " & " <<
      (members.size() > 0 ? "rhs" : "/* rhs */") << ") const" << endl;
    scope_up(out);
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      // Most existing Thrift code does not use isset or optional/required,
      // so we treat "default" fields as required.
      if ((*m_iter)->get_req() != t_field::T_OPTIONAL) {
        out <<
          indent() << "if (!(" << (*m_iter)->get_name()
                   << " == rhs." << (*m_iter)->get_name() << "))" << endl <<
          indent() << "  return false;" << endl;
      } else {
        out <<
          indent() << "if (__isset." << (*m_iter)->get_name()
                   << " != rhs.__isset." << (*m_iter)->get_name() << ")" << endl <<
          indent() << "  return false;" << endl <<
          indent() << "else if (__isset." << (*m_iter)->get_name() << " && !("
                   << (*m_iter)->get_name() << " == rhs." << (*m_iter)->get_name()
                   << "))" << endl <<
          indent() << "  return false;" << endl;
      }
    }
    indent(out) << "return true;" << endl;
    scope_down(out);
    out <<
      indent() << "bool operator != (const " << tstruct->get_name() << " &rhs) const {" << endl <<
      indent() << "  return !(*this == rhs);" << endl <<
      indent() << "}" << endl << endl;

    // Generate the declaration of a less-than operator.  This must be
    // implemented by the application developer if they wish to use it.  (They
    // will get a link error if they try to use it without an implementation.)
    out <<
      indent() << "bool operator < (const "
               << tstruct->get_name() << " & ) const;" << endl << endl;
  }

  if (read) {
    if (gen_templates_) {
      out <<
        indent() << "template <class Protocol_>" << endl <<
        indent() << "uint32_t read(Protocol_* iprot);" << endl;
    } else {
      out <<
        indent() << "uint32_t read(" <<
        "::apache::thrift::protocol::TProtocol* iprot);" << endl;
    }
  }
  if (write) {
    if (gen_templates_) {
      out <<
        indent() << "template <class Protocol_>" << endl <<
        indent() << "uint32_t write(Protocol_* oprot) const;" << endl;
    } else {
      out <<
        indent() << "uint32_t write(" <<
        "::apache::thrift::protocol::TProtocol* oprot) const;" << endl;
    }
  }
  out << endl;

  indent_down();
  indent(out) <<
    "};" << endl <<
    endl;
}

/**
 * Writes the fingerprint of a struct to either the header or implementation.
 *
 * @param out Output stream
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_fingerprint(ofstream& out,
                                                  t_struct* tstruct,
                                                  bool is_definition) {
  string stat, nspace, comment;
  if (is_definition) {
    stat = "";
    nspace = tstruct->get_name() + "::";
    comment = " ";
  } else {
    stat = "static ";
    nspace = "";
    comment = "; // ";
  }

  if (tstruct->has_fingerprint()) {
    out <<
      indent() << stat << "const char* " << nspace
        << "ascii_fingerprint" << comment << "= \"" <<
        tstruct->get_ascii_fingerprint() << "\";" << endl <<
      indent() << stat << "const uint8_t " << nspace <<
        "binary_fingerprint[" << t_type::fingerprint_len << "]" << comment << "= {";
    const char* comma = "";
    for (int i = 0; i < t_type::fingerprint_len; i++) {
      out << comma << "0x" << t_struct::byte_to_hex(tstruct->get_binary_fingerprint()[i]);
      comma = ",";
    }
    out << "};" << endl << endl;
  }
}

/**
 * Writes the local reflection of a type (either declaration or definition).
 */
void t_cpp_generator::generate_local_reflection(std::ofstream& out,
                                                t_type* ttype,
                                                bool is_definition) {
  if (!gen_dense_) {
    return;
  }
  ttype = get_true_type(ttype);
  assert(ttype->has_fingerprint());
  string key = ttype->get_ascii_fingerprint() + (is_definition ? "-defn" : "-decl");
  // Note that we have generated this fingerprint.  If we already did, bail out.
  if (!reflected_fingerprints_.insert(key).second) {
    return;
  }
  // Let each program handle its own structures.
  if (ttype->get_program() != NULL && ttype->get_program() != program_) {
    return;
  }

  // Do dependencies.
  if (ttype->is_list()) {
    generate_local_reflection(out, ((t_list*)ttype)->get_elem_type(), is_definition);
  } else if (ttype->is_set()) {
    generate_local_reflection(out, ((t_set*)ttype)->get_elem_type(), is_definition);
  } else if (ttype->is_map()) {
    generate_local_reflection(out, ((t_map*)ttype)->get_key_type(), is_definition);
    generate_local_reflection(out, ((t_map*)ttype)->get_val_type(), is_definition);
  } else if (ttype->is_struct() || ttype->is_xception()) {
    // Hacky hacky.  For efficiency and convenience, we need a dummy "T_STOP"
    // type at the end of our typespec array.  Unfortunately, there is no
    // T_STOP type, so we use the global void type, and special case it when
    // generating its typespec.

    const vector<t_field*>& members = ((t_struct*)ttype)->get_sorted_members();
    vector<t_field*>::const_iterator m_iter;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      generate_local_reflection(out, (**m_iter).get_type(), is_definition);
    }
    generate_local_reflection(out, g_type_void, is_definition);

    // For definitions of structures, do the arrays of metas and field specs also.
    if (is_definition) {
      out <<
        indent() << "::apache::thrift::reflection::local::FieldMeta" << endl <<
        indent() << local_reflection_name("metas", ttype) <<"[] = {" << endl;
      indent_up();
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        indent(out) << "{ " << (*m_iter)->get_key() << ", " <<
          (((*m_iter)->get_req() == t_field::T_OPTIONAL) ? "true" : "false") <<
          " }," << endl;
      }
      // Zero for the T_STOP marker.
      indent(out) << "{ 0, false }" << endl << "};" << endl;
      indent_down();

      out <<
        indent() << "::apache::thrift::reflection::local::TypeSpec*" << endl <<
        indent() << local_reflection_name("specs", ttype) <<"[] = {" << endl;
      indent_up();
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        indent(out) << "&" <<
          local_reflection_name("typespec", (*m_iter)->get_type(), true) << "," << endl;
      }
      indent(out) << "&" <<
        local_reflection_name("typespec", g_type_void) << "," << endl;
      indent_down();
      indent(out) << "};" << endl;
    }
  }

  out <<
    indent() << "// " << ttype->get_fingerprint_material() << endl <<
    indent() << (is_definition ? "" : "extern ") <<
      "::apache::thrift::reflection::local::TypeSpec" << endl <<
      local_reflection_name("typespec", ttype) <<
      (is_definition ? "(" : ";") << endl;

  if (!is_definition) {
    out << endl;
    return;
  }

  indent_up();

  if (ttype->is_void()) {
    indent(out) << "::apache::thrift::protocol::T_STOP";
  } else {
    indent(out) << type_to_enum(ttype);
  }

  if (ttype->is_struct()) {
    out << "," << endl <<
      indent() << type_name(ttype) << "::binary_fingerprint," << endl <<
      indent() << local_reflection_name("metas", ttype) << "," << endl <<
      indent() << local_reflection_name("specs", ttype);
  } else if (ttype->is_list()) {
    out << "," << endl <<
      indent() << "&" << local_reflection_name("typespec", ((t_list*)ttype)->get_elem_type()) << "," << endl <<
      indent() << "NULL";
  } else if (ttype->is_set()) {
    out << "," << endl <<
      indent() << "&" << local_reflection_name("typespec", ((t_set*)ttype)->get_elem_type()) << "," << endl <<
      indent() << "NULL";
  } else if (ttype->is_map()) {
    out << "," << endl <<
      indent() << "&" << local_reflection_name("typespec", ((t_map*)ttype)->get_key_type()) << "," << endl <<
      indent() << "&" << local_reflection_name("typespec", ((t_map*)ttype)->get_val_type());
  }

  out << ");" << endl << endl;

  indent_down();
}

/**
 * Writes the structure's static pointer to its local reflection typespec
 * into the implementation file.
 */
void t_cpp_generator::generate_local_reflection_pointer(std::ofstream& out,
                                                        t_type* ttype) {
  if (!gen_dense_) {
    return;
  }
  indent(out) <<
    "::apache::thrift::reflection::local::TypeSpec* " <<
      ttype->get_name() << "::local_reflection = " << endl <<
    indent() << "  &" << local_reflection_name("typespec", ttype) << ";" <<
    endl << endl;
}

/**
 * Makes a helper function to gen a struct reader.
 *
 * @param out Stream to write to
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_reader(ofstream& out,
                                             t_struct* tstruct,
                                             bool pointers) {
  if (gen_templates_) {
    out <<
      indent() << "template <class Protocol_>" << endl <<
      indent() << "uint32_t " << tstruct->get_name() <<
      "::read(Protocol_* iprot) {" << endl;
  } else {
    indent(out) <<
      "uint32_t " << tstruct->get_name() <<
      "::read(::apache::thrift::protocol::TProtocol* iprot) {" << endl;
  }
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Declare stack tmp variables
  out <<
    endl <<
    indent() << "uint32_t xfer = 0;" << endl <<
    indent() << "std::string fname;" << endl <<
    indent() << "::apache::thrift::protocol::TType ftype;" << endl <<
    indent() << "int16_t fid;" << endl <<
    endl <<
    indent() << "xfer += iprot->readStructBegin(fname);" << endl <<
    endl <<
    indent() << "using ::apache::thrift::protocol::TProtocolException;" << endl <<
    endl;

  // Required variables aren't in __isset, so we need tmp vars to check them.
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED)
      indent(out) << "bool isset_" << (*f_iter)->get_name() << " = false;" << endl;
  }
  out << endl;


  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;
    scope_up(out);

    // Read beginning field marker
    indent(out) <<
      "xfer += iprot->readFieldBegin(fname, ftype, fid);" << endl;

    // Check for field STOP marker
    out <<
      indent() << "if (ftype == ::apache::thrift::protocol::T_STOP) {" << endl <<
      indent() << "  break;" << endl <<
      indent() << "}" << endl;

    // Switch statement on the field we are reading
    indent(out) <<
      "switch (fid)" << endl;

      scope_up(out);

      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(out) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        indent(out) <<
          "if (ftype == " << type_to_enum((*f_iter)->get_type()) << ") {" << endl;
        indent_up();

        const char *isset_prefix =
          ((*f_iter)->get_req() != t_field::T_REQUIRED) ? "this->__isset." : "isset_";

#if 0
        // This code throws an exception if the same field is encountered twice.
        // We've decided to leave it out for performance reasons.
        // TODO(dreiss): Generate this code and "if" it out to make it easier
        // for people recompiling thrift to include it.
        out <<
          indent() << "if (" << isset_prefix << (*f_iter)->get_name() << ")" << endl <<
          indent() << "  throw TProtocolException(TProtocolException::INVALID_DATA);" << endl;
#endif

        if (pointers && !(*f_iter)->get_type()->is_xception()) {
          generate_deserialize_field(out, *f_iter, "(*(this->", "))");
        } else {
          generate_deserialize_field(out, *f_iter, "this->");
        }
        out <<
          indent() << isset_prefix << (*f_iter)->get_name() << " = true;" << endl;
        indent_down();
        out <<
          indent() << "} else {" << endl <<
          indent() << "  xfer += iprot->skip(ftype);" << endl <<
          // TODO(dreiss): Make this an option when thrift structs
          // have a common base class.
          // indent() << "  throw TProtocolException(TProtocolException::INVALID_DATA);" << endl <<
          indent() << "}" << endl <<
          indent() << "break;" << endl;
        indent_down();
      }

      // In the default case we skip the field
      out <<
        indent() << "default:" << endl <<
        indent() << "  xfer += iprot->skip(ftype);" << endl <<
        indent() << "  break;" << endl;

      scope_down(out);

    // Read field end marker
    indent(out) <<
      "xfer += iprot->readFieldEnd();" << endl;

    scope_down(out);

  out <<
    endl <<
    indent() << "xfer += iprot->readStructEnd();" << endl;

  // Throw if any required fields are missing.
  // We do this after reading the struct end so that
  // there might possibly be a chance of continuing.
  out << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_REQUIRED)
      out <<
        indent() << "if (!isset_" << (*f_iter)->get_name() << ')' << endl <<
        indent() << "  throw TProtocolException(TProtocolException::INVALID_DATA);" << endl;
  }

  indent(out) << "return xfer;" << endl;

  indent_down();
  indent(out) <<
    "}" << endl << endl;
}

/**
 * Generates the write function.
 *
 * @param out Stream to write to
 * @param tstruct The struct
 */
void t_cpp_generator::generate_struct_writer(ofstream& out,
                                             t_struct* tstruct,
                                             bool pointers) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  if (gen_templates_) {
    out <<
      indent() << "template <class Protocol_>" << endl <<
      indent() << "uint32_t " << tstruct->get_name() <<
      "::write(Protocol_* oprot) const {" << endl;
  } else {
    indent(out) <<
      "uint32_t " << tstruct->get_name() <<
      "::write(::apache::thrift::protocol::TProtocol* oprot) const {" << endl;
  }
  indent_up();

  out <<
    indent() << "uint32_t xfer = 0;" << endl;

  indent(out) <<
    "xfer += oprot->writeStructBegin(\"" << name << "\");" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if ((*f_iter)->get_req() == t_field::T_OPTIONAL) {
      indent(out) << "if (this->__isset." << (*f_iter)->get_name() << ") {" << endl;
      indent_up();
    }
    // Write field header
    out <<
      indent() << "xfer += oprot->writeFieldBegin(" <<
      "\"" << (*f_iter)->get_name() << "\", " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ");" << endl;
    // Write field contents
    if (pointers && !(*f_iter)->get_type()->is_xception()) {
      generate_serialize_field(out, *f_iter, "(*(this->", "))");
    } else {
      generate_serialize_field(out, *f_iter, "this->");
    }
    // Write field closer
    indent(out) <<
      "xfer += oprot->writeFieldEnd();" << endl;
    if ((*f_iter)->get_req() == t_field::T_OPTIONAL) {
      indent_down();
      indent(out) << '}' << endl;
    }
  }

  // Write the struct map
  out <<
    indent() << "xfer += oprot->writeFieldStop();" << endl <<
    indent() << "xfer += oprot->writeStructEnd();" << endl <<
    indent() << "return xfer;" << endl;

  indent_down();
  indent(out) <<
    "}" << endl <<
    endl;
}

/**
 * Struct writer for result of a function, which can have only one of its
 * fields set and does a conditional if else look up into the __isset field
 * of the struct.
 *
 * @param out Output stream
 * @param tstruct The result struct
 */
void t_cpp_generator::generate_struct_result_writer(ofstream& out,
                                                    t_struct* tstruct,
                                                    bool pointers) {
  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator f_iter;

  if (gen_templates_) {
    out <<
      indent() << "template <class Protocol_>" << endl <<
      indent() << "uint32_t " << tstruct->get_name() <<
      "::write(Protocol_* oprot) const {" << endl;
  } else {
    indent(out) <<
      "uint32_t " << tstruct->get_name() <<
      "::write(::apache::thrift::protocol::TProtocol* oprot) const {" << endl;
  }
  indent_up();

  out <<
    endl <<
    indent() << "uint32_t xfer = 0;" << endl <<
    endl;

  indent(out) <<
    "xfer += oprot->writeStructBegin(\"" << name << "\");" << endl;

  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      out <<
        endl <<
        indent() << "if ";
    } else {
      out <<
        " else if ";
    }

    out << "(this->__isset." << (*f_iter)->get_name() << ") {" << endl;

    indent_up();

    // Write field header
    out <<
      indent() << "xfer += oprot->writeFieldBegin(" <<
      "\"" << (*f_iter)->get_name() << "\", " <<
      type_to_enum((*f_iter)->get_type()) << ", " <<
      (*f_iter)->get_key() << ");" << endl;
    // Write field contents
    if (pointers) {
      generate_serialize_field(out, *f_iter, "(*(this->", "))");
    } else {
      generate_serialize_field(out, *f_iter, "this->");
    }
    // Write field closer
    indent(out) << "xfer += oprot->writeFieldEnd();" << endl;

    indent_down();
    indent(out) << "}";
  }

  // Write the struct map
  out <<
    endl <<
    indent() << "xfer += oprot->writeFieldStop();" << endl <<
    indent() << "xfer += oprot->writeStructEnd();" << endl <<
    indent() << "return xfer;" << endl;

  indent_down();
  indent(out) <<
    "}" << endl <<
    endl;
}

/**
 * Generates a thrift service. In C++, this comprises an entirely separate
 * header and source file. The header file defines the methods and includes
 * the data types defined in the main header file, and the implementation
 * file contains implementations of the basic printer and default interfaces.
 *
 * @param tservice The service definition
 */
void t_cpp_generator::generate_service(t_service* tservice) {
  string svcname = tservice->get_name();

  // Make output files
  string f_header_name = get_out_dir()+svcname+".h";
  f_header_.open(f_header_name.c_str());

  // Print header file includes
  f_header_ <<
    autogen_comment();
  f_header_ <<
    "#ifndef " << svcname << "_H" << endl <<
    "#define " << svcname << "_H" << endl <<
    endl;
  if (gen_cob_style_) {
    f_header_ <<
      "#include <transport/TBufferTransports.h>" << endl << // TMemoryBuffer
      "#include <tr1/functional>" << endl <<
      "namespace apache { namespace thrift { namespace async {" << endl <<
      "class TAsyncChannel;" << endl <<
      "}}}" << endl;
  }
  f_header_ <<
    "#include <TProcessor.h>" << endl;
  if (gen_cob_style_) {
    f_header_ <<
      "#include <async/TAsyncProcessor.h>" << endl;
  }
  f_header_ <<
    "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
    "_types.h\"" << endl;

  t_service* extends_service = tservice->get_extends();
  if (extends_service != NULL) {
    f_header_ <<
      "#include \"" << get_include_prefix(*(extends_service->get_program())) <<
      extends_service->get_name() << ".h\"" << endl;
  }

  f_header_ <<
    endl <<
    ns_open_ << endl <<
    endl;

  // Service implementation file includes
  string f_service_name = get_out_dir()+svcname+".cpp";
  f_service_.open(f_service_name.c_str());
  f_service_ <<
    autogen_comment();
  f_service_ <<
    "#include \"" << get_include_prefix(*get_program()) << svcname << ".h\"" << endl;
  if (gen_cob_style_) {
    f_service_ <<
      "#include \"async/TAsyncChannel.h\"" << endl;
  }
  if (gen_templates_) {
    f_service_ <<
      "#include \"" << get_include_prefix(*get_program()) << svcname <<
      ".tcc\"" << endl;

    string f_service_tcc_name = get_out_dir()+svcname+".tcc";
    f_service_tcc_.open(f_service_tcc_name.c_str());
    f_service_tcc_ <<
      autogen_comment();
    f_service_tcc_ <<
      "#include \"" << get_include_prefix(*get_program()) << svcname <<
      ".h\"" << endl;

    f_service_tcc_ <<
      "#ifndef " << svcname << "_TCC" << endl <<
      "#define " << svcname << "_TCC" << endl <<
      endl;

    if (gen_cob_style_) {
      f_service_tcc_ <<
        "#include \"async/TAsyncChannel.h\"" << endl;
    }
  }

  f_service_ <<
    endl << ns_open_ << endl << endl;
  f_service_tcc_ <<
    endl << ns_open_ << endl << endl;

  // Generate all the components
  generate_service_interface(tservice, "");
  generate_service_null(tservice, "");
  generate_service_helpers(tservice);
  generate_service_client(tservice, "");
  generate_service_processor(tservice, "");
  generate_service_multiface(tservice);
  generate_service_skeleton(tservice);

  // Generate all the cob components
  if (gen_cob_style_) {
    generate_service_interface(tservice, "CobCl");
    generate_service_interface(tservice, "CobSv");
    generate_service_null(tservice, "CobSv");
    generate_service_client(tservice, "Cob");
    generate_service_processor(tservice, "Cob");
    generate_service_async_skeleton(tservice);
  }

  // Close the namespace
  f_service_ <<
    ns_close_ << endl <<
    endl;
  f_service_tcc_ <<
    ns_close_ << endl <<
    endl;
  f_header_ <<
    ns_close_ << endl <<
    endl;

  // TODO(simpkins): Make this a separate option
  if (gen_templates_) {
    f_header_ <<
      "#include \"" << get_include_prefix(*get_program()) << svcname <<
      ".tcc\"" << endl <<
      "#include \"" << get_include_prefix(*get_program()) << program_name_ <<
      "_types.tcc\"" << endl <<
      endl;
  }

  f_header_ <<
    "#endif" << endl;
  f_service_tcc_ <<
    "#endif" << endl;

  // Close the files
  f_service_tcc_.close();
  f_service_.close();
  f_header_.close();
}

/**
 * Generates helper functions for a service. Basically, this generates types
 * for all the arguments and results to functions.
 *
 * @param tservice The service to generate a header definition for
 */
void t_cpp_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  std::ofstream& out = (gen_templates_ ? f_service_tcc_ : f_service_);

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    string name_orig = ts->get_name();

    // TODO(dreiss): Why is this stuff not in generate_function_helpers?
    ts->set_name(tservice->get_name() + "_" + (*f_iter)->get_name() + "_args");
    generate_struct_definition(f_header_, ts, false);
    generate_struct_reader(out, ts);
    generate_struct_writer(out, ts);
    ts->set_name(tservice->get_name() + "_" + (*f_iter)->get_name() + "_pargs");
    generate_struct_definition(f_header_, ts, false, true, false, true);
    generate_struct_writer(out, ts, true);
    ts->set_name(name_orig);

    generate_function_helpers(tservice, *f_iter);
  }
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_cpp_generator::generate_service_interface(t_service* tservice, string style) {

  string service_if_name = service_name_ + style + "If";
  if (style == "CobCl") {
    // Forward declare the client.
    string client_name = service_name_ + "CobClient";
    if (gen_templates_) {
      client_name += "T";
      service_if_name += "T";
      indent(f_header_) <<
        "template <class Protocol_>" << endl;
    }
    indent(f_header_) << "class " << client_name << ";" <<
      endl << endl;
  }

  string extends = "";
  if (tservice->get_extends() != NULL) {
    extends = " : virtual public " + type_name(tservice->get_extends()) +
      style + "If";
    if (style == "CobCl" && gen_templates_) {
      // TODO(simpkins): If gen_templates_ is enabled, we currently assume all
      // parent services were also generated with templates enabled.
      extends += "T<Protocol_>";
    }
  }

  if (style == "CobCl" && gen_templates_) {
    f_header_ << "template <class Protocol_>" << endl;
  }
  f_header_ <<
    "class " << service_if_name << extends << " {" << endl <<
    " public:" << endl;
  indent_up();
  f_header_ <<
    indent() << "virtual ~" << service_if_name << "() {}" << endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << "virtual " << function_signature(*f_iter, style) << " = 0;" << endl;
  }
  indent_down();
  f_header_ <<
    "};" << endl << endl;

  if (style == "CobCl" && gen_templates_) {
    // generate a backwards-compatible typedef for clients that do not
    // know about the new template-style code
    f_header_ <<
      "typedef " << service_if_name <<
      "< ::apache::thrift::protocol::TProtocol> " <<
      service_name_ << style << "If;" <<
      endl << endl;
  }
}

/**
 * Generates a null implementation of the service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_cpp_generator::generate_service_null(t_service* tservice, string style) {
  string extends = "";
  if (tservice->get_extends() != NULL) {
    extends = " , virtual public " + type_name(tservice->get_extends()) + style + "Null";
  }
  f_header_ <<
    "class " << service_name_ << style << "Null : virtual public " << service_name_ << style << "If" << extends << " {" << endl <<
    " public:" << endl;
  indent_up();
  f_header_ <<
    indent() << "virtual ~" << service_name_ << style << "Null() {}" << endl;
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_header_ <<
      indent() << function_signature(*f_iter, style, "", false) << " {" << endl;
    indent_up();

    t_type* returntype = (*f_iter)->get_returntype();
    t_field returnfield(returntype, "_return");

    if (style == "") {
      if (returntype->is_void() || is_complex_type(returntype)) {
        f_header_ << indent() << "return;" << endl;
      } else {
        f_header_ <<
          indent() << declare_field(&returnfield, true) << endl <<
          indent() << "return _return;" << endl;
      }
    } else if (style == "CobSv") {
      if (returntype->is_void()) {
        f_header_ << indent() << "return cob();" << endl;
    } else {
      t_field returnfield(returntype, "_return");
      f_header_ <<
        indent() << declare_field(&returnfield, true) << endl <<
        indent() << "return cob(_return);" << endl;
    }

    } else {
      throw "UNKNOWN STYLE";
    }

    indent_down();
    f_header_ <<
      indent() << "}" << endl;
  }
  indent_down();
  f_header_ <<
    "};" << endl << endl;
}

void t_cpp_generator::generate_function_call(ostream& out, t_function* tfunction, string target, string iface, string arg_prefix) {
  bool first = true;
  t_type* ret_type = get_true_type(tfunction->get_returntype());
  out << indent();
  if (!tfunction->is_oneway() && !ret_type->is_void()) {
    if (is_complex_type(ret_type)) {
      first = false;
      out << iface << "->" << tfunction->get_name() << "(" << target;
    } else {
      out << target << " = " << iface << "->" << tfunction->get_name() << "(";
    }
  } else {
    out << iface << "->" << tfunction->get_name() << "(";
  }
  const std::vector<t_field*>& fields = tfunction->get_arglist()->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      out << ", ";
    }
    out << arg_prefix << (*f_iter)->get_name();
  }
  out << ");" << endl;
}

void t_cpp_generator::generate_service_async_skeleton(t_service* tservice) {
  string svcname = tservice->get_name();

  // Service implementation file includes
  string f_skeleton_name = get_out_dir()+svcname+"_async_server.skeleton.cpp";

  string ns = namespace_prefix(tservice->get_program()->get_namespace("cpp"));

  ofstream f_skeleton;
  f_skeleton.open(f_skeleton_name.c_str());
  f_skeleton <<
    "// This autogenerated skeleton file illustrates one way to adapt a synchronous" << endl <<
    "// interface into an asynchronous interface. You should copy it to another" << endl <<
    "// filename to avoid overwriting it and rewrite as asynchronous any functions" << endl <<
    "// that would otherwise introduce unwanted latency." << endl <<
    endl <<
    "#include \"" << get_include_prefix(*get_program()) << svcname << ".h\"" << endl <<
    "#include <protocol/TBinaryProtocol.h>" << endl <<
    "#include <async/TEventServer.h>" << endl <<
    endl <<
    "using namespace ::apache::thrift;" << endl <<
    "using namespace ::apache::thrift::protocol;" << endl <<
    "using namespace ::apache::thrift::transport;" << endl <<
    "using namespace ::apache::thrift::async;" << endl <<
    endl <<
    "using boost::shared_ptr;" << endl <<
    endl;

  if (!ns.empty()) {
    f_skeleton <<
      "using namespace " << string(ns, 0, ns.size()-2) << ";" << endl <<
      endl;
  }

  f_skeleton <<
    "class " << svcname << "AsyncHandler : " <<
    "public " << svcname << "CobSvIf {" << endl <<
    " public:" << endl;
  indent_up();
  f_skeleton <<
    indent() << svcname << "AsyncHandler() {" << endl <<
    indent() << "  syncHandler_ = std::auto_ptr<" << svcname <<
                "Handler>(new " << svcname << "Handler);" << endl <<
    indent() << "  // Your initialization goes here" << endl <<
    indent() << "}" << endl;
  f_skeleton <<
    indent() << "virtual ~" << service_name_ << "AsyncHandler();" << endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_skeleton <<
      endl <<
      indent() << function_signature(*f_iter, "CobSv", "", true) << " {" << endl;
    indent_up();

    t_type* returntype = (*f_iter)->get_returntype();
    t_field returnfield(returntype, "_return");

    string target = returntype->is_void() ? "" : "_return";
    if (!returntype->is_void()) {
      f_skeleton <<
        indent() << declare_field(&returnfield, true) << endl;
    }
    generate_function_call(f_skeleton, *f_iter, target, "syncHandler_", "");
    f_skeleton << indent() << "return cob(" << target << ");" << endl;

    scope_down(f_skeleton);
  }
  f_skeleton << endl <<
    " protected:" << endl <<
    indent() << "std::auto_ptr<" << svcname << "Handler> syncHandler_;" << endl;
  indent_down();
  f_skeleton <<
    "};" << endl << endl;
}

/**
 * Generates a multiface, which is a single server that just takes a set
 * of objects implementing the interface and calls them all, returning the
 * value of the last one to be called.
 *
 * @param tservice The service to generate a multiserver for.
 */
void t_cpp_generator::generate_service_multiface(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  string extends = "";
  string extends_multiface = "";
  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends());
    extends_multiface = ", public " + extends + "Multiface";
  }

  string list_type = string("std::vector<boost::shared_ptr<") + service_name_ + "If> >";

  // Generate the header portion
  f_header_ <<
    "class " << service_name_ << "Multiface : " <<
    "virtual public " << service_name_ << "If" <<
    extends_multiface << " {" << endl <<
    " public:" << endl;
  indent_up();
  f_header_ <<
    indent() << service_name_ << "Multiface(" << list_type << "& ifaces) : ifaces_(ifaces) {" << endl;
  if (!extends.empty()) {
    f_header_ <<
      indent() << "  std::vector<boost::shared_ptr<" + service_name_ + "If> >::iterator iter;" << endl <<
      indent() << "  for (iter = ifaces.begin(); iter != ifaces.end(); ++iter) {" << endl <<
      indent() << "    " << extends << "Multiface::add(*iter);" << endl <<
      indent() << "  }" << endl;
  }
  f_header_ <<
    indent() << "}" << endl <<
    indent() << "virtual ~" << service_name_ << "Multiface() {}" << endl;
  indent_down();

  // Protected data members
  f_header_ <<
    " protected:" << endl;
  indent_up();
  f_header_ <<
    indent() << list_type << " ifaces_;" << endl <<
    indent() << service_name_ << "Multiface() {}" << endl <<
    indent() << "void add(boost::shared_ptr<" << service_name_ << "If> iface) {" << endl;
  if (!extends.empty()) {
    f_header_ <<
      indent() << "  " << extends << "Multiface::add(iface);" << endl;
  }
  f_header_ <<
    indent() << "  ifaces_.push_back(iface);" << endl <<
    indent() << "}" << endl;
  indent_down();

  f_header_ <<
    indent() << " public:" << endl;
  indent_up();

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arglist = (*f_iter)->get_arglist();
    const vector<t_field*>& args = arglist->get_members();
    vector<t_field*>::const_iterator a_iter;

    string call = string("ifaces_[i]->") + (*f_iter)->get_name() + "(";
    bool first = true;
    if (is_complex_type((*f_iter)->get_returntype())) {
      call += "_return";
      first = false;
    }
    for (a_iter = args.begin(); a_iter != args.end(); ++a_iter) {
      if (first) {
        first = false;
      } else {
        call += ", ";
      }
      call += (*a_iter)->get_name();
    }
    call += ")";

    f_header_ <<
      indent() << function_signature(*f_iter, "") << " {" << endl;
    indent_up();
    f_header_ <<
      indent() << "size_t sz = ifaces_.size();" << endl <<
      indent() << "for (size_t i = 0; i < sz; ++i) {" << endl;
    if (!(*f_iter)->get_returntype()->is_void()) {
      f_header_ <<
        indent() << "  if (i == sz - 1) {" << endl;
      if (is_complex_type((*f_iter)->get_returntype())) {
        f_header_ <<
          indent() << "    " << call << ";" << endl <<
          indent() << "    return;" << endl;
      } else {
        f_header_ <<
          indent() << "    return " << call << ";" << endl;
      }
      f_header_ <<
        indent() << "  } else {" << endl <<
        indent() << "    " << call << ";" << endl <<
        indent() << "  }" << endl;
    } else {
      f_header_ <<
        indent() << "  " << call << ";" << endl;
    }

    f_header_ <<
      indent() << "}" << endl;

    indent_down();
    f_header_ <<
      indent() << "}" << endl <<
      endl;
  }

  indent_down();
  f_header_ <<
    indent() << "};" << endl <<
    endl;
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_client(t_service* tservice, string style) {
  string ifstyle;
  if (style == "Cob") {
    ifstyle = "CobCl";
  }

  std::ofstream& out = (gen_templates_ ? f_service_tcc_ : f_service_);
  string template_header, template_suffix, short_suffix, protocol_type, _this;
  string const prot_factory_type =
    "::apache::thrift::protocol::TProtocolFactory";
  if (gen_templates_) {
    template_header = "template <class Protocol_>\n";
    short_suffix = "T";
    template_suffix = "T<Protocol_>";
    protocol_type = "Protocol_";
    _this = "this->";
  } else {
    protocol_type = "::apache::thrift::protocol::TProtocol";
  }
  string prot_ptr = "boost::shared_ptr< " + protocol_type + ">";
  string client_suffix = "Client" + template_suffix;
  string if_suffix = "If";
  if (style == "Cob") {
    if_suffix += template_suffix;
  }

  string extends = "";
  string extends_client = "";
  if (tservice->get_extends() != NULL) {
    // TODO(simpkins): If gen_templates_ is enabled, we currently assume all
    // parent services were also generated with templates enabled.
    extends = type_name(tservice->get_extends());
    extends_client = ", public " + extends + style + client_suffix;
  }

  // Generate the header portion
  f_header_ <<
    template_header <<
    "class " << service_name_ << style << "Client" << short_suffix << " : " <<
    "virtual public " << service_name_ << ifstyle << if_suffix <<
    extends_client << " {" << endl <<
    " public:" << endl;

  indent_up();
  if (style != "Cob") {
    f_header_ <<
      indent() << service_name_ << style << "Client" << short_suffix <<
      "(" << prot_ptr << " prot) :" <<
      endl;
    if (extends.empty()) {
      f_header_ <<
        indent() << "  piprot_(prot)," << endl <<
        indent() << "  poprot_(prot) {" << endl <<
        indent() << "  iprot_ = prot.get();" << endl <<
        indent() << "  oprot_ = prot.get();" << endl <<
        indent() << "}" << endl;
    } else {
      f_header_ <<
        indent() << "  " << extends << style << client_suffix <<
        "(prot, prot) {}" << endl;
    }

    f_header_ <<
      indent() << service_name_ << style << "Client" << short_suffix <<
      "(" << prot_ptr << " iprot, " << prot_ptr << " oprot) :" << endl;
    if (extends.empty()) {
      f_header_ <<
        indent() << "  piprot_(iprot)," << endl <<
        indent() << "  poprot_(oprot) {" << endl <<
        indent() << "  iprot_ = iprot.get();" << endl <<
        indent() << "  oprot_ = oprot.get();" << endl <<
        indent() << "}" << endl;
    } else {
      f_header_ <<
        indent() << "  " << extends << style << client_suffix <<
        "(iprot, oprot) {}" << endl;
    }

    // Generate getters for the protocols.
    // Note that these are not currently templated for simplicity.
    // TODO(simpkins): should they be templated?
    f_header_ <<
      indent() << "boost::shared_ptr< ::apache::thrift::protocol::TProtocol> getInputProtocol() {" << endl <<
      indent() << "  return " << _this << "piprot_;" << endl <<
      indent() << "}" << endl;

    f_header_ <<
      indent() << "boost::shared_ptr< ::apache::thrift::protocol::TProtocol> getOutputProtocol() {" << endl <<
      indent() << "  return " << _this << "poprot_;" << endl <<
      indent() << "}" << endl;

  } else /* if (style == "Cob") */ {
    f_header_ <<
      indent() << service_name_ << style << "Client" << short_suffix << "(" <<
      "boost::shared_ptr< ::apache::thrift::async::TAsyncChannel> channel, " <<
      "::apache::thrift::protocol::TProtocolFactory* protocolFactory) :" <<
      endl;
    if (extends.empty()) {
      f_header_ <<
        indent() << "  channel_(channel)," << endl <<
        indent() << "  itrans_(new ::apache::thrift::transport::TMemoryBuffer())," << endl <<
        indent() << "  otrans_(new ::apache::thrift::transport::TMemoryBuffer())," << endl;
      if (gen_templates_) {
        // TProtocolFactory classes return generic TProtocol pointers.
        // We have to dynamic cast to the Protocol_ type we are expecting.
        f_header_ <<
          indent() << "  piprot_(boost::dynamic_pointer_cast<Protocol_>(" <<
          "protocolFactory->getProtocol(itrans_)))," << endl <<
          indent() << "  poprot_(boost::dynamic_pointer_cast<Protocol_>(" <<
          "protocolFactory->getProtocol(otrans_))) {" << endl;
        // Throw a TException if either dynamic cast failed.
        f_header_ <<
          indent() << "  if (!piprot_ || !poprot_) {" << endl <<
          indent() << "    throw ::apache::thrift::TException(\"" <<
          "TProtocolFactory returned unexpected protocol type in " <<
          service_name_ << style << "Client" << short_suffix <<
          " constructor\");" << endl <<
          indent() << "  }" << endl;
      } else {
        f_header_ <<
          indent() << "  piprot_(protocolFactory->getProtocol(itrans_))," <<
          endl <<
          indent() << "  poprot_(protocolFactory->getProtocol(otrans_)) {" <<
          endl;
      }
      f_header_ <<
        indent() << "  iprot_ = piprot_.get();" << endl <<
        indent() << "  oprot_ = poprot_.get();" << endl <<
        indent() << "}" << endl;
    } else {
      f_header_ <<
        indent() << "  " << extends << style << client_suffix <<
        "(channel, protocolFactory) {}" << endl;
    }
  }

  if (style == "Cob") {
    f_header_ <<
      indent() << "boost::shared_ptr< ::apache::thrift::async::TAsyncChannel> getChannel() {" << endl <<
      indent() << "  return " << _this << "channel_;" << endl <<
      indent() << "}" << endl;
    if (!gen_no_client_completion_) {
      f_header_ <<
        indent() << "virtual void completed__(bool /* success */) {}" << endl;
    }
  }

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_header_) << function_signature(*f_iter, ifstyle) << ";" << endl;
    // TODO(dreiss): Use private inheritance to avoid generating thise in cob-style.
    t_function send_function(g_type_void,
        string("send_") + (*f_iter)->get_name(),
        (*f_iter)->get_arglist());
    indent(f_header_) << function_signature(&send_function, "") << ";" << endl;
    if (!(*f_iter)->is_oneway()) {
      t_struct noargs(program_);
      t_function recv_function((*f_iter)->get_returntype(),
          string("recv_") + (*f_iter)->get_name(),
          &noargs);
      indent(f_header_) << function_signature(&recv_function, "") << ";" << endl;
    }
  }
  indent_down();

  if (extends.empty()) {
    f_header_ <<
      " protected:" << endl;
    indent_up();

    if (style == "Cob") {
      f_header_ <<
        indent() << "boost::shared_ptr< ::apache::thrift::async::TAsyncChannel> channel_;"  << endl <<
        indent() << "boost::shared_ptr< ::apache::thrift::transport::TMemoryBuffer> itrans_;"  << endl <<
        indent() << "boost::shared_ptr< ::apache::thrift::transport::TMemoryBuffer> otrans_;"  << endl;
    }
    f_header_ <<
      indent() << prot_ptr << " piprot_;"  << endl <<
      indent() << prot_ptr << " poprot_;"  << endl <<
      indent() << protocol_type << "* iprot_;"  << endl <<
      indent() << protocol_type << "* oprot_;"  << endl;

    indent_down();
  }

  f_header_ <<
    "};" << endl <<
    endl;

  if (gen_templates_) {
    // Output a backwards compatibility typedef using
    // TProtocol as the template parameter.
    f_header_ <<
      "typedef " << service_name_ << style <<
      "ClientT< ::apache::thrift::protocol::TProtocol> " <<
      service_name_ << style << "Client;" << endl <<
      endl;
  }

  string scope = service_name_ + style + client_suffix + "::";

  // Generate client method implementations
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    // Open function
    if (gen_templates_) {
      indent(out) << template_header;
    }
    indent(out) <<
      function_signature(*f_iter, ifstyle, scope) << endl;
    scope_up(out);
    indent(out) <<
      "send_" << funname << "(";

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
        out << ", ";
      }
      out << (*fld_iter)->get_name();
    }
    out << ");" << endl;

    if (style != "Cob") {
      if (!(*f_iter)->is_oneway()) {
        out << indent();
        if (!(*f_iter)->get_returntype()->is_void()) {
          if (is_complex_type((*f_iter)->get_returntype())) {
            out << "recv_" << funname << "(_return);" << endl;
          } else {
            out << "return recv_" << funname << "();" << endl;
          }
        } else {
          out <<
            "recv_" << funname << "();" << endl;
        }
      }
    } else {
      if (!(*f_iter)->is_oneway()) {
        out <<
          indent() << _this << "channel_->sendAndRecvMessage(" <<
          "std::tr1::bind(cob, this), " << _this << "otrans_.get(), " <<
          _this << "itrans_.get());" << endl;
      } else {
        out <<
        indent() << _this << "channel_->sendMessage(" <<
          "std::tr1::bind(cob, this), " << _this << "otrans_.get());" << endl;
      }
    }
    scope_down(out);
    out << endl;

    //if (style != "Cob") // TODO(dreiss): Libify the client and don't generate this for cob-style
    if (true) {
      // Function for sending
      t_function send_function(g_type_void,
                               string("send_") + (*f_iter)->get_name(),
                               (*f_iter)->get_arglist());

      // Open the send function
      if (gen_templates_) {
        indent(out) << template_header;
      }
      indent(out) <<
        function_signature(&send_function, "", scope) << endl;
      scope_up(out);

      // Function arguments and results
      string argsname = tservice->get_name() + "_" + (*f_iter)->get_name() + "_pargs";
      string resultname = tservice->get_name() + "_" + (*f_iter)->get_name() + "_presult";

      // Serialize the request
      out <<
        indent() << "int32_t cseqid = 0;" << endl <<
        indent() << _this << "oprot_->writeMessageBegin(\"" <<
        (*f_iter)->get_name() <<
        "\", ::apache::thrift::protocol::T_CALL, cseqid);" << endl <<
        endl <<
        indent() << argsname << " args;" << endl;

      for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
        out <<
          indent() << "args." << (*fld_iter)->get_name() << " = &" << (*fld_iter)->get_name() << ";" << endl;
      }

      out <<
        indent() << "args.write(" << _this << "oprot_);" << endl <<
        endl <<
        indent() << _this << "oprot_->writeMessageEnd();" << endl <<
        indent() << _this << "oprot_->getTransport()->writeEnd();" << endl <<
        indent() << _this << "oprot_->getTransport()->flush();" << endl;

      scope_down(out);
      out << endl;

      // Generate recv function only if not an oneway function
      if (!(*f_iter)->is_oneway()) {
        t_struct noargs(program_);
        t_function recv_function((*f_iter)->get_returntype(),
                                 string("recv_") + (*f_iter)->get_name(),
                                 &noargs);
        // Open the recv function
        if (gen_templates_) {
          indent(out) << template_header;
        }
        indent(out) <<
          function_signature(&recv_function, "", scope) << endl;
        scope_up(out);

        out <<
          endl <<
          indent() << "int32_t rseqid = 0;" << endl <<
          indent() << "std::string fname;" << endl <<
          indent() << "::apache::thrift::protocol::TMessageType mtype;" << endl;
        if (style == "Cob" && !gen_no_client_completion_) {
          out <<
            indent() << "bool completed = false;" << endl << endl <<
            indent() << "try {";
          indent_up();
        }
        out << endl <<
          indent() << _this << "iprot_->readMessageBegin(fname, mtype, rseqid);" << endl <<
          indent() << "if (mtype == ::apache::thrift::protocol::T_EXCEPTION) {" << endl <<
          indent() << "  ::apache::thrift::TApplicationException x;" << endl <<
          indent() << "  x.read(" << _this << "iprot_);" << endl <<
          indent() << "  " << _this << "iprot_->readMessageEnd();" << endl <<
          indent() << "  " << _this << "iprot_->getTransport()->readEnd();" <<
          endl;
        if (style == "Cob" && !gen_no_client_completion_) {
          out <<
            indent() << "  completed = true;" << endl <<
            indent() << "  completed__(true);" << endl;
        }
        out <<
          indent() << "  throw x;" << endl <<
          indent() << "}" << endl <<
          indent() << "if (mtype != ::apache::thrift::protocol::T_REPLY) {" << endl <<
          indent() << "  " << _this << "iprot_->skip(" <<
          "::apache::thrift::protocol::T_STRUCT);" << endl <<
          indent() << "  " << _this << "iprot_->readMessageEnd();" << endl <<
          indent() << "  " << _this << "iprot_->getTransport()->readEnd();" <<
          endl;
        if (style == "Cob" && !gen_no_client_completion_) {
          out <<
            indent() << "  completed = true;" << endl <<
            indent() << "  completed__(false);" << endl;
        }
        out <<
          indent() << "}" << endl <<
          indent() << "if (fname.compare(\"" << (*f_iter)->get_name() << "\") != 0) {" << endl <<
          indent() << "  " << _this << "iprot_->skip(" <<
          "::apache::thrift::protocol::T_STRUCT);" << endl <<
          indent() << "  " << _this << "iprot_->readMessageEnd();" << endl <<
          indent() << "  " << _this << "iprot_->getTransport()->readEnd();" <<
          endl;
        if (style == "Cob" && !gen_no_client_completion_) {
          out <<
            indent() << "  completed = true;" << endl <<
            indent() << "  completed__(false);" << endl;
        }
        out <<
          indent() << "}" << endl;

        if (!(*f_iter)->get_returntype()->is_void() &&
            !is_complex_type((*f_iter)->get_returntype())) {
          t_field returnfield((*f_iter)->get_returntype(), "_return");
          out <<
            indent() << declare_field(&returnfield) << endl;
        }

        out <<
          indent() << resultname << " result;" << endl;

        if (!(*f_iter)->get_returntype()->is_void()) {
          out <<
            indent() << "result.success = &_return;" << endl;
        }

        out <<
          indent() << "result.read(" << _this << "iprot_);" << endl <<
          indent() << _this << "iprot_->readMessageEnd();" << endl <<
          indent() << _this << "iprot_->getTransport()->readEnd();" << endl <<
          endl;

        // Careful, only look for _result if not a void function
        if (!(*f_iter)->get_returntype()->is_void()) {
          if (is_complex_type((*f_iter)->get_returntype())) {
            out <<
              indent() << "if (result.__isset.success) {" << endl <<
              indent() << "  // _return pointer has now been filled" << endl;
            if (style == "Cob" && !gen_no_client_completion_) {
              out <<
                indent() << "  completed = true;" << endl <<
                indent() << "  completed__(true);" << endl;
            }
            out <<
              indent() << "  return;" << endl <<
              indent() << "}" << endl;
          } else {
            out <<
              indent() << "if (result.__isset.success) {" << endl;
            if (style == "Cob" && !gen_no_client_completion_) {
              out <<
                indent() << "  completed = true;" << endl <<
                indent() << "  completed__(true);" << endl;
            }
            out <<
              indent() << "  return _return;" << endl <<
              indent() << "}" << endl;
          }
        }

        t_struct* xs = (*f_iter)->get_xceptions();
        const std::vector<t_field*>& xceptions = xs->get_members();
        vector<t_field*>::const_iterator x_iter;
        for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
          out <<
            indent() << "if (result.__isset." << (*x_iter)->get_name() << ") {" << endl;
          if (style == "Cob" && !gen_no_client_completion_) {
            out <<
              indent() << "  completed = true;" << endl <<
              indent() << "  completed__(true);" << endl;
          }
          out  <<
            indent() << "  throw result." << (*x_iter)->get_name() << ";" << endl <<
            indent() << "}" << endl;
        }

        // We only get here if we are a void function
        if ((*f_iter)->get_returntype()->is_void()) {
          if (style == "Cob" && !gen_no_client_completion_) {
            out <<
              indent() << "completed = true;" << endl <<
              indent() << "completed__(true);" << endl;
          }
          indent(out) <<
            "return;" << endl;
        } else {
          if (style == "Cob" && !gen_no_client_completion_) {
            out <<
              indent() << "completed = true;" << endl <<
              indent() << "completed__(true);" << endl;
          }
          out <<
            indent() << "throw ::apache::thrift::TApplicationException(::apache::thrift::TApplicationException::MISSING_RESULT, \"" << (*f_iter)->get_name() << " failed: unknown result\");" << endl;
        }
        if (style == "Cob" && !gen_no_client_completion_) {
          indent_down();
          out <<
            indent() << "} catch (...) {" << endl <<
            indent() << "  if (!completed) {" << endl <<
            indent() << "    completed__(false);" << endl <<
            indent() << "  }" << endl <<
            indent() << "  throw;" << endl <<
            indent() << "}" << endl;
        }
        // Close function
        scope_down(out);
        out << endl;
      }
    }
  }
}

/**
 * Generates a service processor definition.
 *
 * @param tservice The service to generate a processor for.
 */
void t_cpp_generator::generate_service_processor(t_service* tservice, string style) {
  string ifstyle;
  string pstyle;
  string finish_cob;
  string finish_cob_decl;
  string cob_arg;
  string call_context = ", void* callContext";
  string call_context_arg = ", callContext";
  string call_context_decl = ", void*";
  string ret_type = "bool ";
  if (style == "Cob") {
    ifstyle = "CobSv";
    pstyle = "Async";
    finish_cob = "std::tr1::function<void(bool ok)> cob, ";
    finish_cob_decl = "std::tr1::function<void(bool ok)>, ";
    call_context = "";  // TODO(edhall) remove when callContext is aded to TAsyncProcessor
    call_context_arg = "";  // ditto
    call_context_decl = ""; // ditto
    cob_arg = "cob, ";
    ret_type = "void ";
  }

  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  string extends = "";
  string extends_processor = "";
  if (tservice->get_extends() != NULL) {
    extends = type_name(tservice->get_extends()) + pstyle + "Processor";
    if (gen_templates_) {
      // TODO(simpkins): If gen_templates_ is enabled, we currently assume all
      // parent services were also generated with templates enabled.
      extends += "T<Protocol_>";
    }
    extends_processor = ", public " + extends;
  }

  string template_header, template_suffix, typename_str;
  string class_suffix = "Processor";
  if (gen_templates_) {
    class_suffix = "ProcessorT";
    template_header = "template <class Protocol_>\n";
    template_suffix = "<Protocol_>";
    typename_str = "typename ";
  }

  // Generate the header portion
  f_header_ <<
    template_header <<
    "class " << service_name_ << pstyle << class_suffix <<
    " : virtual public ::apache::thrift::T" << pstyle << "Processor" <<
    extends_processor << " {" << endl;

  // Protected data members
  f_header_ <<
    " protected:" << endl;
  indent_up();
  f_header_ <<
    indent() << "boost::shared_ptr<" << service_name_ << ifstyle << "If> iface_;" << endl;
  f_header_ <<
    indent() << "virtual " << ret_type << "process_fn(" << finish_cob << "::apache::thrift::protocol::TProtocol* iprot, ::apache::thrift::protocol::TProtocol* oprot, std::string& fname, int32_t seqid" << call_context << ");" << endl;
  indent_down();

  // Process function declarations
  f_header_ <<
    " private:" << endl;
  indent_up();
  f_header_ <<
    indent() << "std::map<std::string, void (" <<
    service_name_ << pstyle << class_suffix << "::*)(" << finish_cob_decl <<
    "int32_t, ::apache::thrift::protocol::TProtocol*, " <<
    "::apache::thrift::protocol::TProtocol*" << call_context_decl <<
    ")> processMap_;" << endl;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    indent(f_header_) <<
      "void process_" << (*f_iter)->get_name() << "(" << finish_cob << "int32_t seqid, ::apache::thrift::protocol::TProtocol* iprot, ::apache::thrift::protocol::TProtocol* oprot" << call_context << ");" << endl;
    if (gen_templates_) {
      indent(f_header_) <<
        "void process_" << (*f_iter)->get_name() << "(" << finish_cob <<
        "int32_t seqid, Protocol_* iprot, Protocol_* oprot" <<
        call_context << ");" << endl;
    }
    if (style == "Cob") {
      // XXX Factor this out, even if it is a pain.
      string ret_arg = ((*f_iter)->get_returntype()->is_void()
                        ? ""
                        : ", const " + type_name((*f_iter)->get_returntype()) + "& _return");
      f_header_ <<
        indent() << "void return_" << (*f_iter)->get_name() <<
        "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
        "::apache::thrift::protocol::TProtocol* oprot, " <<
        "void* ctx" << ret_arg << ");" << endl;
      if (gen_templates_) {
        f_header_ <<
          indent() << "void return_" << (*f_iter)->get_name() <<
          "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
          "Protocol_* oprot, void* ctx" << ret_arg << ");" << endl;
      }
      // XXX Don't declare throw if it doesn't exist
      f_header_ <<
        indent() << "void throw_" << (*f_iter)->get_name() <<
        "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
        "::apache::thrift::protocol::TProtocol* oprot, void* ctx, " <<
        "::apache::thrift::TDelayedException* _throw);" << endl;
      if (gen_templates_) {
        f_header_ <<
          indent() << "void throw_" << (*f_iter)->get_name() <<
          "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
          "Protocol_* oprot, void* ctx, " <<
          "::apache::thrift::TDelayedException* _throw);" << endl;
      }
    }
  }
  indent_down();

  indent_up();
  string declare_map = "";
  indent_up();

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    declare_map += indent();
    declare_map += "processMap_[\"";
    declare_map += (*f_iter)->get_name();
    declare_map += "\"] = &";
    declare_map += service_name_;
    declare_map += pstyle;
    declare_map += class_suffix;
    declare_map += "::process_";
    declare_map += (*f_iter)->get_name();
    declare_map += ";\n";
  }
  indent_down();

  f_header_ <<
    " public:" << endl <<
    indent() << service_name_ << pstyle << class_suffix <<
    "(boost::shared_ptr<" << service_name_ << ifstyle << "If> iface) :" << endl;
  if (extends.empty()) {
    f_header_ <<
      indent() << "  iface_(iface) {" << endl;
  } else {
    f_header_ <<
      indent() << "  " << extends << "(iface)," << endl <<
      indent() << "  iface_(iface) {" << endl;
  }
  f_header_ <<
    declare_map <<
    indent() << "}" << endl <<
    endl <<
    indent() << "virtual " << ret_type << "process(" << finish_cob << "boost::shared_ptr< ::apache::thrift::protocol::TProtocol> piprot, boost::shared_ptr< ::apache::thrift::protocol::TProtocol> poprot" << call_context << ");" << endl <<
    indent() << "virtual ~" << service_name_ << pstyle << class_suffix <<
    "() {}" << endl;
  indent_down();
  f_header_ <<
    "};" << endl << endl;

  // Generate the server implementation
  std::ofstream& out = (gen_templates_ ? f_service_tcc_ : f_service_);
  out <<
    template_header <<
    ret_type << service_name_ << pstyle << class_suffix << template_suffix <<
    "::process(" << finish_cob <<
    "boost::shared_ptr< ::apache::thrift::protocol::TProtocol> piprot, " <<
    "boost::shared_ptr< ::apache::thrift::protocol::TProtocol> poprot" <<
    call_context << ") {" << endl;
  indent_up();

  out <<
    endl <<
    indent() << "::apache::thrift::protocol::TProtocol* iprot = piprot.get();" << endl <<
    indent() << "::apache::thrift::protocol::TProtocol* oprot = poprot.get();" << endl <<
    indent() << "std::string fname;" << endl <<
    indent() << "::apache::thrift::protocol::TMessageType mtype;" << endl <<
    indent() << "int32_t seqid;" << endl <<
    endl <<
    indent() << "iprot->readMessageBegin(fname, mtype, seqid);" << endl <<
    endl <<
    indent() << "if (mtype != ::apache::thrift::protocol::T_CALL && mtype != ::apache::thrift::protocol::T_ONEWAY) {" << endl <<
    indent() << "  iprot->skip(::apache::thrift::protocol::T_STRUCT);" << endl <<
    indent() << "  iprot->readMessageEnd();" << endl <<
    indent() << "  iprot->getTransport()->readEnd();" << endl <<
    indent() << "  ::apache::thrift::TApplicationException x(::apache::thrift::TApplicationException::INVALID_MESSAGE_TYPE);" << endl <<
    indent() << "  oprot->writeMessageBegin(fname, ::apache::thrift::protocol::T_EXCEPTION, seqid);" << endl <<
    indent() << "  x.write(oprot);" << endl <<
    indent() << "  oprot->writeMessageEnd();" << endl <<
    indent() << "  oprot->getTransport()->writeEnd();" << endl <<
    indent() << "  oprot->getTransport()->flush();" << endl <<
    indent() << (style == "Cob" ? "  return cob(false);" : "  return false;") << endl <<
    indent() << "}" << endl <<
    endl <<
    indent() << "return process_fn(" << (style == "Cob" ? "cob, " : "")
             << "iprot, oprot, fname, seqid" << call_context_arg << ");" <<
    endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;

  out <<
    template_header <<
    ret_type << service_name_ << pstyle << class_suffix << template_suffix <<
    "::process_fn(" << finish_cob <<
    "::apache::thrift::protocol::TProtocol* iprot, " <<
    "::apache::thrift::protocol::TProtocol* oprot, " <<
    "std::string& fname, int32_t seqid" << call_context << ") {" << endl;
  indent_up();

  // HOT: member function pointer map
  out <<
    indent() << typename_str << "std::map<std::string, void (" <<
    service_name_ << pstyle << class_suffix << "::*)(" << finish_cob_decl <<
    "int32_t, ::apache::thrift::protocol::TProtocol*, " <<
    "::apache::thrift::protocol::TProtocol*" << call_context_decl << ")>::iterator pfn;" << endl <<
    indent() << "pfn = processMap_.find(fname);" << endl <<
    indent() << "if (pfn == processMap_.end()) {" << endl;
  if (extends.empty()) {
    out <<
      indent() << "  iprot->skip(::apache::thrift::protocol::T_STRUCT);" << endl <<
      indent() << "  iprot->readMessageEnd();" << endl <<
      indent() << "  iprot->getTransport()->readEnd();" << endl <<
      indent() << "  ::apache::thrift::TApplicationException x(::apache::thrift::TApplicationException::UNKNOWN_METHOD, \"Invalid method name: '\"+fname+\"'\");" << endl <<
      indent() << "  oprot->writeMessageBegin(fname, ::apache::thrift::protocol::T_EXCEPTION, seqid);" << endl <<
      indent() << "  x.write(oprot);" << endl <<
      indent() << "  oprot->writeMessageEnd();" << endl <<
      indent() << "  oprot->getTransport()->writeEnd();" << endl <<
      indent() << "  oprot->getTransport()->flush();" << endl <<
      indent() << (style == "Cob" ? "  return cob(false);" : "  return false;") << endl;
  } else {
    out <<
      indent() << "  return "
               << extends << "::process_fn("
               << (style == "Cob" ? "cob, " : "")
               << "iprot, oprot, fname, seqid" << call_context_arg << ");" << endl;
  }
  out <<
    indent() << "}" << endl <<
    indent() << "(this->*(pfn->second))(" << cob_arg << "seqid, iprot, oprot" << call_context_arg << ");" << endl;

  // TODO(dreiss): return pfn ret?
  if (style == "Cob") {
    out <<
      indent() << "return;" << endl;
  } else {
    out <<
      indent() << "return true;" << endl;
  }

  indent_down();
  out <<
    "}" << endl <<
    endl;

  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (gen_templates_) {
      generate_process_function(tservice, *f_iter, style, false);
      generate_process_function(tservice, *f_iter, style, true);
    } else {
      generate_process_function(tservice, *f_iter, style, false);
    }
  }

  if (gen_templates_) {
    // Generate a backwards compatible typedef, for callers who don't know
    // about the new template-style code.
    //
    // We can't use TProtocol as the template parameter, since ProcessorT
    // provides overloaded versions of most methods, one of which accepts
    // TProtocol pointers, and one which accepts Protocol_ pointers.  This
    // results in a compile error if instantiated with Protocol_ == TProtocol.
    // Therefore, we define TDummyProtocol solely so we can use it as the
    // template parameter here.
    f_header_ <<
      "typedef " << service_name_ << pstyle <<
      "ProcessorT< ::apache::thrift::protocol::TDummyProtocol> " <<
      service_name_ << pstyle << "Processor;" << endl << endl;
  }
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_cpp_generator::generate_function_helpers(t_service* tservice,
                                                t_function* tfunction) {
  if (tfunction->is_oneway()) {
    return;
  }

  std::ofstream& out = (gen_templates_ ? f_service_tcc_ : f_service_);

  t_struct result(program_, tservice->get_name() + "_" + tfunction->get_name() + "_result");
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

  generate_struct_definition(f_header_, &result, false);
  generate_struct_reader(out, &result);
  generate_struct_result_writer(out, &result);

  result.set_name(tservice->get_name() + "_" + tfunction->get_name() + "_presult");
  generate_struct_definition(f_header_, &result, false, true, true, gen_cob_style_);
  generate_struct_reader(out, &result, true);
  if (gen_cob_style_) {
    generate_struct_writer(out, &result, true);
  }

}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_cpp_generator::generate_process_function(t_service* tservice,
                                                t_function* tfunction,
                                                string style,
                                                bool specialized) {
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;
  string service_func_name = tservice->get_name() + "." + tfunction->get_name();

  std::ofstream& out = (gen_templates_ ? f_service_tcc_ : f_service_);

  string prot_type =
    (specialized ? "Protocol_" : "::apache::thrift::protocol::TProtocol");
  string class_suffix;
  if (gen_templates_) {
    class_suffix = "T<Protocol_>";
  }

  // I tried to do this as one function.  I really did.  But it was too hard.
  if (style != "Cob") {
    // Open function
    if (gen_templates_) {
      out <<
        indent() << "template <class Protocol_>" << endl;
    }
    const bool unnamed_oprot_seqid = tfunction->is_oneway() &&
      !(gen_templates_ && !specialized);
    out <<
      "void " << tservice->get_name() << "Processor" << class_suffix << "::" <<
      "process_" << tfunction->get_name() << "(" <<
      "int32_t" << (unnamed_oprot_seqid ? ", " : " seqid, ") <<
      prot_type << "* iprot, " <<
      prot_type << "*" << (unnamed_oprot_seqid ? ", " : " oprot, ") <<
      "void* callContext)" << endl;
    scope_up(out);

    if (gen_templates_ && !specialized) {
      // If these are instances of Protocol_, instead of any old TProtocol,
      // use the specialized process function instead.
      out <<
        indent() << "Protocol_* _iprot = dynamic_cast<Protocol_*>(iprot);" <<
        endl <<
        indent() << "Protocol_* _oprot = dynamic_cast<Protocol_*>(oprot);" <<
        endl <<
        indent() << "if (_iprot && _oprot) {" << endl <<
        indent() << "  return process_" << tfunction->get_name() <<
        "(seqid, _iprot, _oprot, callContext);" << endl <<
        indent() << "}" << endl <<
        indent() << "T_GENERIC_PROTOCOL(this, iprot, _iprot);" << endl <<
        indent() << "T_GENERIC_PROTOCOL(this, oprot, _oprot);" << endl << endl;
    }

    string argsname = tservice->get_name() + "_" + tfunction->get_name() + "_args";
    string resultname = tservice->get_name() + "_" + tfunction->get_name() + "_result";

    out <<
      indent() << "void* ctx = NULL;" << endl <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  ctx = eventHandler_->getContext(\"" << service_func_name << "\", callContext);" << endl <<
      indent() << "}" << endl <<
      indent() << "::apache::thrift::TProcessorContextFreer freer(eventHandler_.get(), ctx, \"" << service_func_name << "\");" << endl << endl <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->preRead(ctx, \"" << service_func_name << "\");" << endl <<
      indent() << "}" << endl << endl <<
      indent() << argsname << " args;" << endl <<
      indent() << "args.read(iprot);" << endl <<
      indent() << "iprot->readMessageEnd();" << endl <<
      indent() << "uint32_t bytes = iprot->getTransport()->readEnd();" << endl << endl <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->postRead(ctx, \"" << service_func_name << "\", bytes);" << endl <<
      indent() << "}" << endl <<
      endl;

    // Declare result
    if (!tfunction->is_oneway()) {
      out <<
        indent() << resultname << " result;" << endl;
    }

    // Try block for functions with exceptions
    out <<
      indent() << "try {" << endl;
    indent_up();

    // Generate the function call
    bool first = true;
    out << indent();
    if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
      if (is_complex_type(tfunction->get_returntype())) {
        first = false;
        out << "iface_->" << tfunction->get_name() << "(result.success";
      } else {
        out << "result.success = iface_->" << tfunction->get_name() << "(";
      }
    } else {
      out <<
        "iface_->" << tfunction->get_name() << "(";
    }
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      if (first) {
        first = false;
      } else {
        out << ", ";
      }
      out << "args." << (*f_iter)->get_name();
    }
    out << ");" << endl;

    // Set isset on success field
    if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
      out <<
        indent() << "result.__isset.success = true;" << endl;
    }

    indent_down();
    out << indent() << "}";

    if (!tfunction->is_oneway()) {
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        out << " catch (" << type_name((*x_iter)->get_type()) << " &" << (*x_iter)->get_name() << ") {" << endl;
        indent_up();
        out <<
          indent() << "result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name() << ";" << endl <<
          indent() << "result.__isset." << (*x_iter)->get_name() << " = true;" << endl;
        indent_down();
        out << indent() << "}";
      }
    }

    out << " catch (const std::exception& e) {" << endl;

    indent_up();
    out <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->handlerError(ctx, \"" << service_func_name << "\");" << endl <<
      indent() << "}" << endl;

    if (!tfunction->is_oneway()) {
      out <<
        endl <<
        indent() << "::apache::thrift::TApplicationException x(e.what());" << endl <<
        indent() << "oprot->writeMessageBegin(\"" << tfunction->get_name() << "\", ::apache::thrift::protocol::T_EXCEPTION, seqid);" << endl <<
        indent() << "x.write(oprot);" << endl <<
        indent() << "oprot->writeMessageEnd();" << endl <<
        indent() << "oprot->getTransport()->writeEnd();" << endl <<
        indent() << "oprot->getTransport()->flush();" << endl;
    }
    out << indent() << "return;" << endl;
    indent_down();
    out << indent() << "}" << endl << endl;

    // Shortcut out here for oneway functions
    if (tfunction->is_oneway()) {
      out <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  eventHandler_->asyncComplete(ctx, \"" << service_func_name << "\");" << endl <<
        indent() << "}" << endl << endl <<
        indent() << "return;" << endl;
      indent_down();
      out << "}" << endl <<
        endl;
      return;
    }

    // Serialize the result into a struct
    out <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->preWrite(ctx, \"" << service_func_name << "\");" << endl <<
      indent() << "}" << endl << endl <<
      indent() << "oprot->writeMessageBegin(\"" << tfunction->get_name() << "\", ::apache::thrift::protocol::T_REPLY, seqid);" << endl <<
      indent() << "result.write(oprot);" << endl <<
      indent() << "oprot->writeMessageEnd();" << endl <<
      indent() << "bytes = oprot->getTransport()->writeEnd();" << endl <<
      indent() << "oprot->getTransport()->flush();" << endl << endl <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->postWrite(ctx, \"" << service_func_name << "\", bytes);" << endl <<
      indent() << "}" << endl;

    // Close function
    scope_down(out);
    out << endl;
  }

  // Cob style.
  else {
    // Processor entry point.
    // TODO(edhall) update for callContext when TEventServer is ready
    if (gen_templates_) {
      out <<
        indent() << "template <class Protocol_>" << endl;
    }
    out <<
      "void " << tservice->get_name() << "AsyncProcessor" << class_suffix <<
      "::process_" << tfunction->get_name() <<
      "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
      prot_type << "* iprot, " << prot_type << "* oprot)" << endl;
    scope_up(out);

    // TODO(simpkins): we could try to consoldate this
    // with the non-cob code above
    if (gen_templates_ && !specialized) {
      // If these are instances of Protocol_, instead of any old TProtocol,
      // use the specialized process function instead.
      out <<
        indent() << "Protocol_* _iprot = dynamic_cast<Protocol_*>(iprot);" <<
        endl <<
        indent() << "Protocol_* _oprot = dynamic_cast<Protocol_*>(oprot);" <<
        endl <<
        indent() << "if (_iprot && _oprot) {" << endl <<
        indent() << "  return process_" << tfunction->get_name() <<
        "(cob, seqid, _iprot, _oprot);" << endl <<
        indent() << "}" << endl <<
        indent() << "T_GENERIC_PROTOCOL(this, iprot, _iprot);" << endl <<
        indent() << "T_GENERIC_PROTOCOL(this, oprot, _oprot);" << endl << endl;
    }

    if (tfunction->is_oneway()) {
      out <<
        indent() << "(void) seqid;" << endl <<
        indent() << "(void) oprot;" << endl;
    }

    out <<
      indent() << tservice->get_name() + "_" + tfunction->get_name() + "_args" << " args;" << endl <<
      indent() << "void* ctx = NULL;" << endl <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  ctx = eventHandler_->getContext(\"" << service_func_name << "\", NULL);" << endl <<
      indent() << "}" << endl <<
      indent() << "::apache::thrift::TProcessorContextFreer freer(eventHandler_.get(), ctx, \"" << service_func_name << "\");" << endl << endl <<
      indent() << "try {" << endl;
    indent_up();
    out <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->preRead(ctx, \"" << service_func_name << "\");" << endl <<
      indent() << "}" << endl <<
      indent() << "args.read(iprot);" << endl <<
      indent() << "iprot->readMessageEnd();" << endl <<
      indent() << "uint32_t bytes = iprot->getTransport()->readEnd();" << endl <<
      indent() << "if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "  eventHandler_->postRead(ctx, \"" << service_func_name << "\", bytes);" << endl <<
      indent() << "}" << endl;
    scope_down(out);

    // TODO(dreiss): Handle TExceptions?  Expose to server?
    out <<
      indent() << "catch (const std::exception& exn) {" << endl <<
      indent() << "  if (eventHandler_.get() != NULL) {" << endl <<
      indent() << "    eventHandler_->handlerError(ctx, \"" << service_func_name << "\");" << endl <<
      indent() << "  }" << endl <<
      indent() << "  return cob(false);" << endl <<
      indent() << "}" << endl;

    if (tfunction->is_oneway()) {
      out <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  eventHandler_->asyncComplete(ctx, \"" << service_func_name << "\");" << endl <<
        indent() << "}" << endl;
    }
    // TODO(dreiss): Figure out a strategy for exceptions in async handlers.
    out <<
      indent() << "freer.unregister();" << endl;
    if (tfunction->is_oneway()) {
      // No return.  Just hand off our cob.
      // TODO(dreiss): Call the cob immediately?
      out <<
        indent() << "iface_->" << tfunction->get_name() << "(" <<
        "std::tr1::bind(cob, true)" << endl;
      indent_up(); indent_up();
    } else {
      string ret_arg, ret_placeholder;
      if (!tfunction->get_returntype()->is_void()) {
        ret_arg = ", const " + type_name(tfunction->get_returntype()) +
          "& _return";
        ret_placeholder = ", std::tr1::placeholders::_1";
      }

      // When gen_templates_ is true, the return_ and throw_ functions are
      // overloaded.  We have to declare pointers to them so that the compiler
      // can resolve the correct overloaded version.
      out <<
        indent() << "void (" << tservice->get_name() << "AsyncProcessor" <<
        class_suffix << "::*return_fn)(std::tr1::function<void(bool ok)> " <<
        "cob, int32_t seqid, " << prot_type << "* oprot, void* ctx" <<
        ret_arg << ") =" << endl;
      out <<
        indent() << "  &" << tservice->get_name() << "AsyncProcessor" <<
        class_suffix << "::return_" << tfunction->get_name() << ";" << endl;
      if (!xceptions.empty()) {
        out <<
          indent() << "void (" << tservice->get_name() << "AsyncProcessor" <<
          class_suffix << "::*throw_fn)(std::tr1::function<void(bool ok)> " <<
          "cob, int32_t seqid, " << prot_type << "* oprot, void* ctx, " <<
          "::apache::thrift::TDelayedException* _throw) =" << endl;
        out <<
          indent() << "  &" << tservice->get_name() << "AsyncProcessor" <<
          class_suffix << "::throw_" << tfunction->get_name() << ";" << endl;
      }

      out <<
        indent() << "iface_->" << tfunction->get_name() << "(" << endl;
      indent_up(); indent_up();
      out <<
        indent() << "std::tr1::bind(return_fn, this, cob, seqid, oprot, ctx" <<
        ret_placeholder << ")";
      if (!xceptions.empty()) {
        out
          << ',' << endl <<
          indent() << "std::tr1::bind(throw_fn, this, cob, seqid, oprot, " <<
          "ctx, std::tr1::placeholders::_1)";
      }
    }

    // XXX Whitespace cleanup.
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      out
                 << ',' << endl <<
        indent() << "args." << (*f_iter)->get_name();
    }
    out << ");" << endl;
    indent_down(); indent_down();
    scope_down(out);
    out << endl;

    // Normal return.
    if (!tfunction->is_oneway()) {
      string ret_arg_decl, ret_arg_name;
      if (!tfunction->get_returntype()->is_void()) {
        ret_arg_decl = ", const " + type_name(tfunction->get_returntype()) +
          "& _return";
        ret_arg_name = ", _return";
      }
      if (gen_templates_) {
        out <<
          indent() << "template <class Protocol_>" << endl;
      }
      out <<
        "void " << tservice->get_name() << "AsyncProcessor" << class_suffix <<
        "::return_" << tfunction->get_name() <<
        "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
        prot_type << "* oprot, void* ctx" << ret_arg_decl << ')' << endl;
      scope_up(out);

      if (gen_templates_ && !specialized) {
        // If oprot is a Protocol_ instance,
        // use the specialized return function instead.
        out <<
          indent() << "Protocol_* _oprot = dynamic_cast<Protocol_*>(oprot);" <<
          endl <<
          indent() << "if (_oprot) {" << endl <<
          indent() << "  return return_" << tfunction->get_name() <<
          "(cob, seqid, _oprot, ctx" << ret_arg_name << ");" << endl <<
          indent() << "}" << endl <<
          indent() << "T_GENERIC_PROTOCOL(this, oprot, _oprot);" <<
          endl << endl;
      }

      out <<
        indent() << tservice->get_name() << "_" << tfunction->get_name() << "_presult result;" << endl;
      if (!tfunction->get_returntype()->is_void()) {
        // The const_cast here is unfortunate, but it would be a pain to avoid,
        // and we only do a write with this struct, which is const-safe.
        out <<
          indent() << "result.success = const_cast<" << type_name(tfunction->get_returntype()) << "*>(&_return);" << endl <<
          indent() << "result.__isset.success = true;" << endl;
      }
      // Serialize the result into a struct
      out <<
        endl <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  ctx = eventHandler_->getContext(\"" << service_func_name << "\", NULL);" << endl <<
        indent() << "}" << endl <<
        indent() << "::apache::thrift::TProcessorContextFreer freer(eventHandler_.get(), ctx, \"" << service_func_name << "\");" << endl << endl <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  eventHandler_->preWrite(ctx, \"" << service_func_name << "\");" << endl <<
        indent() << "}" << endl << endl <<
        indent() << "oprot->writeMessageBegin(\"" << tfunction->get_name() << "\", ::apache::thrift::protocol::T_REPLY, seqid);" << endl <<
        indent() << "result.write(oprot);" << endl <<
        indent() << "oprot->writeMessageEnd();" << endl <<
        indent() << "uint32_t bytes = oprot->getTransport()->writeEnd();" << endl <<
        indent() << "oprot->getTransport()->flush();" << endl <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  eventHandler_->postWrite(ctx, \"" << service_func_name << "\", bytes);" << endl <<
        indent() << "}" << endl <<
        indent() << "return cob(true);" << endl;
      scope_down(out);
      out << endl;
    }

    // Exception return.
    if (!tfunction->is_oneway() && !xceptions.empty()) {
      if (gen_templates_) {
        out <<
          indent() << "template <class Protocol_>" << endl;
      }
      out <<
        "void " << tservice->get_name() << "AsyncProcessor" << class_suffix <<
        "::throw_" << tfunction->get_name() <<
        "(std::tr1::function<void(bool ok)> cob, int32_t seqid, " <<
        prot_type << "* oprot, void* ctx, " <<
        "::apache::thrift::TDelayedException* _throw)" << endl;
      scope_up(out);

      if (gen_templates_ && !specialized) {
        // If oprot is a Protocol_ instance,
        // use the specialized throw function instead.
        out <<
          indent() << "Protocol_* _oprot = dynamic_cast<Protocol_*>(oprot);" <<
          endl <<
          indent() << "if (_oprot) {" << endl <<
          indent() << "  return throw_" << tfunction->get_name() <<
          "(cob, seqid, _oprot, ctx, _throw);" << endl <<
          indent() << "}" << endl <<
          indent() << "  T_GENERIC_PROTOCOL(this, oprot, _oprot);" <<
          endl << endl;
      }

      out <<
        indent() << tservice->get_name() << "_" << tfunction->get_name() << "_result result;" << endl << endl <<
        indent() << "try {" << endl;
      indent_up();
      out <<
        indent() << "_throw->throw_it();" << endl <<
        indent() << "return cob(false);" << endl;  // Is this possible?  TBD.
      indent_down();
      out <<
        indent() << '}';
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        out << "  catch (" << type_name((*x_iter)->get_type()) << " &" << (*x_iter)->get_name() << ") {" << endl;
        indent_up();
        out <<
          indent() << "result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name() << ";" << endl <<
          indent() << "result.__isset." << (*x_iter)->get_name() << " = true;" << endl;
        scope_down(out);
      }
      // TODO(dreiss): Handle the case where an undeclared exception is thrown?

      // Serialize the result into a struct
      out <<
        endl <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  ctx = eventHandler_->getContext(\"" << service_func_name << "\", NULL);" << endl <<
        indent() << "}" << endl <<
        indent() << "::apache::thrift::TProcessorContextFreer freer(eventHandler_.get(), ctx, \"" << service_func_name << "\");" << endl << endl <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  eventHandler_->preWrite(ctx, \"" << service_func_name << "\");" << endl <<
        indent() << "}" << endl << endl <<
        indent() << "oprot->writeMessageBegin(\"" << tfunction->get_name() << "\", ::apache::thrift::protocol::T_REPLY, seqid);" << endl <<
        indent() << "result.write(oprot);" << endl <<
        indent() << "oprot->writeMessageEnd();" << endl <<
        indent() << "uint32_t bytes = oprot->getTransport()->writeEnd();" << endl <<
        indent() << "oprot->getTransport()->flush();" << endl <<
        indent() << "if (eventHandler_.get() != NULL) {" << endl <<
        indent() << "  eventHandler_->postWrite(ctx, \"" << service_func_name << "\", bytes);" << endl <<
        indent() << "}" << endl <<
        indent() << "return cob(true);" << endl;
      scope_down(out);
      out << endl;
    } // for each function
  } // cob style
}

/**
 * Generates a skeleton file of a server
 *
 * @param tservice The service to generate a server for.
 */
void t_cpp_generator::generate_service_skeleton(t_service* tservice) {
  string svcname = tservice->get_name();

  // Service implementation file includes
  string f_skeleton_name = get_out_dir()+svcname+"_server.skeleton.cpp";

  string ns = namespace_prefix(tservice->get_program()->get_namespace("cpp"));

  ofstream f_skeleton;
  f_skeleton.open(f_skeleton_name.c_str());
  f_skeleton <<
    "// This autogenerated skeleton file illustrates how to build a server." << endl <<
    "// You should copy it to another filename to avoid overwriting it." << endl <<
    endl <<
    "#include \"" << get_include_prefix(*get_program()) << svcname << ".h\"" << endl <<
    "#include <protocol/TBinaryProtocol.h>" << endl <<
    "#include <server/TSimpleServer.h>" << endl <<
    "#include <transport/TServerSocket.h>" << endl <<
    "#include <transport/TBufferTransports.h>" << endl <<
    endl <<
    "using namespace ::apache::thrift;" << endl <<
    "using namespace ::apache::thrift::protocol;" << endl <<
    "using namespace ::apache::thrift::transport;" << endl <<
    "using namespace ::apache::thrift::server;" << endl <<
    endl <<
    "using boost::shared_ptr;" << endl <<
    endl;

  if (!ns.empty()) {
    f_skeleton <<
      "using namespace " << string(ns, 0, ns.size()-2) << ";" << endl <<
      endl;
  }

  f_skeleton <<
    "class " << svcname << "Handler : virtual public " << svcname << "If {" << endl <<
    " public:" << endl;
  indent_up();
  f_skeleton <<
    indent() << svcname << "Handler() {" << endl <<
    indent() << "  // Your initialization goes here" << endl <<
    indent() << "}" << endl <<
    endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_skeleton <<
      indent() << function_signature(*f_iter, "") << " {" << endl <<
      indent() << "  // Your implementation goes here" << endl <<
      indent() << "  printf(\"" << (*f_iter)->get_name() << "\\n\");" << endl <<
      indent() << "}" << endl <<
      endl;
  }

  indent_down();
  f_skeleton <<
    "};" << endl <<
    endl;

  f_skeleton <<
    indent() << "int main(int argc, char **argv) {" << endl;
  indent_up();
  f_skeleton <<
    indent() << "int port = 9090;" << endl <<
    indent() << "shared_ptr<" << svcname << "Handler> handler(new " << svcname << "Handler());" << endl <<
    indent() << "shared_ptr<TProcessor> processor(new " << svcname << "Processor(handler));" << endl <<
    indent() << "shared_ptr<TServerTransport> serverTransport(new TServerSocket(port));" << endl <<
    indent() << "shared_ptr<TTransportFactory> transportFactory(new TBufferedTransportFactory());" << endl <<
    indent() << "shared_ptr<TProtocolFactory> protocolFactory(new TBinaryProtocolFactory());" << endl <<
    endl <<
    indent() << "TSimpleServer server(processor, serverTransport, transportFactory, protocolFactory);" << endl <<
    indent() << "server.serve();" << endl <<
    indent() << "return 0;" << endl;
  indent_down();
  f_skeleton <<
    "}" << endl <<
    endl;

  // Close the files
  f_skeleton.close();
}

/**
 * Deserializes a field of any type.
 */
void t_cpp_generator::generate_deserialize_field(ofstream& out,
                                                 t_field* tfield,
                                                 string prefix,
                                                 string suffix) {
  t_type* type = get_true_type(tfield->get_type());

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " +
      prefix + tfield->get_name();
  }

  string name = prefix + tfield->get_name() + suffix;

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out, (t_struct*)type, name);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type, name);
  } else if (type->is_base_type()) {
    indent(out) <<
      "xfer += iprot->";
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "compiler error: cannot serialize void field in a struct: " + name;
      break;
    case t_base_type::TYPE_STRING:
      if (((t_base_type*)type)->is_binary()) {
        out << "readBinary(" << name << ");";
      }
      else {
        out << "readString(" << name << ");";
      }
      break;
    case t_base_type::TYPE_BOOL:
      out << "readBool(" << name << ");";
      break;
    case t_base_type::TYPE_BYTE:
      out << "readByte(" << name << ");";
      break;
    case t_base_type::TYPE_I16:
      out << "readI16(" << name << ");";
      break;
    case t_base_type::TYPE_I32:
      out << "readI32(" << name << ");";
      break;
    case t_base_type::TYPE_I64:
      out << "readI64(" << name << ");";
      break;
    case t_base_type::TYPE_DOUBLE:
      out << "readDouble(" << name << ");";
      break;
    default:
      throw "compiler error: no C++ reader for base type " + t_base_type::t_base_name(tbase) + name;
    }
    out <<
      endl;
  } else if (type->is_enum()) {
    string t = tmp("ecast");
    out <<
      indent() << "int32_t " << t << ";" << endl <<
      indent() << "xfer += iprot->readI32(" << t << ");" << endl <<
      indent() << name << " = (" << type_name(type) << ")" << t << ";" << endl;
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type_name(type).c_str());
  }
}

/**
 * Generates an unserializer for a variable. This makes two key assumptions,
 * first that there is a const char* variable named data that points to the
 * buffer for deserialization, and that there is a variable protocol which
 * is a reference to a TProtocol serialization object.
 */
void t_cpp_generator::generate_deserialize_struct(ofstream& out,
                                                  t_struct* tstruct,
                                                  string prefix) {
  (void) tstruct;
  indent(out) <<
    "xfer += " << prefix << ".read(iprot);" << endl;
}

void t_cpp_generator::generate_deserialize_container(ofstream& out,
                                                     t_type* ttype,
                                                     string prefix) {
  scope_up(out);

  string size = tmp("_size");
  string ktype = tmp("_ktype");
  string vtype = tmp("_vtype");
  string etype = tmp("_etype");

  t_container* tcontainer = (t_container*)ttype;
  bool use_push = tcontainer->has_cpp_name();

  indent(out) <<
    prefix << ".clear();" << endl <<
    indent() << "uint32_t " << size << ";" << endl;

  // Declare variables, read header
  if (ttype->is_map()) {
    out <<
      indent() << "::apache::thrift::protocol::TType " << ktype << ";" << endl <<
      indent() << "::apache::thrift::protocol::TType " << vtype << ";" << endl <<
      indent() << "iprot->readMapBegin(" <<
                   ktype << ", " << vtype << ", " << size << ");" << endl;
  } else if (ttype->is_set()) {
    out <<
      indent() << "::apache::thrift::protocol::TType " << etype << ";" << endl <<
      indent() << "iprot->readSetBegin(" <<
                   etype << ", " << size << ");" << endl;
  } else if (ttype->is_list()) {
    out <<
      indent() << "::apache::thrift::protocol::TType " << etype << ";" << endl <<
      indent() << "iprot->readListBegin(" <<
      etype << ", " << size << ");" << endl;
    if (!use_push) {
      indent(out) << prefix << ".resize(" << size << ");" << endl;
    }
  }


  // For loop iterates over elements
  string i = tmp("_i");
  out <<
    indent() << "uint32_t " << i << ";" << endl <<
    indent() << "for (" << i << " = 0; " << i << " < " << size << "; ++" << i << ")" << endl;

    scope_up(out);

    if (ttype->is_map()) {
      generate_deserialize_map_element(out, (t_map*)ttype, prefix);
    } else if (ttype->is_set()) {
      generate_deserialize_set_element(out, (t_set*)ttype, prefix);
    } else if (ttype->is_list()) {
      generate_deserialize_list_element(out, (t_list*)ttype, prefix, use_push, i);
    }

    scope_down(out);

  // Read container end
  if (ttype->is_map()) {
    indent(out) << "iprot->readMapEnd();" << endl;
  } else if (ttype->is_set()) {
    indent(out) << "iprot->readSetEnd();" << endl;
  } else if (ttype->is_list()) {
    indent(out) << "iprot->readListEnd();" << endl;
  }

  scope_down(out);
}


/**
 * Generates code to deserialize a map
 */
void t_cpp_generator::generate_deserialize_map_element(ofstream& out,
                                                       t_map* tmap,
                                                       string prefix) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_field fkey(tmap->get_key_type(), key);
  t_field fval(tmap->get_val_type(), val);

  out <<
    indent() << declare_field(&fkey) << endl;

  generate_deserialize_field(out, &fkey);
  indent(out) <<
    declare_field(&fval, false, false, false, true) << " = " <<
    prefix << "[" << key << "];" << endl;

  generate_deserialize_field(out, &fval);
}

void t_cpp_generator::generate_deserialize_set_element(ofstream& out,
                                                       t_set* tset,
                                                       string prefix) {
  string elem = tmp("_elem");
  t_field felem(tset->get_elem_type(), elem);

  indent(out) <<
    declare_field(&felem) << endl;

  generate_deserialize_field(out, &felem);

  indent(out) <<
    prefix << ".insert(" << elem << ");" << endl;
}

void t_cpp_generator::generate_deserialize_list_element(ofstream& out,
                                                        t_list* tlist,
                                                        string prefix,
                                                        bool use_push,
                                                        string index) {
  if (use_push) {
    string elem = tmp("_elem");
    t_field felem(tlist->get_elem_type(), elem);
    indent(out) << declare_field(&felem) << endl;
    generate_deserialize_field(out, &felem);
    indent(out) << prefix << ".push_back(" << elem << ");" << endl;
  } else {
    t_field felem(tlist->get_elem_type(), prefix + "[" + index + "]");
    generate_deserialize_field(out, &felem);
  }
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param prefix Name to prepend to field name
 */
void t_cpp_generator::generate_serialize_field(ofstream& out,
                                               t_field* tfield,
                                               string prefix,
                                               string suffix) {
  t_type* type = get_true_type(tfield->get_type());

  string name = prefix + tfield->get_name() + suffix;

  // Do nothing for void types
  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " + name;
  }



  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out,
                              (t_struct*)type,
                              name);
  } else if (type->is_container()) {
    generate_serialize_container(out, type, name);
  } else if (type->is_base_type() || type->is_enum()) {

    indent(out) <<
      "xfer += oprot->";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + name;
        break;
      case t_base_type::TYPE_STRING:
        if (((t_base_type*)type)->is_binary()) {
          out << "writeBinary(" << name << ");";
        }
        else {
          out << "writeString(" << name << ");";
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool(" << name << ");";
        break;
      case t_base_type::TYPE_BYTE:
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
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble(" << name << ");";
        break;
      default:
        throw "compiler error: no C++ writer for base type " + t_base_type::t_base_name(tbase) + name;
      }
    } else if (type->is_enum()) {
      out << "writeI32((int32_t)" << name << ");";
    }
    out << endl;
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s' TYPE '%s'\n",
           name.c_str(),
           type_name(type).c_str());
  }
}

/**
 * Serializes all the members of a struct.
 *
 * @param tstruct The struct to serialize
 * @param prefix  String prefix to attach to all fields
 */
void t_cpp_generator::generate_serialize_struct(ofstream& out,
                                                t_struct* tstruct,
                                                string prefix) {
  (void) tstruct;
  indent(out) <<
    "xfer += " << prefix << ".write(oprot);" << endl;
}

void t_cpp_generator::generate_serialize_container(ofstream& out,
                                                   t_type* ttype,
                                                   string prefix) {
  scope_up(out);

  if (ttype->is_map()) {
    indent(out) <<
      "xfer += oprot->writeMapBegin(" <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << ", " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << ", " <<
      "static_cast<uint32_t>(" << prefix << ".size()));" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "xfer += oprot->writeSetBegin(" <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << ", " <<
      "static_cast<uint32_t>(" << prefix << ".size()));" << endl;
  } else if (ttype->is_list()) {
    indent(out) <<
      "xfer += oprot->writeListBegin(" <<
      type_to_enum(((t_list*)ttype)->get_elem_type()) << ", " <<
      "static_cast<uint32_t>(" << prefix << ".size()));" << endl;
  }

  string iter = tmp("_iter");
  out <<
    indent() << type_name(ttype) << "::const_iterator " << iter << ";" << endl <<
    indent() << "for (" << iter << " = " << prefix  << ".begin(); " << iter << " != " << prefix << ".end(); ++" << iter << ")" << endl;
  scope_up(out);
    if (ttype->is_map()) {
      generate_serialize_map_element(out, (t_map*)ttype, iter);
    } else if (ttype->is_set()) {
      generate_serialize_set_element(out, (t_set*)ttype, iter);
    } else if (ttype->is_list()) {
      generate_serialize_list_element(out, (t_list*)ttype, iter);
    }
  scope_down(out);

  if (ttype->is_map()) {
    indent(out) <<
      "xfer += oprot->writeMapEnd();" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "xfer += oprot->writeSetEnd();" << endl;
  } else if (ttype->is_list()) {
    indent(out) <<
      "xfer += oprot->writeListEnd();" << endl;
  }

  scope_down(out);
}

/**
 * Serializes the members of a map.
 *
 */
void t_cpp_generator::generate_serialize_map_element(ofstream& out,
                                                     t_map* tmap,
                                                     string iter) {
  t_field kfield(tmap->get_key_type(), iter + "->first");
  generate_serialize_field(out, &kfield, "");

  t_field vfield(tmap->get_val_type(), iter + "->second");
  generate_serialize_field(out, &vfield, "");
}

/**
 * Serializes the members of a set.
 */
void t_cpp_generator::generate_serialize_set_element(ofstream& out,
                                                     t_set* tset,
                                                     string iter) {
  t_field efield(tset->get_elem_type(), "(*" + iter + ")");
  generate_serialize_field(out, &efield, "");
}

/**
 * Serializes the members of a list.
 */
void t_cpp_generator::generate_serialize_list_element(ofstream& out,
                                                      t_list* tlist,
                                                      string iter) {
  t_field efield(tlist->get_elem_type(), "(*" + iter + ")");
  generate_serialize_field(out, &efield, "");
}

/**
 * Makes a :: prefix for a namespace
 *
 * @param ns The namespace, w/ periods in it
 * @return Namespaces
 */
string t_cpp_generator::namespace_prefix(string ns) {
  if (ns.size() == 0) {
    return "";
  }
  string result = "";
  string::size_type loc;
  while ((loc = ns.find(".")) != string::npos) {
    result += ns.substr(0, loc);
    result += "::";
    ns = ns.substr(loc+1);
  }
  if (ns.size() > 0) {
    result += ns + "::";
  }
  return result;
}

/**
 * Opens namespace.
 *
 * @param ns The namespace, w/ periods in it
 * @return Namespaces
 */
string t_cpp_generator::namespace_open(string ns) {
  if (ns.size() == 0) {
    return "";
  }
  string result = "";
  string separator = "";
  string::size_type loc;
  while ((loc = ns.find(".")) != string::npos) {
    result += separator;
    result += "namespace ";
    result += ns.substr(0, loc);
    result += " {";
    separator = " ";
    ns = ns.substr(loc+1);
  }
  if (ns.size() > 0) {
    result += separator + "namespace " + ns + " {";
  }
  return result;
}

/**
 * Closes namespace.
 *
 * @param ns The namespace, w/ periods in it
 * @return Namespaces
 */
string t_cpp_generator::namespace_close(string ns) {
  if (ns.size() == 0) {
    return "";
  }
  string result = "}";
  string::size_type loc;
  while ((loc = ns.find(".")) != string::npos) {
    result += "}";
    ns = ns.substr(loc+1);
  }
  result += " // namespace";
  return result;
}

/**
 * Returns a C++ type name
 *
 * @param ttype The type
 * @return String of the type name, i.e. std::set<type>
 */
string t_cpp_generator::type_name(t_type* ttype, bool in_typedef, bool arg) {
  if (ttype->is_base_type()) {
    string bname = base_type_name(((t_base_type*)ttype)->get_base());
    if (!arg) {
      return bname;
    }

    if (((t_base_type*)ttype)->get_base() == t_base_type::TYPE_STRING) {
      return "const " + bname + "&";
    } else {
      return "const " + bname;
    }
  }

  // Check for a custom overloaded C++ name
  if (ttype->is_container()) {
    string cname;

    t_container* tcontainer = (t_container*) ttype;
    if (tcontainer->has_cpp_name()) {
      cname = tcontainer->get_cpp_name();
    } else if (ttype->is_map()) {
      t_map* tmap = (t_map*) ttype;
      cname = "std::map<" +
        type_name(tmap->get_key_type(), in_typedef) + ", " +
        type_name(tmap->get_val_type(), in_typedef) + "> ";
    } else if (ttype->is_set()) {
      t_set* tset = (t_set*) ttype;
      cname = "std::set<" + type_name(tset->get_elem_type(), in_typedef) + "> ";
    } else if (ttype->is_list()) {
      t_list* tlist = (t_list*) ttype;
      cname = "std::vector<" + type_name(tlist->get_elem_type(), in_typedef) + "> ";
    }

    if (arg) {
      return "const " + cname + "&";
    } else {
      return cname;
    }
  }

  string class_prefix;
  if (in_typedef && (ttype->is_struct() || ttype->is_xception())) {
    class_prefix = "class ";
  }

  // Check if it needs to be namespaced
  string pname;
  t_program* program = ttype->get_program();
  if (program != NULL && program != program_) {
    pname =
      class_prefix +
      namespace_prefix(program->get_namespace("cpp")) +
      ttype->get_name();
  } else {
    pname = class_prefix + ttype->get_name();
  }

  if (ttype->is_enum() && !gen_pure_enums_) {
    pname += "::type";
  }

  if (arg) {
    if (is_complex_type(ttype)) {
      return "const " + pname + "&";
    } else {
      return "const " + pname;
    }
  } else {
    return pname;
  }
}

/**
 * Returns the C++ type that corresponds to the thrift type.
 *
 * @param tbase The base type
 * @return Explicit C++ type, i.e. "int32_t"
 */
string t_cpp_generator::base_type_name(t_base_type::t_base tbase) {
  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "void";
  case t_base_type::TYPE_STRING:
    return "std::string";
  case t_base_type::TYPE_BOOL:
    return "bool";
  case t_base_type::TYPE_BYTE:
    return "int8_t";
  case t_base_type::TYPE_I16:
    return "int16_t";
  case t_base_type::TYPE_I32:
    return "int32_t";
  case t_base_type::TYPE_I64:
    return "int64_t";
  case t_base_type::TYPE_DOUBLE:
    return "double";
  default:
    throw "compiler error: no C++ base type name for base type " + t_base_type::t_base_name(tbase);
  }
}

/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param ttype The type
 * @return Field declaration, i.e. int x = 0;
 */
string t_cpp_generator::declare_field(t_field* tfield, bool init, bool pointer, bool constant, bool reference) {
  // TODO(mcslee): do we ever need to initialize the field?
  string result = "";
  if (constant) {
    result += "const ";
  }
  result += type_name(tfield->get_type());
  if (pointer) {
    result += "*";
  }
  if (reference) {
    result += "&";
  }
  result += " " + tfield->get_name();
  if (init) {
    t_type* type = get_true_type(tfield->get_type());

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        break;
      case t_base_type::TYPE_STRING:
        result += " = \"\"";
        break;
      case t_base_type::TYPE_BOOL:
        result += " = false";
        break;
      case t_base_type::TYPE_BYTE:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        result += " = 0";
        break;
      case t_base_type::TYPE_DOUBLE:
        result += " = (double)0";
        break;
      default:
        throw "compiler error: no C++ initializer for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      result += " = (" + type_name(type) + ")0";
    }
  }
  if (!reference) {
    result += ";";
  }
  return result;
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_cpp_generator::function_signature(t_function* tfunction,
                                           string style,
                                           string prefix,
                                           bool name_params) {
  t_type* ttype = tfunction->get_returntype();
  t_struct* arglist = tfunction->get_arglist();
  bool has_xceptions = !tfunction->get_xceptions()->get_members().empty();

  if (style == "") {
    if (is_complex_type(ttype)) {
      return
        "void " + prefix + tfunction->get_name() +
        "(" + type_name(ttype) + (name_params ? "& _return" : "& /* _return */") +
        argument_list(arglist, name_params, true) + ")";
    } else {
      return
        type_name(ttype) + " " + prefix + tfunction->get_name() +
        "(" + argument_list(arglist, name_params) + ")";
    }
  } else if (style.substr(0,3) == "Cob") {
    string cob_type;
    string exn_cob;
    if (style == "CobCl") {
      cob_type = "(" + service_name_ + "CobClient";
      if (gen_templates_) {
        cob_type += "T<Protocol_>";
      }
      cob_type += "* client)";
    } else if (style =="CobSv") {
      cob_type = (ttype->is_void()
                  ? "()"
                  : ("(" + type_name(ttype) + " const& _return)"));
      if (has_xceptions) {
        exn_cob = ", std::tr1::function<void(::apache::thrift::TDelayedException* _throw)> /* exn_cob */";
      }
    } else {
      throw "UNKNOWN STYLE";
    }

    return
      "void " + prefix + tfunction->get_name() +
      "(std::tr1::function<void" + cob_type + "> cob" + exn_cob +
      argument_list(arglist, name_params, true) + ")";
  } else {
    throw "UNKNOWN STYLE";
  }
}

/**
 * Renders a field list
 *
 * @param tstruct The struct definition
 * @return Comma sepearated list of all field names in that struct
 */
string t_cpp_generator::argument_list(t_struct* tstruct, bool name_params, bool start_comma) {
  string result = "";

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = !start_comma;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      result += ", ";
    }
    result += type_name((*f_iter)->get_type(), false, true) + " " +
      (name_params ? (*f_iter)->get_name() : "/* " + (*f_iter)->get_name() + " */");
  }
  return result;
}

/**
 * Converts the parse type to a C++ enum string for the given type.
 *
 * @param type Thrift Type
 * @return String of C++ code to definition of that type constant
 */
string t_cpp_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "::apache::thrift::protocol::T_STRING";
    case t_base_type::TYPE_BOOL:
      return "::apache::thrift::protocol::T_BOOL";
    case t_base_type::TYPE_BYTE:
      return "::apache::thrift::protocol::T_BYTE";
    case t_base_type::TYPE_I16:
      return "::apache::thrift::protocol::T_I16";
    case t_base_type::TYPE_I32:
      return "::apache::thrift::protocol::T_I32";
    case t_base_type::TYPE_I64:
      return "::apache::thrift::protocol::T_I64";
    case t_base_type::TYPE_DOUBLE:
      return "::apache::thrift::protocol::T_DOUBLE";
    }
  } else if (type->is_enum()) {
    return "::apache::thrift::protocol::T_I32";
  } else if (type->is_struct()) {
    return "::apache::thrift::protocol::T_STRUCT";
  } else if (type->is_xception()) {
    return "::apache::thrift::protocol::T_STRUCT";
  } else if (type->is_map()) {
    return "::apache::thrift::protocol::T_MAP";
  } else if (type->is_set()) {
    return "::apache::thrift::protocol::T_SET";
  } else if (type->is_list()) {
    return "::apache::thrift::protocol::T_LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

/**
 * Returns the symbol name of the local reflection of a type.
 */
string t_cpp_generator::local_reflection_name(const char* prefix, t_type* ttype, bool external) {
  ttype = get_true_type(ttype);

  // We have to use the program name as part of the identifier because
  // if two thrift "programs" are compiled into one actual program
  // you would get a symbol collision if they both defined list<i32>.
  // trlo = Thrift Reflection LOcal.
  string prog;
  string name;
  string nspace;

  // TODO(dreiss): Would it be better to pregenerate the base types
  //               and put them in Thrift.{h,cpp} ?

  if (ttype->is_base_type()) {
    prog = program_->get_name();
    name = ttype->get_ascii_fingerprint();
  } else if (ttype->is_enum()) {
    assert(ttype->get_program() != NULL);
    prog = ttype->get_program()->get_name();
    name = ttype->get_ascii_fingerprint();
  } else if (ttype->is_container()) {
    prog = program_->get_name();
    name = ttype->get_ascii_fingerprint();
  } else {
    assert(ttype->is_struct() || ttype->is_xception());
    assert(ttype->get_program() != NULL);
    prog = ttype->get_program()->get_name();
    name = ttype->get_ascii_fingerprint();
  }

  if (external &&
      ttype->get_program() != NULL &&
      ttype->get_program() != program_) {
    nspace = namespace_prefix(ttype->get_program()->get_namespace("cpp"));
  }

  return nspace + "trlo_" + prefix + "_" + prog + "_" + name;
}

string t_cpp_generator::get_include_prefix(const t_program& program) const {
  string include_prefix = program.get_include_prefix();
  if (!use_include_prefix_ ||
      (include_prefix.size() > 0 && include_prefix[0] == '/')) {
    // if flag is turned off or this is absolute path, return empty prefix
    return "";
  }

  string::size_type last_slash = string::npos;
  if ((last_slash = include_prefix.rfind("/")) != string::npos) {
    return include_prefix.substr(0, last_slash) + "/" + out_dir_base_ + "/";
  }

  return "";
}


THRIFT_REGISTER_GENERATOR(cpp, "C++",
"    cob_style:       Generate \"Continuation OBject\"-style classes.\n"
"    no_client_completion:\n"
"                     Omit calls to completion__() in CobClient class.\n"
"    templates:       Generate templatized reader/writer methods.\n"
"    pure_enums:      Generate pure enums instead of wrapper classes.\n"
"    dense:           Generate type specifications for the dense protocol.\n"
"    include_prefix:  Use full include paths in generated files.\n"
)

