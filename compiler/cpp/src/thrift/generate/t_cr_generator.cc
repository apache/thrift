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

#include <algorithm>
#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <sstream>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "thrift/generate/t_oop_generator.h"
#include "thrift/platform.h"
#include "thrift/version.h"

using std::map;
using std::ofstream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const string endl = "\n"; // avoid ostream << std::endl flushes

/**
 * A subclass of std::ofstream that includes indenting functionality.
 */
class t_cr_ofstream : public std::ofstream {
private:
  int indent_;

public:
  t_cr_ofstream() : std::ofstream(), indent_(0) {}
  explicit t_cr_ofstream(const char* filename,
                         ios_base::openmode mode = ios_base::out,
                         int indent = 0)
    : std::ofstream(filename, mode), indent_(indent) {}

  t_cr_ofstream& indent() {
    for (int i = 0; i < indent_; ++i) {
      *this << "  ";
    }
    return *this;
  }

  void indent_up() { ++indent_; }

  void indent_down() { --indent_; }
};

class t_cr_generator : public t_oop_generator {
public:
  t_cr_generator(t_program* program,
                 const std::map<std::string, std::string>& parsed_options,
                 const std::string&)
    : t_oop_generator(program) {
    map<string, string>::const_iterator iter;
    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if (iter->first.compare("no-skeleton") == 0) {
        no_skeleton_ = true;
      }
    }

    out_dir_base_ = "gen-cr";
  }

  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;

  void generate_typedef(t_typedef*) override;
  void generate_enum(t_enum* tenum) override;
  void generate_const(t_const* tconst) override;
  void generate_struct(t_struct* tstruct) override;
  void generate_forward_declaration(t_struct* tstruct) override;
  void generate_union(t_struct* tunion);
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;

  t_cr_ofstream& render_const_value(t_cr_ofstream& out, t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_cr_struct(t_cr_ofstream& out, t_struct* tstruct, bool is_exception, bool is_helper);
  void generate_cr_struct_initializer(t_cr_ofstream& out, t_struct* tstruct);
  void generate_cr_union(t_cr_ofstream& out, t_struct* tstruct, bool is_exception);
  void generate_cr_function_helpers(t_function* tfunction);
  void generate_cr_simple_constructor(t_cr_ofstream& out, t_struct* tstruct);
  void generate_cr_exception_initializer(t_cr_ofstream& out, t_struct* tstruct);
  void generate_serialize_struct(t_cr_ofstream& out, t_struct* tstruct, string prefix);
  void generate_serialize_union(t_cr_ofstream& out, t_struct* tunion);
  void generate_deserialize_union(t_cr_ofstream& out, t_struct* tunion);
  void generate_field_defns(t_cr_ofstream& out, t_struct* tstruct);
  void generate_field_data(t_cr_ofstream& out,
                           t_type* field_type,
                           t_const_value* value,
                           const std::string& field_name,
                           t_field::e_req req);
  void generate_cr_struct_serialize(t_cr_ofstream& out, t_struct* tstruct);
  void generate_cr_struct_deserialize(t_cr_ofstream& out, t_struct* tstruct);
  void generate_union_field_data(t_cr_ofstream& out,
                                 t_type* field_type,
                                 const std::string& field_name);

  /**
   * Service-level generation functions
   */

  void generate_service_helpers(t_service* tservice);
  void generate_service_interface(t_service* tservice);
  void generate_service_client(t_service* tservice);
  void generate_service_server(t_service* tservice);
  void generate_process_function(t_service* tservice, t_function* tfunction);
  void generate_service_skeleton(t_service* tservice);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field(t_cr_ofstream& out,
                                  t_field* tfield,
                                  std::string class_name,
                                  const std::string& isset_check = "");

  void generate_serialize_field(t_cr_ofstream& out, t_field* tfield);

  void generate_crdoc(std::ostream& out, t_doc* tdoc);

  /**
   * Helper rendering functions
   */

  std::string render_crystal_type(t_type* ttype,
                                     bool base = false,
                                     bool optional = false);
  std::string cr_underscore(std::string in);
  std::string fix_name_conflict(std::string in);
  std::string generate_cr_struct_required_fields(t_struct* tstruct);
  std::string cr_autogen_comment();
  std::string render_require_thrift();
  std::string render_includes();
  std::string render_property_type(t_field::e_req req, int key, bool name_altered, const std::string& orig_name = "");
  std::string type_name(const t_type* ttype);
  std::string full_type_name(const t_type* ttype);
  std::string function_signature(t_function* tfunction, std::string prefix = "");
  std::string argument_list(t_struct* tstruct);
  std::string cr_namespace_to_path_prefix(std::string cr_namespace);

  std::vector<std::string> crystal_modules(const t_program* p) {
    std::string ns = p->get_namespace("cr");
    std::vector<std::string> modules;
    if (ns.empty()) {
      return modules;
    }

    std::string::iterator pos = ns.begin();
    while (true) {
      std::string::iterator delim = std::find(pos, ns.end(), '.');
      modules.push_back(capitalize(std::string(pos, delim)));
      pos = delim;
      if (pos == ns.end()) {
        break;
      }
      ++pos;
    }

    return modules;
  }

  std::string begin_namespace(std::vector<std::string>);
  std::string end_namespace(std::vector<std::string>);

private:
  /**
   * File streams
   */

  t_cr_ofstream f_types_;
  t_cr_ofstream f_consts_;
  t_cr_ofstream f_service_;

  std::string namespace_dir_;

  /**
   * save service namespace opening as a string to add where ever we need
   */
  std::string module_begin_;
  /**
   * save service namespace closure as a string to write whenever we need it
   */
  std::string module_end_;

  bool no_skeleton_;
};

/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_cr_generator::init_generator() {
  string subdir = get_out_dir();

  // Make output directory
  MKDIR(subdir.c_str());

  namespace_dir_ = subdir;

  // Make output file
  string f_types_name = namespace_dir_ + underscore(program_name_) + "_types.cr";
  f_types_.open(f_types_name.c_str());

  string f_consts_name = namespace_dir_ + underscore(program_name_) + "_constants.cr";
  f_consts_.open(f_consts_name.c_str());

  module_begin_ = begin_namespace(crystal_modules(program_));
  module_end_ = end_namespace(crystal_modules(program_));
  // Print header
  f_types_ << cr_autogen_comment() << endl << render_require_thrift() << render_includes() << endl
           << module_begin_;

  f_consts_ << cr_autogen_comment() << endl
            << render_require_thrift() << "require \"./"
            << underscore(program_name_) << "_types\"" << endl
            << endl
            << module_begin_;
}

string t_cr_generator::render_includes() {
  const vector<t_program*>& includes = program_->get_includes();
  string result = "";
  for (auto include : includes) {
    t_program* included = include;
    string included_name = included->get_name();
    result
        += "require \"./" + underscore(included_name) + "_types.cr\"" + endl;
  }
  if (includes.size() > 0) {
    result += endl;
  }
  return result;
}

/**
 * Autogen'd comment
 */
string t_cr_generator::cr_autogen_comment() {
  return std::string("#\n") + "# Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")\n"
         + "#\n" + "# DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n" + "#\n";
}

/**
 * Closes the type files
 */
void t_cr_generator::close_generator() {
  // Close types file
  f_types_ << module_end_;
  f_consts_ << module_end_;
  f_types_.close();
  f_consts_.close();
}

/**
 * Generates a typedef. alias is the equivalent of a typedef
 */
void t_cr_generator::generate_typedef(t_typedef* ttypedef) {
  f_types_ << endl;
  generate_crdoc(f_types_, ttypedef);
  indent(f_types_) << "alias " << ttypedef->get_symbolic() << " = " << render_crystal_type(ttypedef->get_type(), false, false) << endl;
}

/**
 * Generates code for an enumerated type.
 */
void t_cr_generator::generate_enum(t_enum* tenum) {
  f_types_ << endl;
  indent(f_types_) << "enum " << capitalize(tenum->get_name()) << endl;
  indent_up();

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();

    string name = capitalize(lowercase((*c_iter)->get_name()));

    generate_crdoc(f_types_, *c_iter);
    indent(f_types_) << name << " = " << value << endl;
  }
  indent_down();
  indent(f_types_) << "end" << endl;
}

/**
 * Generate a constant value
 */
void t_cr_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = tconst->get_name();
  t_const_value* value = tconst->get_value();

  name = uppercase(cr_underscore(name));

  generate_crdoc(f_consts_, tconst);
  indent(f_consts_) << name << " = ";
  render_const_value(f_consts_, type, value) << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
t_cr_ofstream& t_cr_generator::render_const_value(t_cr_ofstream& out,
                                                  t_type* type,
                                                  t_const_value* value) {
  type = get_true_type(type);
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      out << "%q(" << get_escaped_string(value) << ')';
      if (type->is_binary()) {
        out << ".to_slice";
      }
      break;
    case t_base_type::TYPE_BOOL:
      out << (value->get_integer() > 0 ? "true" : "false");
      break;
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      out << value->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        out << value->get_integer();
      } else {
        out << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    t_enum* as_enum = (t_enum*)type;
    out << as_enum->get_name() << "::" << capitalize(lowercase(as_enum->get_constant_by_value(value->get_integer())->get_name()));
  } else if (type->is_struct() || type->is_xception()) {
    out << full_type_name(type) << ".new(" << endl;
    indent_up();
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
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
      indent(out);
      render_const_value(out, g_type_string, v_iter->first) << " => ";
      render_const_value(out, field_type, v_iter->second) << "," << endl;
    }
    indent_down();
    indent(out) << ")";
  } else if (type->is_map()) {
    bool first = true;
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    out << "{ ";
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (!first) {
        out << ", ";
      } else {
        first = false;
      }
      render_const_value(out, ktype, v_iter->first) << " => ";
      render_const_value(out, vtype, v_iter->second);
    }
    out << " }";
    if (value->get_map().empty()) {
      out << " of " << render_crystal_type(ktype) << " => " << render_crystal_type(vtype);
    }
  } else if (type->is_list() || type->is_set()) {
    bool first = true;
    t_type* etype;
    if (type->is_list()) {
      etype = ((t_list*)type)->get_elem_type();
    } else {
      etype = ((t_set*)type)->get_elem_type();
    }
    out << render_crystal_type(type);
    if (value->get_list().empty()) {
      out << ".new";
    } else {
      out << "{ ";
      const vector<t_const_value*>& val = value->get_list();
      vector<t_const_value*>::const_iterator v_iter;
      for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
        if (!first) {
          out << ", ";
        } else {
          first = false;
        }
        render_const_value(out, etype, *v_iter);
      }
      out << " }";
    }
  } else {
    throw "CANNOT GENERATE CONSTANT FOR TYPE: " + type->get_name();
  }
  return out;
}

/**
 * Generates a Crystal struct
 */
void t_cr_generator::generate_struct(t_struct* tstruct) {
  if (tstruct->is_union()) {
    generate_cr_union(f_types_, tstruct, false);
  } else {
    generate_cr_struct(f_types_, tstruct, false, false);
  }
}

/**
 * Generates the "forward declarations" for Crystal structs.
 * These are simply a declaration of each class with proper inheritance.
 * The rest of the struct is still generated in generate_struct as has
 * always been the case. These declarations allow thrift to generate valid
 * Crystal in cases where thrift structs rely on recursive definitions.
 */
void t_cr_generator::generate_forward_declaration(t_struct* tstruct) {
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 */
void t_cr_generator::generate_xception(t_struct* txception) {

  auto& out = f_types_;
  out << endl;
  indent(out) << "class " << type_name(txception) << " < ::Thrift::Exception" << endl;
  indent_up();
  indent(out) << "include ::Thrift::Struct" << endl
              << indent() << "include ::Thrift::Struct::ExceptionAdapter" << endl;

  const std::vector<t_field*>& xception_fields = txception->get_members();
  std::vector<t_field*>::const_iterator f_iter;

  generate_field_defns(out, txception);
  generate_cr_struct_initializer(out, txception);
  indent_down();
  indent(out) << "end" << endl;
}

/**
 * Generates a crystal struct
 */
void t_cr_generator::generate_cr_struct(t_cr_ofstream& out,
                                        t_struct* tstruct,
                                        bool is_exception = false,
                                        bool is_helper = false) {
  out << endl;
  generate_crdoc(out, tstruct);
  indent(out) << "class " << type_name(tstruct) << endl;
  indent_up();
  indent(out) << "include ::Thrift::Struct" << endl;

  const std::vector<t_field*>& fields = tstruct->get_members();
  std::vector<t_field*>::const_iterator f_iter;

  generate_field_defns(out, tstruct);
  generate_cr_struct_initializer(out, tstruct);
  indent_down();
  indent(out) << "end" << endl;
}

/**
 * Generates an initializer for a struct
 */
void t_cr_generator::generate_cr_struct_initializer(t_cr_ofstream& out, t_struct* tstruct)
{
  out << endl;
    // we take a copy because we need to sort these by requiredness
  std::vector<t_field*> members = tstruct->get_members();
  std::vector<t_field*>::iterator required_wo_default_iter = std::partition(std::begin(members), std::end(members), [](const t_field* tfield) {
    return tfield->get_req() == t_field::T_REQUIRED && tfield->get_value() == nullptr;
  });
  std::vector<t_field*>::const_iterator m_iter;
  bool first = true;
  indent(out) << "def initialize(";
  // required fields come first in initialization order
  for (m_iter = std::cbegin(members); m_iter != required_wo_default_iter; ++m_iter) {
    if (first) {
      first = false;
    } else {
      out << ", ";
    }
    out << "@" << (*m_iter)->get_name();
  }

  if (required_wo_default_iter != std::end(members)) {
    if (!first) {
      out << ", ";
    }
    // for crystal we use positional args for required value without default args
    // and use keyword args for all fields that are not required or required fields with default arguments
    out << "*";

    //non-required fields with defaults come after
    for (m_iter = required_wo_default_iter; m_iter != std::cend(members); ++m_iter) {
      out << ", " << fix_name_conflict((*m_iter)->get_name()) << " = nil";
    }
  }
  out << ")" << endl;
  indent_up();

  first = true;
  for (m_iter = required_wo_default_iter; m_iter != std::cend(members); ++m_iter) {
    // basically we can lump in required with default values and optionals
    if (first) {
      first = false;
    } else {
      out << endl;
    }
    indent(out) << fix_name_conflict((*m_iter)->get_name()) << ".try do |" << fix_name_conflict((*m_iter)->get_name()) << "|" << endl;
    indent_up();
    indent(out) << "@" << fix_name_conflict((*m_iter)->get_name()) << " = " << fix_name_conflict((*m_iter)->get_name()) << endl;
    // don't generate isset for required fields
    if ((*m_iter)->get_req() == t_field::T_OPTIONAL) {
      indent(out) << "@__isset." << fix_name_conflict((*m_iter)->get_name()) << " = true" << endl;
    }
    indent_down();
    indent(out) << "end" << endl;
  }
  indent_down();

  indent(out) << "end" << endl;
}

/**
 * Generates a Crystal union
 */
void t_cr_generator::generate_cr_union(t_cr_ofstream& out,
                                       t_struct* tstruct,
                                       bool is_exception = false) {
  out << endl;
  (void)is_exception;
  generate_crdoc(out, tstruct);
  indent(out) << "class " << type_name(tstruct) << endl;
  indent_up();
  indent(out) << "include ::Thrift::Union" << endl;

  generate_field_defns(out, tstruct);
  indent_down();
  indent(out) << "end" << endl;
}

/**
 * Takes Thrift type and converts it to associated Crystal type
 *
 * @return std::string
 */
std::string t_cr_generator::render_crystal_type(t_type* ttype,
                                                   bool base,
                                                   bool optional) {
  std::string rendered_type = "";
  if (ttype->is_typedef()) {
    rendered_type += full_type_name(ttype);
    return rendered_type;
  }
  ttype = get_true_type(ttype);
  if (ttype->is_base_type()) {
    t_base_type::t_base base = ((t_base_type*)ttype)->get_base();
    switch (base) {
    case t_base_type::TYPE_BOOL:
      rendered_type += "Bool";
      break;
    case t_base_type::TYPE_STRING:
      if (ttype->is_binary()) {
        rendered_type += "Bytes";
      } else {
        rendered_type += "String";
      }
      break;
    case t_base_type::TYPE_VOID:
      rendered_type += "Nil";
      break;
    case t_base_type::TYPE_UUID:
      rendered_type += "UUID";
      break;
    case t_base_type::TYPE_I8:
      rendered_type += "Int8";
      break;
    case t_base_type::TYPE_I16:
      rendered_type += "Int16";
      break;
    case t_base_type::TYPE_I32:
      rendered_type += "Int32";
      break;
    case t_base_type::TYPE_I64:
      rendered_type += "Int64";
      break;
    case t_base_type::TYPE_DOUBLE:
      rendered_type += "Float64";
      break;
    default:
      break;
    }
  } else if (ttype->is_enum()) {
    rendered_type += full_type_name(ttype);
  } else if (ttype->is_xception() || ttype->is_struct()) {
    rendered_type += full_type_name(ttype);
  } else if (ttype->is_map()) {
    rendered_type += "Hash";
    if (base) {
      return rendered_type;
    }
    rendered_type += "(" + render_crystal_type(((t_map*)ttype)->get_key_type()) + ", " + render_crystal_type(((t_map*)ttype)->get_val_type()) + ")";
  } else if (ttype->is_list() || ttype->is_set()) {
    t_type* ele_type;
    if (ttype->is_list()) {
      ele_type = ((t_list*)ttype)->get_elem_type();
      rendered_type += "Array";
      if (base) {
        return rendered_type;
      }
      rendered_type += "(" + render_crystal_type(ele_type) + ")";
    } else {
      ele_type = ((t_set*)ttype)->get_elem_type();
      rendered_type += "Set";
      if (base) {
        return rendered_type;
      }
      rendered_type += "(" + render_crystal_type(ele_type) + ")";
    }
  }
  if (optional) {
    rendered_type += "?";
  }

  return rendered_type;
}

/**
 * Generates properties of class, adds metadata to properties
 *
 * @param out Ofstream to write field definitions to
 * @param tstruct definition of struct
 */
void t_cr_generator::generate_field_defns(t_cr_ofstream& out, t_struct* tstruct) {
  out << endl;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    first = false;
    generate_crdoc(out, *f_iter);
    std::string cr_safe_name = fix_name_conflict((*f_iter)->get_name());
    indent(out) << "@[SerialOpts(" << render_property_type((*f_iter)->get_req(), (*f_iter)->get_key(), cr_safe_name != (*f_iter)->get_name(), (*f_iter)->get_name()) << ")]" << endl;
    if (tstruct->is_union()) {
      indent(out) << "union_property ";
      generate_union_field_data(out, (*f_iter)->get_type(), cr_safe_name);
    } else if(tstruct->is_xception()) {
      indent(out) << "xception_getter ";
      generate_field_data(out, (*f_iter)->get_type(), (*f_iter)->get_value(), cr_safe_name,
                          (*f_iter)->get_req());
    } else {
      indent(out) << "struct_property ";
      generate_field_data(out, (*f_iter)->get_type(), (*f_iter)->get_value(), cr_safe_name,
                          (*f_iter)->get_req());
    }
    out << endl;
  }
}

/**
 * Generate a crystal compliant property declaration
 */
void t_cr_generator::generate_field_data(t_cr_ofstream& out,
                                         t_type*  field_type,
                                         t_const_value* value,
                                         const std::string& field_name = "",
                                         t_field::e_req req = t_field::T_OPTIONAL) {
  out << field_name << " : " << render_crystal_type(get_true_type(field_type), false, req != t_field::T_REQUIRED);
  if (value) {
    out << " = ";
    render_const_value(out, field_type, value);
  }
}

/**
 * Generate crystal compliant property for a thrift union
 */
void t_cr_generator::generate_union_field_data(t_cr_ofstream& out,
                                               t_type* field_type,
                                               const std::string& field_name) {
  out << field_name << " : " << render_crystal_type(get_true_type(field_type));
}

/**
 * Generates namespace string and modifies indent level
 *
 * @param modules Modules for service
 * @return std::string
 */
std::string t_cr_generator::begin_namespace(vector<std::string> modules) {
  std::string scope = "";
  for (auto& module : modules) {
    scope += indent() + "module " + module + endl;
    indent_up();
  }
  return scope;
}

/**
 * Generates closures and maintains indent level
 *
 * @param modules Modules for service
 * @return std::string
 */
std::string t_cr_generator::end_namespace(vector<std::string> modules) {
  int indents = indent_count();
  std::string scope = "";
  while (indent_count() < modules.size()) {
    indent_up();
  }
  for (vector<std::string>::reverse_iterator m_iter = modules.rbegin(); m_iter != modules.rend();
       ++m_iter) {
    indent_down();
    scope += indent() + "end" + endl;
  }
  while (indent_count() < indents) {
    indent_up();
  }
  return scope;
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_cr_generator::generate_service(t_service* tservice) {
  string f_service_name = namespace_dir_ + underscore(service_name_) + ".cr";
  f_service_.open(f_service_name.c_str());

  // generate skeleton for service
  if (!no_skeleton_) {
    generate_service_skeleton(tservice);
  }

  f_service_ << cr_autogen_comment() << endl << render_require_thrift();

  if (tservice->get_extends() != nullptr) {
    f_service_ << "require \"./" << underscore(tservice->get_extends()->get_name()) << ".cr\"" << endl;
  }

  f_service_ << "require \"./" << underscore(program_name_) << "_types.cr\"" << endl
             << endl;


  f_service_ << module_begin_;
  indent(f_service_) << "module " << capitalize(tservice->get_name()) << endl;
  indent_up();

  // Generate the three main parts of the service
  generate_service_client(tservice);
  generate_service_server(tservice);
  generate_service_helpers(tservice);

  indent_down();
  indent(f_service_) << "end" << endl;

  f_service_ << module_end_;
  // Close service file
  f_service_.close();
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_cr_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  indent(f_service_) << "# HELPER FUNCTIONS AND STRUCTURES" << endl;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_cr_struct(f_service_, ts, false, true);
    generate_cr_function_helpers(*f_iter);
  }
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_cr_generator::generate_cr_function_helpers(t_function* tfunction) {
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
  generate_cr_struct(f_service_, &result, false, true);
}

/**
 * Generates a service client definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cr_generator::generate_service_client(t_service* tservice) {
  string extends = "";
  string extends_client = "";
  if (tservice->get_extends() != nullptr) {
    extends = full_type_name(tservice->get_extends());
    extends_client = " < " + extends + "::Client ";
  }

  f_service_ << endl;
  indent(f_service_) << "class Client" << extends_client << endl;
  indent_up();

  indent(f_service_) << "include ::Thrift::Client" << endl << endl;

  // Generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const std::vector<t_field*>& fields = arg_struct->get_members();
    std::vector<t_field*>::const_iterator fld_iter;
    std::string funname = cr_underscore((*f_iter)->get_name());
    std::string outgoing_name = (*f_iter)->get_name();

    // Open function
    indent(f_service_) << "def " << function_signature(*f_iter) << endl;
    indent_up();
    indent(f_service_) << "send_" << funname << "(";

    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      if (first) {
        first = false;
      } else {
        f_service_ << ", ";
      }
      f_service_ << cr_underscore((*fld_iter)->get_name());
    }
    f_service_ << ")" << endl;

    if (!(*f_iter)->is_oneway()) {
      indent(f_service_);
      if (!(*f_iter)->get_returntype()->is_void()) {
        f_service_ << "return ";
      }
      f_service_ << "recv_" << funname << "()" << endl;
    }
    indent_down();
    indent(f_service_) << "end" << endl;
    f_service_ << endl;

    indent(f_service_) << "def send_" << function_signature(*f_iter) << endl;
    indent_up();

    std::string argsname = capitalize((*f_iter)->get_name() + "_args");
    std::string messageSendProc = (*f_iter)->is_oneway() ? "send_oneway_message" : "send_message";

    indent(f_service_) << messageSendProc << "(\"" << outgoing_name << "\", " << argsname;

    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      f_service_ << ", " << cr_underscore((*fld_iter)->get_name()) << ": " << cr_underscore((*fld_iter)->get_name());
    }

    f_service_ << ")" << endl;

    indent_down();
    indent(f_service_) << "end" << endl;

    if (!(*f_iter)->is_oneway()) {
      std::string resultname = capitalize((*f_iter)->get_name() + "_result");
      t_struct noargs(program_);

      t_function recv_function((*f_iter)->get_returntype(), string("recv_") + cr_underscore((*f_iter)->get_name()),
                               &noargs);
      // Open function
      f_service_ << endl;
      indent(f_service_) << "def " << function_signature(&recv_function) << endl;
      indent_up();

      indent(f_service_) << "fname, mtype, rseqid = receive_message_begin()" << endl;
      indent(f_service_) << "handle_exception(mtype)" << endl;

      indent(f_service_) << "if reply_seqid(rseqid)==false" << endl;
      indent(f_service_) << "  raise \"seqid reply faild\"" << endl;
      indent(f_service_) << "end" << endl;

      indent(f_service_) << "result = receive_message(" << resultname << ")" << endl;
      // Careful, only return _result if not a void function

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        indent(f_service_) << "result." << (*x_iter)->get_name() << ".try{|ex| raise ex}" << endl;
      }

      if ((*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) << "return" << endl;
      } else {
        indent(f_service_) << "raise "
                               "::Thrift::ApplicationException.new(::Thrift::ApplicationException::"
                               "MISSING_RESULT, \""
                            << (*f_iter)->get_name() << " failed: unknown result\") if result.success.nil?" << endl;
      }
      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        indent(f_service_) << "return result.success.not_nil!" << endl;
      }

      // Close function
      indent_down();
      indent(f_service_) << "end" << endl;
    }
  }

  indent_down();
  indent(f_service_) << "end" << endl << endl;
}

/**
 * Generates a service server definition.
 *
 * @param tservice The service to generate a server for.
 */
void t_cr_generator::generate_service_server(t_service* tservice) {
  // Generate the dispatch methods
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  std::string svcname = tservice->get_name();

  string extends = "";
  string extends_processor = "";
  if (tservice->get_extends()) {
    extends = full_type_name(tservice->get_extends());
    extends_processor = " < " + extends + "::Processor ";
  }

  // Generate the header portion
  indent(f_service_) << "class Processor(T)" << endl;
  indent_up();
  indent(f_service_) << "include ::Thrift::Processor" << extends_processor << endl;
  indent(f_service_) << "@handler : T" << endl << endl;

  bool first = true;
  // Generate the process subfunctions
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << endl;
    }
    generate_process_function(tservice, *f_iter);
  }

  indent_down();
  indent(f_service_) << "end" << endl << endl;
}

/**
 * Generates a process function definition.
 *
 * @param tfunction The function to write a dispatcher for
 */
void t_cr_generator::generate_process_function(t_service* tservice, t_function* tfunction) {
  (void)tservice;
  // Open function
  indent(f_service_) << "def process_" << cr_underscore(tfunction->get_name()) << "(seqid, iprot, oprot)" << endl;
  //#1
  indent_up();

  string argsname = capitalize(tfunction->get_name()) + "_args";
  string resultname = capitalize(tfunction->get_name()) + "_result";

  indent(f_service_) << "args = read_args(iprot, " << argsname << ")" << endl;

  t_struct* xs = tfunction->get_xceptions();
  const std::vector<t_field*>& xceptions = xs->get_members();
  vector<t_field*>::const_iterator x_iter;

  // Declare result for non oneway function
  if (!tfunction->is_oneway()) {
    indent(f_service_) << "result = " << resultname << ".new" << endl;
  }

  // Try block for a function with exceptions
  if (xceptions.size() > 0) {
    indent(f_service_) << "begin" << endl;
    //#2
    indent_up();
  }

  // Generate the function call
  t_struct* arg_struct = tfunction->get_arglist();
  const std::vector<t_field*>& fields = arg_struct->get_members();
  vector<t_field*>::const_iterator f_iter;

  indent(f_service_);
  if (!tfunction->is_oneway() && !tfunction->get_returntype()->is_void()) {
    f_service_ << "result.success = ";
  }
  f_service_ << "@handler." << cr_underscore(tfunction->get_name()) << "(";
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
    } else {
      f_service_ << ", ";
    }
    f_service_ << "args." << cr_underscore((*f_iter)->get_name());
  }
  f_service_ << ")" << endl;

  if (!tfunction->is_oneway() && xceptions.size() > 0) {
    //#2 close
    indent_down();
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      indent(f_service_) << "rescue " << (*x_iter)->get_name() << " : "
                          << full_type_name((*x_iter)->get_type()) << endl;
      if (!tfunction->is_oneway()) {
        indent_up();
        indent(f_service_) << "result." << (*x_iter)->get_name() << " = " << (*x_iter)->get_name()
                            << endl;
        indent_down();
      }
    }
    indent(f_service_) << "end" << endl;
  }

  // Shortcut out here for oneway functions
  if (tfunction->is_oneway()) {
    indent(f_service_) << "return" << endl;
    //#1 close
    indent_down();
    indent(f_service_) << "end" << endl;
    return;
  }

  indent(f_service_) << "write_result(result, oprot, \"" << tfunction->get_name() << "\", seqid)"
                      << endl;

  // Close function
  indent_down();
  indent(f_service_) << "end" << endl;
}

/**
 * @brief underscore method to match crystal
 *        underscore ex: IAMUPCASE -> iamupcase
 *                       IAMlowercase -> ia_mlowercase
 *
 * @param in
 * @return std::string
 */
std::string t_cr_generator::cr_underscore(std::string in)
{
  bool recently_inserted = false;
  in[0] = tolower(in[0]);
  size_t i = 1;
  for(; i < in.size(); ++i)
  {
    if (isupper(in[i])) {
      in[i] = tolower(in[i]);
      if(i < in.size() - 1 && islower(in[i + 1]) && !recently_inserted) {
        if (!(isalpha(in[i - 1]) || isdigit(in[i - 1]))) {
          continue;
        }
        recently_inserted = true;
        in.insert(i++, "_");
      } else {
        recently_inserted = false;
      }
    } else if (islower(in[i])) {
      if (i < in.size() - 1 && isupper(in[i + 1]) && !recently_inserted) {
        recently_inserted = true;
        in.insert(++i, "_");
      } else {
        recently_inserted = false;
      }
    }
  }
  return in;
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_cr_generator::function_signature(t_function* tfunction, string prefix) {
  return prefix + cr_underscore(tfunction->get_name()) + "(" + argument_list(tfunction->get_arglist()) + ")";
}

/**
 * Renders the require of thrift itself
 */
string t_cr_generator::render_require_thrift() {
  return "require \"thrift\"\n";
}

/**
 * Renders a field list
 */
string t_cr_generator::argument_list(t_struct* tstruct) {
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
    result += fix_name_conflict((*f_iter)->get_name());
  }
  return result;
}

string t_cr_generator::type_name(const t_type* ttype) {
  string prefix = "";

  string name = ttype->get_name();
  if (ttype->is_struct() || ttype->is_xception() || ttype->is_enum()) {
    name = capitalize(ttype->get_name());
  }

  return prefix + name;
}

/**
 * For generated types we want to namespace the types properly
 *
 * @param ttype the type we wan't to namespace
 * @return string
 */
string t_cr_generator::full_type_name(const t_type* ttype) {
  string prefix = "::";
  vector<std::string> modules = crystal_modules(ttype->get_program());
  for (auto& module : modules) {
    prefix += module + "::";
  }
  return prefix + type_name(ttype);
}

void t_cr_generator::generate_crdoc(std::ostream& out, t_doc* tdoc) {
  if (tdoc->has_doc()) {
    generate_docstring_comment(out, "", "# ", tdoc->get_doc(), "");
  }
}

std::string t_cr_generator::display_name() const {
  return "Crystal";
}

/**
 * Generates a skeleton file of a server
 *
 * @param tservice The service to generate a server for.
 */
void t_cr_generator::generate_service_skeleton(t_service* tservice) {

  if (no_skeleton_) {
    return;
  }

  // indent generally is somewhere else at this point so we reset it just for this file
  int previous_indent = indent_count();
  while (0 < indent_count()) {
    indent_down();
  }
  string svcname = tservice->get_name();

  // Service implementation file includes
  string f_skeleton_name = get_out_dir() + lowercase(svcname) + "_server.skeleton.cr";

  ofstream_with_content_based_conditional_update f_skeleton;
  f_skeleton.open(f_skeleton_name.c_str());
  f_skeleton << "# This autogenerated skeleton file illustrates how to build a server." << endl
             << "# You should copy it to another filename to avoid overwriting it." << endl << endl
            //  << "require \"./" << get_include_prefix(*get_program()) << svcname << ".h\"" << endl
             << "require \"thrift\"" << endl
             << "require \"./" << underscore(svcname) << ".cr\"" << endl << endl
             << "include Thrift" << endl;
  std::vector<std::string> modules = crystal_modules(program_);
  if (modules.size() > 0) {
    f_skeleton << "include ";
    for(const auto& module : modules) {
      f_skeleton << "::" << module;
    }
    f_skeleton << endl << endl;
  }

  f_skeleton << "class " << svcname << "Handler" << endl << endl;
  indent_up();
  indent(f_skeleton) << "def initialize" << endl;
  indent_up();
  indent(f_skeleton) << "# Your initialization goes here" << endl;
  indent_down();
  indent(f_skeleton) << "end" << endl << endl;

  std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_crdoc(f_skeleton, *f_iter);
    indent(f_skeleton) << "def " << function_signature(*f_iter, "") << endl;
    indent_up();
    indent(f_skeleton) << "# Your implementation goes here" << endl << indent()
               << "raise ::Thrift::ApplicationException.new message: \"" << (*f_iter)->get_name() << "\"" << endl;
    indent_down();
    indent(f_skeleton) << "end" << endl;
  }

  indent_down();
  indent(f_skeleton) << "end" << endl << endl;

  f_skeleton
      << "port = 9090" << endl
      << "handler = " << svcname << "Handler.new" << endl
      << "processor = " << svcname << "::Processor(" << svcname << "Handler).new(handler)" << endl
      << "serverTransport = Transport::ServerSocketTransport.new(port)" << endl
      << "transportFactory = Transport::BufferedTransportFactory.new" << endl
      << "protocolFactory = Protocol::BinaryProtocolFactory.new"
      << endl << endl
      << "server = Server::SimpleServer.new(processor, serverTransport, transportFactory, protocolFactory)" << endl
      << "server.serve";

  // Close the files
  f_skeleton.close();

  // restore indent level if we changed it
  while (indent_count() < previous_indent) {
    indent_up();
  }
}

/**
 * Generate metadata used for Crystal thrift library
 *
 * @param req requirement for the field
 * @param key field id for serialization
 * @param name_altered denotes if we adjusted the name to make name Crystal compatible
 * @param orig_name if we adjusted the name of this field we will pass the original name
 * @return std::string
 */
std::string t_cr_generator::render_property_type(t_field::e_req req,int key, bool name_altered, const std::string& orig_name)
{
  static const std::vector<std::string> req_to_symbol = {
    ":required",
    ":optional",
    ":opt_in_req_out"
  };
  std::string ret = "fid: " + std::to_string(key) + ", requirement: " + req_to_symbol[static_cast<int>(req)] + (name_altered ? ", transmit_name: \"" + orig_name + "\"" : "");
  return ret;
}

/**
 * Generate initializer for Crystal exception
 *
 * @param out ofstream we write generated code to
 * @param tstruct struct to generate code for
 */
void t_cr_generator::generate_cr_exception_initializer(t_cr_ofstream& out, t_struct* tstruct)
{
  out << endl;
  std::vector<t_field*> fields = tstruct->get_members();
  std::vector<t_field*>::iterator required_fields_wo_iter = std::partition(std::begin(fields), std::end(fields), [](const t_field* tfield) {
    return tfield->get_req() == t_field::T_REQUIRED && tfield->get_value() == nullptr;
  });
  std::vector<t_field*>::const_iterator f_iter;

  indent(out) << "def initialize(";
  bool first = true;
  for (f_iter = std::cbegin(fields); f_iter != required_fields_wo_iter; ++f_iter) {
    std::string cr_safe_name = fix_name_conflict((*f_iter)->get_name());
    if(first) {
      first = false;
    } else {
      out << ", ";
    }
    out << "@" << cr_safe_name;
  }

  if (required_fields_wo_iter != std::end(fields)) {
    if(!first) {
      out << ", ";
    }
    out << "*";
    for (f_iter = required_fields_wo_iter; f_iter != std::cend(fields); ++f_iter) {
      std::string cr_safe_name = fix_name_conflict((*f_iter)->get_name());
      out << ", " << cr_safe_name << " = nil";
    }
  }
  out << ")" << endl;
  indent(out) << "end" << endl;
}

/**
 * Fix any name conflicts from idl in Crystal
 *
 * @param in
 */
std::string t_cr_generator::fix_name_conflict(std::string in)
{
  const static std::vector<std::string> no_no_words = {
    "message",
    "backtrace",
    "callstack",
    "module",
    "end",
    "class",
    "struct",
    "enum",
    "union",
    "lib"
  };

  in = cr_underscore(in);

  if (std::count(std::cbegin(no_no_words), std::cend(no_no_words), in)) {
    in += "_";
  }
  return in;
}


THRIFT_REGISTER_GENERATOR(
    cr,
    "Crystal",
    "    no-skeleton:     Omits generation of skeleton.\n"
    "    namespaced:      Generate files in idiomatic namespaced directories.\n")
