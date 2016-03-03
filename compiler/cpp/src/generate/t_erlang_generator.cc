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

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sstream>
#include "t_generator.h"
#include "platform.h"
#include "version.h"

using std::map;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const std::string endl = "\n"; // avoid ostream << std::endl flushes

/**
 * Erlang code generator.
 *
 */
class t_erlang_generator : public t_generator {
public:
  t_erlang_generator(t_program* program,
                  const std::map<std::string, std::string>& parsed_options,
                  const std::string& option_string)
    : t_generator(program)
    , idiomatic_names_(false) {
    (void)option_string;
    std::map<std::string, std::string>::const_iterator iter;

    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if (iter->first.compare("idiomatic") == 0) {
        idiomatic_names_ = true;
        continue;
      }
      throw "unknown option:" + iter->first;
    }

    out_dir_base_ = "gen-erlang";
  }

  struct indenter {
    indenter() : indent(0), indent_str("    ") {}

    std::string nl() {
      string s = endl;
      for (size_t i = 0; i < indent; ++i) {
        s += indent_str;
      }
      return s;
    }

    std::string up() { ++indent; return string(); }
    std::string down() { indent = indent ? indent - 1 : 0; return string(); }

    std::string nlup() { up(); return nl(); }
    std::string nldown() { down(); return nl(); }

    private:
      size_t indent;
      const std::string indent_str;
  };

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum);
  void generate_const(t_const* tconst);
  void generate_struct(t_struct* tstruct);
  void generate_xception(t_struct* txception);
  void generate_service(t_service* tservice);
  void generate_member_type(std::ostream& out, t_type* type);
  void generate_member_value(std::ostream& out, t_type* type, t_const_value* value);

  std::string render_member_type(t_field* field);
  std::string render_member_value(t_field* field);
  std::string render_member_requiredness(t_field* field);
  std::string render_include(std::string);
  std::string render_export(std::string, int);
  std::string render_export_type(std::string, int);
  std::string render_attribute_list(std::string, std::string);
  std::string render_attribute(std::string, std::string);

  std::string render_default_value(t_field* field);
  std::string render_const_value(t_type* type, t_const_value* value);
  std::string render_type_term(t_type* ttype, bool expand_structs, indenter& ind);

  /**
   * Struct generation code
   */

  void generate_erl_struct(t_struct* tstruct, bool is_exception);
  void generate_erl_struct_definition(std::ostream& out, t_struct* tstruct);
  void generate_erl_struct_member(std::ostream& out, t_field* tmember);
  void generate_erl_struct_info(std::ostream& out, t_struct* tstruct);
  void generate_erl_function_helpers(t_function* tfunction);
  void generate_typespecs(std::ostream& out);
  template <class Type>
  void generate_type_metadata(std::ostream& out, std::string function_name, vector<Type*> types);
  void generate_enum_info(std::ostream& out, t_enum* tenum);
  void generate_struct_metadata(std::ostream& out);
  void generate_enum_metadata(std::ostream& out);

  /**
   * Service-level generation functions
   */

  std::string generate_service_name(t_service* tservice);
  void generate_service_helpers(t_service* tservice);
  void generate_service_metadata(t_service* tservice);
  void generate_service_interface(t_service* tservice);
  void generate_function_info(t_service* tservice, t_function* tfunction);

  /**
   * Helper rendering functions
   */

  std::string get_ns_prefix();
  std::string get_ns_prefix(t_program*);

  std::string erl_autogen_comment();
  std::string erl_imports();
  std::string render_includes();
  std::string render_include(t_program* p);
  std::string type_name(t_type* ttype);
  std::string function_name(t_function* ttype);
  std::string field_name(t_field* ttype);

  std::string function_signature(t_function* tfunction, std::string prefix = "");

  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);
  std::string type_module(t_type* ttype);

  std::string underscore(std::string const& in) {
    string r = "";
    char c1, c2, c3;
    for (size_t i = 0, j = in.size() - 1; i < j + 1; ++i) {
      c1 = in[0 == i ? i : i - 1];
      c2 = in[i];
      c3 = in[j == i ? i : i + 1];
      if (isupper(c2) && islower(c3) && i > 0) {
        r.push_back('_');
      }
      else if (islower(c1) && isupper(c2)) {
        r.push_back('_');
      }
      r.push_back(tolower(c2));
    }
    return r;
  }

  std::string modulify() {
    return modulify(program_);
  }

  std::string modulify(t_program* p) {
    return get_ns_prefix(p) + underscore(p->get_name());
  }

  std::string atomify(std::string in) {
    return "'" + in + "'";
  }

  std::string constify(std::string in) {
    return uppercase(in);
  }

  static std::string comment(string in);

private:
  /**
   * options
   */
  bool idiomatic_names_;

  bool has_default_value(t_field*);

  /**
   * add function to export list
   */

  std::string render_export_function(t_function* tfunction, std::string prefix = "");
  std::string render_export_types_function(t_function* tfunction, std::string prefix = "");

  /**
   * write out headers and footers for hrl files
   */

  std::string render_hrl_header(std::string name);
  std::string render_hrl_footer(std::string name);

  /**
   * set default four space indent
   */

  std::string indent_str() const {
    return "    ";
  }

  /**
   * File streams
   */

  std::ofstream f_types_file_;
  std::ofstream f_types_hrl_file_;

  std::ofstream f_consts_;
  std::ofstream f_service_file_;
  std::ofstream f_service_hrl_;

  /**
   * Metadata containers
   */
  std::vector<t_struct*> v_structs_;
  std::vector<t_enum*> v_enums_;
};

/**
 * UI for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_erlang_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());

  // types files
  string base_name = modulify() + "_types";
  string f_types_name = get_out_dir() + base_name + ".erl";
  string f_types_hrl_name = get_out_dir() + base_name + ".hrl";

  f_types_file_.open(f_types_name.c_str());
  f_types_hrl_file_.open(f_types_hrl_name.c_str());

  f_types_file_ << erl_autogen_comment() << endl
                << render_attribute("module", base_name) << endl
                << erl_imports() << endl
                << render_include(base_name + ".hrl") << endl;

  f_types_hrl_file_ << render_hrl_header(base_name) << endl
                    << render_includes() << endl;

  // consts file
  string f_consts_name = get_out_dir() + modulify() + "_constants.hrl";
  f_consts_.open(f_consts_name.c_str());

  f_consts_ << erl_autogen_comment() << endl
            << erl_imports() << endl
            << render_include(base_name + ".hrl") << endl;
}

std::string t_erlang_generator::get_ns_prefix() {
  return get_ns_prefix(program_);
}

std::string t_erlang_generator::get_ns_prefix(t_program* program) {
  string ns = program->get_namespace("erl");
  struct xf {
    static char evade_special(char c) {
      switch (c) {
        case '.':
        case '-':
        case '/':
        case '\\':
          return '_';
        default:
          return c;
      }
    }
  };
  std::transform(ns.begin(), ns.end(), ns.begin(), xf::evade_special);
  if (ns.size()) {
    return ns + "_";
  }
  return ns;
}

/**
 * Boilerplate at beginning and end of header files
 */
string t_erlang_generator::render_hrl_header(string name) {
  return "-ifndef(_" + name + "_included)." + endl
       + "-define(_" + name + "_included, yeah)." + endl;
}

string t_erlang_generator::render_hrl_footer(string name) {
  (void)name;
  return "-endif." + endl;
}

/**
 * Renders all the imports necessary for including another Thrift program
 */
string t_erlang_generator::render_includes() {
  const vector<t_program*>& includes = program_->get_includes();
  string result = "";
  for (size_t i = 0; i < includes.size(); ++i) {
    result += render_include(includes[i]);
  }
  if (includes.size() > 0) {
    result += endl;
  }
  return result;
}

string t_erlang_generator::render_include(t_program* p) {
  return render_include(modulify(p) + "_types.hrl");
}

/**
 * Autogen'd comment
 */
string t_erlang_generator::erl_autogen_comment() {
  return string("%%\n")
         + "%% Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")\n"
         + "%%\n" + "%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n"
         + "%%\n";
}

/**
 * Comment out text
 */

string t_erlang_generator::comment(string in) {
  size_t pos = 0;
  in.insert(pos, "%% ");
  while ((pos = in.find_first_of('\n', pos)) != string::npos) {
    in.insert(++pos, "%% ");
  }
  return in;
}

/**
 * Prints standard thrift imports
 */
string t_erlang_generator::erl_imports() {
  return "";
}

/**
 * Closes the type files
 */
void t_erlang_generator::close_generator() {

  generate_typespecs(f_types_file_);

  f_types_file_ << render_export("enum_names", 0)
                << render_export("struct_names", 0)
                << render_export("enum_info", 1)
                << render_export("struct_info", 1)
                << endl;


  f_types_file_ << "-spec struct_names() -> [atom()]." << endl << endl;
  generate_type_metadata(f_types_file_, "struct_names", v_structs_);
  f_types_file_ << "-spec enum_names() -> [atom()]." << endl << endl;
  generate_type_metadata(f_types_file_, "enum_names", v_enums_);

  generate_struct_metadata(f_types_file_);
  generate_enum_metadata(f_types_file_);

  f_types_hrl_file_ << render_hrl_footer(string("BOGUS"));

  f_types_file_.close();
  f_types_hrl_file_.close();
  f_consts_.close();
}

void t_erlang_generator::generate_typespecs(std::ostream& os) {

  os << "-type type_ref() :: {module(), atom()}." << endl
     << "-type field_num() :: pos_integer()." << endl
     << "-type field_name() :: atom()." << endl
     << "-type field_req() :: required | optional." << endl;

  {
    indenter i;
    os << "-type field_type() ::" << i.nlup()
       << "bool | byte | i16 | i32 | i64 | string | double |" << i.nl()
       << "{enum, type_ref()} |" << i.nl()
       << "{struct, type_ref()} |" << i.nl()
       << "{list, field_type()} |" << i.nl()
       << "{set, field_type()} |" << i.nl()
       << "{map, field_type(), field_type()}." << endl << endl;
  }


  os << "-type struct_field_info() :: {field_num(), field_req(), field_type(), field_name(), any()}." << endl;
  os << "-type enum_field_info() :: {atom(), integer()}." << endl;

  os << endl;

}

template <class Type>
void t_erlang_generator::generate_type_metadata(std::ostream& os, std::string function_name, vector<Type*> types) {
  indenter i;
  os << function_name << "() ->" << i.nlup()
     << "[" << i.nlup();

  for (size_t j = 0; j < types.size();) {
    os << type_name(types[j]);
    if (++j != types.size()) {
      os << ", " << i.nl();
    }
  }

  os << i.nldown() << "]." << endl << endl;
}

/**
 * Generates a typedef. no op
 *
 * @param ttypedef The type definition
 */
void t_erlang_generator::generate_typedef(t_typedef* ttypedef) {
  (void)ttypedef;
}

/**
 * Generates code for an enumerated type. Done using a class to scope
 * the values.
 *
 * @param tenum The enumeration
 */
void t_erlang_generator::generate_enum(t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;

  v_enums_.push_back(tenum);

  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    int value = (*c_iter)->get_value();
    string name = (*c_iter)->get_name();
    indent(f_types_hrl_file_) << "-define(" << constify(modulify())
                              << "_" << constify(tenum->get_name()) << "_" << constify(name) << ", "
                              << value << ")." << endl;
  }

  f_types_hrl_file_ << endl;
}

void t_erlang_generator::generate_enum_info(std::ostream& buf, t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;

  indenter i;
  buf << "enum_info(" << type_name(tenum) << ") ->" << i.nlup()
      << "{enum, [" << i.nlup();

  for (c_iter = constants.begin(); c_iter != constants.end(); ) {
    int value = (*c_iter)->get_value();
    string name = underscore((*c_iter)->get_name());
    buf << "{" << name << ", " << value << "}";
    if (++c_iter != constants.end()) {
      buf << "," << i.nl();
    }
  }

  buf << i.nldown() << "]};" << endl << endl;
}

void t_erlang_generator::generate_enum_metadata(std::ostream& os) {
  size_t enum_count = v_enums_.size();

  f_types_file_ << "-spec enum_info(atom()) -> {enum, [enum_field_info()]}." << endl << endl;
  for(size_t i=0; i < enum_count; i++) {
    t_enum* tenum = v_enums_.at(i);
    generate_enum_info(os, tenum);
  }

  os << "enum_info(_) -> erlang:error(badarg)." << endl << endl;
}

void t_erlang_generator::generate_struct_metadata(std::ostream& os) {
  size_t count = v_structs_.size();

  f_types_file_ << "-spec struct_info(atom()) -> {struct, [struct_field_info()]}." << endl << endl;
  for(size_t i=0; i < count; i++) {
    t_struct* tstruct = v_structs_.at(i);
    generate_erl_struct_info(os, tstruct);
  }

  os << "struct_info(_) -> erlang:error(badarg)." << endl << endl;
}

/**
 * Generate a constant value
 */
void t_erlang_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = tconst->get_name();
  t_const_value* value = tconst->get_value();
  f_consts_ << "-define(" << constify(modulify() + "_" + name) << ", " << render_const_value(type, value) << ")." << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_erlang_generator::render_const_value(t_type* type, t_const_value* value) {
  type = get_true_type(type);
  std::ostringstream out;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      out << '"' << get_escaped_string(value) << '"';
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
    indent(out) << value->get_integer();

  } else if (type->is_struct() || type->is_xception()) {
    out << "#" << type_name(type) << "{";
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;

    bool first = true;
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

      if (first) {
        first = false;
      } else {
        out << ",";
      }
      out << v_iter->first->get_string();
      out << " = ";
      out << render_const_value(field_type, v_iter->second);
    }
    indent_down();
    indent(out) << "}";

  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();

    out << "#{";
    map<t_const_value*, t_const_value*>::const_iterator i, end = value->get_map().end();
    for (i = value->get_map().begin(); i != end;) {
      out << render_const_value(ktype, i->first) << " => "
          << render_const_value(vtype, i->second);
      if (++i != end) {
        out << ", ";
      }
    }
    out << "}";
  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    out << "ordsets:from_list([";
    vector<t_const_value*>::const_iterator i, end = value->get_list().end();
    for (i = value->get_list().begin(); i != end;) {
      out << render_const_value(etype, *i);
      if (++i != end) {
        out << ",";
      }
    }
    out << "])";
  } else if (type->is_list()) {
    t_type* etype;
    etype = ((t_list*)type)->get_elem_type();
    out << "[";

    bool first = true;
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (first) {
        first = false;
      } else {
        out << ",";
      }
      out << render_const_value(etype, *v_iter);
    }
    out << "]";
  } else {
    throw "CANNOT GENERATE CONSTANT FOR TYPE: " + type->get_name();
  }
  return out.str();
}

string t_erlang_generator::render_default_value(t_field* field) {
  t_type* type = field->get_type();
  if (type->is_struct() || type->is_xception()) {
    return "#" + type_name(type) + "{}";
  } else if (type->is_map()) {
    return "#{}";
  } else if (type->is_set()) {
    return "ordsets:new()";
  } else if (type->is_list()) {
    return "[]";
  } else {
    return "undefined";
  }
}

string t_erlang_generator::render_member_type(t_field* field) {
  t_type* type = get_true_type(field->get_type());
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      return "binary()";
    case t_base_type::TYPE_BOOL:
      return "boolean()";
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      return "integer()";
    case t_base_type::TYPE_DOUBLE:
      return "float()";
    default:
      throw "compiler error: unsupported base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    return "integer()";
  } else if (type->is_struct() || type->is_xception()) {
    return type_name(type) + "()";
  } else if (type->is_map()) {
    return "#{}";
  } else if (type->is_set()) {
    return "ordsets:set()";
  } else if (type->is_list()) {
    return "list()";
  } else {
    throw "compiler error: unsupported type " + type->get_name();
  }
}

string t_erlang_generator::render_member_requiredness(t_field* field) {
  switch (field->get_req()) {
  case t_field::T_REQUIRED:
    return "required";
  case t_field::T_OPTIONAL:
    return "optional";
  default:
    return "undefined";
  }
}

/**
 * Generates a struct
 */
void t_erlang_generator::generate_struct(t_struct* tstruct) {
  v_structs_.push_back(tstruct);
  generate_erl_struct_definition(f_types_hrl_file_, tstruct);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 *
 * @param txception The struct definition
 */
void t_erlang_generator::generate_xception(t_struct* txception) {
  generate_struct(txception);
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_erlang_generator::generate_erl_struct_definition(ostream& out, t_struct* tstruct) {
  indenter i;
  out << "%% struct " << type_name(tstruct) << endl << endl;

  out << "-record(" << type_name(tstruct) << ", {" << i.nlup();

  const vector<t_field*>& members = tstruct->get_members();
  for (vector<t_field*>::const_iterator m_iter = members.begin(); m_iter != members.end();) {
    generate_erl_struct_member(out, *m_iter);
    if (++m_iter != members.end()) {
      out << "," << i.nl();
    }
  }
  out << i.nldown() << "})." << endl << endl;

  out << "-type " + type_name(tstruct) << "() :: #" + type_name(tstruct) + "{}." << endl << endl;
}

/**
 * Generates the record field definition
 */

void t_erlang_generator::generate_erl_struct_member(ostream& out, t_field* tmember) {
  out << field_name(tmember);
  if (has_default_value(tmember))
    out << " = " << render_member_value(tmember);
  out << " :: " << render_member_type(tmember);
}

bool t_erlang_generator::has_default_value(t_field* field) {
  t_type* type = field->get_type();
  if (!field->get_value()) {
    if (field->get_req() == t_field::T_REQUIRED) {
      if (type->is_struct() || type->is_xception() || type->is_map() || type->is_set()
          || type->is_list()) {
        return true;
      } else {
        return false;
      }
    } else {
      return false;
    }
  } else {
    return true;
  }
}

string t_erlang_generator::render_member_value(t_field* field) {
  if (!field->get_value()) {
    return render_default_value(field);
  } else {
    return render_const_value(field->get_type(), field->get_value());
  }
}

/**
 * Generates the read method for a struct
 */
void t_erlang_generator::generate_erl_struct_info(ostream& out, t_struct* tstruct) {
  indenter i;
  out << "struct_info(" << type_name(tstruct) << ") ->" << i.nlup()
      << render_type_term(tstruct, true, i) << ";" << endl << endl;
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_erlang_generator::generate_service(t_service* tservice) {
  service_name_ = generate_service_name(tservice);

  string f_service_hrl_name = get_out_dir() + service_name_ + ".hrl";
  string f_service_name = get_out_dir() + service_name_ + ".erl";
  f_service_file_.open(f_service_name.c_str());
  f_service_hrl_.open(f_service_hrl_name.c_str());

  f_service_hrl_ << render_hrl_header(service_name_);

  if (tservice->get_extends() != NULL) {
    f_service_hrl_ << render_include(generate_service_name(tservice->get_extends()) + ".hrl") << endl;
  }

  f_service_hrl_ << "-include(\"" << modulify() << "_types.hrl\")."
                 << endl << endl;

  f_service_file_ << erl_autogen_comment() << endl << "-module(" << service_name_ << ")."
                  << endl << "-behaviour(thrift_service)." << endl << endl << erl_imports() << endl;

  f_service_file_ << render_include(service_name_ + ".hrl") << endl;

  f_service_file_ << render_export("function_info", 2)
                  << render_export("struct_info", 1)
                  << render_export("function_names", 0) << endl;

  // Generate the three main parts of the service (well, two for now in PHP)
  generate_service_metadata(tservice);
  generate_service_helpers(tservice);
  generate_service_interface(tservice);

  f_service_hrl_ << render_hrl_footer(f_service_name);

  // Close service file
  f_service_file_.close();
  f_service_hrl_.close();
}

std::string t_erlang_generator::generate_service_name(t_service* tservice) {
  return modulify(tservice->get_program())
       + "_" + underscore(tservice->get_name())
       + "_service";
}

void t_erlang_generator::generate_service_metadata(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  size_t num_functions = functions.size();

  indenter ind;
  f_service_file_ << "function_names() -> " << ind.nlup()
                  << "[" << ind.nlup();

  for (size_t i=0; i < num_functions; i++) {
    t_function* current = functions.at(i);
    f_service_file_ << function_name(current);
    if (i < num_functions - 1) {
      f_service_file_ << "," << ind.nl();
    }
  }

  f_service_file_ << ind.nldown() << "]." << endl << endl;
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_erlang_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_erl_function_helpers(*f_iter);
  }
  f_service_file_ << "struct_info(_) -> erlang:error(badarg)." << endl << endl;
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_erlang_generator::generate_erl_function_helpers(t_function* tfunction) {
  (void)tfunction;
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_erlang_generator::generate_service_interface(t_service* tservice) {

  indenter i;
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_service_file_ << "% " << function_signature(*f_iter) << endl;
    generate_function_info(tservice, *f_iter);
  }

  // Inheritance - pass unknown functions to base class
  if (tservice->get_extends() != NULL) {
    f_service_file_ << "function_info(Function, InfoType) ->" << i.nlup()
                    << generate_service_name(tservice->get_extends())
                    << ":function_info(Function, InfoType)." << i.nldown();
  } else {
    // return badarg error for non-existent functions
    f_service_file_ << "function_info(_Func, _Info) -> erlang:error(badarg)." << endl;
  }

  f_service_file_ << endl;
}

/**
 * Generates a function_info(FunctionName, params_type) and
 * function_info(FunctionName, reply_type)
 */
void t_erlang_generator::generate_function_info(t_service* tservice, t_function* tfunction) {
  (void)tservice;
  string name_atom = function_name(tfunction);

  t_struct* xs = tfunction->get_xceptions();
  t_struct* arg_struct = tfunction->get_arglist();

  // function_info(Function, params_type):
  indenter i;
  f_service_file_ << "function_info(" << name_atom << ", params_type) ->" << i.nlup()
             << render_type_term(arg_struct, true, i) << ";" << i.nldown();

  // function_info(Function, reply_type):
  f_service_file_ << "function_info(" << name_atom << ", reply_type) ->" << i.nlup();

  if (!tfunction->get_returntype()->is_void())
    f_service_file_ << render_type_term(tfunction->get_returntype(), false, i) << ";" << i.nldown();
  else if (tfunction->is_oneway())
    f_service_file_ << "oneway_void;" << i.nldown();
  else
    f_service_file_ << "{struct, []};" << i.nldown();

  // function_info(Function, exceptions):
  f_service_file_ << "function_info(" << name_atom << ", exceptions) ->" << i.nlup()
             << render_type_term(xs, true, i) << ";" << endl;
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_erlang_generator::function_signature(t_function* tfunction, string prefix) {
  return prefix + tfunction->get_name() + "(This"
         + capitalize(argument_list(tfunction->get_arglist())) + ")";
}

string t_erlang_generator::render_export_types_function(t_function* tfunction, string prefix) {
  return render_export(prefix + tfunction->get_name(),
                       1 // This
                       + ((tfunction->get_arglist())->get_members()).size());
}

string t_erlang_generator::render_include(string filename) {
  return render_attribute("include", "\"" + filename + "\"");
}

string t_erlang_generator::render_export(string name, int arity) {
  return render_attribute_list("export", name + "/" + std::to_string(arity));
}

string t_erlang_generator::render_export_type(string type, int arity) {
  return render_attribute_list("export_type", type + "/" + std::to_string(arity));
}

string t_erlang_generator::render_attribute_list(string type, string content) {
  return render_attribute(type, "[" + content + "]");
}

string t_erlang_generator::render_attribute(string type, string content) {
  return "-" + type + "(" + content + ")." + endl;
}

string t_erlang_generator::render_export_function(t_function* tfunction, string prefix) {
  return render_export(prefix + tfunction->get_name(),
                1 // This
                + ((tfunction->get_arglist())->get_members()).size());
}

/**
 * Renders a field list
 */
string t_erlang_generator::argument_list(t_struct* tstruct) {
  string result = "";

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      result += ", "; // initial comma to compensate for initial This
    } else {
      result += ", ";
    }
    result += capitalize((*f_iter)->get_name());
  }
  return result;
}

string t_erlang_generator::type_name(t_type* ttype) {
  string const& n = ttype->get_name();
  return atomify(idiomatic_names_ ? underscore(n) : n);
}

string t_erlang_generator::function_name(t_function* tfun) {
  string const& n = tfun->get_name();
  return atomify(idiomatic_names_ ? underscore(n) : n);
}

string t_erlang_generator::field_name(t_field* tfield) {
  string const& n = tfield->get_name();
  return atomify(idiomatic_names_ ? underscore(n) : n);
}

/**
 * Converts the parse type to a Erlang "type" (macro for int constants)
 */
string t_erlang_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "?tType_STRING";
    case t_base_type::TYPE_BOOL:
      return "?tType_BOOL";
    case t_base_type::TYPE_I8:
      return "?tType_I8";
    case t_base_type::TYPE_I16:
      return "?tType_I16";
    case t_base_type::TYPE_I32:
      return "?tType_I32";
    case t_base_type::TYPE_I64:
      return "?tType_I64";
    case t_base_type::TYPE_DOUBLE:
      return "?tType_DOUBLE";
    }
  } else if (type->is_enum()) {
    return "?tType_I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "?tType_STRUCT";
  } else if (type->is_map()) {
    return "?tType_MAP";
  } else if (type->is_set()) {
    return "?tType_SET";
  } else if (type->is_list()) {
    return "?tType_LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

/**
 * Generate an Erlang term which represents a thrift type
 */
std::string t_erlang_generator::render_type_term(t_type* type,
                                                 bool expand_structs,
                                                 t_erlang_generator::indenter& ind) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "string";
    case t_base_type::TYPE_BOOL:
      return "bool";
    case t_base_type::TYPE_I8:
      return "byte";
    case t_base_type::TYPE_I16:
      return "i16";
    case t_base_type::TYPE_I32:
      return "i32";
    case t_base_type::TYPE_I64:
      return "i64";
    case t_base_type::TYPE_DOUBLE:
      return "double";
    }
  } else if (type->is_enum()) {
    return "{enum, {" + type_module(type) + ", " + type_name(type) + "}}";
  } else if (type->is_struct() || type->is_xception()) {
    if (expand_structs) {

      t_struct::members_type const& fields = static_cast<t_struct*>(type)->get_members();
      if (0 == fields.size()) {
        return "{struct, []}";
      }

      std::stringstream buf;
      buf << "{struct, [" << ind.nlup();

      t_struct::members_type::const_iterator i, end = fields.end();
      for (i = fields.begin(); i != end;) {
        t_struct::members_type::value_type member = *i;
        int32_t key = member->get_key();
        string type = render_type_term(member->get_type(), false, ind); // recursive call

        // Convert to format: {struct, [{Fid, Req, Type, Name, Def}|...]}
        string name = field_name(member);
        string value = render_member_value(member);
        string requiredness = render_member_requiredness(member);
        buf << "{" << key << ", " << requiredness << ", " << type << ", " << name << ", " << value << "}";

        if (++i != end) {
          buf << "," << ind.nl();
        }
      }

      buf << ind.nldown() << "]}";
      return buf.str();
    } else {
      return "{struct, {" + type_module(type) + ", " + type_name(type) + "}}";
    }
  } else if (type->is_map()) {
    // {map, KeyType, ValType}
    t_type* key_type = ((t_map*)type)->get_key_type();
    t_type* val_type = ((t_map*)type)->get_val_type();

    return "{map, " + render_type_term(key_type, false, ind) + ", " + render_type_term(val_type, false, ind)
           + "}";

  } else if (type->is_set()) {
    t_type* elem_type = ((t_set*)type)->get_elem_type();

    return "{set, " + render_type_term(elem_type, false, ind) + "}";

  } else if (type->is_list()) {
    t_type* elem_type = ((t_list*)type)->get_elem_type();

    return "{list, " + render_type_term(elem_type, false, ind) + "}";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

std::string t_erlang_generator::type_module(t_type* ttype) {
  return modulify(ttype->get_program()) + "_types";
}

THRIFT_REGISTER_GENERATOR(
  erlang,
  "Erlang",
  "    idiomatic: Adapt every name to look idiomatically correct in Erlang (i.e. snake case).\n")
