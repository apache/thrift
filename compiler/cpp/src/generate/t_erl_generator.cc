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

using namespace std;


/**
 * Erlang code generator.
 *
 */
class t_erl_generator : public t_generator {
 public:
  t_erl_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_generator(program)
  {
    program_name_[0] = tolower(program_name_[0]);
    service_name_[0] = tolower(service_name_[0]);
    out_dir_base_ = "gen-erl";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */

  void generate_typedef  (t_typedef*  ttypedef);
  void generate_enum     (t_enum*     tenum);
  void generate_const    (t_const*    tconst);
  void generate_struct   (t_struct*   tstruct);
  void generate_xception (t_struct*   txception);
  void generate_service  (t_service*  tservice);

  std::string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_erl_struct(t_struct* tstruct, bool is_exception);
  void generate_erl_struct_definition(std::ostream& out, std::ostream& hrl_out, t_struct* tstruct, bool is_xception=false, bool is_result=false);
  void generate_erl_struct_info(std::ostream& out, t_struct* tstruct);
  void generate_erl_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_service_helpers   (t_service*  tservice);
  void generate_service_interface (t_service* tservice);
  void generate_function_info     (t_service* tservice, t_function* tfunction);

  /**
   * Helper rendering functions
   */

  std::string erl_autogen_comment();
  std::string erl_imports();
  std::string render_includes();
  std::string declare_field(t_field* tfield);
  std::string type_name(t_type* ttype);

  std::string function_signature(t_function* tfunction, std::string prefix="");


  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);
  std::string generate_type_term(t_type* ttype, bool expand_structs);
  std::string type_module(t_type* ttype);

  std::string capitalize(std::string in) {
    in[0] = toupper(in[0]);
    return in;
  }

  std::string uncapitalize(std::string in) {
    in[0] = tolower(in[0]);
    return in;
  }

 private:

  /**
   * add function to export list
   */

  void export_function(t_function* tfunction, std::string prefix="");
  void export_string(std::string name, int num);

  void export_types_function(t_function* tfunction, std::string prefix="");
  void export_types_string(std::string name, int num);

  /**
   * write out headers and footers for hrl files
   */

  void hrl_header(std::ostream& out, std::string name);
  void hrl_footer(std::ostream& out, std::string name);

  /**
   * stuff to spit out at the top of generated files
   */

  bool export_lines_first_;
  std::ostringstream export_lines_;

  bool export_types_lines_first_;
  std::ostringstream export_types_lines_;

  /**
   * File streams
   */

  std::ostringstream f_types_;
  std::ofstream f_types_file_;
  std::ofstream f_types_hrl_file_;

  std::ofstream f_consts_;
  std::ostringstream f_service_;
  std::ofstream f_service_file_;
  std::ofstream f_service_hrl_;

};


/**
 * UI for file generation by opening up the necessary file output
 * streams.
 *
 * @param tprogram The program to generate
 */
void t_erl_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());

  // setup export lines
  export_lines_first_ = true;
  export_types_lines_first_ = true;

  // types files
  string f_types_name = get_out_dir()+program_name_+"_types.erl";
  string f_types_hrl_name = get_out_dir()+program_name_+"_types.hrl";

  f_types_file_.open(f_types_name.c_str());
  f_types_hrl_file_.open(f_types_hrl_name.c_str());

  hrl_header(f_types_hrl_file_, program_name_ + "_types");

  f_types_file_ <<
    erl_autogen_comment() << endl <<
    "-module(" << program_name_ << "_types)." << endl <<
    erl_imports() << endl;

  f_types_file_ <<
    "-include(\"" << program_name_ << "_types.hrl\")." << endl <<
    endl;

  f_types_hrl_file_ << render_includes() << endl;

  // consts file
  string f_consts_name = get_out_dir()+program_name_+"_constants.hrl";
  f_consts_.open(f_consts_name.c_str());

  f_consts_ <<
    erl_autogen_comment() << endl <<
    erl_imports() << endl <<
    "-include(\"" << program_name_ << "_types.hrl\")." << endl <<
    endl;
}

/**
 * Boilerplate at beginning and end of header files
 */
void t_erl_generator::hrl_header(ostream& out, string name) {
  out << "-ifndef(_" << name << "_included)." << endl <<
    "-define(_" << name << "_included, yeah)." << endl;
}

void t_erl_generator::hrl_footer(ostream& out, string name) {
  out << "-endif." << endl;
}

/**
 * Renders all the imports necessary for including another Thrift program
 */
string t_erl_generator::render_includes() {
  const vector<t_program*>& includes = program_->get_includes();
  string result = "";
  for (size_t i = 0; i < includes.size(); ++i) {
    result += "-include(\"" + uncapitalize(includes[i]->get_name()) + "_types.hrl\").\n";
  }
  if (includes.size() > 0) {
    result += "\n";
  }
  return result;
}

/**
 * Autogen'd comment
 */
string t_erl_generator::erl_autogen_comment() {
  return
    std::string("%%\n") +
    "%% Autogenerated by Thrift\n" +
    "%%\n" +
    "%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n" +
    "%%\n";
}

/**
 * Prints standard thrift imports
 */
string t_erl_generator::erl_imports() {
  return "";
}

/**
 * Closes the type files
 */
void t_erl_generator::close_generator() {
  // Close types file
  export_types_string("struct_info", 1);

  f_types_file_ << "-export([" << export_types_lines_.str() << "])." << endl;
  f_types_file_ << f_types_.str();
  f_types_file_ << "struct_info('i am a dummy struct') -> undefined." << endl;

  hrl_footer(f_types_hrl_file_, string("BOGUS"));

  f_types_file_.close();
  f_types_hrl_file_.close();
  f_consts_.close();
}

/**
 * Generates a typedef. no op
 *
 * @param ttypedef The type definition
 */
void t_erl_generator::generate_typedef(t_typedef* ttypedef) {
}

/**
 * Generates code for an enumerated type. Done using a class to scope
 * the values.
 *
 * @param tenum The enumeration
 */
void t_erl_generator::generate_enum(t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;

  int value = -1;

  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if ((*c_iter)->has_value()) {
      value = (*c_iter)->get_value();
    } else {
      ++value;
    }

    string name = capitalize((*c_iter)->get_name());

    f_types_hrl_file_ <<
      indent() << "-define(" << program_name_ << "_" << name << ", " << value << ")."<< endl;
  }

  f_types_hrl_file_ << endl;
}

/**
 * Generate a constant value
 */
void t_erl_generator::generate_const(t_const* tconst) {
  t_type* type = tconst->get_type();
  string name = capitalize(tconst->get_name());
  t_const_value* value = tconst->get_value();

  f_consts_ << "-define(" << program_name_ << "_" << name << ", " << render_const_value(type, value) << ")." << endl << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_erl_generator::render_const_value(t_type* type, t_const_value* value) {
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
    case t_base_type::TYPE_BYTE:
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
    out << "#" << type->get_name() << "{";
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
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;

    bool first = true;
    out << "dict:from_list([";
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (first) {
        first=false;
      } else {
        out << ",";
      }
      out << "("
          << render_const_value(ktype, v_iter->first)  << ","
          << render_const_value(vtype, v_iter->second) << ")";
    }
    out << "])";

  } else if (type->is_set()) {
    t_type* etype;
    etype = ((t_set*)type)->get_elem_type();

    bool first = true;
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    out << "sets:from_list([";
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (first) {
        first=false;
      } else {
        out << ",";
      }
      out << "(" << render_const_value(etype, *v_iter) << ",true)";
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
        first=false;
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

/**
 * Generates a struct
 */
void t_erl_generator::generate_struct(t_struct* tstruct) {
  generate_erl_struct(tstruct, false);
}

/**
 * Generates a struct definition for a thrift exception. Basically the same
 * as a struct but extends the Exception class.
 *
 * @param txception The struct definition
 */
void t_erl_generator::generate_xception(t_struct* txception) {
  generate_erl_struct(txception, true);
}

/**
 * Generates a struct
 */
void t_erl_generator::generate_erl_struct(t_struct* tstruct,
                                          bool is_exception) {
  generate_erl_struct_definition(f_types_, f_types_hrl_file_, tstruct, is_exception);
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_erl_generator::generate_erl_struct_definition(ostream& out,
                                                     ostream& hrl_out,
                                                     t_struct* tstruct,
                                                     bool is_exception,
                                                     bool is_result)
{
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  indent(out) << "%% struct " << type_name(tstruct) << endl;

  if (is_exception) {
  }

  out << endl;

  if (members.size() > 0) {
    indent(out)     << "% -record(" << type_name(tstruct) << ", {";
    indent(hrl_out) <<   "-record(" << type_name(tstruct) << ", {";

    bool first = true;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      if (first) {
        first = false;
      } else {
        out     << ", ";
        hrl_out << ", ";
      }
      std::string name = uncapitalize((*m_iter)->get_name());
      out     << name;
      hrl_out << name;
    }
    out     << "})." << endl;
    hrl_out << "})." << endl;
  } else { // no members; explicit comment
    indent(out)     << "% -record(" << type_name(tstruct) << ", {})." << endl;
    indent(hrl_out) <<   "-record(" << type_name(tstruct) << ", {})." << endl;
  }

  out << endl;
  hrl_out << endl;


  generate_erl_struct_info(out, tstruct);
}

/**
 * Generates the read method for a struct
 */
void t_erl_generator::generate_erl_struct_info(ostream& out,
                                                  t_struct* tstruct) {
  string name = type_name(tstruct);

  indent(out) << "struct_info('" << name << "') ->" << endl;
  indent_up();

  out << indent() << generate_type_term(tstruct, true) << ";" << endl;

  indent_down();
  out << endl;
}


/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_erl_generator::generate_service(t_service* tservice) {
  // somehow this point is reached before the constructor and it's not downcased yet
  // ...awesome
  service_name_[0] = tolower(service_name_[0]);

  string f_service_hrl_name = get_out_dir()+service_name_+"_thrift.hrl";
  string f_service_name = get_out_dir()+service_name_+"_thrift.erl";
  f_service_file_.open(f_service_name.c_str());
  f_service_hrl_.open(f_service_hrl_name.c_str());

  // Reset service text aggregating stream streams
  f_service_.str("");
  export_lines_.str("");
  export_lines_first_ = true;

  hrl_header(f_service_hrl_, service_name_);

  if (tservice->get_extends() != NULL) {
    f_service_hrl_ << "-include(\"" <<
      uncapitalize(tservice->get_extends()->get_name()) << "_thrift.hrl\"). % inherit " << endl;
  }

  f_service_hrl_ <<
    "-include(\"" << program_name_ << "_types.hrl\")." << endl <<
    endl;

  // Generate the three main parts of the service (well, two for now in PHP)
  generate_service_helpers(tservice); // cpiro: New Erlang Order

  generate_service_interface(tservice);

  // indent_down();

  f_service_file_ <<
    erl_autogen_comment() << endl <<
    "-module(" << service_name_ << "_thrift)." << endl <<
    "-behaviour(thrift_service)." << endl << endl <<
    erl_imports() << endl;

  f_service_file_ << "-include(\"" << uncapitalize(tservice->get_name()) << "_thrift.hrl\")." << endl << endl;

  f_service_file_ << "-export([" << export_lines_.str() << "])." << endl << endl;

  f_service_file_ << f_service_.str();

  hrl_footer(f_service_hrl_, f_service_name);

  // Close service file
  f_service_file_.close();
  f_service_hrl_.close();
}

/**
 * Generates helper functions for a service.
 *
 * @param tservice The service to generate a header definition for
 */
void t_erl_generator::generate_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  //  indent(f_service_) <<
  //  "% HELPER FUNCTIONS AND STRUCTURES" << endl << endl;

  export_string("struct_info", 1);

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_erl_function_helpers(*f_iter);
  }
  f_service_    << "struct_info('i am a dummy struct') -> undefined." << endl;
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_erl_generator::generate_erl_function_helpers(t_function* tfunction) {
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_erl_generator::generate_service_interface(t_service* tservice) {

  export_string("function_info", 2);

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  f_service_ << "%%% interface" << endl;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    f_service_ <<
      indent() << "% " << function_signature(*f_iter) << endl;

    generate_function_info(tservice, *f_iter);
  }

  // Inheritance - pass unknown functions to base class
  if (tservice->get_extends() != NULL) {
      indent(f_service_) << "function_info(Function, InfoType) ->" << endl;
      indent_up();
      indent(f_service_) << uncapitalize(tservice->get_extends()->get_name())
                         << "_thrift:function_info(Function, InfoType)." << endl;
      indent_down();
  } else {
      // Dummy function_info so we don't worry about the ;s
      indent(f_service_) << "function_info(xxx, dummy) -> dummy." << endl;
  }

  indent(f_service_) << endl;
}


/**
 * Generates a function_info(FunctionName, params_type) and
 * function_info(FunctionName, reply_type)
 */
void t_erl_generator::generate_function_info(t_service* tservice,
                                                t_function* tfunction) {

  string name_atom = "'" + tfunction->get_name() + "'";



  t_struct* xs = tfunction->get_xceptions();
  t_struct* arg_struct = tfunction->get_arglist();

  // function_info(Function, params_type):
  indent(f_service_) <<
    "function_info(" << name_atom << ", params_type) ->" << endl;
  indent_up();

  indent(f_service_) << generate_type_term(arg_struct, true) << ";" << endl;

  indent_down();

  // function_info(Function, reply_type):
  indent(f_service_) <<
    "function_info(" << name_atom << ", reply_type) ->" << endl;
  indent_up();

  if (!tfunction->get_returntype()->is_void())
    indent(f_service_) <<
        generate_type_term(tfunction->get_returntype(), false) << ";" << endl;
  else if (tfunction->is_oneway())
    indent(f_service_) << "oneway_void;" << endl;
  else
    indent(f_service_) << "{struct, []}" << ";" << endl;
  indent_down();

  // function_info(Function, exceptions):
  indent(f_service_) <<
    "function_info(" << name_atom << ", exceptions) ->" << endl;
  indent_up();
  indent(f_service_) << generate_type_term(xs, true) << ";" << endl;
  indent_down();
}


/**
 * Declares a field, which may include initialization as necessary.
 *
 * @param ttype The type
 */
string t_erl_generator::declare_field(t_field* tfield) {  // TODO
  string result = "@" + tfield->get_name();
  t_type* type = get_true_type(tfield->get_type());
  if (tfield->get_value() != NULL) {
    result += " = " + render_const_value(type, tfield->get_value());
  } else {
    result += " = nil";
  }
  return result;
}

/**
 * Renders a function signature of the form 'type name(args)'
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_erl_generator::function_signature(t_function* tfunction,
                                           string prefix) {
  return
    prefix + tfunction->get_name() +
    "(This" +  capitalize(argument_list(tfunction->get_arglist())) + ")";
}

/**
 * Add a function to the exports list
 */
void t_erl_generator::export_string(string name, int num) {
  if (export_lines_first_) {
    export_lines_first_ = false;
  } else {
    export_lines_ << ", ";
  }
  export_lines_ << name << "/" << num;
}

void t_erl_generator::export_types_function(t_function* tfunction,
                                               string prefix) {

  export_types_string(prefix + tfunction->get_name(),
                      1 // This
                      + ((tfunction->get_arglist())->get_members()).size()
                      );
}

void t_erl_generator::export_types_string(string name, int num) {
  if (export_types_lines_first_) {
    export_types_lines_first_ = false;
  } else {
    export_types_lines_ << ", ";
  }
  export_types_lines_ << name << "/" << num;
}

void t_erl_generator::export_function(t_function* tfunction,
                                      string prefix) {

  export_string(prefix + tfunction->get_name(),
                1 // This
                + ((tfunction->get_arglist())->get_members()).size()
                );
}


/**
 * Renders a field list
 */
string t_erl_generator::argument_list(t_struct* tstruct) {
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

string t_erl_generator::type_name(t_type* ttype) {
  string prefix = "";
  string name = ttype->get_name();

  if (ttype->is_struct() || ttype->is_xception() || ttype->is_service()) {
    name = uncapitalize(ttype->get_name());
  }

  return prefix + name;
}

/**
 * Converts the parse type to a Erlang "type" (macro for int constants)
 */
string t_erl_generator::type_to_enum(t_type* type) {
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
    case t_base_type::TYPE_BYTE:
      return "?tType_BYTE";
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
std::string t_erl_generator::generate_type_term(t_type* type,
                                                   bool expand_structs) {
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
    case t_base_type::TYPE_BYTE:
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
    return "i32";
  } else if (type->is_struct() || type->is_xception()) {
    if (expand_structs) {
      // Convert to format: {struct, [{Fid, TypeTerm}, {Fid, TypeTerm}...]}
      std::stringstream ret;


      ret << "{struct, [";

      int first = true;
      const vector<t_field*>& fields = ((t_struct*)type)->get_members();
      vector<t_field*>::const_iterator f_iter;

      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        // Comma separate the tuples
        if (!first) ret << "," << endl << indent();
        first = false;

        ret << "{" << (*f_iter)->get_key() << ", " <<
          generate_type_term((*f_iter)->get_type(), false) << "}";
      }

      ret << "]}" << endl;

      return ret.str();
    } else {
      return "{struct, {'" + type_module(type) + "', '" + type_name(type) + "'}}";
    }
  } else if (type->is_map()) {
    // {map, KeyType, ValType}
    t_type *key_type = ((t_map*)type)->get_key_type();
    t_type *val_type = ((t_map*)type)->get_val_type();

    return "{map, " + generate_type_term(key_type, false) + ", " +
      generate_type_term(val_type, false) + "}";

  } else if (type->is_set()) {
    t_type *elem_type = ((t_set*)type)->get_elem_type();

    return "{set, " + generate_type_term(elem_type, false) + "}";

  } else if (type->is_list()) {
    t_type *elem_type = ((t_list*)type)->get_elem_type();

    return "{list, " + generate_type_term(elem_type, false) + "}";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

std::string t_erl_generator::type_module(t_type* ttype) {
  return uncapitalize(ttype->get_program()->get_name()) + "_types";
}

THRIFT_REGISTER_GENERATOR(erl, "Erlang", "");
