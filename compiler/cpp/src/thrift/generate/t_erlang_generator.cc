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

#include <algorithm>
#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>

#include "thrift/generate/t_generator.h"
#include "thrift/platform.h"
#include "thrift/version.h"
#include <sstream>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#define OUT_FILE_SUFFIX "_thrift"
#define SERVICE_FUNC_TYPE_SUFFIX "_service_functions"
#define ERROR_SPEC "(_) -> no_return()."

using std::map;
using std::ofstream;
using std::ostream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const std::string endl = "\n"; // avoid ostream << std::endl flushes

static string & strip_unsafe(string & s) {
  struct detail {
    static char strip_unsafe(char c) {
      switch (c) {
        case '.': case '-': case '/': case '\\':
          return '_';
        default:
          return c;
      }
    }
  };
  std::transform(s.begin(), s.end(), s.begin(), detail::strip_unsafe);
  return s;
}

static std::string snake_case(std::string const& in) {
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

/**
 * Erlang code generator.
 */
class t_erlang_generator : public t_generator {
public:
  t_erlang_generator(t_program* program,
                  const std::map<std::string, std::string>& parsed_options,
                  const std::string& option_string)
    : t_generator(program)
    , app_namespaces_(false)
    , idiomatic_names_(false)
    , scoped_typenames_(false)
    , app_prefix_("")
    , use_maps_(false) {
    (void)option_string;
    std::map<std::string, std::string>::const_iterator iter;

    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if (iter->first.compare("idiomatic") == 0) {
        idiomatic_names_ = true;
        continue;
      }
      if (iter->first.compare("scoped_typenames") == 0) {
        scoped_typenames_ = true;
        continue;
      }
      if(iter->first.compare("app_prefix") == 0) {
        app_prefix_ = iter->second;
        strip_unsafe(app_prefix_);
        continue;
      }
      if(iter->first.compare("app_namespaces") == 0) {
        app_namespaces_ = true;
        continue;
      if (iter->first.compare("use_maps") == 0) {
        use_maps_ = true;
        continue;
      }
      throw "unknown option: " + iter->first;
    }

    if (app_namespaces_ && scoped_typenames_) {
      throw "conflicting options: app_namespaces, scoped_typenames";
    }
    if (app_namespaces_ && !app_prefix_.empty()) {
      throw "conflicting options: app_namespaces, app_prefix";
    }

    out_dir_base_ = "gen-erlang";
  }

  struct indenter {
    indenter() : indent(0), indent_str(4, ' ') {}
    indenter(size_t size) : indent(0), indent_str(size, ' ') {}

    std::string nl() {
      string s = endl;
      for (size_t i = 0; i < indent; ++i) {
        s += indent_str;
      }
      return s;
    }

    std::string nl(size_t _indent) {
      string s = endl;
      for (size_t i = 0; i < _indent; ++i) {
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
  std::string display_name() const override;

  /**
   * Program-level generation functions
   */
  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum);
  void generate_const(t_const* tconst);
  void generate_struct(t_struct* tstruct);
  void generate_service(t_service* tservice);
  void generate_member_type(std::ostream& out, t_type* type);
  void generate_member_value(std::ostream& out, t_type* type, t_const_value* value);


private:
  /**
   * options
   */
  bool app_namespaces_;
  bool idiomatic_names_;
  bool scoped_typenames_;
  bool use_maps_;
  std::string app_prefix_; // deprecated

  /**
   * write out headers and footers for hrl files
   */
  static std::string render_hrl_header(std::string name);
  static std::string render_hrl_footer();

  /**
   * Helper rendering functions
   */
  static std::string render_namespace(const t_program*);
  static std::string render_namespaced(const t_program*, std::string const& s);
  static std::string render_application_name(const t_program*);
  static std::string render_appmodule_name(const t_program*);
  static std::string render_member_requiredness(t_field* field);
  static std::string render_export(std::string, int);
  static std::string render_export_type(std::string, int);

  template <class Type>
  void render_export_specific_types(std::ostream& os, vector<Type*> types);

  static std::string render_attribute_list(std::string, std::string);
  static std::string render_attribute(std::string, std::string);
  std::string render_include(t_program* p);
  std::string render_local_include(const t_program*);

  std::string render_includes();
  std::string render_member_value(std::string name, t_field* field, indenter& ind);
  std::string render_member_type(t_field* field, bool force_full_type);
  std::string render_type(t_type* type, bool force_full_type);
  std::string render_const_value(t_type* type, std::string name, t_const_value* value, indenter& ind);
  std::string render_type_term(t_type* ttype, bool expand_structs, indenter& ind);
  std::string render_module_name(const t_program* p);
  std::string render_module_scoped(const std::string in, const t_program*);

  template <class Type>
  static std::string render_string(Type const& v);

  static const std::string render_double(const double);

  /**
   * Struct generation code
   */
  void generate_namespace_type(std::ostream& out);
  void generate_namespace_definition(std::ostream& out);
  void generate_union_definition(std::ostream& out, t_struct* tstruct);
  void generate_struct_definition(std::ostream& out, t_struct* tstruct);
  void generate_struct_member(std::ostream& out, std::string name, t_field* tmember, indenter& ind);
  void generate_struct_info(std::ostream& out, t_struct* tstruct);
  void generate_struct_api_new(ostream& out, vector<t_struct*> structs);
  void generate_struct_api_new_spec(ostream& out, vector<t_struct*> structs, vector<t_struct*> xceptions);
  void generate_struct_api_get(ostream& out, vector<t_struct*> structs);
  void generate_struct_api_get_type(ostream& out, vector<t_struct*> structs);
  void generate_typespecs(std::ostream& out);
  std::string comment_title(const std::string& title);
  void generate_typespec_function_name(std::ostream& os);
  void generate_typespec_service_function_types(std::ostream& os);
  void generate_typespec_service_function_type(std::ostream& os, t_service* s);
  void generate_enum_types(std::ostream& os);
  void generate_typespec_enum_choice(std::ostream& os);
  void generate_enum_info(std::ostream& out, t_enum* tenum);
  void generate_typedef_info(std::ostream& os, t_typedef*);
  void generate_typedef_types(std::ostream& os);
  void generate_struct_types(std::ostream& os);
  void generate_struct_type_member(std::ostream& os, t_struct* tstruct);
  void generate_typedef_metadata(std::ostream& erlout);
  void generate_struct_metadata(std::ostream& erlout, std::ostream& hrlout);
  void generate_record_metadata(std::ostream& erlout);
  void generate_enum_metadata(std::ostream& out);


  void gather_struct_types(const t_program*, std::vector<t_struct*>&);

  template <class Type>
  void iterate_type(std::ostream& os, vector<Type*> type, std::string delim, std::string end, indenter& i);
  template <class Type>
  void generate_type_list(
    std::ostream& out, std::string function_name, std::string el_type, vector<Type*> types
  );
  template <class Type>
  void generate_typespec_list(std::ostream& os, std::string type_name, vector<Type*> types);

  /**
   * Service-level generation functions
   */
  void generate_service_metadata(std::ostream& os);
  void generate_service_metadata(std::ostream& os, t_service* tservice);
  void generate_service_interface(std::ostream& os, t_service* tservice);
  void generate_function_info(std::ostream& os, t_service* tservice, t_function* tfunction);

  static std::string erl_autogen_comment();

  std::string type_name(t_type* ttype);
  std::string scoped_type_name(t_type* ttype);
  std::string service_name(t_service* ttype);
  std::string service_name(t_service* ttype, bool do_atomify);
  std::string idiomify(const std::string& str);
  std::string function_name(t_function* ttype);
  std::string field_name(t_field* ttype);

  std::string type_to_enum(t_type* ttype);
  std::string type_module(const t_type* ttype);

  static std::string atomify(std::string in) {
    return "'" + in + "'";
  }

  static std::string constify(std::string in) {
    return uppercase(in);
  }

  /**
   * File streams
   */
  std::ofstream f_erl_file_;
  std::ofstream f_hrl_file_;

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
  string base_name = type_module(get_program());
  string f_erl_filename = get_out_dir() + base_name + ".erl";
  string f_hrl_filename = get_out_dir() + base_name + ".hrl";

  f_erl_file_.open(f_erl_filename.c_str());
  f_hrl_file_.open(f_hrl_filename.c_str());

  f_erl_file_ << erl_autogen_comment() << endl
              << render_attribute("module", base_name) << endl;
  if(!use_maps_)
  {
    f_erl_file_ << render_local_include(get_program()) << endl;
  }
  f_hrl_file_ << render_hrl_header(base_name) << endl
              << render_includes() << endl;

  f_erl_file_ << render_export("namespace", 0)
              << render_export("enums", 0)
              << render_export("typedefs", 0)
              << render_export("structs", 0)
              << render_export("services", 0)
              << render_export("flags", 0)
              << render_export("typedef_info", 1)
              << render_export("enum_info", 1)
              << render_export("struct_info", 1);
  if(!use_maps_)
  {
    f_erl_file_ << render_export("record_name", 1);
  }
  else
  {
    f_erl_file_ << render_export("struct_new", 2)
                << render_export("struct_get", 1)
                << render_export("struct_get_type", 1);
  }
  f_erl_file_ << render_export("functions", 1)
              << render_export("function_info", 3)
              << endl
              << render_export_type("namespace", 0)
              << render_export_type("typedef_name", 0)
              << render_export_type("enum_name", 0)
              << render_export_type("struct_name", 0)
              << render_export_type("exception_name", 0)
              << render_export_type("service_name", 0)
              << render_export_type("function_name", 0)
              << endl
              // The following 2 types are not supposed to be used externally.
              // They are exported to trick Dialyzer in cases, when there are
              // no enums/structs/exceptions defined.
              // @ToDo: handle thouse cases in types generation routines instead.
              << render_export_type("enum_info", 0)
              << render_export_type("struct_info", 0)
              << endl;

  generate_typespecs(f_erl_file_);

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
  if (app_namespaces_) {
    return "%% YOU MIGHT ALSO CONDIDER INCLUDING \"" + render_module_name(p) + ".hrl\"" + endl;
  }
  else {
    return render_local_include(p);
  }
}

string t_erlang_generator::render_local_include(const t_program* p) {
  return render_attribute("include", "\"" + render_module_name(p) + ".hrl\"");
}

/**
 * Closes the type files
 */
void t_erlang_generator::close_generator() {
  f_hrl_file_ << endl;

  generate_type_list(f_erl_file_, "typedefs", "typedef_name()", get_program()->get_typedefs());
  generate_type_list(f_erl_file_, "enums", "enum_name()", get_program()->get_enums());
  generate_type_list(f_erl_file_, "structs", "struct_name()", get_program()->get_structs());
  generate_type_list(f_erl_file_, "services", "service_name()", get_program()->get_services());

  generate_namespace_definition(f_erl_file_);
  generate_typedef_metadata(f_erl_file_);
  generate_enum_metadata(f_erl_file_);
  generate_struct_metadata(f_erl_file_, f_hrl_file_);
  if(!use_maps_){
    generate_record_metadata(f_erl_file_);
  }
  generate_service_metadata(f_erl_file_);

  f_hrl_file_ << render_hrl_footer();

  f_erl_file_.close();
  f_hrl_file_.close();
}

const std::string t_erlang_generator::render_double(const double value) {
  std::stringstream double_output_stream;
  // sets the maximum precision: http://en.cppreference.com/w/cpp/io/manip/setprecision
  // sets the output format to fixed: http://en.cppreference.com/w/cpp/io/manip/fixed (not in scientific notation)
  double_output_stream << std::setprecision(std::numeric_limits<double>::digits10 + 1);

  #ifdef _MSC_VER
      // strtod is broken in MSVC compilers older than 2015, so std::fixed fails to format a double literal.
      // more details: https://blogs.msdn.microsoft.com/vcblog/2014/06/18/
      //               c-runtime-crt-features-fixes-and-breaking-changes-in-visual-studio-14-ctp1/
      //               and
      //               http://www.exploringbinary.com/visual-c-plus-plus-strtod-still-broken/
      #if _MSC_VER >= MSC_2015_VER
          double_output_stream << std::fixed;
      #else
          // note that if this function is called from the erlangang generator and the MSVC compiler is older than 2015,
          // the double literal must be output in the scientific format. There can be some cases where the
          // mantissa of the output does not have fractionals, which is illegal in Erlang.
          // example => 10000000000000000.0 being output as 1e+16
          double_output_stream << std::scientific;
      #endif
  #else
      double_output_stream << std::fixed;
  #endif

  double_output_stream << value;

  return double_output_stream.str();
}

void t_erlang_generator::generate_typespecs(std::ostream& os) {
  indenter i;
  render_export_specific_types(os, get_program()->get_typedefs());
  render_export_specific_types(os, get_program()->get_enums());
  render_export_specific_types(os, get_program()->get_structs());
  render_export_specific_types(os, get_program()->get_xceptions());
  generate_namespace_type(os);
  os << endl << comment_title("typedefs");
  generate_typespec_list(os, "typedef_name", get_program()->get_typedefs());
  generate_typedef_types(os);
  os << comment_title("enums");
  generate_typespec_list(os, "enum_name", get_program()->get_enums());
  generate_enum_types(os);
  os << comment_title("structs, unions and exceptions");
  generate_typespec_list(os, "struct_name", get_program()->get_structs());
  generate_typespec_list(os, "exception_name", get_program()->get_xceptions());
  generate_struct_types(os);
  os << comment_title("services and functions");
  generate_typespec_list(os, "service_name", get_program()->get_services());
  generate_typespec_function_name(os);
  generate_typespec_service_function_types(os);
  os << endl
     << "-type struct_flavour() :: struct | exception | union." << endl
     << "-type field_num() :: pos_integer()." << endl
     << "-type field_name() :: atom()." << endl
     << "-type field_req() :: required | optional | undefined." << endl
     << endl
     << "-type type_ref() :: {module(), atom()}." << endl
     << "-type field_type() ::" << i.nlup()
     << "bool | byte | i16 | i32 | i64 | string | double |" << i.nl()
     << "{enum, type_ref()} |" << i.nl()
     << "{struct, struct_flavour(), type_ref()} |" << i.nl()
     << "{list, field_type()} |" << i.nl()
     << "{set, field_type()} |" << i.nl()
     << "{map, field_type(), field_type()}." << i.nldown()
     << endl
     << "-type struct_field_info() ::" << i.nlup()
     << "{field_num(), field_req(), field_type(), field_name(), any()}." << i.nldown()
     << "-type struct_info() ::" << i.nlup()
     << "{struct, struct_flavour(), [struct_field_info()]}." << i.nldown()
     << endl;
  generate_typespec_enum_choice(os);
  os << "-type enum_field_info() ::" << i.nlup()
     << "{enum_choice(), integer()}." << i.nldown()
     << "-type enum_info() ::" << i.nlup()
     << "{enum, [enum_field_info()]}." << i.nldown()
     << endl;
}

string t_erlang_generator::comment_title(const std::string& title) {
  return "%%" + endl + "%% " + title + endl + "%%" + endl;
}

std::string t_erlang_generator::render_namespace(const t_program* p) {
  return atomify(p->get_namespace("erlang"));
}

void t_erlang_generator::generate_namespace_type(std::ostream& os) {
  os << endl;
  os << "-type namespace() :: " << render_namespace(get_program()) << "." << endl;
}

template <class Type>
void t_erlang_generator::render_export_specific_types(
  std::ostream& os, vector<Type*> types
) {
  indenter i;
  if (types.size() > 0) {
    os << "-export_type([";
    iterate_type(os, types, "/0,", "/0", i);
    os << "])." << endl;
  }
}

template <class Type>
void t_erlang_generator::generate_typespec_list(
  std::ostream& os, std::string type_name, vector<Type*> types
) {
  indenter i;
  os << "-type " << type_name << "() ::";
  iterate_type(os, types, " |", ".", i);
  if (types.size() == 0) {
    os << " none()." << i.nldown();
  }
  os << endl;
}

void t_erlang_generator::generate_namespace_definition(std::ostream& os) {
  indenter i;
  os << "-spec namespace() -> namespace()." << i.nl()
    << i.nl();
  os << "namespace() ->" << i.nlup()
     << render_namespace(get_program()) << "." << i.nldown()
     << i.nl();
}

void t_erlang_generator::generate_typedef_types(std::ostream& os) {
  typedef vector<t_typedef*> vec;
  vec const& tdefs = get_program()->get_typedefs();
  for(vec::const_iterator it = tdefs.begin(); it != tdefs.end(); ++it) {
    os << "-type " << type_name(*it) << "() :: " << render_type((*it)->get_type(), false) << "." << endl;
  }
  os << endl;
}

void t_erlang_generator::generate_typespec_function_name(std::ostream& os) {
  typedef vector<t_service*> vec_s;
  indenter i;
  vec_s const& services = get_program()->get_services();
  os << "-type function_name() ::";
  if (services.size() > 0) {
    os << i.nlup();
    for(vec_s::const_iterator s = services.begin(); s != services.end();) {
      os << atomify(service_name(*s, false) + SERVICE_FUNC_TYPE_SUFFIX) << "()";
      if (++s != services.end()) {
        os << " |" << i.nl();
      } else {
        os << "." << i.nldown();
      }
    }
  } else {
    os << " none()." << endl;
  }
  os << endl;
}

void t_erlang_generator::generate_typespec_service_function_types(std::ostream& os) {
  typedef vector<t_service*> vec_s;
  vec_s const& services = get_program()->get_services();
  if (services.size() > 0) {
    for(vec_s::const_iterator s = services.begin(); s != services.end(); ++s)
      generate_typespec_service_function_type(os, *s);
  }
}

void t_erlang_generator::generate_typespec_service_function_type(std::ostream& os, t_service* s) {
  indenter i;
  typedef vector<t_function*> vec_f;
  os << "-type " << atomify(service_name(s, false) + SERVICE_FUNC_TYPE_SUFFIX) << "() ::" << i.nlup();

  vec_f const& functions = s->get_functions();
  if (functions.size() > 0) {
    for (vec_f::const_iterator f = functions.begin(); f != functions.end();) {
      os << function_name(*f);
      if (++f != functions.end()) {
        os << " |" << i.nl();
      }
    }
  } else {
    os << "none()";
  }

  // Add base class function types as well
  t_service* base = s->get_extends();
  if (base) {
    os << " |" << i.nl();
    os << type_module(base) << ":" << atomify(service_name(base, false) + SERVICE_FUNC_TYPE_SUFFIX) << "()";
  }
  os << "." << i.nldown() << endl;

  os << render_export_type(atomify(service_name(s, false) + SERVICE_FUNC_TYPE_SUFFIX), 0) << endl;
}


void t_erlang_generator::generate_typespec_enum_choice(std::ostream& os) {
  vector<t_enum*> const& enums = get_program()->get_enums();
  indenter i;
  os << "-type enum_choice() ::";
  iterate_type(os, enums, "() |", "().", i);
  if (enums.size() == 0) {
    os << " none()." << i.nl();
  }
  os << endl;
}

void t_erlang_generator::generate_enum_types(std::ostream& os) {
  typedef vector<t_enum*> vec_e;
  typedef vector<t_enum_value*> vec_ev;
  indenter i;
  vec_e const& enums = get_program()->get_enums();
  for(vec_e::const_iterator e = enums.begin(); e != enums.end(); ++e) {
    vec_ev const& constants = (*e)->get_constants();
    os << "%% enum " << type_name(*e) << i.nl();
    os << "-type " << type_name(*e) << "() ::";
    if (constants.size() > 0) {
      os << i.nlup();
      for (vec_ev::const_iterator ev = constants.begin(); ev != constants.end();) {
        os << atomify(snake_case((*ev)->get_name()));
        if (++ev != constants.end()) {
        os << " |" << i.nl();
        }
      }
      os << "." << i.nldown();
    } else {
      os << " none()." << endl;
    }
    os << endl;
  }
}

template <class Type>
void t_erlang_generator::iterate_type(
  std::ostream& os, vector<Type*> type, std::string delim, std::string end, indenter& i
) {
  if (type.size() > 0) {
    os << i.nlup();
    for (size_t j = 0; j < type.size();) {
      os << type_name(type[j]);
      if (++j != type.size()) {
        os << delim << i.nl();
      }
    }
    os << end << i.nldown();
  }
}

template <class Type>
void t_erlang_generator::generate_type_list(
  std::ostream& os, std::string function_name, std::string el_type, vector<Type*> types
) {
  indenter i;
  os << "-spec " << function_name << "() -> [";
  if (types.size() > 0) {
    os << el_type;
  }
  os << "]." << endl << endl
     << function_name << "() ->" << i.nlup()
     << "[";
  iterate_type(os, types, ",", "", i);
  os << "]." << endl << endl;
}

void t_erlang_generator::generate_struct_types(std::ostream& os) {
  typedef vector<t_struct*> vec;

  vec const& structs = get_program()->get_structs();
  for(vec::const_iterator it = structs.begin(); it != structs.end(); ++it) {
    if ((*it)->is_union()) {
      generate_union_definition(os, *it);
    } else {
        os << "%% struct " << type_name(*it) << endl;
        if(!use_maps_){
            os << "-type " + type_name(*it) << "() :: #" + scoped_type_name(*it) + "{}." << endl << endl;
        }
        else{
            os << "-type " + type_name(*it) << "() :: #{";
            generate_struct_type_member(os, *it);
            os << "}." << endl << endl;
        }
    }
  }

  vec const& xceptions = get_program()->get_xceptions();
  for(vec::const_iterator it = xceptions.begin(); it != xceptions.end(); ++it) {
    os << "%% exception " << type_name(*it) << endl;
    if(!use_maps_){
        os << "-type " + type_name(*it) << "() :: #" + scoped_type_name(*it) + "{}." << endl << endl;
    }
    else{
        os << "-type " + type_name(*it) << "() :: #{";
        generate_struct_type_member(os, *it);
        os << "}." << endl << endl;
    }
  }
}

/**
 * Generates a typedef. no op
 *
 * @param ttypedef The type definition
 */
void t_erlang_generator::generate_typedef(t_typedef* ttypedef) {
  (void)ttypedef;
}

void t_erlang_generator::generate_typedef_metadata(std::ostream& erl) {
  typedef vector<t_typedef*> vec;
  vec const& tdefs = get_program()->get_typedefs();

  erl << "-spec typedef_info";
  if (tdefs.size() > 0) {
    erl <<"(typedef_name()) -> field_type() | no_return()." << endl << endl;
    for(vec::const_iterator it = tdefs.begin(); it != tdefs.end(); ++it) {
      generate_typedef_info(erl, *it);
    }
  }
  else {
    erl << ERROR_SPEC << endl << endl;
  }
  erl << "typedef_info(_) -> erlang:error(badarg)." << endl << endl;
}

void t_erlang_generator::generate_typedef_info(std::ostream& os, t_typedef* tdef) {
  indenter i;
  os << "typedef_info(" << type_name(tdef) << ") ->" << i.nlup()
     << render_type_term(tdef->get_type(), false, i) << ";" << i.nldown() << endl;
}

/**
 * Generates code for an enumerated type. Done using a class to scope
 * the values.
 *
 * @param tenum The enumeration
 */
void t_erlang_generator::generate_enum(t_enum* tenum) {
  (void)tenum;
}

void t_erlang_generator::generate_enum_info(std::ostream& buf, t_enum* tenum) {
  vector<t_enum_value*> const& constants = tenum->get_constants();

  indenter i;
  buf << "enum_info(" << type_name(tenum) << ") ->" << i.nlup()
      << "{enum, [";
  if (constants.size() > 0) {
    buf << i.nlup();
    for (vector<t_enum_value*>::const_iterator it = constants.begin(); it != constants.end(); ) {
      int value = (*it)->get_value();
      string name = atomify(snake_case((*it)->get_name()));
      buf << "{" << name << ", " << value << "}";
      if (++it != constants.end()) {
        buf << "," << i.nl();
      }
    }
    buf << i.nldown();
  }

  buf << "]};" << endl << endl;
}

void t_erlang_generator::generate_enum_metadata(std::ostream& os) {
  typedef vector<t_enum*> vec;
  vec const& enums = get_program()->get_enums();

  os << "-spec enum_info";
  if (enums.size() > 0) {
    os << "(enum_name()) -> enum_info() | no_return()." << endl << endl;
    for(vec::const_iterator it = enums.begin(); it != enums.end(); ++it) {
      generate_enum_info(os, *it);
    }
  } else {
    os << ERROR_SPEC << endl << endl;
  }
  os << "enum_info(_) -> erlang:error(badarg)." << endl << endl;
}

/**
 * Generate a constant value
 */
void t_erlang_generator::generate_const(t_const* tconst) {
  indenter i;
  std::string name = constify(render_module_scoped(tconst->get_name(), tconst->get_type()->get_program()));
  std::string value = render_const_value(tconst->get_type(), tconst->get_name(), tconst->get_value(), i);
  f_hrl_file_ << "-define(" << name << ", " << value << ")." << endl << endl;
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
string t_erlang_generator::render_const_value(t_type* type, std::string name, t_const_value* value, indenter& ind) {
  type = get_true_type(type);
  std::ostringstream out;
  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      out << "<<\"" << get_escaped_string(value) << "\">>";
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
        out << "float(" << value->get_integer() << ")";
      } else {
        out << render_double(value->get_double());
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    out << atomify(((t_enum *)(type))->get_constant_by_value(value->get_integer())->get_name());
  } else if (type->is_struct() && ((t_struct*)type)->is_union()) {
    const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter = val.begin();
    if (val.size() != 1) {
      throw "compiler error: '" + name + "' - only one member is allowed for Union type";
    }
    out << "{";
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
      t_type* field_type = NULL;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
          break;
        }
      }
      if (field_type == NULL) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }
      out << v_iter->first->get_string();
      out << "," << ind.nlup();
      out << render_const_value(field_type, name + "." + v_iter->first->get_string(), v_iter->second, ind);
      out << ind.nldown() << "}";
  } else if (type->is_struct() || type->is_xception()) {
    out << "#" << scoped_type_name(type) << "{";
    if (value->get_map().size() > 0) {
      out << ind.nlup();
      const vector<t_field*>& fields = ((t_struct*)type)->get_members();
      vector<t_field*>::const_iterator f_iter;
      const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = value->get_map();
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
          out << "," << ind.nl();
        }
        out << v_iter->first->get_string();
        out << " = ";
        out << render_const_value(field_type, name + "." + v_iter->first->get_string(), v_iter->second, ind);
      }
      out << ind.nldown();
    }
    out << "}";
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();

    out << "#{";
    if (value->get_map().size() > 0) {
      out << ind.nlup();
      map<t_const_value*, t_const_value*>::const_iterator i, end = value->get_map().end();
      for (i = value->get_map().begin(); i != end;) {
        out << render_const_value(ktype, name + "." + ktype->get_name(), i->first, ind) << " => "
            << render_const_value(vtype, name + "." + vtype->get_name(), i->second, ind);
        if (++i != end) {
          out << "," << ind.nl();
        }
      }
      out << ind.nldown();
    }
    out << "}";
  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    out << "ordsets:from_list([";
    if (value->get_list().size() > 0) {
      out << ind.nlup();
      vector<t_const_value*>::const_iterator i, end = value->get_list().end();
      for (i = value->get_list().begin(); i != end;) {
        out << render_const_value(etype, name + "." + etype->get_name(), *i, ind);
        if (++i != end) {
          out << "," << ind.nl();
        }
      }
      out << ind.nldown();
    }
    out << "])";
  } else if (type->is_list()) {
    t_type* etype;
    etype = ((t_list*)type)->get_elem_type();
    out << "[";
    if (value->get_list().size() > 0) {
      out << ind.nlup();
      bool first = true;
      const vector<t_const_value*>& val = value->get_list();
      vector<t_const_value*>::const_iterator v_iter;
      for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
        if (first) {
          first = false;
        } else {
          out << "," << ind.nl();
        }
        out << render_const_value(etype, name + "." + etype->get_name(), *v_iter, ind);
      }
      out << ind.nldown();
    }
    out << "]";
  } else {
    throw "CANNOT GENERATE CONSTANT FOR TYPE: " + type->get_name();
  }
  return out.str();
}

string t_erlang_generator::render_member_type(t_field* field, bool force_full_type) {
  return render_type(field->get_type(), force_full_type);
}

string t_erlang_generator::render_type(t_type* type, bool force) {
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
  } else if (type->is_struct() || type->is_xception() || type->is_typedef() || type->is_enum()) {
    return force || type->get_program() != get_program() ?
      type_module(type) + ":" + type_name(type) + "()" :
      type_name(type) + "()";
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    return "#{" + render_type(ktype, force) + " => " + render_type(vtype, force) + "}";
  } else if (type->is_set()) {
    return "ordsets:ordset(" + render_type(((t_set*)type)->get_elem_type(), force) + ")";
  } else if (type->is_list()) {
    return "[" + render_type(((t_list*)type)->get_elem_type(), force) + "]";
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
  (void)tstruct;
}

void t_erlang_generator::gather_struct_types(const t_program* p, std::vector<t_struct*>& v) {

  struct filter {
    static bool check (const t_struct* s) { return s->is_union(); }
  };

  std::vector<t_struct*> const& structs = p->get_structs();
  std::vector<t_struct*> const& xceptions = p->get_xceptions();
  std::remove_copy_if(structs.begin(), structs.end(), std::back_inserter(v), &filter::check);
  std::copy(xceptions.begin(), xceptions.end(), std::back_inserter(v));

}

void t_erlang_generator::generate_struct_metadata(std::ostream& erl, std::ostream& hrl) {
  typedef vector<t_struct*> vec;
  vec const& structs = get_program()->get_structs();
  vec const& xceptions = get_program()->get_xceptions();

  erl << "-spec struct_info";
  if (structs.size() > 0 || xceptions.size() > 0) {
    erl << "(struct_name() | exception_name()) -> struct_info() | no_return()." << endl << endl;
    for(vec::const_iterator it = structs.begin(); it != structs.end(); ++it) {
      generate_struct_info(erl, *it);
      if (!(*it)->is_union() && !use_maps_) {
        generate_struct_definition(hrl, *it);
      }
    }
    for(vec::const_iterator it = xceptions.begin(); it != xceptions.end(); ++it) {
      generate_struct_info(erl, *it);
      if(!use_maps_){
            generate_struct_definition(hrl, *it);
        }
    }
  } else {
    erl << ERROR_SPEC << endl << endl;
  }
  erl << "struct_info(_) -> erlang:error(badarg)." << endl << endl;

  if(use_maps_){
    if (structs.size() > 0 || xceptions.size() > 0) {
        generate_struct_api_new_spec(erl, structs, xceptions);
        generate_struct_api_new(erl, structs);
        generate_struct_api_new(erl, xceptions);
    }
    else{
        erl << "-spec struct_new(_, _) -> no_return()." << endl << endl;
    }
    erl << "struct_new(_, _) -> error(badarg)." << endl << endl;

    if (structs.size() > 0 || xceptions.size() > 0) {
        erl << "-spec struct_get(map()) -> map() | no_return()."
            << endl << endl;
        generate_struct_api_get(erl, structs);
        generate_struct_api_get(erl, xceptions);
    }
    else{
        erl << "-spec struct_get" << ERROR_SPEC << endl << endl;
    }
    erl << "struct_get(_) -> error(badarg)." << endl << endl;

    if (structs.size() > 0 || xceptions.size() > 0) {
        erl << "-spec struct_get_type(map()) -> atom() | no_return()."
            << endl << endl;
        generate_struct_api_get_type(erl, structs);
        generate_struct_api_get_type(erl, xceptions);
    }
    else{
        erl << "-spec struct_get_type" << ERROR_SPEC << endl << endl;
    }
    erl << "struct_get_type(_) -> error(badarg)." << endl << endl;
    erl << "-spec flags() -> list()." << endl << endl;
    erl << "flags() -> [structs_as_maps]." << endl << endl;
  }
  else
  {
    erl << "-spec flags() -> list()." << endl << endl;
    erl << "flags() -> []." << endl << endl;
  }
}

void t_erlang_generator::generate_record_metadata(std::ostream& erl) {
  typedef vector<t_struct*> vec;

  vec structs;
  gather_struct_types(get_program(), structs);
  // std::string const& ns = get_program()->get_namespace("erlang");

  indenter i;
  if (structs.size() > 0) {
    erl << "-spec record_name(struct_name() | exception_name()) -> atom() | no_return()." << i.nl()
        << i.nl();
    for(vec::const_iterator it = structs.begin(); it != structs.end(); ++it) {
      erl << "record_name(" << type_name(*it) << ") ->" << i.nlup()
          << scoped_type_name(*it) << ";" << i.nldown()
          << i.nl();
    }
  }
  else {
    erl << "-spec record_name" << ERROR_SPEC << i.nl()
        << i.nl();
  }
  erl << "record_name(_) -> error(badarg)." << i.nl()
      << i.nl();
}

/**
 * Generates a union definition for a thrift data type.
 *
 * @param tstruct The union definition
 */
void t_erlang_generator::generate_union_definition(ostream& out, t_struct* tstruct) {
  indenter i;
  out << "%% union " << type_name(tstruct) << endl
      << "-type " << type_name(tstruct) << "() ::";
  vector<t_field*> const& members = tstruct->get_members();
  if (members.size() > 0) {
    out << i.nlup();
    for (vector<t_field*>::const_iterator it = members.begin(); it != members.end();) {
      out << "{" << field_name(*it) << ", " << render_member_type(*it, false) << "}";
      if (++it != members.end()) {
        out << " |" << i.nl();
      } else {
        out << "." << i.nldown();
      }
    }
  } else {
    out << " none()." << endl;
  }
  out << endl;
}

/**
 * Generates a struct definition for a thrift data type.
 *
 * @param tstruct The struct definition
 */
void t_erlang_generator::generate_struct_definition(ostream& out, t_struct* tstruct) {
  indenter ind;
  if (tstruct->is_xception()) {
    out << "%% exception ";
  } else {
    out << "%% struct ";
  }
  out << type_name(tstruct) << endl
      << "-record(" << scoped_type_name(tstruct) << ", {";
  vector<t_field*> const& members = tstruct->get_members();
  if (members.size() > 0) {
    out << ind.nlup();
    for (vector<t_field*>::const_iterator it = members.begin(); it != members.end();) {
      generate_struct_member(out, type_name(tstruct), *it, ind);
      if (++it != members.end()) {
        out << "," << ind.nl();
      }
    }
    out << ind.nldown();
  }
  out << "})." << endl << endl;
}

void t_erlang_generator::generate_struct_type_member(std::ostream& os, t_struct* tstruct)
{
    indenter ind;
    vector<t_field*> const& members = tstruct->get_members();
    os << ind.nlup();
    os << "'$struct' := " << scoped_type_name(tstruct);
    if (members.size() > 0) {
        os << ",";
        os << ind.nlup();
        for (vector<t_field*>::const_iterator _it = members.begin(); _it != members.end();) {
          generate_struct_member(os, type_name(tstruct), *_it, ind);
          if (++_it != members.end()) {
            os << "," << ind.nl();
          }
        }
        os << ind.nldown();
    }
    else{
        os << ind.nldown();
    }
}
/**
 * Generates the record field definition
 */
void t_erlang_generator::generate_struct_member(ostream& out, std::string name, t_field* tmember, indenter& ind) {
  out << field_name(tmember);
  if(!use_maps_){
      if (tmember->get_value()) {
        out << " = " << render_member_value(name + "." + field_name(tmember), tmember, ind);
      }
      out << " :: " << render_member_type(tmember, true);
      if (tmember->get_req() == t_field::T_OPTIONAL) {
        out << " | undefined";
      }
  }
  else{
      if (tmember->get_req() == t_field::T_OPTIONAL) {
        out << " => " << render_member_type(tmember, true);
      }
      else{
        out << " := " << render_member_type(tmember, true);
      }
  }
}

string t_erlang_generator::render_member_value(std::string name, t_field* field, indenter& ind) {
  if (field->get_value()) {
    return render_const_value(field->get_type(), name, field->get_value(), ind);
  }
  return "undefined";
}

/**
 * Generates the read method for a struct
 */

void t_erlang_generator::generate_struct_info(ostream& out, t_struct* tstruct) {
  indenter i;
  out << "struct_info(" << type_name(tstruct) << ") ->" << i.nlup()
      << render_type_term(tstruct, true, i) << ";" << endl << endl;
}

/**
 * Generates the new, get and get_type method for a struct
 */

void t_erlang_generator::generate_struct_api_new_spec(ostream& out, vector<t_struct*> structs, vector<t_struct*> xceptions) {
    //erl << "-spec struct_new(struct_name() | exception_name(), map()) -> map() | no_return()."
    indenter ind;
    out << "-spec struct_new" << ind.nl(1);
    for(vector<t_struct*>::const_iterator _it = structs.begin(); _it != structs.end(); ++_it)
    {
        out << "(" << type_name(*_it) << ", map()) -> " << type_name(*_it) << "()";
        if(_it + 1 == structs.end() && xceptions.size() == 0){
            out << ".";
        }
        else{
            out << ";" << ind.nl(1);
        }
    }
    for(vector<t_struct*>::const_iterator _it = xceptions.begin(); _it != xceptions.end(); ++_it)
    {
        out << "(" << type_name(*_it) << ", map()) -> " << type_name(*_it) << "()";
        if(_it + 1 == xceptions.end()){
            out << ".";
        }
        else{
            out << ";" << ind.nl(1);
        }
    }
    out << endl << endl;
}
void t_erlang_generator::generate_struct_api_new(ostream& out, vector<t_struct*> structs) {
    for(vector<t_struct*>::const_iterator _it = structs.begin(); _it != structs.end(); ++_it)
    {
        indenter ind;
        string f_head = "";
        string f_body = "";
        vector<t_field*> const& members = (*_it)->get_members();

        if (members.size() > 0) {
            f_head += "struct_new(" + type_name(*_it) + ", Map = #{";

            f_body += "Map#{" + ind.nl(2);
            f_body += "'$struct' => " + type_name(*_it);
            int arg_count = 0;
            for (vector<t_field*>::const_iterator it = members.begin(); it != members.end();) {
                if ((*it)->get_req() == t_field::T_REQUIRED) {
                    if(arg_count++){
                        f_head += ", ";
                    }
                    string name = field_name(*it);
                    string arg_name = "_Arg_" + name;
                    arg_name.erase(std::remove(arg_name.begin(), arg_name.end(), '\''), arg_name.end());

                    f_head += name + " := " + arg_name;
                }
                it++;
            }
            f_body += ind.nl(1) + "};" + endl + endl;
            f_head += "}) ->" + ind.nl(1);
            out << f_head << f_body;
        }
        else{
            out << "struct_new(" << type_name(*_it) << ", _) ->" << ind.nl(1);
            out << "#{" << ind.nl(2);
            out << "'$struct' => " << type_name(*_it) << ind.nl(1);
            out << "};" << endl << endl;
        }
    }
}

void t_erlang_generator::generate_struct_api_get(ostream& out, vector<t_struct*> structs) {
    for(vector<t_struct*>::const_iterator _it = structs.begin(); _it != structs.end(); ++_it)
    {
        indenter ind;
        out << "struct_get(#{'$struct' := " << type_name(*_it) << "} = Map) ->" << ind.nl(1);
        out << "maps:remove('$struct', Map);" << endl << endl;
    }
}

void t_erlang_generator::generate_struct_api_get_type(ostream& out, vector<t_struct*> structs) {
    for(vector<t_struct*>::const_iterator _it = structs.begin(); _it != structs.end(); ++_it)
    {
        indenter ind;
        out << "struct_get_type(#{'$struct' := " << type_name(*_it) << "}) ->" << ind.nl(1);
        out << type_name(*_it) << ";" << endl << endl;
    }
}

/**
 * Generates a thrift service.
 *
 * @param tservice The service definition
 */
void t_erlang_generator::generate_service(t_service* tservice) {
  (void)tservice;
}

void t_erlang_generator::generate_service_metadata(ostream& os) {
  typedef vector<t_service*> vec;
  vec const& services = get_program()->get_services();
  vec::const_iterator it;
  bool empty = false;

  os << "-spec functions";
  if (services.size() > 0) {
    bool no_functions = true;
    os << "(service_name()) -> [";
    for(it = services.begin(); it != services.end(); ++it) {
      if ((*it)->get_functions().size() > 0) {
        no_functions = false;
        break;
      }
    }
    if (no_functions) {
      empty = true;
    } else {
      os << "function_name()";
    }
    os << "] | no_return()." << endl << endl;
  } else {
    empty = true;
    os << ERROR_SPEC << endl << endl;
  }

  for(it = services.begin(); it != services.end(); ++it) {
    generate_service_metadata(os, *it);
  }
  os << "functions(_) -> error(badarg)." << endl << endl;

  indenter i;
  os << "-spec function_info";
  if (empty) {
    os << "(_,_,_) -> no_return()." << endl << endl;
  } else {
    os << "(service_name(), function_name(), params_type | reply_type | exceptions) ->"
       << i.nlup()
       << "struct_info() | no_return()." << endl << endl;
    for(vec::const_iterator it = services.begin(); it != services.end(); ++it) {
      generate_service_interface(os, *it);
    }
  }
  os << "function_info(_Service, _Function, _InfoType) -> erlang:error(badarg)." << endl;
}

void t_erlang_generator::generate_service_metadata(ostream& os, t_service* tservice) {
  typedef vector<t_function*> vec;

  indenter i;
  os << "functions(" << service_name(tservice) << ") ->" << i.nlup()
     << "[";

  vec const& functions = tservice->get_functions();
  if (functions.size() > 0) {
    os << i.nlup();
    for (vec::const_iterator it = functions.begin(); it != functions.end();) {
      os << function_name(*it);
      if (++it != functions.end()) {
        os << "," << i.nl();
      }
    }
    os << i.nldown();
  }
  os << "]";

  // List inherited functions as well
  t_service* base = tservice->get_extends();
  if (base) {
    os << " ++ " << type_module(base) << ":functions(" << service_name(base) << ")";
  }

  os << ";" << endl << endl;
}

/**
 * Generates a service interface definition.
 *
 * @param tservice The service to generate a header definition for
 */
void t_erlang_generator::generate_service_interface(ostream& os, t_service* tservice) {
  typedef vector<t_function*> vec;

  string name = service_name(tservice);
  vec const& functions = tservice->get_functions();
  for (vec::const_iterator it = functions.begin(); it != functions.end(); ++it) {
    generate_function_info(os, tservice, *it);
  }

  // Inheritance - pass unknown functions to base class
  indenter i;
  t_service* base = tservice->get_extends();
  if (base != NULL) {
    os << "function_info(" << name << ", Function, InfoType) ->" << i.nlup()
       << type_module(tservice->get_extends())
       << ":function_info(" << service_name(base) << ", Function, InfoType);"
       << i.nldown();
  }

  os << endl;
}

/**
 * Generates a function_info(FunctionName, params_type) and
 * function_info(FunctionName, reply_type)
 */
void t_erlang_generator::generate_function_info(ostream& os, t_service* tservice, t_function* tfunction) {
  string fname = function_name(tfunction);
  string svcname = service_name(tservice);

  t_struct* xs = tfunction->get_xceptions();
  t_struct* arg_struct = tfunction->get_arglist();

  // function_info(Function, params_type):
  indenter i;
  os << "function_info(" << svcname << ", " << fname << ", params_type) ->" << i.nlup()
     << render_type_term(arg_struct, true, i) << ";" << i.nldown();

  // function_info(Function, reply_type):
  os << "function_info(" << svcname << ", " << fname << ", reply_type) ->" << i.nlup();

  if (!tfunction->get_returntype()->is_void())
    os << render_type_term(tfunction->get_returntype(), false, i) << ";" << i.nldown();
  else if (tfunction->is_oneway())
    os << "oneway_void;" << i.nldown();
  else
    os << "{struct, struct, []};" << i.nldown();

  // function_info(Function, exceptions):
  os << "function_info(" << svcname << ", " << fname << ", exceptions) ->" << i.nlup()
     << render_type_term(xs, true, i) << ";" << endl;
}

/**
 * Boilerplate at beginning and end of header files
 */
string t_erlang_generator::render_hrl_header(string name) {
  return render_attribute("ifndef", name + "_included__") +
         render_attribute("define", name + "_included__, yeah");
}

string t_erlang_generator::render_hrl_footer() {
  return "-endif." + endl;
}

string t_erlang_generator::render_export(string name, int arity) {
  return render_attribute_list("export", name + "/" + render_string(arity));
}

string t_erlang_generator::render_export_type(string type, int arity) {
  return render_attribute_list("export_type", type + "/" + render_string(arity));
}

string t_erlang_generator::render_attribute_list(string type, string content) {
  return render_attribute(type, "[" + content + "]");
}

string t_erlang_generator::render_attribute(string type, string content) {
  return "-" + type + "(" + content + ")." + endl;
}

string t_erlang_generator::render_namespaced(const t_program* program, string const& s) {
  string ns = program->get_namespace("erlang");
  if (strip_unsafe(ns).size() > 0) {
    return ns + "_" + s;
  }
  return s;
}

template <class Type>
std::string t_erlang_generator::render_string(Type const& v) {
  std::ostringstream s;
  s << v;
  return s.str();
}

string t_erlang_generator::type_name(t_type* ttype) {
  string const& n = ttype->get_name();
  return atomify(idiomify(n));
}

string t_erlang_generator::scoped_type_name(t_type* ttype) {
  string const& n = ttype->get_name();
  return atomify(render_module_scoped(idiomify(n), ttype->get_program()));
}

string t_erlang_generator::service_name(t_service* tservice) {
  return service_name(tservice, true);
}

string t_erlang_generator::service_name(t_service* tservice, bool do_atomify) {
  string const& n = tservice->get_name();
  return do_atomify ? atomify(idiomify(n)) : idiomify(n);
}

string t_erlang_generator::idiomify(const std::string& str) {
  return idiomatic_names_ ? snake_case(str) : str;
}

string t_erlang_generator::function_name(t_function* tfun) {
  return atomify(idiomify(tfun->get_name()));
}

string t_erlang_generator::field_name(t_field* tfield) {
  return atomify(idiomify(tfield->get_name()));
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
    default:
      break;
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
    default:
      break;
    }
  } else if (type->is_enum()) {
    return "{enum, {" + type_module(type) + ", " + type_name(type) + "}}";
  } else if (type->is_struct() || type->is_xception()) {
    t_struct* tstruct = static_cast<t_struct*>(type);
    string stype = "struct";
    if (tstruct->is_xception()) {
      stype = "exception";
    }
    if (tstruct->is_union()) {
      stype = "union";
    }
    if (expand_structs) {

      t_struct::members_type const& fields = tstruct->get_members();
      if (0 == fields.size()) {
        return "{struct, " + stype + ", []}";
      }

      std::stringstream buf;
      buf << "{struct, " + stype + ", [" << ind.nlup();

      t_struct::members_type::const_iterator i, end = fields.end();
      for (i = fields.begin(); i != end;) {
        t_struct::members_type::value_type member = *i;
        int32_t key = member->get_key();
        string type_term = render_type_term(member->get_type(), false, ind); // recursive call

        // Convert to format: {struct, [{Fid, Req, Type, Name, Def}|...]}
        string name = field_name(member);
        string value = render_member_value(type_name(type) + "." + name, member, ind);
        string requiredness = render_member_requiredness(member);
        buf << "{" << key << ", " << requiredness << ", " << type_term << ", " << name << ", " << value << "}";

        if (++i != end) {
          buf << "," << ind.nl();
        }
      }

      buf << ind.nldown() << "]}";
      return buf.str();
    } else {
      return "{struct, " + stype + ", {" + type_module(type) + ", " + type_name(type) + "}}";
    }
  } else if (type->is_map()) {
    // {map, KeyType, ValType}
    t_type* key_type = ((t_map*)type)->get_key_type();
    t_type* val_type = ((t_map*)type)->get_val_type();
    return "{map, " + render_type_term(key_type, false, ind) + ", "
      + render_type_term(val_type, false, ind) + "}";
  } else if (type->is_set()) {
    t_type* elem_type = ((t_set*)type)->get_elem_type();
    return "{set, " + render_type_term(elem_type, false, ind) + "}";
  } else if (type->is_list()) {
    t_type* elem_type = ((t_list*)type)->get_elem_type();
    return "{list, " + render_type_term(elem_type, false, ind) + "}";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}

std::string t_erlang_generator::type_module(const t_type* ttype) {
  return render_module_name(ttype->get_program());
}

namespace detail {

static std::string render_thrift_filename(const t_program* p) {
  if (p->get_include_prefix().size() > 0) {
    return p->get_include_prefix() + p->get_name() + ".thrift";
  } else {
    return p->get_name() + ".thrift";
  }
}

static std::pair<std::string, std::string> render_appns_names(const t_program* p) {
  std::string app_name;
  std::string mod_name;
  std::string ns = p->get_namespace("erlang");
  if (ns.empty()) {
    throw "[" + render_thrift_filename(p) + "] namespace 'erlang' is not defined but is required by 'app_namespaces'";
  }
  size_t ploc = ns.find(".");
  if (ploc != std::string::npos) {
    app_name = ns.substr(0, ploc);
    strip_unsafe(app_name);
    mod_name = ns.substr(ploc + 1);
    strip_unsafe(mod_name);
  }
  if (app_name.empty()
    || mod_name.empty()
    || size_t(std::count(app_name.begin(), app_name.end(), '_')) == app_name.size()
    || size_t(std::count(mod_name.begin(), mod_name.end(), '_')) == mod_name.size()) {
    throw "[" + render_thrift_filename(p) + "] namespace 'erlang' MUST have the form `<app-name>.<module-name>` when 'app_namespaces' is on";
  }
  return std::make_pair(app_name, mod_name);
}

}

std::string t_erlang_generator::render_application_name(const t_program* p) {
  return detail::render_appns_names(p).first;
}

std::string t_erlang_generator::render_appmodule_name(const t_program* p) {
  return detail::render_appns_names(p).second;
}

std::string t_erlang_generator::render_module_name(const t_program* p) {
  std::string name;
  if (app_namespaces_) {
    name = render_application_name(p) + "_" + render_appmodule_name(p);
  }
  else if (!app_prefix_.empty()) {
    name = app_prefix_ + "_" + snake_case(p->get_name());
  }
  else {
    name = snake_case(p->get_name());
  }
  return name + OUT_FILE_SUFFIX;
}

std::string t_erlang_generator::render_module_scoped(std::string in, const t_program* p) {
  if (app_namespaces_) {
    return render_appmodule_name(p != nullptr ? p : get_program()) + "_" + in;
  }
  if (scoped_typenames_) {
    return render_namespaced(get_program(), in);
  }
  return in;
}

/**
 * Autogen'd comment
 */
string t_erlang_generator::erl_autogen_comment() {
  return string(
    "%%\n"
    "%% Autogenerated by Thrift Compiler (" THRIFT_VERSION ")\n"
    "%%\n"
    "%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING\n"
    "%%\n"
  );
}

std::string t_erlang_generator::display_name() const {
  return "Erlang";
}

THRIFT_REGISTER_GENERATOR(
  erlang,
  "Erlang",
  "    app_namespaces:   Treat namespaces defined in Thrift modules as Erlang application names.\n"
  "                      (conflicts with 'scoped_typenames', 'app_prefix')\n"
  "                       * Thrift module MUST contain 'erlang' namespace of the form '<app-name>.<module-name>'"
  "                       * Each generated Erlang module filename takes the form '<app-name>_<module-name>_thrift.erl'.\n"
  "                       * Each generated Erlang header filename takes the form '<app-name>_<module-name>_thrift.hrl'.\n"
  "                       * Each generated Erlang record's name is qualified with <module-name>.\n"
  "                       * Generated Erlang headers will not contain include directives.\n"
  "    idiomatic:        Adapt every name to look idiomatically correct in Erlang (i.e. snake case).\n"
  "    scoped_typenames: Prefix generated Erlang records with the namespace defined for erl.\n"
  "                      (conflicts with 'app_namespaces')\n"
  "    app_prefix=       Application prefix for generated Erlang files.\n"
  "                      (deprecated, conflicts with 'app_namespaces')\n"
  "    use_maps:         Generate maps from structs instead of records.\n"
)
