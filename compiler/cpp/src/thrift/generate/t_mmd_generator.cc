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
#include <map>
#include <list>
#include <set>
#include <sstream>

#include <stdlib.h>
#include <sys/stat.h>
#include "thrift/platform.h"
#include "thrift/generate/t_generator.h"

using std::list;
using std::map;
using std::ofstream;
using std::ostringstream;
using std::set;
using std::string;
using std::stringstream;
using std::vector;

/**
 * Mermaid diagram code generator.
 *
 * Produces a classDiagram in Mermaid syntax from a Thrift IDL file.
 * When invoked with -r, one .mmd file is produced per .thrift file, each
 * containing only the types declared in that file.
 */
class t_mmd_generator : public t_generator {
public:
  t_mmd_generator(t_program* program,
                  const std::map<std::string, std::string>& parsed_options,
                  const std::string& option_string)
    : t_generator(program) {
    (void)option_string;
    std::map<std::string, std::string>::const_iterator iter;

    exception_arrows_ = false;
    for (iter = parsed_options.begin(); iter != parsed_options.end(); ++iter) {
      if (iter->first.compare("exceptions") == 0) {
        exception_arrows_ = true;
      } else {
        throw "unknown option mmd:" + iter->first;
      }
    }

    out_dir_base_ = "gen-mmd";
  }

  void generate_program() override;
  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;

  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_struct(t_struct* tstruct) override;
  void generate_service(t_service* tservice) override;

private:
  void emit_program_types(t_program* program);
  string mmd_type_str(t_type* ttype);
  void print_type(t_type* ttype, const string& owner_ref, const string& edge_label = "");
  string format_params(t_function* tfunc);

  ofstream_with_content_based_conditional_update f_out_;
  list<string> edges_;
  bool exception_arrows_;
};

// Override the base generate_program() to route exceptions through generate_struct
// (which renders the <<exception>> stereotype); the base class calls generate_xception,
// which has no override here and would silently drop exceptions from the diagram.
void t_mmd_generator::generate_program() {
  init_generator();
  emit_program_types(program_);
  close_generator();
}

/**
 * Emits all diagram-relevant types for one program: enums, typedefs,
 * structs/unions/exceptions (in declaration order), and services.
 * Constants are intentionally omitted.
 */
void t_mmd_generator::emit_program_types(t_program* program) {
  const vector<t_enum*>& enums = program->get_enums();
  for (auto en : enums) {
    generate_enum(en);
  }

  const vector<t_typedef*>& typedefs = program->get_typedefs();
  for (auto td : typedefs) {
    generate_typedef(td);
  }

  // get_objects() returns structs, unions and exceptions in declaration order
  const vector<t_struct*>& objects = program->get_objects();
  for (auto obj : objects) {
    generate_struct(obj);
  }

  const vector<t_service*>& services = program->get_services();
  for (auto svc : services) {
    service_name_ = get_service_name(svc);
    generate_service(svc);
  }
}

void t_mmd_generator::init_generator() {
  MKDIR(get_out_dir().c_str());
  string fname = get_out_dir() + program_->get_name() + ".mmd";
  f_out_.open(fname.c_str());
  f_out_ << "classDiagram" << '\n';
  f_out_ << "  direction LR" << '\n';
}

void t_mmd_generator::close_generator() {
  for (const auto& edge : edges_) {
    f_out_ << edge << '\n';
  }
  f_out_.close();
}

std::string t_mmd_generator::display_name() const {
  return "Mermaid";
}

void t_mmd_generator::generate_enum(t_enum* tenum) {
  string name = tenum->get_name();
  f_out_ << "  class " << name << " {" << '\n';
  f_out_ << "    <<enumeration>>" << '\n';
  const vector<t_enum_value*>& values = tenum->get_constants();
  for (auto val : values) {
    f_out_ << "    " << val->get_name() << " = " << val->get_value() << '\n';
  }
  f_out_ << "  }" << '\n';
}

void t_mmd_generator::generate_typedef(t_typedef* ttypedef) {
  string name = ttypedef->get_name();
  t_type* base = ttypedef->get_type();

  f_out_ << "  class " << name << " {" << '\n';
  f_out_ << "    <<typedef>>" << '\n';

  if (base->is_base_type() || base->is_container()) {
    f_out_ << "    " << mmd_type_str(base) << '\n';
  } else {
    // named type: emit edge, no attribute line
    edges_.push_back("  " + name + " --> " + base->get_name());
  }

  f_out_ << "  }" << '\n';
}

void t_mmd_generator::generate_struct(t_struct* tstruct) {
  string name = tstruct->get_name();
  f_out_ << "  class " << name << " {" << '\n';

  if (tstruct->is_xception()) {
    f_out_ << "    <<exception>>" << '\n';
  } else if (tstruct->is_union()) {
    f_out_ << "    <<union>>" << '\n';
  } else {
    f_out_ << "    <<struct>>" << '\n';
  }

  const vector<t_field*>& members = tstruct->get_members();
  for (auto mem : members) {
    string field_name = mem->get_name();
    f_out_ << "    +";
    print_type(mem->get_type(), name, field_name);
    f_out_ << " " << field_name << '\n';
  }

  f_out_ << "  }" << '\n';
}

void t_mmd_generator::generate_service(t_service* tservice) {
  string svc_name = get_service_name(tservice);
  f_out_ << "  class " << svc_name << " {" << '\n';
  f_out_ << "    <<service>>" << '\n';

  const vector<t_function*>& functions = tservice->get_functions();
  for (auto fn : functions) {
    string ret = fn->is_oneway() ? "oneway void" : mmd_type_str(fn->get_returntype());
    f_out_ << "    +" << fn->get_name() << "(" << format_params(fn) << ") " << ret << '\n';
  }

  f_out_ << "  }" << '\n';

  if (tservice->get_extends() != nullptr) {
    edges_.push_back("  " + svc_name + " --|> " + tservice->get_extends()->get_name());
  }

  if (exception_arrows_) {
    set<string> emitted;
    for (auto fn : functions) {
      const vector<t_field*>& xceptions = fn->get_xceptions()->get_members();
      for (auto ex : xceptions) {
        string edge = "  " + svc_name + " ..> " + ex->get_type()->get_name();
        if (emitted.find(edge) == emitted.end()) {
          edges_.push_back(edge);
          emitted.insert(edge);
        }
      }
    }
  }
}

/**
 * Returns the Mermaid-escaped string representation of a type.
 * Container generics use tilde syntax: list~T~, set~T~, map~K,V~.
 */
string t_mmd_generator::mmd_type_str(t_type* ttype) {
  if (ttype->is_list()) {
    return "list~" + mmd_type_str(((t_list*)ttype)->get_elem_type()) + "~";
  } else if (ttype->is_set()) {
    return "set~" + mmd_type_str(((t_set*)ttype)->get_elem_type()) + "~";
  } else if (ttype->is_map()) {
    return "map~" + mmd_type_str(((t_map*)ttype)->get_key_type()) + ","
           + mmd_type_str(((t_map*)ttype)->get_val_type()) + "~";
  } else if (ttype->is_base_type()) {
    return ttype->is_binary() ? "binary" : ttype->get_name();
  } else {
    return ttype->get_name();
  }
}

/**
 * Writes the Mermaid type string to f_out_. When owner_ref is non-empty and
 * the type is a named (non-primitive, non-container) type, pushes a directed
 * edge to edges_. edge_label, when non-empty, is appended as " : label".
 */
void t_mmd_generator::print_type(t_type* ttype,
                                  const string& owner_ref,
                                  const string& edge_label) {
  f_out_ << mmd_type_str(ttype);
  if (!owner_ref.empty() && !ttype->is_base_type() && !ttype->is_container()) {
    string edge = "  " + owner_ref + " --> " + ttype->get_name();
    if (!edge_label.empty()) {
      edge += " : " + edge_label;
    }
    edges_.push_back(edge);
  }
}

/**
 * Returns comma-separated "type name" pairs for all function arguments.
 */
string t_mmd_generator::format_params(t_function* tfunc) {
  const vector<t_field*>& args = tfunc->get_arglist()->get_members();
  string result;
  bool first = true;
  for (auto arg : args) {
    if (!first) {
      result += ", ";
    }
    first = false;
    result += mmd_type_str(arg->get_type()) + " " + arg->get_name();
  }
  return result;
}

THRIFT_REGISTER_GENERATOR(
    mmd,
    "Mermaid",
    "    exceptions:      Draw dashed arrows from service functions to their declared exceptions.\n")
