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

#include <map>
#include <fstream>
#include <string>
#include <vector>

#include "t_oop_generator.h"
#include "platform.h"
#include "version.h"
#include "logging.h"

using std::map;
using std::ofstream;
using std::string;
using std::vector;

/**
 * Rust code generator.
 */
class t_rs_generator : public t_oop_generator {
 public:
  t_rs_generator(t_program* program,
                 const map<string, string>& parsed_options,
                 const string& option_string)
    : t_oop_generator(program)
  {
    (void) parsed_options;
    (void) option_string;
    // FIXME: change back to gen-rs when we finalize mod structure for generated code
    out_dir_base_ = "src";
  }

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */
  void generate_program();
  void generate_typedef(t_typedef*  ttypedef);
  void generate_enum(t_enum*     tenum);
  void generate_struct(t_struct*   tstruct);
  void generate_service(t_service*  tservice);

 private:
  string rs_autogen_comment();
  string rs_imports();

  string render_rs_type(t_type* type, bool split_generics = false);
  string render_protocol_type(t_type* type);
  string render_suffix(t_type* type);
  string render_type_init(t_type* type);

  void generate_service_uses(t_service* tservice);

  void generate_service_helpers(t_service* tservice);
  void generate_service_client_trait(t_service* tservice);
  void generate_service_trait_function(t_function* tfunction);
  void generate_service_client_impl(t_service* tservice);

  void generate_service_function(t_service* tservice, t_function* tfunction);
  void generate_function_helpers(t_service* tservice, t_function* tfunction);
  void generate_function_args(t_function* tfunction);
  void generate_args_init(t_function* tfunction);

  void generate_struct_declaration(t_struct* tstruct);
  void generate_struct_ctor(t_struct* tstruct);
  void generate_struct_writer(t_struct* tstruct);
  void generate_struct_reader(t_struct* tstruct);

  void generate_field_declaration(t_field* tfield);
  void generate_field_read(t_field* field);
  void generate_read_map(t_type* type, const string& name);
  void generate_read_list(t_type* type, const string& name);
  void generate_read_set(t_type* type, const string& name);

  void generate_field_write(t_field* field);
  void generate_write_map(t_type* type, const string& name);
  void generate_write_list(t_type* type, const string& name);
  void generate_write_set(t_type* type, const string& name);

  /**
   *Transforms a string with words separated by underscores to a pascal case equivalent
   * e.g. a_multi_word -> AMultiWord
   *      some_name    ->  SomeName
   *      name         ->  Name
   */
  std::string pascalcase(const std::string& in) { 
    return capitalize(camelcase(in)); 
  }

  bool is_string(t_type* type) {
    return type->is_base_type() && (((t_base_type*)type)->get_base() == t_base_type::TYPE_STRING);
  }

 private:
  ofstream f_mod_;
};

/*
 * This is necessary because we want to generate use clauses for all services,
 */
void t_rs_generator::generate_program() {
  // Initialize the generator
  init_generator();

  // Generate service uses
  vector<t_service*> services = program_->get_services();
  vector<t_service*>::iterator sv_iter;
  for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
    service_name_ = get_service_name(*sv_iter);
    generate_service_uses(*sv_iter);
  }

  // Generate enums
  vector<t_enum*> enums = program_->get_enums();
  vector<t_enum*>::iterator en_iter;
  for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
    generate_enum(*en_iter);
  }

  // Generate typedefs
  vector<t_typedef*> typedefs = program_->get_typedefs();
  vector<t_typedef*>::iterator td_iter;
  for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
    generate_typedef(*td_iter);
  }

  // Generate structs, exceptions, and unions in declared order
  vector<t_struct*> objects = program_->get_objects();

  vector<t_struct*>::iterator o_iter;
  for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
    generate_forward_declaration(*o_iter);
  }
  for (o_iter = objects.begin(); o_iter != objects.end(); ++o_iter) {
    if ((*o_iter)->is_xception()) {
      generate_xception(*o_iter);
    } else {
      generate_struct(*o_iter);
    }
  }

  // Generate constants
  vector<t_const*> consts = program_->get_consts();
  generate_consts(consts);

  // Generate services
  services = program_->get_services();
  for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
    service_name_ = get_service_name(*sv_iter);
    generate_service(*sv_iter);
  }

  // Close the generator
  close_generator();
}

void t_rs_generator::init_generator() {
  // Make output directory
  // FIXME: enable when finalizing the code structure
  //MKDIR(get_out_dir().c_str());
  string pname = underscore(program_name_);
  string moddirname = get_out_dir() + pname + "/";
  MKDIR(moddirname.c_str());

  // Make output file
  string f_mod_name = moddirname + "mod.rs";
  f_mod_.open(f_mod_name.c_str());

  // Print header
  f_mod_ << rs_autogen_comment() << "\n";
  f_mod_ << rs_imports() << "\n";
}

void t_rs_generator::close_generator() {
  f_mod_.close();
}

string t_rs_generator::rs_autogen_comment() {
  return string(
    "///////////////////////////////////////////////////////////////\n") +
    "// Autogenerated by Thrift Compiler (" + THRIFT_VERSION + ")\n" +
    "//\n" +
    "// DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING\n" +
    "///////////////////////////////////////////////////////////////\n";
}

string t_rs_generator::rs_imports() {
  return string(
    "#[allow(unused_imports)]\n"
    "use std::collections::{HashMap, HashSet};\n"
    "use thrift::protocol::{MessageType, Type};\n"
    "use thrift::transport::Transport;\n"
    "use thrift::protocol::Protocol;\n"
    "use thrift::protocol::{Readable, Writeable};\n"
    "use thrift::TResult;\n"
    "#[allow(unused_imports)]\n"
    "use thrift::ThriftErr;\n"
    "#[allow(unused_imports)]\n"
    "use thrift::ThriftErr::*;\n"
    "#[allow(unused_imports)]\n"
    "use std::num::FromPrimitive;\n"
    "use thrift::protocol::ProtocolHelpers;\n"
  );
}

void t_rs_generator::generate_typedef(t_typedef* ttypedef) {
  string tname = pascalcase(ttypedef->get_symbolic());
  string tdef = render_rs_type(ttypedef->get_type());
  indent(f_mod_) << "pub type " << tname << " = " << tdef << ";\n";
  f_mod_ << "\n";
}

void t_rs_generator::generate_enum(t_enum* tenum) {
  string ename = pascalcase(tenum->get_name());
  indent(f_mod_) << "#[allow(dead_code)]\n";
  indent(f_mod_) << "#[derive(Copy,Show,FromPrimitive)]\n";
  indent(f_mod_) << "pub enum " << ename << " {\n";
  indent_up();

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator i, end = constants.end();
  for (i = constants.begin(); i != end; ++i) {
    string name = capitalize((*i)->get_name());
    int value = (*i)->get_value();
    indent(f_mod_) << name << " = " << value << ",\n";
  }

  indent_down();
  indent(f_mod_) << "}\n\n";

  // generate ctor
  indent(f_mod_) << "impl " << ename << " {\n";
  indent_up();
    indent(f_mod_) << "#[allow(dead_code)]\n";
    indent(f_mod_) << "pub fn new() -> " << ename << " {\n";
    indent_up();

      if (tenum->get_constants().empty()) {
        indent(f_mod_) << ename << "\n";
      }
      else {
        indent(f_mod_) << ename << "::" 
                       << capitalize((*tenum->get_constants().begin())->get_name()) << "\n";
      }
    indent_down();
    indent(f_mod_) << "}\n";
  indent_down();
  indent(f_mod_) << "}\n\n";
}

void t_rs_generator::generate_struct(t_struct* tstruct) {
  generate_struct_declaration(tstruct);
  generate_struct_ctor(tstruct);
  generate_struct_writer(tstruct);
  generate_struct_reader(tstruct);
}

void t_rs_generator::generate_field_declaration(t_field* tfield) {
  t_type* t = get_true_type(tfield->get_type());

  f_mod_ << underscore(tfield->get_name()) << ": ";
  // FIXME: handle T_OPT_IN_REQ_OUT
  if (tfield->get_req() == t_field::T_OPTIONAL) {
    f_mod_ << "Option<" << render_rs_type(t) << ">,\n";
  }
  else {
    f_mod_ << render_rs_type(t) << ",\n";
  }  
}

void t_rs_generator::generate_struct_declaration(t_struct* tstruct) {
  string struct_name = pascalcase(tstruct->get_name());
  indent(f_mod_) << "#[allow(dead_code)]\n";
  indent(f_mod_) << "#[derive(Show)]\n";
  if (tstruct->get_members().empty()) {
    indent(f_mod_) << "pub struct " << struct_name << ";\n\n";
  }
  else {
    indent(f_mod_) << "pub struct " << struct_name << " {\n";
    indent_up();

    vector<t_field*>::const_iterator m_iter;
    const vector<t_field*>& members = tstruct->get_members();
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      indent(f_mod_) << "pub ";
      generate_field_declaration(*m_iter);
    }

    indent_down();
    indent(f_mod_) << "}\n\n";
  }
}

void t_rs_generator::generate_struct_ctor(t_struct* tstruct) {
  string struct_name = pascalcase(tstruct->get_name());

  indent(f_mod_) << "impl " << struct_name << " {\n";
  indent_up();

    indent(f_mod_) << "#[allow(dead_code)]\n";
    indent(f_mod_) << "pub fn new() -> " << struct_name << " {\n";
    indent_up();

      if (tstruct->get_members().empty()) {
        indent(f_mod_) << struct_name << "\n";
      }
      else {
        indent(f_mod_) << struct_name << " {\n";
        indent_up();
          vector<t_field*>::const_iterator m_iter;
          const vector<t_field*>& members = tstruct->get_members();
          for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
            string init;
            if ((*m_iter)->get_req() == t_field::T_OPTIONAL) {
              init = "None";
            }
            else {
              init = render_type_init((*m_iter)->get_type());
            }
            indent(f_mod_) << (*m_iter)->get_name() << ": " << init << ",\n";
          }
        indent_down();
        indent(f_mod_) << "}\n";
      }
    indent_down();
    indent(f_mod_) << "}\n";
  indent_down();
  indent(f_mod_) << "}\n\n";
}

void t_rs_generator::generate_service_uses(t_service* tservice) {
  t_service* service = tservice->get_extends();
  while (service) {
    indent(f_mod_) << "use " << service->get_program()->get_name() << "::*;\n";
    service = service->get_extends();
  }
  indent(f_mod_) << "\n";
}

void t_rs_generator::generate_service(t_service* tservice) {
  generate_service_helpers(tservice);
  generate_service_client_trait(tservice);
  generate_service_client_impl(tservice);
}

void t_rs_generator::generate_service_helpers(t_service* tservice) {
  // TODO
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;

  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    generate_function_helpers(tservice, *f_iter);
  }
}

void t_rs_generator::generate_function_args(t_function* tfunction) {
  const vector<t_field*>& fields = tfunction->get_arglist()->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    indent(f_mod_);
    generate_field_declaration(*f_iter);
  }
}

void t_rs_generator::generate_args_init(t_function* tfunction) {
  if (!tfunction->get_arglist()->get_members().empty()) {
    f_mod_ << " {\n";
    const vector<t_field*>& fields = tfunction->get_arglist()->get_members();
    vector<t_field*>::const_iterator f_iter;
    for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
      indent(f_mod_) << (*f_iter)->get_name() << ": " << (*f_iter)->get_name() << ",\n";
    }
    indent(f_mod_) << "}";
  }
}

void t_rs_generator::generate_service_trait_function(t_function* tfunction) {

  indent(f_mod_) << "#[allow(non_snake_case)]\n";
  indent(f_mod_) << "fn " << tfunction->get_name() << "(\n";
  indent_up();
    indent(f_mod_) << "&mut self,\n";
    generate_function_args(tfunction);
    indent(f_mod_) << ") -> TResult<" << render_rs_type(tfunction->get_returntype()) << ">;\n";
  indent_down();
}

void t_rs_generator::generate_service_function(t_service* tservice, t_function* tfunction) {
  std::string helper_prefix = pascalcase(tservice->get_name() + "_" + tfunction->get_name());

  indent(f_mod_) << "#[allow(non_snake_case)]\n";
  indent(f_mod_) << "fn " << tfunction->get_name() << "(\n";
  indent_up();
    indent(f_mod_) << "&mut self,\n";
    generate_function_args(tfunction);
    indent(f_mod_) << ") -> TResult<" << render_rs_type(tfunction->get_returntype()) << "> {\n";
    indent_up();
      indent(f_mod_) << "let args = " << helper_prefix << "Args";
      generate_args_init(tfunction);
      f_mod_ << ";\n";
      indent(f_mod_) << "try!(ProtocolHelpers::send(&self.protocol, &mut self.transport, \"" 
                     << tfunction->get_name()<< "\", MessageType::MtCall, &args));\n";

      if (!tfunction->is_oneway()) {
        indent(f_mod_) << "let mut result = " << helper_prefix << "Result::new();\n";
        indent(f_mod_) << "try!(ProtocolHelpers::receive(&self.protocol, &mut self.transport, \""
                       << tfunction->get_name()<< "\", &mut result));\n";
      }
      if (tfunction->get_returntype()->is_void()) {
        indent(f_mod_) << "Ok(())\n";
      }
      else {
        indent(f_mod_) << "Ok(result.success)\n";        
      }

    indent_down();
  indent_down();
  indent(f_mod_) << "}\n\n";
}

void t_rs_generator::generate_service_client_trait(t_service* tservice) {
  indent(f_mod_) << "pub trait " << tservice->get_name() << "Client {\n";
  indent_up();

  t_service* service = tservice;
  while(service) {
    vector<t_function*> functions = service->get_functions();
    vector<t_function*>::const_iterator f_iter;
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      generate_service_trait_function(*f_iter);
    }
    service = service->get_extends();
  }

  indent_down();
  indent(f_mod_) << "}\n\n";
}

void t_rs_generator::generate_service_client_impl(t_service* tservice) {
  string trait_name = tservice->get_name() + "Client";
  string impl_name = tservice->get_name() + "ClientImpl";

  indent(f_mod_) << "#[allow(dead_code)]\n";
  indent(f_mod_) << "pub struct " << impl_name << "<P: Protocol, T: Transport> {\n";
  indent_up();
    indent(f_mod_) << "pub protocol: P,\n";
    indent(f_mod_) << "pub transport: T,\n";
  indent_down();
  indent(f_mod_) << "}\n\n";

  // generate ctor
  indent(f_mod_) << "impl <P: Protocol, T: Transport> " 
                 << impl_name << "<P, T> {\n";
  indent_up();
    indent(f_mod_) << "#[allow(dead_code)]\n";
    indent(f_mod_) << "pub fn new(protocol: P, transport: T) -> " << impl_name << "<P, T> {\n";
      indent_up();
        indent(f_mod_) << impl_name << " {\n";
        indent_up();
            indent(f_mod_) << "protocol: protocol,\n";
            indent(f_mod_) << "transport: transport,\n";
      indent_down();
      indent(f_mod_) << "}\n";
    indent_down();
    indent(f_mod_) << "}\n";
  indent_down();
  indent(f_mod_) << "}\n\n";

  indent(f_mod_) << "impl <P: Protocol, T: Transport> " 
                 << trait_name << " for " << impl_name << "<P, T> {\n\n";
  indent_up();

  t_service* service = tservice;
  while (service) {
    vector<t_function*> functions = service->get_functions();
    vector<t_function*>::const_iterator f_iter;
    for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
      generate_service_function(service, *f_iter);
    }
    service = service->get_extends();
  }

  indent_down();
  indent(f_mod_) << "}\n\n";
}

/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_rs_generator::generate_function_helpers(t_service* tservice, t_function* tfunction) {

  t_struct* ts = tfunction->get_arglist();
  string name_orig = ts->get_name();

  ts->set_name(tservice->get_name() + "_" + tfunction->get_name() + "_args");
  generate_struct_declaration(ts);
  generate_struct_writer(ts);

  // FIXME: when implementing the server
  //ts->set_name(tservice->get_name() + "_" + tfunction->get_name() + "_pargs");
  //generate_struct(ts);

  ts->set_name(name_orig);

  if (tfunction->is_oneway()) {
    return;
  }

  t_struct result(program_, tservice->get_name() + "_" + tfunction->get_name() + "_result");
  t_field success(tfunction->get_returntype(), "success", 0);
  if (!tfunction->get_returntype()->is_void()) {
    result.append(&success);
  }

  t_struct* xs = tfunction->get_xceptions();
  const vector<t_field*>& fields = xs->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    // FIXME: shall we restore the optional flag?
    (*f_iter)->set_req(t_field::T_OPTIONAL);
    result.append(*f_iter);
  }

  generate_struct_declaration(&result);
  generate_struct_ctor(&result);
  generate_struct_reader(&result);

  // FIXME: when implementing the server
  //result.set_name(tservice->get_name() + "_" + tfunction->get_name() + "_pesult");
  //generate_struct(&result);
}

void t_rs_generator::generate_struct_writer(t_struct* tstruct) {
  string struct_name = pascalcase(tstruct->get_name());
  indent(f_mod_) << "impl Writeable for " << struct_name << " {\n\n";
  indent_up();

    indent(f_mod_) << "#[allow(unused_variables)]\n";
    indent(f_mod_) << "#[allow(dead_code)]\n";
    indent(f_mod_) << "fn write(&self, oprot: &Protocol, transport: &mut Transport) -> TResult<()> {\n";
    indent_up();
      indent(f_mod_) << "oprot.write_struct_begin(transport, \"" << tstruct->get_name() << "\");\n\n";

      vector<t_field*>::const_iterator m_iter;
      const vector<t_field*>& members = tstruct->get_members();
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        generate_field_write(*m_iter);
      }

      indent(f_mod_) << "oprot.write_field_stop(transport);\n";
      indent(f_mod_) << "oprot.write_struct_end(transport);\n";
      indent(f_mod_) << "Ok(())\n";

    indent_down();
    indent(f_mod_) << "}\n\n";

  indent_down();
  indent(f_mod_) << "}\n\n";
}

void t_rs_generator::generate_field_write(t_field* field) {
  string qualified_name = "self." + field->get_name();
  t_type* type = get_true_type(field->get_type());
  bool is_optional = field->get_req() == t_field::T_OPTIONAL;

  // FIXME: handle T_OPT_IN_REQ_OUT
  if (is_optional) {
    indent(f_mod_) << "match " << qualified_name << " {\n";
    indent_up();
    string need_ref = is_string(type) ? "ref " : "";
    indent(f_mod_) << "Some(" << need_ref << " x) => {\n";
    indent_up();
    qualified_name = "x";
  }

  indent(f_mod_) << "oprot.write_field_begin(transport, \"" << field->get_name()
                 << "\", Type::" << render_protocol_type(type)
                 << ", " << field->get_key() << ");\n";
  if (type->is_base_type() || type->is_enum()) {
    // FIXME: extract method?
    string decorated_name = 
      type->is_enum() ? qualified_name + " as i32" : 
      ((is_string(type) && !is_optional) ? "&" + qualified_name : qualified_name);
    indent(f_mod_) << "oprot.write_" << render_suffix(type) 
                   << "(transport, " << decorated_name << ");\n";
  } else if(type->is_struct() || type->is_xception()) {
    indent(f_mod_) << "try!(" << qualified_name << ".write(oprot, transport));\n";

  } else if(type->is_map()) {
    generate_write_map(type, qualified_name);

  } else if(type->is_list()) {
    generate_write_list(type, qualified_name);

  } else if(type->is_set()) {
    generate_write_set(type, qualified_name);

  } else {
    throw "INVALID TYPE IN generate_field_write: " + type->get_name();
  }
  indent(f_mod_) << "oprot.write_field_end(transport);\n";

  if (is_optional) {
    indent_down();
    indent(f_mod_) << "}\n";
    indent(f_mod_) << "_ => {}\n";
    indent_down();
    indent(f_mod_) << "}\n";
  }
  indent(f_mod_) << "\n";
}

void t_rs_generator::generate_write_map(t_type* type, const string& name) {
  // FIXME: write entries key, value
}

void t_rs_generator::generate_write_list(t_type* type, const string& name) {
  // FIXME: write entries
}

void t_rs_generator::generate_write_set(t_type* type, const string& name) {
  // FIXME: write entries
}

void t_rs_generator::generate_struct_reader(t_struct* tstruct) {
  string struct_name = pascalcase(tstruct->get_name());

  indent(f_mod_) << "impl Readable for " << struct_name << " {\n\n";
  indent_up();

    indent(f_mod_) << "fn read(& mut self, iprot: &Protocol, transport: & mut Transport) -> TResult<()> {\n";
    indent_up();
      if (tstruct->get_members().empty()) {
        indent(f_mod_) << "let have_result = true;\n";
      } else {
        indent(f_mod_) << "let mut have_result = false;\n";
      }
      indent(f_mod_) << "try!(iprot.read_struct_begin(transport));\n";
      indent(f_mod_) << "loop {\n";
      indent_up();
        indent(f_mod_) << "match try!(iprot.read_field_begin(transport)) {\n";
        indent_up();        

          indent(f_mod_) << "(_, Type::TStop, _) => {\n";
          indent_up();
            indent(f_mod_) << "try!(iprot.read_field_end(transport));\n";
            indent(f_mod_) << "break;\n";
          indent_down();
          indent(f_mod_) << "}\n";

          vector<t_field*>::const_iterator m_iter;
          const vector<t_field*>& members = tstruct->get_members();
          for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
            generate_field_read(*m_iter);
          }            

          indent(f_mod_) << "(_, ftype, _) => {\n";
          indent_up();
            indent(f_mod_) << "try!(iprot.skip(transport, ftype));\n";
          indent_down();
          indent(f_mod_) << "}\n";

        indent_down();
        indent(f_mod_) << "}\n";
        indent(f_mod_) << "try!(iprot.read_field_end(transport));\n";
      indent_down();
      indent(f_mod_) << "}\n";
      indent(f_mod_) << "try!(iprot.read_struct_end(transport));\n";
      indent(f_mod_) << "if have_result { Ok(()) } else { Err(ProtocolError) }\n";
    indent_down();
    indent(f_mod_) << "}\n";
  indent_down();
  indent(f_mod_) << "}\n\n";
}

void t_rs_generator::generate_field_read(t_field* field) {
  string qualified_name = "self." + field->get_name();
  t_type* type = get_true_type(field->get_type());
  bool is_optional = field->get_req() == t_field::T_OPTIONAL;

  if (is_optional) {
    indent(f_mod_) << "/*\n";
  }

  string prefix = is_optional ? "Some(" : "";
  string suffix = is_optional ? ")" : "";

  indent(f_mod_) << "(_, Type::" << render_protocol_type(type) 
                 << ", " << field->get_key() << ") => {\n";
  indent_up();

  if (type->is_base_type()) {
    indent(f_mod_) << qualified_name << " = " << prefix 
                   << "try!(iprot.read_" << render_suffix(type) << "(transport))"
                   << suffix << ";\n";
  }
  else if (type->is_enum()) {
    indent(f_mod_) << qualified_name << " = try!(ProtocolHelpers::read_enum(iprot, transport));\n";
  
  } else if(type->is_struct() || type->is_xception()) {
    indent(f_mod_) << "try!(" << qualified_name << ".read(iprot, transport));\n";

  } else if(type->is_map()) {
    generate_read_map(type, qualified_name);

  } else if(type->is_list()) {
    generate_read_list(type, qualified_name);

  } else if(type->is_set()) {
    generate_read_set(type, qualified_name);

  } else {
    throw "INVALID TYPE IN generate_field_write: " + type->get_name();
  }
  indent(f_mod_) << "have_result = true;\n";
  indent_down();
  indent(f_mod_) << "}\n";

  if (is_optional) {
    // FIXME
    indent(f_mod_) << "*/\n";
  }
}

void t_rs_generator::generate_read_map(t_type* type, const string& name) {
  // FIXME: write entries key, value
}

void t_rs_generator::generate_read_list(t_type* type, const string& name) {
  // FIXME: write entries
}

void t_rs_generator::generate_read_set(t_type* type, const string& name) {
  // FIXME: write entries
}

string t_rs_generator::render_rs_type(t_type* type, bool split_generics) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "()";
    case t_base_type::TYPE_STRING:
      return (((t_base_type*)type)->is_binary() ? "Vec<u8>" : "String");
    case t_base_type::TYPE_BOOL:
      return "bool";
    case t_base_type::TYPE_BYTE:
      return "i8";
    case t_base_type::TYPE_I16:
      return "i16";
    case t_base_type::TYPE_I32:
      return "i32";
    case t_base_type::TYPE_I64:
      return "i64";
    case t_base_type::TYPE_DOUBLE:
      return "f64";
    }

  } else if (type->is_enum()) {
    return capitalize(((t_enum*)type)->get_name());

  } else if (type->is_struct() || type->is_xception()) {
    return capitalize(((t_struct*)type)->get_name());

  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    string colcol = split_generics ? "::" : "";
    return "HashMap" + colcol + "<" + render_rs_type(ktype) + ", " + render_rs_type(vtype) + ">";

  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    string colcol = split_generics ? "::" : "";
    return "HashSet" + colcol + "<" + render_rs_type(etype) + ">";

  } else if (type->is_list()) {
    t_type* etype = ((t_list*)type)->get_elem_type();
    string colcol = split_generics ? "::" : "";
    return "Vec" + colcol + "<" + render_rs_type(etype) + ">";

  } else {
    throw "INVALID TYPE IN type_to_enum: " + type->get_name();
  }
  return ""; // silence the compiler warning
}

string t_rs_generator::render_protocol_type(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "TVoid";
    case t_base_type::TYPE_STRING:
      return "TString";
    case t_base_type::TYPE_BOOL:
      return "TBool";
    case t_base_type::TYPE_BYTE:
      return "TByte";
    case t_base_type::TYPE_I16:
      return "TI16";
    case t_base_type::TYPE_I32:
      return "TI32";
    case t_base_type::TYPE_I64:
      return "TI64";
    case t_base_type::TYPE_DOUBLE:
      return "TDouble";
    }

  } else if (type->is_enum()) {
    return "TI32";

  } else if (type->is_struct() || type->is_xception()) {
    return "TStruct";

  } else if (type->is_map()) {
    return "TMap";

  } else if (type->is_set()) {
    return "TSet";

  } else if (type->is_list()) {
    return "TList";

  } else {
    throw "INVALID TYPE IN render_protocol_type: " + type->get_name();
  }
  return ""; // silence the compiler warning
}


string t_rs_generator::render_suffix(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "void fields are not read or written";
    case t_base_type::TYPE_STRING:
      return (((t_base_type*)type)->is_binary() ? "binary" : "string");
    case t_base_type::TYPE_BOOL:
      return "bool";
    case t_base_type::TYPE_BYTE:
      return "i8";
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

  } else {
    throw "INVALID TYPE IN render_suffix: " + type->get_name();
  }
  return ""; // silence the compiler warning
}


string t_rs_generator::render_type_init(t_type* type) {
  type = get_true_type(type);
    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        return "()";
      case t_base_type::TYPE_STRING:
        return (((t_base_type*)type)->is_binary() ? "Vec<u8>::new()" : "String::new()");
      case t_base_type::TYPE_BOOL:
        return "false";
      case t_base_type::TYPE_BYTE:
        return "0";
      case t_base_type::TYPE_I16:
        return "0";
      case t_base_type::TYPE_I32:
        return "0";
      case t_base_type::TYPE_I64:
        return "0";
      case t_base_type::TYPE_DOUBLE:
        return "0";
      }

  } else if (type->is_struct() || type->is_xception()) {
    return capitalize(((t_struct*)type)->get_name()) + "::new()";

  } else if (type->is_enum()) {
    return capitalize(type->get_name()) + "::new()";

  } else if (type->is_map() || type->is_set() || type->is_list()) {
    return render_rs_type(type, true) + "::new()";

  } else {
    throw "INVALID TYPE IN render_type_init: " + type->get_name();
  }
  return ""; // silence the compiler warning    
}
THRIFT_REGISTER_GENERATOR(rs, "Rust", "")
