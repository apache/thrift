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

#include "thrift/platform.h"
#include "thrift/version.h"
#include "thrift/generate/t_generator.h"

using std::ofstream;
using std::ostringstream;
using std::string;
using std::vector;

static const string endl = "\n"; // avoid ostream << std::endl flushes
static const string SERVICE_CALL_RESULT_VARIABLE = "result_value";
static const string ARGS_STRUCT_SUFFIX = "Args";
static const string RESULT_STRUCT_SUFFIX = "Result";

// IMPORTANT: by default generator functions include extra endlines!

class t_rs_generator : public t_generator {
public:
  t_rs_generator(
    t_program* program,
    const std::map<std::string, std::string>& parsed_options,
    const std::string& options
  ) : t_generator(program) {
    gen_dir_ = get_out_dir();
  }

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

private:
  enum e_struct_type { T_REGULAR, T_ARGS, T_RESULT, T_EXCEPTION };

  // Directory to which generated code is written.
  string gen_dir_;

  // File to which generated code is written.
  std::ofstream f_gen_;

  void render_attributes_and_includes();
  void render_utility_functions();

  void render_rust_enum_definition(t_enum* tenum);
  void render_rust_enum_impl(t_enum* tenum);

  // Write the rust representation of a thrift struct to the generated file.
  // Set `is_args_struct` to `true` if rendering the struct used to pack
  // arguments for a service call. When `true` the struct and its members have
  // private visibility, and all fields are required. When `false` the
  // struct and its members have public visibility and fields are optional
  // if set as such in their thrift definition.
  void render_rust_struct(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_struct_comment(t_struct* tstruct);

  void render_rust_struct_definition(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_struct_impl(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_result_struct_to_result_method(t_struct* tstruct);

  void render_rust_exception_struct_error_trait_impls(t_struct* tstruct);

  void render_rust_struct_write_to_out_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  void render_rust_struct_field_write(const string& prefix, t_field* tfield, t_field::e_req req);
  void render_rust_type_write(const string& field_prefix, t_field* tfield, t_field::e_req req);
  void render_rust_list_write(const string& field_name, t_list* tlist);
  void render_rust_set_write(const string& field_name, t_set* tset);
  void render_rust_map_write(const string& field_name, t_map* tset);

  void render_rust_struct_read_from_in_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  void render_rust_struct_field_read(t_field* tfield);
  void render_rust_type_read(t_type* ttype, const string& type_var);
  void render_rust_list_read(t_list* tlist, const string& list_var);
  void render_rust_set_read(t_set* tset, const string& set_var);
  void render_rust_map_read(t_map* tmap, const string& map_var);

  void render_rust_result_value_struct(t_function* tfunc);

  // Write the rust representation of a thrift enum to the generated file.
  void render_rust_union(t_struct* tstruct);

  void render_rust_const_value(t_const_value* tconstvalue);
  void render_rust_sync_client(t_service* tservice);
  void render_rust_sync_client_lifecycle_functions(const string& client_struct);
  void render_rust_sync_send_recv_wrapper(t_function* tfunc);

  // Render the `send` functionality for a Thrift service call represented
  // by a `t_service->t_function`.
  void render_rust_sync_send(t_function* tfunc);

  // Render the `recv` functionality for a Thrift service call represented
  // by a `t_service->t_function`. This method is only rendered if the function
  // is *not* oneway.
  void render_rust_sync_recv(t_function* tfunc);
  void render_rust_sync_server(t_service* tservice);

  void render_rust_service_sync_client_trait(t_service* tservice);
  void render_rust_service_sync_handler_trait(t_service* tservice);
  void render_rust_service_server_comment(t_service* tservice);
  void render_rust_service_process_function(t_function* tfunc);
  void render_rust_service_processor(t_service* tservice);
  void render_rust_service_call_structs(t_service* tservice);
  void render_rust_rift_error(const string& error_kind, const string& error_struct, const string& sub_error_kind, const string& error_message);

  // Return a string containing all the unpacked service call args
  // given a service call function `t_function`.
  // If `is_declaration` is `true` we prepend the args with `&mut self`
  // and include the arg types in the returned string. If `false` we omit
  // the `self` qualifier and only use the arg names.
  string rust_sync_service_call_args(t_function* tfunc, bool is_declaration, const string& prefix = "");

  // Return a string representing the rust type given a `t_type`.
  string to_rust_type(t_type* ttype);

  // Return a string representing the rift `protocol::TType` given a `t_type`.
  string to_rust_field_type_enum(t_type* ttype);

  // Return `true` if this type is a void, and should be
  // represented by the rust `()` type.
  bool is_void(t_type* ttype);

  // Return `true` if the rust type can be passed by value, `false` otherwise.
  bool can_pass_by_value(t_type* ttype);

  // Return `true` if this struct field is optional and needs to be wrapped
  // by an `Option<TYPE_NAME>`, `false` otherwise.
  bool is_optional(t_field* tfield);

  // Return `true` if this `t_field::e_req` is either `t_field::T_OPTIONAL`
  // or `t_field::T_OPT_IN_REQ_OUT` and needs to be wrapped by an
  // `Option<TYPE_NAME>`, `false` otherwise.
  bool is_optional(t_field::e_req req);

  // Return `true` if the service call has arguments, `false` otherwise.
  bool has_args(t_function* tfunc);

  // Return `true` if we need to dereference ths type when writing an element from a container.
  // Iterations on rust containers are performed as follows:
  // `for v in &values { ... }` where `v` has type `&RUST_TYPE`
  // All defined functions take primitives by value, so, if the
  // rendered code is calling such a function it has to dereference `v`.
  bool needs_deref_on_container_write(t_type* ttype);

  // Return `pub ` (notice trailing whitespace!) if the struct
  // should be public, `` (empty string) otherwise.
  string visibility_qualifier(t_rs_generator::e_struct_type struct_type);

  // Return the trait name for the sync service client given a `t_service` name.
  string rust_sync_client_trait_name(t_service* tservice); // FIXME: remove

  // Return the trait name for the sync service processor given a `t_service` name.
  string rust_sync_handler_trait_name(t_service* tservice); // FIXME: remove

  // Return the name of the struct used to pack arguments for the thrift service call.
  string service_call_args_struct_name(t_function* tfunc);

  // Return the name of the struct used to pack the return value
  // and user-defined exceptions for the thrift service call.
  string service_call_result_struct_name(t_function* tfunc);

  string rust_service_call_client_function_name(t_function* tfunc);
  string rust_service_call_sync_send_client_function_name(t_function* tfunc);
  string rust_service_call_sync_recv_client_function_name(t_function* tfunc);
  string rust_service_call_handler_function_name(t_function* tfunc);

  string handler_successful_return_struct(t_function* tfunc);

  string struct_name(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  string args_struct_name(const string& name);
  string result_struct_name(const string& name);
  string default_struct_name(t_struct* tstruct);
};

// FIXME: underscore field names and function parameters

void t_rs_generator::init_generator() {
  // make output directory for this thrift program
  MKDIR(gen_dir_.c_str());

  // create the file into which we're going to write the generated code
  string f_gen_name = gen_dir_ + "/" + underscore(get_program()->get_name()) + ".rs";
  f_gen_.open(f_gen_name.c_str());

  // header comment
  f_gen_ << "// " << autogen_summary() << endl;
  f_gen_ << "// DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING" << endl;
  f_gen_ << endl;

  render_attributes_and_includes();
  render_utility_functions();
}

void t_rs_generator::render_attributes_and_includes() {
  // turn off some warnings
  f_gen_ << "#![allow(unused_imports)]" << endl; // generated code always includes BTreeMap/BTreeSet
  f_gen_ << "#![allow(non_snake_case)]" << endl; // generated code keeps user-specified names (FIXME: change to underscore?)
  f_gen_ << "#![allow(non_camel_case_types)]" << endl; // generated code keeps user-specified names for types
  f_gen_ << endl;

  // add standard includes
  f_gen_ << "extern crate rift;" << endl;
  f_gen_ << endl;
  f_gen_ << "use std::collections::{BTreeMap, BTreeSet};" << endl;
  f_gen_ << "use std::cell::RefCell;" << endl;
  f_gen_ << "use std::error::Error;" << endl;
  f_gen_ << "use std::fmt;" << endl;
  f_gen_ << "use std::fmt::{Display, Formatter};" << endl;
  f_gen_ << "use std::rc::Rc;" << endl;
  f_gen_ << endl;
  f_gen_ << "use rift::{ApplicationError, ApplicationErrorKind, ProtocolError, ProtocolErrorKind};" << endl;
  f_gen_ << "use rift::protocol::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TMessageType, TProtocol, TSetIdentifier, TStructIdentifier, TType};" << endl;
  f_gen_ << "use rift::server::TProcessor;" << endl;
  f_gen_ << endl;

  // add thrift includes
  const vector<t_program*> includes = get_program()->get_includes();
  if (!includes.empty()) {
    vector<t_program*>::const_iterator includes_iter;
    for(includes_iter = includes.begin(); includes_iter != includes.end(); ++includes_iter) {
      f_gen_ << "pub use " << underscore((*includes_iter)->get_name()) << ";" << endl;
    }
    f_gen_ << endl;
  }
}

void t_rs_generator::render_utility_functions() {
  // check that the sequence number is what you expect
  f_gen_ << "fn verify_expected_sequence_number(expected: i32, actual: i32) -> rift::Result<()> {" << endl;
  indent_up();
  f_gen_ << indent() << "if expected == actual {" << endl;
  indent_up();
  f_gen_ << indent() << "Ok(())" << endl;
  indent_down();
  f_gen_ << indent() << "} else {" << endl;
  indent_up();
  f_gen_ << indent() << "Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::Application(" << endl;
  indent_up();
  f_gen_ << indent() << "ApplicationError { kind: ApplicationErrorKind::BadSequenceId, message: format!(\"expected {} got {}\", expected, actual) }" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // check that the method name is what you expect
  f_gen_ << "fn verify_expected_service_call(expected: &str, actual: &str) -> rift::Result<()> {" << endl;
  indent_up();
  f_gen_ << indent() << "if expected == actual {" << endl;
  indent_up();
  f_gen_ << indent() << "Ok(())" << endl;
  indent_down();
  f_gen_ << indent() << "} else {" << endl;
  indent_up();
  f_gen_ << indent() << "Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::Application(" << endl;
  indent_up();
  f_gen_ << indent() << "ApplicationError { kind: ApplicationErrorKind::WrongMethodName, message: format!(\"expected {} got {}\", expected, actual) }" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // check that the message type is what you expect
  f_gen_ << "fn verify_expected_message_type(expected: TMessageType, actual: TMessageType) -> rift::Result<()> {" << endl;
  indent_up();
  f_gen_ << indent() << "if expected == actual {" << endl;
  indent_up();
  f_gen_ << indent() << "Ok(())" << endl;
  indent_down();
  f_gen_ << indent() << "} else {" << endl;
  indent_up();
  f_gen_ << indent() << "Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::Application(" << endl;
  indent_up();
  f_gen_ << indent() << "ApplicationError { kind: ApplicationErrorKind::InvalidMessageType, message: format!(\"expected {} got {}\", expected, actual) }" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // check that a required field exists
  f_gen_ << "fn verify_required_field_exists<T>(field_name: &str, field: &Option<T>) -> rift::Result<()> {" << endl;
  indent_up();
  f_gen_ << indent() << "match *field {" << endl;
  indent_up();
  f_gen_ << indent() << "Some(_) => Ok(())," << endl;
  f_gen_ << indent() << "None => Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::Protocol(" << endl;
  indent_up();
  f_gen_ << indent() << "ProtocolError { kind: ProtocolErrorKind::Unknown, message: format!(\"missing required field {}\", field_name) }" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")," << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // check that the field id exists; return the id if it does
  f_gen_ << "fn field_id(field_ident: &TFieldIdentifier) -> rift::Result<i16> {" << endl;
  indent_up();
  f_gen_ << indent() << "field_ident.id.ok_or(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::Protocol(" << endl;
  indent_up();
  f_gen_ << indent() << "ProtocolError { kind: ProtocolErrorKind::Unknown, message: format!(\"missing field in in {:?}\", field_ident) }" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::close_generator() {
  f_gen_.close();
}

//-----------------------------------------------------------------------------
//
// Consts, Typedefs and Enums
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_const(t_const* tconst) {
  /*
  f_gen_
    << "const " << tconst->get_name() << ": " << to_rust_type(tconst->get_type())
    << " = 1" << ";" // FIXME: WTF is going on with constants?!
    << endl;
  f_gen_ << endl;
  */
}

void t_rs_generator::render_rust_const_value(t_const_value* tconstvalue) {

}

void t_rs_generator::generate_typedef(t_typedef* ttypedef) {
  std::string actual_type = to_rust_type(ttypedef->get_type());
  f_gen_ << "pub type " << ttypedef->get_symbolic() << " = " << actual_type << ";" << endl;
  f_gen_ << endl;
}

void t_rs_generator::generate_enum(t_enum* tenum) {
  render_rust_enum_definition(tenum);
  render_rust_enum_impl(tenum);
}

void t_rs_generator::render_rust_enum_definition(t_enum* tenum) {
  f_gen_ << "#[derive(Copy, Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]" << endl;
  f_gen_ << "pub enum " << tenum->get_name() << " {" << endl;
  indent_up();

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator constants_iter;
  for (constants_iter = constants.begin(); constants_iter != constants.end(); ++constants_iter) {
    f_gen_ << indent() << (*constants_iter)-> get_name() << " = " << (*constants_iter)->get_value() << "," << endl;
  }

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_enum_impl(t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator constants_iter;

  f_gen_ << "impl " << tenum->get_name() << " {" << endl;
  indent_up();

  f_gen_
    << indent()
    << "pub fn write_to_out_protocol(&self, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();
  f_gen_ << indent() << "try!(o_prot.write_i32(*self as i32));" << endl;
  f_gen_ << indent() << "o_prot.flush()" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  f_gen_
    << indent()
    << "pub fn read_from_in_protocol(i_prot: &mut TProtocol) -> rift::Result<" << tenum->get_name() << "> {"
    << endl;
  indent_up();

  f_gen_ << indent() << "let enum_value = try!(i_prot.read_i32());" << endl;
  f_gen_ << indent() << "match enum_value {" << endl;
  indent_up();
  for (constants_iter = constants.begin(); constants_iter != constants.end(); ++constants_iter) {
    f_gen_
      << indent()
      << (*constants_iter)->get_value()
      << " => Ok(" << tenum->get_name() << "::" << (*constants_iter)->get_name() << "),"
      << endl;
  }
  f_gen_ << indent() << "_ => Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::Protocol(" << endl;
  indent_up();
  f_gen_ << indent() << "ProtocolError { kind: ProtocolErrorKind::InvalidData, message: format!(\"cannot convert enum constant {} to " << tenum->get_name() << "\", enum_value) }" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")," << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_union(t_struct* tstruct) {
  // FIXME!
}

//-----------------------------------------------------------------------------
//
// Structs and Exceptions
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_xception(t_struct* txception) {
  render_rust_struct(txception, t_rs_generator::T_EXCEPTION);
}

void t_rs_generator::generate_struct(t_struct* tstruct) {
  if (tstruct->is_struct()) {
    render_rust_struct(tstruct, t_rs_generator::T_REGULAR);
  } else if (tstruct->is_union()) {
    render_rust_union(tstruct);
  } else {
    throw "cannot generate struct for exception";
  }
}

void t_rs_generator::render_rust_struct(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  render_rust_struct_comment(tstruct);
  render_rust_struct_definition(tstruct, struct_type);
  render_rust_struct_impl(tstruct, struct_type);
  if (struct_type == t_rs_generator::T_EXCEPTION) {
    render_rust_exception_struct_error_trait_impls(tstruct);
  }
}

void t_rs_generator::render_rust_struct_comment(t_struct* tstruct) {
  f_gen_ << "//" << endl;
  f_gen_ << "// " << tstruct->get_name() << endl;
  f_gen_ << "//" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_struct_definition(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_ << "#[derive(Debug, Eq, Ord, PartialEq, PartialOrd)]" << endl;
  f_gen_ << visibility_qualifier(struct_type) << "struct " << tstruct->get_name() << " {" << endl;

  // render the members
  vector<t_field*> members = tstruct->get_sorted_members();
  if (!members.empty()) {
    indent_up();

    vector<t_field*>::iterator members_iter;
    for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
      string rust_type = to_rust_type(tfield->get_type());
      rust_type = is_optional(req) ? "Option<" + rust_type + ">" : rust_type;
      f_gen_ << indent() << visibility_qualifier(struct_type) << tfield->get_name() << ": " << rust_type << "," << endl;
    }

    indent_down();
  }

  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_exception_struct_error_trait_impls(t_struct* tstruct) {
  // error::Error trait
  f_gen_ << "impl Error for " << tstruct->get_name() << " {" << endl;
  indent_up();
  f_gen_ << indent() << "fn description(&self) -> &str {" << endl;
  indent_up();
  f_gen_ << indent() << "\"" << "remote service threw " << tstruct->get_name() << "\"" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // fmt::Display trait
  f_gen_ << "impl Display for " << tstruct->get_name() << " {" << endl;
  indent_up();
  f_gen_ << indent() << "fn fmt(&self, f: &mut Formatter) -> fmt::Result {" << endl;
  indent_up();
  f_gen_ << indent() << "self.description().fmt(f)" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_struct_impl(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_ << "impl " << tstruct->get_name() << " {" << endl;
  indent_up();

  render_rust_struct_read_from_in_protocol(tstruct, struct_type);
  render_rust_struct_write_to_out_protocol(tstruct, struct_type);
  if (struct_type == t_rs_generator::T_RESULT) {
    render_rust_result_struct_to_result_method(tstruct);
  }

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_result_struct_to_result_method(t_struct* tstruct) {
  // find the service call name for this result struct
  string service_call_name = tstruct->get_name();
  size_t index = service_call_name.find(RESULT_STRUCT_SUFFIX, 0);
  if (index == std::string::npos) {
    throw "result struct " + service_call_name + " missing result suffix";
  } else {
     service_call_name.replace(index, 7, "");
  }

  const vector<t_field*>& members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;

  // find out what the call's expected return type was
  string rust_return_type = "()";
  for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    if (tfield->get_name() == SERVICE_CALL_RESULT_VARIABLE) {
      rust_return_type = to_rust_type(tfield->get_type());
      break;
    }
  }

  // NOTE: ideally I would generate the branches and render them separately
  // I tried this however, and the resulting code was harder to understand
  // maintaining a rendered branch count while a little ugly, got me the
  // rendering I wanted with code that was reasonably understandable

  f_gen_ << indent() << "fn ok_or(self) -> rift::Result<" << rust_return_type << "> {" << endl;
  indent_up();

  int rendered_branch_count = 0;

  // render the exception branches
  for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    if (tfield->get_name() != SERVICE_CALL_RESULT_VARIABLE) {
      string field_name = "self." + tfield->get_name();
      string branch_statement = rendered_branch_count == 0 ? "if" : "} else if";

      f_gen_ << indent() << branch_statement << " " << field_name << ".is_some() {" << endl;
      indent_up();
      f_gen_ << indent() << "Err(rift::Error::User(Box::new(" << field_name << ".unwrap())))" << endl;
      indent_down();

      rendered_branch_count++;
    }
  }

  // render the return value branches
  if (rust_return_type == "()") {
    if (rendered_branch_count == 0) {
      // we have the unit return and this service call has no user-defined
      // exceptions. this means that we've a trivial return (happens with oneways)
      f_gen_ << indent() << "Ok(())" << endl;
    } else {
      // we have the unit return, but there are user-defined exceptions
      // if we've gotten this far then we have the default return (i.e. call successful)
      f_gen_ << indent() << "} else {" << endl;
      indent_up();
      f_gen_ << indent() << "Ok(())" << endl;
      indent_down();
      f_gen_ << indent() << "}" << endl;
    }
  } else {
    string branch_statement = rendered_branch_count == 0 ? "if" : "} else if";
    f_gen_ << indent() << branch_statement << " self." << SERVICE_CALL_RESULT_VARIABLE << ".is_some() {" << endl;
    indent_up();
    f_gen_ << indent() << "Ok(self." << SERVICE_CALL_RESULT_VARIABLE << ".unwrap())" << endl;
    indent_down();
    f_gen_ << indent() << "} else {" << endl;
    indent_up();
    // if we haven't found a valid return value *or* a user exception
    // then we're in trouble; return a default error
    f_gen_ << indent() << "Err(" << endl;
    indent_up();
    f_gen_ << indent() << "rift::Error::Application(" << endl;
    indent_up();
    f_gen_ << indent() << "ApplicationError { kind: ApplicationErrorKind::MissingResult, message: " << "\"no result received for " << service_call_name << "\".to_owned() }" << endl;
    indent_down();
    f_gen_ << indent() << ")" << endl;
    indent_down();
    f_gen_ << indent() << ")" << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  }

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

//-----------------------------------------------------------------------------
//
// Sync Write
//
//-----------------------------------------------------------------------------

void t_rs_generator::render_rust_struct_write_to_out_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_
    << indent()
    << visibility_qualifier(struct_type)
    << "fn write_to_out_protocol(&self, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();

  // write struct header to output protocol
  f_gen_ << indent() << "let struct_ident = TStructIdentifier { name: \"" + tstruct->get_name() + "\".to_owned() };" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_begin(&struct_ident));" << endl;

  // write struct members to output protocol
  vector<t_field*> members = tstruct->get_sorted_members();
  if (!members.empty()) {
    vector<t_field*>::iterator members_iter;
    for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
      render_rust_struct_field_write("self", tfield, req);
    }
  }

  // write struct footer to output protocol
  f_gen_ << indent() << "try!(o_prot.write_field_stop());" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_end());" << endl;
  f_gen_ << indent() << "try!(o_prot.flush());" << endl;
  f_gen_ << indent() << "Ok(())" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_struct_field_write(const string& prefix, t_field* tfield, t_field::e_req req) {
  string field_prefix = "";
  if (!prefix.empty()) {
    field_prefix = prefix + ".";
  }

  ostringstream field_stream;
  field_stream
    << "TFieldIdentifier {"
    << "name: Some(\"" << tfield->get_name() << "\".to_owned()" << "), "
    << "field_type: " << to_rust_field_type_enum(tfield->get_type()) << ", "
    << "id: Some(" << tfield->get_key() << ") "
    << "}";
  string field_ident_string = field_stream.str();

  if (is_optional(req)) {
    string field_name = field_prefix + tfield->get_name();
    f_gen_ << indent() << "if " << field_name << ".is_some() {" << endl;
    indent_up();
    f_gen_ << indent() << "try!(o_prot.write_field_begin(&" << field_ident_string << "));" << endl;
    render_rust_type_write(field_prefix, tfield, req);
    f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
    f_gen_ << indent() << "()" << endl;
    indent_down();
    f_gen_ << indent() << "} else {" << endl;
    indent_up();
    /* FIXME: rethink how I deal with OPT_IN_REQ_OUT
    if (req == t_field::T_OPT_IN_REQ_OUT) {
      f_gen_ << indent() << "let field_ident = " << field_ident_string << ";" << endl;
      f_gen_ << indent() << "try!(o_prot.write_field_begin(&field_ident));" << endl;
      f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
    }*/
    f_gen_ << indent() << "()" << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  } else {
    f_gen_ << indent() << "try!(o_prot.write_field_begin(&" << field_ident_string << "));" << endl;
    render_rust_type_write(field_prefix, tfield, req);
    f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
  }
}

void t_rs_generator::render_rust_type_write(const string& field_prefix, t_field* tfield, t_field::e_req req) {
  t_type* ttype = tfield->get_type();

  string field_name;
  if (is_optional(req)) {
    if (ttype->is_base_type() && !ttype->is_string()) {
      field_name = field_prefix + tfield->get_name() + ".unwrap()";
    } else {
      field_name = field_prefix + tfield->get_name() + ".as_ref().unwrap()";
    }
  } else {
    field_name = field_prefix + tfield->get_name();
  }

  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "Cannot write field of type TYPE_VOID to output protocol";
      return;
    case t_base_type::TYPE_STRING:
      f_gen_ << indent() << "try!(o_prot.write_string(&" + field_name + "));" << endl;
      return;
    case t_base_type::TYPE_BOOL:
      f_gen_ << indent() << "try!(o_prot.write_bool(" + field_name + "));" << endl;
      return;
    case t_base_type::TYPE_I8:
      f_gen_ << indent() << "try!(o_prot.write_i8(" + field_name + "));" << endl;
      return;
    case t_base_type::TYPE_I16:
      f_gen_ << indent() << "try!(o_prot.write_i16(" + field_name + "));" << endl;
      return;
    case t_base_type::TYPE_I32:
      f_gen_ << indent() << "try!(o_prot.write_i32(" + field_name + "));" << endl;
      return;
    case t_base_type::TYPE_I64:
      f_gen_ << indent() << "try!(o_prot.write_i64(" + field_name + "));" << endl;
      return;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << indent() << "try!(o_prot.write_double(" + field_name + "));" << endl;
      return;
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    t_field typedef_field(ttypedef->get_type(), tfield->get_name());
    render_rust_type_write(field_prefix, &typedef_field, req);
    return;
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << indent() << "try!(" +  field_name + ".write_to_out_protocol(o_prot));" << endl;
    return;
  } else if (ttype->is_map()) {
    render_rust_map_write(field_name, (t_map*)ttype);
    return;
  } else if (ttype->is_set()) {
    render_rust_set_write(field_name, (t_set*)ttype);
    return;
  } else if (ttype->is_list()) {
    render_rust_list_write(field_name, (t_list*)ttype);
    return;
  }

  throw "Cannot write unsupported type " + ttype->get_name();
}

void t_rs_generator::render_rust_list_write(const string& field_name, t_list* tlist) {
  t_type* elem_type = tlist->get_elem_type();

  f_gen_
    << indent()
    << "try!(o_prot.write_list_begin("
    << "&TListIdentifier {"
    << " element_type: " << to_rust_field_type_enum(elem_type)
    << ", size: " << field_name << ".len() as i32"
    << " }"
    << "));" << endl;

  f_gen_ << indent() << "for e in " << field_name << ".iter() {" << endl;
  indent_up();

  string prefix = needs_deref_on_container_write(elem_type) ? "*e" : "e";
  t_field elem_field(elem_type, "");
  render_rust_type_write(prefix, &elem_field, t_field::e_req::T_REQUIRED);

  f_gen_ << indent() << "try!(o_prot.write_list_end());" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_set_write(const string& field_name, t_set* tset) {
  t_type* elem_type = tset->get_elem_type();

  f_gen_
    << indent()
    << "try!(o_prot.write_set_begin("
    << "&TSetIdentifier {"
    << " element_type: " << to_rust_field_type_enum(elem_type)
    << ", size: " << field_name << ".len() as i32"
    << " }"
    << "));" << endl;

  f_gen_ << indent() << "for e in " << field_name << ".iter() {" << endl;
  indent_up();

  string prefix = needs_deref_on_container_write(elem_type) ? "*e" : "e";
  t_field elem_field(elem_type, "");
  render_rust_type_write(prefix, &elem_field, t_field::e_req::T_REQUIRED);

  f_gen_ << indent() << "try!(o_prot.write_set_end());" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_map_write(const string& field_name, t_map* tmap) {
  t_type* key_type = tmap->get_key_type();
  t_type* val_type = tmap->get_val_type();

  f_gen_
    << indent()
    << "try!(o_prot.write_map_begin("
    << "&TMapIdentifier {"
    << " key_type: " << to_rust_field_type_enum(key_type)
    << ", value_type: " << to_rust_field_type_enum(val_type)
    << ", size: " << field_name << ".len() as i32"
    << " }"
    << "));" << endl;

  f_gen_ << indent() << "for (k, v) in " << field_name << ".iter() {" << endl;
  indent_up();

  string key_prefix = needs_deref_on_container_write(key_type) ? "*k" : "k";
  t_field key_field(key_type, "");
  render_rust_type_write(key_prefix, &key_field, t_field::e_req::T_REQUIRED);

  string val_prefix = needs_deref_on_container_write(val_type) ? "*v" : "v";
  t_field val_field(val_type, "");
  render_rust_type_write(val_prefix, &val_field, t_field::e_req::T_REQUIRED);

  f_gen_ << indent() << "try!(o_prot.write_map_end());" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

bool t_rs_generator::needs_deref_on_container_write(t_type* ttype) {
  if (ttype->is_base_type() && !ttype->is_string()) {
    return true;
  } else {
    if (ttype->is_typedef()) {
      t_typedef* ttypedef = (t_typedef*)ttype;
      return needs_deref_on_container_write(ttypedef->get_type());
    } else {
      return false;
    }
  }
}

//-----------------------------------------------------------------------------
//
// Sync Read
//
//-----------------------------------------------------------------------------

void t_rs_generator::render_rust_struct_read_from_in_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_
    << indent()
    << visibility_qualifier(struct_type)
    << "fn read_from_in_protocol(i_prot: &mut TProtocol) -> rift::Result<" << tstruct->get_name() << "> {"
    << endl;
  indent_up();

  f_gen_ << indent() << "try!(i_prot.read_struct_begin());" << endl;

  // create temporary variables: one for each field in the struct
  const vector<t_field*> members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    f_gen_
      << indent()
      << "let mut f" << tfield->get_key()
      << ": Option<" << to_rust_type(tfield->get_type())
      << "> = None;"
      << endl;
  }

  // now loop through the fields we've received
  f_gen_ << indent() << "loop {" << endl; // start loop
  indent_up();

  // break out if you've found the Stop field
  f_gen_ << indent() << "let field_ident = try!(i_prot.read_field_begin());" << endl;
  f_gen_ << indent() << "if field_ident.field_type == TType::Stop {" << endl;
  indent_up();
  f_gen_ << indent() << "break;" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  // now read all the fields found
  f_gen_ << indent() << "let field_id = try!(field_id(&field_ident));" << endl;
  f_gen_ << indent() << "match field_id {" << endl; // start match
  indent_up();

  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    f_gen_ << indent() << tfield->get_key() << " => {" << endl;
    indent_up();
    render_rust_struct_field_read(tfield);
    indent_down();
    f_gen_ << indent() << "}," << endl;
  }

  // default case (skip fields)
  f_gen_ << indent() << "_ => {" << endl;
  indent_up();
  f_gen_ << indent() << "try!(i_prot.skip(field_ident.field_type));" << endl;
  indent_down();
  f_gen_ << indent() << "}," << endl;

  indent_down();
  f_gen_ << indent() << "};" << endl; // finish match

  f_gen_ << indent() << "try!(i_prot.read_field_end());" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl; // finish loop

  // finish reading the message from the wire
  f_gen_ << indent() << "try!(i_prot.read_struct_end());" << endl;

  // verify that all required fields exist
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
    if (!is_optional(req)) {
      f_gen_
        << indent()
        << "try!(verify_required_field_exists("
        << "\"" << tstruct->get_name()<< "." << tfield->get_name() << "\""
        << ", "
        << "&f" << tfield->get_key()
        << "));" << endl;
    }
  }

  // construct the struct
  if (members.size() == 0) {
    f_gen_ << indent() << "let ret = " << tstruct->get_name() << " {};" << endl;
  } else {
    f_gen_ << indent() << "let ret = " << tstruct->get_name() << " {" << endl;
    indent_up();

    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
      if (is_optional(req)) {
        f_gen_ << indent() << tfield->get_name() << ": f" << tfield->get_key() << "," << endl;
      } else {
        f_gen_
          << indent()
          << tfield->get_name()
          << ": f" << tfield->get_key()
          << ".expect(\"auto-generated code should have checked for presence of required fields\")"
          << ","
          << endl;
      }
    }

    indent_down();
    f_gen_ << indent() << "};" << endl;
  }

  // return the constructed value
  f_gen_ << indent() << "Ok(ret)" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_struct_field_read(t_field* tfield) {
  render_rust_type_read(tfield->get_type(), "val");
  f_gen_ << indent() << "f" << tfield->get_key() << " = Some(val);" << endl;
}

// Construct the rust representation of all supported types from the wire.
void t_rs_generator::render_rust_type_read(t_type* ttype, const string& type_var) {
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "Cannot read field of type TYPE_VOID from input protocol";
    case t_base_type::TYPE_STRING:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_string());" << endl;
      return;
    case t_base_type::TYPE_BOOL:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_bool());" << endl;
      return;
    case t_base_type::TYPE_I8:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_i8());" << endl;
      return;
    case t_base_type::TYPE_I16:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_i16());" << endl;
      return;
    case t_base_type::TYPE_I32:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_i32());" << endl;
      return;
    case t_base_type::TYPE_I64:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_i64());" << endl;
      return;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_double());" << endl;
      return;
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*) ttype;
    render_rust_type_read(ttypedef->get_type(), type_var);
    return;
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << indent() << "let " << type_var << " = try!(" <<  to_rust_type(ttype) << "::read_from_in_protocol(i_prot));" << endl;
    return;
  } else if (ttype->is_map()) {
    render_rust_map_read((t_map*) ttype, type_var);
    return;
  } else if (ttype->is_set()) {
    render_rust_set_read((t_set*) ttype, type_var);
    return;
  } else if (ttype->is_list()) {
    render_rust_list_read((t_list*) ttype, type_var);
    return;
  }

  throw "Cannot read unsupported type " + ttype->get_name();
}

// Construct the rust representation of a list from the wire.
void t_rs_generator::render_rust_list_read(t_list* tlist, const string& list_var) {
  t_type* elem_type = tlist->get_elem_type();

  f_gen_ << indent() << "let list_ident = try!(i_prot.read_list_begin());" << endl;
  f_gen_ << indent() << "let mut " << list_var << ": " << to_rust_type((t_type*) tlist) << " = Vec::with_capacity(list_ident.size as usize);" << endl;
  f_gen_ << indent() << "for _ in 0..list_ident.size {" << endl;

  indent_up();

  string list_elem_var = tmp("list_elem_");
  render_rust_type_read(elem_type, list_elem_var);
  f_gen_ << indent() << list_var << ".push(" << list_elem_var << ");" << endl;

  indent_down();

  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(i_prot.read_list_end());" << endl;
}

// Construct the rust representation of a set from the wire.
void t_rs_generator::render_rust_set_read(t_set* tset, const string& set_var) {
  t_type* elem_type = tset->get_elem_type();

  f_gen_ << indent() << "let set_ident = try!(i_prot.read_set_begin());" << endl;
  f_gen_ << indent() << "let mut " << set_var << ": " << to_rust_type((t_type*) tset) << " = BTreeSet::new();" << endl;
  f_gen_ << indent() << "for _ in 0..set_ident.size {" << endl;

  indent_up();

  string set_elem_var = tmp("set_elem_");
  render_rust_type_read(elem_type, set_elem_var);
  f_gen_ << indent() << set_var << ".insert(" << set_elem_var << ");" << endl;

  indent_down();

  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(i_prot.read_set_end());" << endl;
}

// Construct the rust representation of a map from the wire.
void t_rs_generator::render_rust_map_read(t_map* tmap, const string& map_var) {
  t_type* key_type = tmap->get_key_type();
  t_type* val_type = tmap->get_val_type();

  f_gen_ << indent() << "let map_ident = try!(i_prot.read_map_begin());" << endl;
  f_gen_ << indent() << "let mut " << map_var << ": " << to_rust_type((t_type*) tmap) << " = BTreeMap::new();" << endl;
  f_gen_ << indent() << "for _ in 0..map_ident.size {" << endl;

  indent_up();

  string key_elem_var = tmp("map_key_");
  render_rust_type_read(key_type, key_elem_var);
  string val_elem_var = tmp("map_val_");
  render_rust_type_read(val_type, val_elem_var);
  f_gen_ << indent() << map_var << ".insert(" << key_elem_var << ", " << val_elem_var << ");" << endl;

  indent_down();

  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(i_prot.read_map_end());" << endl;
}

//-----------------------------------------------------------------------------
//
// Sync Client
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_service(t_service* tservice) {
  render_rust_sync_client(tservice);
  render_rust_sync_server(tservice);
  render_rust_service_call_structs(tservice);
}

void t_rs_generator::render_rust_sync_client(t_service* tservice) {
  // service comment demarcation
  f_gen_ << "//" << endl;
  f_gen_ << "// " << tservice->get_name() << " service client "<< endl;
  f_gen_ << "//" << endl;
  f_gen_ << endl;

  // render the trait through which the service calls will be mad
  render_rust_service_sync_client_trait(tservice);

  // [sigh] this is annoying
  // to create a parameterized rust struct I have to declare the type parameters twice
  //
  // struct declaration:
  // struct HasFoo<I: Foo, O: Foo> {
  //     i_foo: I,
  //     o_foo: O,
  // }
  //
  // struct implementation:
  // impl<I: Foo, O: Foo> HasFoo<I, O> {
  //     // code goes here ...
  // }
  //
  // even more annoyingly, those bounds have to be added on *every* impl block for the struct

  string client_impl_struct_name = "T" + tservice->get_name() + "SyncClient";

  // render the implementing struct
  f_gen_ << "pub struct " << client_impl_struct_name << " {" << endl;
  indent_up();
  f_gen_ << indent() << "i_prot: Rc<RefCell<Box<TProtocol>>>," << endl;
  f_gen_ << indent() << "o_prot: Rc<RefCell<Box<TProtocol>>>," << endl;
  f_gen_ << indent() << "sequence_number: i32," << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  // render the struct implementation
  // this includes the new() function as well as the helper send/recv methods for each service call
  f_gen_ << "impl " << client_impl_struct_name << " {" << endl;
  indent_up();
  render_rust_sync_client_lifecycle_functions(client_impl_struct_name);
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    render_rust_sync_send(tfunc);
    if (!tfunc->is_oneway()) {
      render_rust_sync_recv(tfunc);
    }
  }
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // render all the service methods for the implementing struct
  f_gen_ << "impl " << rust_sync_client_trait_name(tservice) << " for " << client_impl_struct_name << " {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* func = (*func_iter);
    render_rust_sync_send_recv_wrapper(func);
  }
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_service_sync_client_trait(t_service* tservice) {
  string extension = "";
  if (tservice->get_extends() != NULL) {
    t_service* extends = tservice->get_extends();
    extension = " : " + extends->get_program()->get_namespace() + "::" + rust_sync_client_trait_name(extends);
  }

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  f_gen_ << "pub trait " << rust_sync_client_trait_name(tservice) << extension << " {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    string func_name = rust_service_call_client_function_name(tfunc);
    string func_args = rust_sync_service_call_args(tfunc, true);
    string func_return = to_rust_type(tfunc->get_returntype());
    f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<" << func_return << ">;" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}" << endl;

  f_gen_ << endl;
}

void t_rs_generator::render_rust_sync_client_lifecycle_functions(const string& client_struct) {
  // constructor (shared protocol)
  f_gen_ << indent() << "pub fn new(protocol: Box<TProtocol>) -> " << client_struct << " {" << endl;
  indent_up();
  f_gen_ << indent() << "let p = Rc::new(RefCell::new(protocol));" << endl;
  f_gen_ << indent() << client_struct << " { i_prot: p.clone(), o_prot: p.clone(), sequence_number: 0 }" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  // constructor (separate protcols)
  f_gen_ << indent() << "pub fn with_separate_protocols(input_protocol: Box<TProtocol>, output_protocol: Box<TProtocol>) -> " << client_struct << " {" << endl;
  indent_up();
  f_gen_ << indent() << client_struct << " { i_prot: Rc::new(RefCell::new(input_protocol)), o_prot: Rc::new(RefCell::new(output_protocol)), sequence_number: 0 }" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  // FIXME: render open and close methods (have to consider case when transport is shared between protocols)
}

void t_rs_generator::render_rust_result_value_struct(t_function* tfunc) {
  t_struct result(program_, service_call_result_struct_name(tfunc));

  t_field return_value(tfunc->get_returntype(), SERVICE_CALL_RESULT_VARIABLE, 0);
  return_value.set_req(t_field::T_OPTIONAL);
  if (!tfunc->get_returntype()->is_void()) {
    result.append(&return_value);
  }

  t_struct* exceptions = tfunc->get_xceptions();
  const vector<t_field*>& exception_types = exceptions->get_members();
  vector<t_field*>::const_iterator exception_iter;
  for(exception_iter = exception_types.begin(); exception_iter != exception_types.end(); ++exception_iter) {
    t_field* exception_type = *exception_iter;
    exception_type->set_req(t_field::T_OPTIONAL);
    result.append(exception_type);
  }

  render_rust_struct(&result, t_rs_generator::T_RESULT);
}

void t_rs_generator::render_rust_sync_send_recv_wrapper(t_function* tfunc) {
  string func_name = rust_service_call_client_function_name(tfunc);
  string func_decl_args = rust_sync_service_call_args(tfunc, true);
  string func_call_args = rust_sync_service_call_args(tfunc, false);
  string func_return = to_rust_type(tfunc->get_returntype());

  f_gen_ << indent() << "fn " << func_name <<  func_decl_args << " -> rift::Result<" << func_return << "> {" << endl;
  indent_up();
  f_gen_ << indent() << "try!(self.send_" << func_name << func_call_args << ");" << endl;
  if (tfunc->is_oneway()) {
    f_gen_ << indent() << "Ok(())" << endl;
  } else {
    f_gen_ << indent() << "self.recv_" << func_name << "()" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_sync_send(t_function* tfunc) {
  string func_name = rust_service_call_sync_send_client_function_name(tfunc);
  string func_args = rust_sync_service_call_args(tfunc, true);

  // declaration
  f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<()> {" << endl;
  indent_up();

  // increment the sequence number and generate the call header
  string message_type = tfunc->is_oneway() ? "TMessageType::OneWay" : "TMessageType::Call";
  f_gen_ << indent() << "self.sequence_number = self.sequence_number + 1;" << endl;
  f_gen_
    << indent()
    << "let message_ident = "
    << "TMessageIdentifier { name:\"" << tfunc->get_name() << "\".to_owned(), "
    << "message_type: " << message_type << ", "
    << "sequence_number: self.sequence_number };"
    << endl;
  // pack the arguments into the containing struct that we'll write out over the wire
  // note that this struct is generated even if we have 0 args
  ostringstream struct_definition;
  vector<t_field*> members = tfunc->get_arglist()->get_sorted_members();
  vector<t_field*>::iterator members_iter;
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    struct_definition << tfield->get_name() << ": " << tfield->get_name() << ", ";
  }
  string struct_fields = struct_definition.str();
  if (struct_fields.size() > 0) {
    struct_fields = struct_fields.substr(0, struct_fields.size() - 2); // strip trailing comma
  }
  f_gen_
    << indent()
    << "let call_args = "
    << tfunc->get_arglist()->get_name()
    << " { "
    << struct_fields
    << " };"
    << endl;
  // write everything over the wire
  f_gen_ << indent() << "try!(self.o_prot.borrow_mut().write_message_begin(&message_ident));" << endl;
  f_gen_ << indent() << "try!(call_args.write_to_out_protocol(&mut **self.o_prot.borrow_mut()));" << endl; // written even if we have 0 args
  f_gen_ << indent() << "try!(self.o_prot.borrow_mut().write_message_end());" << endl;
  f_gen_ << indent() << "Ok(())" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_sync_recv(t_function* tfunc) {
  string func_name = rust_service_call_sync_recv_client_function_name(tfunc);
  string func_return = to_rust_type(tfunc->get_returntype());
  f_gen_ << indent() << "fn " << func_name << "(&mut self) -> rift::Result<" << func_return << "> {" << endl;
  indent_up();
  f_gen_ << indent() << "let message_ident = try!(self.i_prot.borrow_mut().read_message_begin());" << endl;
  f_gen_ << indent() << "try!(verify_expected_sequence_number(self.sequence_number, message_ident.sequence_number));" << endl;
  f_gen_ << indent() << "try!(verify_expected_service_call(\"" << tfunc->get_name() <<"\", &message_ident.name));" << endl;
  // FIXME: replace with a "try" block
  f_gen_ << indent() << "if message_ident.message_type == TMessageType::Exception {" << endl;
  indent_up();
  f_gen_ << indent() << "let remote_error = try!(rift::Error::read_application_error_from_in_protocol(&mut **self.i_prot.borrow_mut()));" << endl;
  f_gen_ << indent() << "try!(self.i_prot.borrow_mut().read_message_end());" << endl;
  f_gen_ << indent() << "return Err(rift::Error::Application(remote_error))" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(verify_expected_message_type(TMessageType::Reply, message_ident.message_type));" << endl;
  f_gen_ << indent() << "let result = try!(" << service_call_result_struct_name(tfunc) << "::read_from_in_protocol(&mut **self.i_prot.borrow_mut()));" << endl;
  f_gen_ << indent() << "try!(self.i_prot.borrow_mut().read_message_end());" << endl;
  f_gen_ << indent() << "result.ok_or()" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

string t_rs_generator::rust_sync_service_call_args(t_function* tfunc, bool is_declaration, const string& prefix) {
  ostringstream func_args;
  func_args << (is_declaration ? "(&mut self" : "(");

  // service call args are packed into a struct.
  // unpack them and expand into a valid rust function arg list.
  // types that can be passed by value are, and those that can't
  // will be prepended with '&' so that they can be passed by ref
  int is_first_arg = true;
  if (has_args(tfunc)) {
    t_struct* args = tfunc->get_arglist();
    std::vector<t_field*> fields = args->get_sorted_members();
    std::vector<t_field*>::iterator field_iter;
    for (field_iter = fields.begin(); field_iter != fields.end(); ++field_iter) {
      t_field* tfield = (*field_iter);
      string rust_type = to_rust_type(tfield->get_type());
      if (is_first_arg) { // FIXME: remove conditional
        if (is_declaration) {
          func_args << ", ";
        }
        is_first_arg = false;
      } else {
        func_args << ", ";
      }
      func_args << prefix << tfield->get_name() << (is_declaration ? ": " + rust_type : "");
    }
  }

  func_args << ")";
  return func_args.str();
}

//-----------------------------------------------------------------------------
//
// Sync Server
//
//-----------------------------------------------------------------------------

void t_rs_generator::render_rust_sync_server(t_service* tservice) {
  render_rust_service_server_comment(tservice);
  render_rust_service_sync_handler_trait(tservice);
  render_rust_service_processor(tservice);
}

void t_rs_generator::render_rust_service_server_comment(t_service* tservice) {
  f_gen_ << "//" << endl;
  f_gen_ << "// " << tservice->get_name() << " service server"<< endl;
  f_gen_ << "//" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_service_sync_handler_trait(t_service* tservice) {
  string extension = "";
  if (tservice->get_extends() != NULL) {
    t_service* extends = tservice->get_extends();
    extension = " : " + extends->get_program()->get_namespace() + "::" + rust_sync_handler_trait_name(extends);
  }

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  f_gen_ << "pub trait " << rust_sync_handler_trait_name(tservice) << extension << " {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    string func_name = rust_service_call_handler_function_name(tfunc);
    string func_args = rust_sync_service_call_args(tfunc, true);
    string func_return = to_rust_type(tfunc->get_returntype());
    f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<" << func_return << ">;" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}" << endl;

  f_gen_ << endl;
}

void t_rs_generator::render_rust_service_processor(t_service* tservice) {
  string service_processor_name = "T" + tservice->get_name() + "Processor";
  string handler_trait_name = rust_sync_handler_trait_name(tservice);

  // struct
  f_gen_ << indent() << "pub struct " << service_processor_name << "<H: " << handler_trait_name << "> {" << endl;
  indent_up();
  f_gen_ << indent() << "handler: H," << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  f_gen_ << endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator func_iter;

  // impl
  f_gen_ << indent() << "impl <H: " << handler_trait_name << "> " << service_processor_name << "<H> {" << endl;
  indent_up();

  f_gen_ << indent() << "pub fn new(handler: H) -> " << service_processor_name << "<H> {" << endl;
  indent_up();
  f_gen_ << indent() << service_processor_name << " { handler: handler }" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    render_rust_service_process_function(tfunc);
  }

  indent_down();
  f_gen_ << indent() << "}" << endl;
  f_gen_ << endl;

  // processor impl
  f_gen_ << indent() << "impl <H: " << handler_trait_name << "> TProcessor for " << service_processor_name << "<H> {" << endl;
  indent_up();

  f_gen_
    << indent()
    << "fn process(&mut self, i_prot: &mut TProtocol, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();
  f_gen_ << indent() << "let message_ident = try!(i_prot.read_message_begin());" << endl;
  f_gen_ << indent() << "match &*message_ident.name {" << endl; // [sigh] explicit deref coercion
  indent_up();

  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    f_gen_ << indent() << "\"" << tfunc->get_name() << "\"" << " => {" << endl;
    indent_up();
    f_gen_ << indent() << "self.process_" << underscore(tfunc->get_name()) << "(message_ident.sequence_number, i_prot, o_prot)" << endl;
    indent_down();
    f_gen_ << indent() << "}," << endl;
  }
  f_gen_ << indent() << "method => {" << endl;
  indent_up();
  render_rust_rift_error("Application", "ApplicationError", "ApplicationErrorKind::UnknownMethod", "format!(\"unknown method {}\", method)");
  indent_down();
  f_gen_ << indent() << "}," << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_service_process_function(t_function* tfunc) {
  f_gen_
    << indent()
    << "fn process_" << underscore(tfunc->get_name())
    << "(&mut self, incoming_sequence_number: i32, i_prot: &mut TProtocol, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();

  f_gen_
    << indent()
    << "let "
    << "args" // FIXME: deal with oneway functions and only void args
    << " = try!("
    << tfunc->get_arglist()->get_name()
    << "::read_from_in_protocol(i_prot));"
    << endl;
  f_gen_
    << indent()
    << "match self.handler."
    << rust_service_call_handler_function_name(tfunc)
    << rust_sync_service_call_args(tfunc, false, "args.")
    << " {"
    << endl;
  indent_up();

  string handler_return_variable = tfunc->is_oneway() || tfunc->get_returntype()->is_void() ? "_" : "handler_return";

  // OK case
  f_gen_ << indent() << "Ok(" << handler_return_variable << ") => {" << endl;
  indent_up();
  if (tfunc->is_oneway()) {
    f_gen_ << indent() << "Ok(())" << endl;
  } else {
    f_gen_
      << indent()
      << "try!(o_prot.write_message_begin(&TMessageIdentifier { "
      << "name: \"" << tfunc->get_name() << "\".to_owned(), "
      << "message_type: TMessageType::Reply,"
      << "sequence_number: incoming_sequence_number }));"
      << endl;
    f_gen_ << indent() << "let ret = " << handler_successful_return_struct(tfunc) <<";" << endl;
    f_gen_ << indent() << "try!(ret.write_to_out_protocol(o_prot));" << endl;
    f_gen_ << indent() << "o_prot.write_message_end()" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}," << endl;

  // Error case
  f_gen_ << indent() << "Err(e) => {" << endl;
  indent_up();
  f_gen_ << indent() << "unimplemented!()" << endl;
  // have to check if it's a user error or a generic error
  indent_down();
  f_gen_ << indent() << "}," << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

string t_rs_generator::handler_successful_return_struct(t_function* tfunc) {
  int member_count = 0;
  ostringstream return_struct;

  return_struct << service_call_result_struct_name(tfunc) << " { ";

  // actual return
  if (!tfunc->get_returntype()->is_void()) {
    return_struct << "result_value: Some(handler_return)";
    member_count++;
  }

  // any user-defined exceptions
  if (tfunc->get_xceptions() != NULL) {
    t_struct* txceptions = tfunc->get_xceptions();
    const vector<t_field*> members = txceptions->get_sorted_members();
    vector<t_field*>::const_iterator members_iter;
    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* txception = (*members_iter);
      if (member_count > 0) { return_struct << ", "; }
      return_struct << txception->get_name() << ": None";
      member_count++;
    }
  }

  return_struct << " }";

  return  return_struct.str();
}

//-----------------------------------------------------------------------------
//
// Service Utility
//
//-----------------------------------------------------------------------------



void t_rs_generator::render_rust_service_call_structs(t_service* tservice) {
  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  // thrift args for service calls are packed
  // into a struct that's transmitted over the wire, so
  // generate structs for those too
  //
  // thrift returns are *also* packed into a struct
  // that's passed over the wire, so, generate the struct
  // for that too. Note that this result struct *also*
  // contains the exceptions as well
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    render_rust_struct(tfunc->get_arglist(), t_rs_generator::T_ARGS);
    if (!tfunc->is_oneway()) {
      render_rust_result_value_struct(tfunc);
    }
  }
}

void t_rs_generator::render_rust_rift_error(const string& error_kind, const string& error_struct, const string& sub_error_kind, const string& error_message) {
  f_gen_ << indent() << "Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::" << error_kind << "(" << endl;
  indent_up();
  f_gen_ << indent() << error_struct << " {" << endl;
  indent_up();
  f_gen_ << indent() << "kind: " << sub_error_kind << "," << endl;
  f_gen_ << indent() << "message: " << error_message << "," << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
}

//-----------------------------------------------------------------------------
//
// Utility
//
//-----------------------------------------------------------------------------

string t_rs_generator::to_rust_type(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      return "()";
    case t_base_type::TYPE_STRING:
      return "String";
    case t_base_type::TYPE_BOOL:
      return "bool";
    case t_base_type::TYPE_I8:
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
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception() || ttype->is_typedef()) {
    if (ttype->get_program()->get_name() != get_program()->get_name()) {
      return ttype->get_program()->get_name() + "::" + ttype->get_name();
    } else {
      return ttype->get_name();
    }
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    return "BTreeMap<" + to_rust_type(tmap->get_key_type()) + ", " + to_rust_type(tmap->get_val_type()) + ">";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    return "BTreeSet<" + to_rust_type(tset->get_elem_type()) + ">";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    return "Vec<" + to_rust_type(tlist->get_elem_type()) + ">";
  }

  throw "cannot find rust type for " + ttype->get_name();
}

string t_rs_generator::to_rust_field_type_enum(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "will not generate protocol::TType for TYPE_VOID";
    case t_base_type::TYPE_STRING:
      return "TType::String";
    case t_base_type::TYPE_BOOL:
      return "TType::Bool";
    case t_base_type::TYPE_I8:
      return "TType::I08";
    case t_base_type::TYPE_I16:
      return "TType::I16";
    case t_base_type::TYPE_I32:
      return "TType::I32";
    case t_base_type::TYPE_I64:
      return "TType::I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType::Double";
    }
  } else if (ttype->is_enum()) {
    return "TType::I32";
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return "TType::Struct";
  } else if (ttype->is_map()) {
    return "TType::Map";
  } else if (ttype->is_set()) {
    return "TType::Set";
  } else if (ttype->is_list()) {
    return "TType::List";
  }

  throw "cannot find TType for " + ttype->get_name();
}

bool t_rs_generator::is_void(t_type* ttype) {
  return ttype->is_base_type() && ((t_base_type*)ttype)->get_base() == t_base_type::TYPE_VOID;
}

bool t_rs_generator::can_pass_by_value(t_type* ttype) {
  return ttype->is_base_type();
}

bool t_rs_generator::is_optional(t_field* tfield) {
  return is_optional(tfield->get_req());
}

bool t_rs_generator::is_optional(t_field::e_req req) {
  return req == t_field::T_OPTIONAL || req == t_field::T_OPT_IN_REQ_OUT;
}

bool t_rs_generator::has_args(t_function* tfunc) {
  return tfunc->get_arglist() != NULL && !tfunc->get_arglist()->get_sorted_members().empty();
}

string t_rs_generator::rust_sync_client_trait_name(t_service* tservice) {
  return "TAbstract" + capitalize(camelcase(tservice->get_name())) + "SyncClient";
}

string t_rs_generator::rust_sync_handler_trait_name(t_service* tservice) {
  return "TAbstract" + capitalize(camelcase(tservice->get_name())) + "SyncHandler";
}

string t_rs_generator::service_call_args_struct_name(t_function* tfunc) {
  return args_struct_name(tfunc->get_name());
}

string t_rs_generator::service_call_result_struct_name(t_function* tfunc) {
  return result_struct_name(tfunc->get_name());
}

string t_rs_generator::struct_name(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  switch(struct_type) {
    case t_rs_generator::e_struct_type::T_ARGS:
      return args_struct_name(tstruct->get_name());
    case t_rs_generator::e_struct_type::T_RESULT:
      return result_struct_name(tstruct->get_name());
    case t_rs_generator::e_struct_type::T_EXCEPTION:
    case t_rs_generator::e_struct_type::T_REGULAR:
      return default_struct_name(tstruct);
    default:
      throw "Cannot generate struct name for unknown struct type";
  }
}

string t_rs_generator::args_struct_name(const string& name) {
  return capitalize(camelcase(name)) + ARGS_STRUCT_SUFFIX;
}

string t_rs_generator::result_struct_name(const string& name) {
  return capitalize(camelcase(name)) + RESULT_STRUCT_SUFFIX;
}

string t_rs_generator::default_struct_name(t_struct* tstruct) {
  return capitalize(camelcase(tstruct->get_name()));
}

string t_rs_generator::rust_service_call_client_function_name(t_function* tfunc) {
  return decapitalize(underscore(tfunc->get_name()));
}

string t_rs_generator::rust_service_call_sync_send_client_function_name(t_function* tfunc) {
  return "send_" + decapitalize(underscore(tfunc->get_name()));
}

string t_rs_generator::rust_service_call_sync_recv_client_function_name(t_function* tfunc) {
  return "recv_" + decapitalize(underscore(tfunc->get_name()));
}

string t_rs_generator::rust_service_call_handler_function_name(t_function* tfunc) {
  return "handle_" + decapitalize(underscore(tfunc->get_name()));
}

string t_rs_generator::visibility_qualifier(t_rs_generator::e_struct_type struct_type) {
  switch(struct_type) {
  case t_rs_generator::T_ARGS:
  case t_rs_generator::T_RESULT:
    return "";
  default:
    return "pub ";
  }
}

THRIFT_REGISTER_GENERATOR(
  rs,
  "Rust",
  "   nodoc\n")
