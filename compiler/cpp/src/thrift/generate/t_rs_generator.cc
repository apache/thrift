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
static const string service_call_result_variable = "result_value";

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
  void render_free_standing_helpers();

  // Write the rust representation of a thrift struct to the generated file.
  // Set `is_args_struct` to `true` if rendering the struct used to pack
  // arguments for a service call. When `true` the struct and its members have
  // private visibility, and all fields are required. When `false` the
  // struct and its members have public visibility and fields are optional
  // if set as such in their thrift definition.
  void render_rust_struct(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_struct_comment(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_struct_definition(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_struct_impl(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_rust_result_struct_to_result_method(t_struct* tstruct);

  void render_rust_exception_struct_error_trait_impls(t_struct* tstruct);

  void render_rust_struct_write_to_out_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  void render_rust_struct_field_write(const string& prefix, t_field* tfield, t_field::e_req req);
  void render_rust_struct_read_from_in_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  void render_rust_result_value_struct(t_function* tfunc);

  // Write the rust representation of a thrift enum to the generated file.
  void render_rust_union(t_struct* tstruct);

  void render_rust_const_value(t_const_value* tconstvalue);
  void render_rust_sync_client(t_service* tservice);
  void render_rust_sync_server(t_service* tservice);
  void render_rust_sync_send_recv_wrapper(t_function* tfunc);
  void render_rust_sync_send(t_function* tfunc);
  void render_rust_sync_recv(t_function* tfunc);

  string rust_field_write(const string& field_prefix, t_field* tfield, t_field::e_req req);

  // Return a string containing all the unpacked service call args
  // given a service call function `t_function`.
  // If `is_declaration` is `true` we prepend the args with `&mut self`
  // and include the arg types in the returned string. If `false` we omit
  // the `self` qualifier and only use the arg names.
  string rust_sync_client_call_args(t_function* tfunc, bool is_declaration);

  // Return a string representing the rust type given a `t_type`.
  string to_rust_type(t_type* ttype);

  // Return a string representing the rift `protocol::TFieldType` given a `t_type`.
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

  // Return `pub ` (notice trailing whitespace!) if the struct
  // should be public, `` (empty string) otherwise.
  string visibility_qualifier(t_rs_generator::e_struct_type struct_type);

  // Return the trait name for the sync service client given a `t_service` name.
  string rust_sync_client_trait_name(t_service* tservice); // FIXME: remove

  // Return the name of the struct used to pack arguments for the thrift service call.
  string service_call_args_struct_name(t_function* tfunc);

  // Return the name of the struct used to pack the return value
  // and user-defined exceptions for the thrift service call.
  string service_call_result_struct_name(t_function* tfunc);
};

// FIXME: who writes the thrift::STOP?!

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
  render_free_standing_helpers();
}

void t_rs_generator::render_attributes_and_includes() {
  // turn off some warnings
  f_gen_ << "#![allow(unused_imports)]" << endl; // generated code always includes BTreeMap/BTreeSet
  f_gen_ << "#![allow(non_snake_case)]" << endl; // generated code keeps user-specified names (FIXME: change to underscore?)
  f_gen_ << endl;

  // add standard includes
  f_gen_ << "extern crate rift;" << endl;
  f_gen_ << endl;
  f_gen_ << "use std::collections::{BTreeMap, BTreeSet};" << endl;
  f_gen_ << "use std::error::Error;" << endl;
  f_gen_ << "use std::fmt;" << endl;
  f_gen_ << "use std::fmt::{Display, Formatter};" << endl;
  f_gen_ << endl;
  f_gen_ << "use rift::protocol::{TFieldIdentifier, TFieldType, TMessageIdentifier, TMessageType, TProtocol, TStructIdentifier};" << endl;
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

void t_rs_generator::render_free_standing_helpers() {
  // check that the sequence number is what you expect
  f_gen_ << "fn verify_expected_sequence_number(expected: i32, actual: i32) -> rift::Result<()> {" << endl;
  indent_up();
  f_gen_ << indent() << "if expected == actual {" << endl;
  indent_up();
  f_gen_ << indent() << "Ok(())" << endl;
  indent_down();
  f_gen_ << indent() << "} else {" << endl;
  indent_up();
  f_gen_ << indent() << "Err(rift::Error::OutOfOrderThriftMessage(expected, actual))" << endl;
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
  f_gen_ << indent() << "Err(rift::Error::WrongServiceCall(String::from(expected), String::from(actual)))" << endl;
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
  f_gen_ << indent() << "Err(rift::Error::UnexpectedThriftMessageType(expected, actual))" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
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
  f_gen_ << "type " << ttypedef->get_symbolic() << " = " << actual_type << ";" << endl;
  f_gen_ << endl;
}

void t_rs_generator::generate_enum(t_enum* tenum) {
  // enum definition
  f_gen_ << "#[derive(Copy, Clone, Debug, PartialEq)]" << endl;
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

  // let x = Foo::Bar as u32;
  // int -> enum reverse matching
  // TODO: avoid double loops
}

//-----------------------------------------------------------------------------
//
// Collections
//
//-----------------------------------------------------------------------------


void t_rs_generator::render_rust_union(t_struct* tstruct) {
}

// FIXME!!!

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
  render_rust_struct_comment(tstruct, struct_type);
  render_rust_struct_definition(tstruct, struct_type);
  render_rust_struct_impl(tstruct, struct_type);
  if (struct_type == t_rs_generator::T_EXCEPTION) {
    render_rust_exception_struct_error_trait_impls(tstruct);
  }
}

void t_rs_generator::render_rust_struct_comment(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_ << "//" << endl;
  f_gen_ << "// " << tstruct->get_name() << endl;
  f_gen_ << "//" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rust_struct_definition(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_ << "#[derive(Debug, PartialEq)]" << endl;
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
  f_gen_ << indent() << "\"" << "remote service threw " << tstruct->get_name() << "\";" << endl;
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
  size_t index = service_call_name.find("_result", 0);
  if (index == std::string::npos) {
    throw "result struct " + service_call_name + " missing result suffix";
  } else {
     service_call_name.replace(index, 7, "");
  }

  const vector<t_field*>& members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;

  // find out what the call's expected return type was
  string rust_return_type = "()"; // default is the unit return
  for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    if (tfield->get_name() == service_call_result_variable) {
      rust_return_type = to_rust_type(tfield->get_type());
      break;
    }
  }

  // FIXME: how do I deal with Box::new!

  // now actually render the "ok_or" method
  f_gen_ << indent() << "fn ok_or(self) -> rift::Result<" << rust_return_type << "> {" << endl;
  indent_up();

  for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    if (tfield->get_name() != service_call_result_variable) {
      // the remaining fields must be potential exceptions
      string field_name = "self." + tfield->get_name();
      f_gen_ << indent() << "if " << field_name << ".is_some() {" << endl;
      indent_up();
      f_gen_ << indent() << "return Err(rift::Error::ApplicationError(Box::new(" << field_name << ".unwrap())))" << endl;
      indent_down();
      f_gen_ << indent() << "}" << endl;
    }
  }

  if (rust_return_type == "()") {
    f_gen_ << indent() << "Ok(())" << endl;
  } else {
    members_iter = members.begin();
    f_gen_ << indent() << "if self." << service_call_result_variable << ".is_some() {" << endl;
    indent_up();
    f_gen_ << indent() << "return Ok(self." << service_call_result_variable << ".unwrap())" << endl;
    indent_down();
    f_gen_ << indent() << "} else {" << endl;
    indent_up();
    // if we have not found a valid return value *or* a user exception
    // then we're in trouble; return a default error
    f_gen_
      << indent()
      << "return Err(rift::Error::MissingServiceCallReturnValue("
      << "\"" << service_call_name << "\".to_owned()"
      << "))"
      << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  }

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_struct_write_to_out_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_
    << indent()
    << visibility_qualifier(struct_type)
    << "fn write_to_out_protocol<O: TProtocol>(&self, o_prot: &mut O) -> rift::Result<()> {"
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
  // FIXME: seqid, name are options f_gen_ << indent() << "try!(o_prot.write_field_stop());" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_end());" << endl;
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
    << "let field_ident = TFieldIdentifier {"
    << "name: Some(\"" << tfield->get_name() << "\".to_owned()" << "), "
    << "field_type: " << to_rust_field_type_enum(tfield->get_type()) << ", "
    << "id: " << tfield->get_key() << " "
    << "};";
  string field_ident_string = field_stream.str();

  if (is_optional(req)) {
    string field_name = field_prefix + tfield->get_name();
    f_gen_ << indent() << "if " << field_name << ".is_some() {" << endl;
    indent_up();
    f_gen_ << indent() << field_ident_string << endl;
    f_gen_ << indent() << "try!(o_prot.write_field_begin(&field_ident));" << endl;
    f_gen_ << indent() << rust_field_write(field_prefix, tfield, req) << endl;
    f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
    f_gen_ << indent() << "()" << endl;
    indent_down();
    f_gen_ << indent() << "} else {" << endl;
    indent_up();
    if (req == t_field::T_OPT_IN_REQ_OUT) {
      f_gen_ << indent() << field_ident_string << endl;
      f_gen_ << indent() << "try!(o_prot.write_field_begin(&field_ident));" << endl;
      f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
    }
    f_gen_ << indent() << "()" << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  } else {
    f_gen_ << indent() << rust_field_write(field_prefix, tfield, req) << endl;
  }
}

string t_rs_generator::rust_field_write(const string& field_prefix, t_field* tfield, t_field::e_req req) {
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
    case t_base_type::TYPE_STRING:
      return "try!(o_prot.write_string(&" + field_name + "));";
    case t_base_type::TYPE_BOOL:
      return "try!(o_prot.write_bool(" + field_name + "));";
    case t_base_type::TYPE_I8:
      return "try!(o_prot.write_i8(" + field_name + "));";
    case t_base_type::TYPE_I16:
      return "try!(o_prot.write_i16(" + field_name + "));";
    case t_base_type::TYPE_I32:
      return "try!(o_prot.write_i32(" + field_name + "));";
    case t_base_type::TYPE_I64:
      return "try!(o_prot.write_i64(" + field_name + "));";
    case t_base_type::TYPE_DOUBLE:
      return "try!(o_prot.write_double(" + field_name + "));";
    }
  } else if (ttype->is_enum()) {
    return "";
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return "try!(" +  field_name + ".write_to_out_protocol(o_prot));";
  } else if (ttype->is_map()) {
    return "";
  } else if (ttype->is_set()) {
    return "";
  } else if (ttype->is_list()) {
    return "";
  }

  throw "Unsupported type " + ttype->get_name();
}

void t_rs_generator::render_rust_struct_read_from_in_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_
    << indent()
    << visibility_qualifier(struct_type)
    << "fn read_from_in_protocol<I: TProtocol>(i_prot: &mut I) -> rift::Result<" << tstruct->get_name() << "> {"
    << endl;
  indent_up();
  // check struct begin, end, fields
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

//-----------------------------------------------------------------------------
//
// Sync Client and Server
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_service(t_service* tservice) {
  render_rust_sync_client(tservice);
  render_rust_sync_server(tservice);
}

void t_rs_generator::render_rust_sync_client(t_service* tservice) {
  string client_trait_name = rust_sync_client_trait_name(tservice);
  string client_impl_struct_name = "T" + tservice->get_name() + "SyncClient";

  string extension = "";
  if (tservice->get_extends() != NULL) {
    t_service* extends = tservice->get_extends();
    extension = " : " + extends->get_program()->get_namespace() + "::" + rust_sync_client_trait_name(extends);
  }

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  // service comment demarcation
  f_gen_ << "//" << endl;
  f_gen_ << "// service " << tservice->get_name() << endl;
  f_gen_ << "//" << endl;
  f_gen_ << endl;

  // render the trait
  // although not strictly necessary, I'm doing this to make testing easier
  // this way users can pass in anything that implements the client facade
  f_gen_ << "pub trait " << client_trait_name << extension << " {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    string func_name = tfunc->get_name();
    string func_args = rust_sync_client_call_args(tfunc, true);
    string func_return = to_rust_type(tfunc->get_returntype());
    f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<" << func_return << ">;" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}" << endl;
  f_gen_ << endl;

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

  // render the implementing struct
  f_gen_ << "pub struct " << client_impl_struct_name << "<I: TProtocol, O: TProtocol> {" << endl;
  indent_up();
  f_gen_ << indent() << "i_prot: I," << endl;
  f_gen_ << indent() << "o_prot: O," << endl;
  f_gen_ << indent() << "sequence_number: i32," << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // render the struct implementation
  // this includes the new() function as well as the helper send/recv methods for each service call
  f_gen_ << "impl<I: TProtocol, O: TProtocol> " << client_impl_struct_name << "<I, O> {" << endl;
  indent_up();
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
  f_gen_ << "impl<I: TProtocol, O:TProtocol>" << client_trait_name << " for " << client_impl_struct_name << "<I, O> {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* func = (*func_iter);
    render_rust_sync_send_recv_wrapper(func); // FIXME: don't generate recv for oneway
  }
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

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

void t_rs_generator::render_rust_result_value_struct(t_function* tfunc) {
  t_struct result(program_, service_call_result_struct_name(tfunc));

  t_field return_value(tfunc->get_returntype(), service_call_result_variable, 0);
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
  string func_name = tfunc->get_name();
  string func_decl_args = rust_sync_client_call_args(tfunc, true);
  string func_call_args = rust_sync_client_call_args(tfunc, false);
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
  string func_name = "send_" + tfunc->get_name();
  string func_args = rust_sync_client_call_args(tfunc, true);

  // declaration
  f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<()> {" << endl;
  indent_up();

  // increment the sequence number and generate the call header
  f_gen_ << indent() << "self.sequence_number = self.sequence_number + 1;" << endl;
  f_gen_
    << indent()
    << "let message_ident = "
    << "TMessageIdentifier { name:\"" << tfunc->get_name() << "\".to_owned(), "
    << "message_type: TMessageType::Call, "
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
  f_gen_ << indent() << "try!(self.o_prot.write_message_begin(&message_ident));" << endl;
  f_gen_ << indent() << "try!(call_args.write_to_out_protocol(&mut self.o_prot));" << endl; // written even if we have 0 args
  f_gen_ << indent() << "try!(self.o_prot.write_message_end());" << endl;
  f_gen_ << indent() << "Ok(())" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_rust_sync_recv(t_function* tfunc) {
  string func_name = "recv_" + tfunc->get_name();
  string func_return = to_rust_type(tfunc->get_returntype());

  // check method name
  // check method type (unexpected message type, unexpected exception)
  // check sequence id)
  // if it's an exception, return Err(...)
  // if it's not an exception: let ret = Struct.read_from_in_protocol(...)
  // read message end
  // Ok(ret)

  f_gen_ << indent() << "fn " << func_name << "(&mut self) -> rift::Result<" << func_return << "> {" << endl;
  indent_up();
  f_gen_ << indent() << "let message_ident = try!(self.i_prot.read_message_begin());" << endl;
  f_gen_ << indent() << "try!(verify_expected_sequence_number(self.sequence_number, message_ident.sequence_number));" << endl;
  f_gen_ << indent() << "try!(verify_expected_service_call(\"" << tfunc->get_name() <<"\", &message_ident.name));" << endl;
  // FIXME: check if exception or not
  // if it's an exception, return the application exception (WTF is the type?!)
  // otherwise...
  // check that it's the message type
  f_gen_ << indent() << "try!(verify_expected_message_type(TMessageType::Reply, message_ident.message_type));" << endl;
  f_gen_ << indent() << "let mut result = try!(" << service_call_result_struct_name(tfunc) << "::read_from_in_protocol(&mut self.i_prot));" << endl;
  f_gen_ << indent() << "try!(self.i_prot.read_message_end());" << endl;
  f_gen_ << indent() << "result.ok_or()" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

string t_rs_generator::rust_sync_client_call_args(t_function* tfunc, bool is_declaration) {
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
      func_args << tfield->get_name() << (is_declaration ? ": " + rust_type : "");
    }
  }

  func_args << ")";
  return func_args.str();
}

void t_rs_generator::render_rust_sync_server(t_service* tservice) {
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
      throw "will not generate protocol::TFieldType for TYPE_VOID";
    case t_base_type::TYPE_STRING:
      return "TFieldType::String";
    case t_base_type::TYPE_BOOL:
      return "TFieldType::Bool";
    case t_base_type::TYPE_I8:
      return "TFieldType::I08";
    case t_base_type::TYPE_I16:
      return "TFieldType::I16";
    case t_base_type::TYPE_I32:
      return "TFieldType::I32";
    case t_base_type::TYPE_I64:
      return "TFieldType::I64";
    case t_base_type::TYPE_DOUBLE:
      return "TFieldType::Double";
    }
  } else if (ttype->is_enum()) {
    return "TFieldType::I32";
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return "TFieldType::Struct";
  } else if (ttype->is_map()) {
    return "TFieldType::Map";
  } else if (ttype->is_set()) {
    return "TFieldType::Set";
  } else if (ttype->is_list()) {
    return "TFieldType::List";
  }

  throw "cannot find TFieldType for " + ttype->get_name();
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
  return "TAbstract" + tservice->get_name() + "SyncClient";
}

string t_rs_generator::service_call_args_struct_name(t_function* tfunc) {
  return tfunc->get_name() + "_args";
}

string t_rs_generator::service_call_result_struct_name(t_function* tfunc) {
  return tfunc->get_name() + "_result";
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
