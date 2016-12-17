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
#include <boost/algorithm/string/replace.hpp>

#include "thrift/platform.h"
#include "thrift/version.h"
#include "thrift/generate/t_generator.h"

using std::map;
using std::ofstream;
using std::ostringstream;
using std::string;
using std::vector;

static const string endl = "\n"; // avoid ostream << std::endl flushes
static const string SERVICE_CALL_RESULT_VARIABLE = "result_value";
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

    // warn the user that I'm ignoring all options
    if (parsed_options.size() > 0) {
      std::cerr << "ignoring " << parsed_options.size() << " options" << endl;
    }
    if (options.size() > 0) {
      std::cerr << "ignoring " << options.size() << " options" << endl;
    }
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

  // Write the common compiler attributes and module includes
  // to the top of the auto-generated file.
  void render_attributes_and_includes();

  // Write the rust representation of an enum.
  void render_enum_definition(t_enum* tenum);

  void render_enum_conversion(t_enum* tenum);

  // Write the impl block associated with the rust
  // representation of an enum. This includes methods
  // to write the enum to a protocol, read it from
  // a protocol, etc.
  void render_enum_impl(t_enum* tenum);

  void render_const_value(const string& name, t_type* ttype, t_const_value* tvalue);
  void render_const_value_holder(const string& name, t_type* ttype, t_const_value* tvalue);
  void render_const_value(t_type* ttype, t_const_value* tvalue);

  // Write the rust representation of a thrift struct to the generated file.
  // Set `struct_type` to `T_ARGS` if rendering the struct used to pack
  // arguments for a service call. When `struct_type` is `T_ARGS` the struct
  // and its members have module visibility, and all fields are required. When
  // `struct_type` is anything else the struct and its members have public
  // visibility and fields have the visibiliy set in their definition.
  void render_struct(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  // Write the comment block preceding a type definition (and implementation).
  void render_type_comment(const string& struct_name);

  // Write the rust representation of a thrift struct. Supports
  // argument structs, result structs, user-defined structs and
  // exception structs. The exact struct type to be generated is
  // controlled by the `struct_type` parameter, which (among other things)
  // modifies the visibility of the written struct and members,
  // controls which trait implementations are generated.
  void render_struct_definition(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  // Writes the impl block associated with the rust
  // representation of a struct. At minimum this contains the
  // methods to read from a protocol and write to a protocol.
  // Additional methods may be generated depending on `struct_type`.
  void render_struct_impl(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type);

  void render_result_struct_to_result_method(t_struct* tstruct);

  void render_exception_struct_error_trait_impls(const string& struct_name, t_struct* tstruct);

  void render_struct_write_to_out_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  void render_struct_field_write(const string& field_var, t_field* tfield, t_field::e_req req);
  void render_type_write(const string& type_var, t_type* ttype);

  // Write a list to the output protocol. `list_variable`
  // is the variable containing the list that will be written
  // to the output protocol.
  void render_list_write(const string& list_variable, t_list* tlist);

  // Write a set to the output protocol. `set_variable`
  // is the variable containing the set that will be written to
  // the output protocol.
  void render_set_write(const string& set_variable, t_set* tset);

  // Write a map to the output protocol. `map_variable` is the
  // variable containing the map that will be written to the output
  // protocol.
  void render_map_write(const string& map_variable, t_map* tset);

  // Return `true` if we need to dereference ths type when writing an element from a container.
  // Iterations on rust containers are performed as follows:
  // `for v in &values { ... }` where `v` has type `&RUST_TYPE`
  // All defined functions take primitives by value, so, if the
  // rendered code is calling such a function it has to dereference `v`.
  bool needs_deref_on_container_write(t_type* ttype);

  void render_struct_read_from_in_protocol(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type);
  void render_type_read(const string& type_var, t_type* ttype);

  // Read a list from the output protocol.
  void render_list_read(t_list* tlist, const string& list_variable);
  void render_set_read(t_set* tset, const string& set_variable);
  void render_map_read(t_map* tmap, const string& map_variable);

  string struct_field_read_temp_variable(t_field* tfield);

  // Write the rust representation of a thrift enum to the generated file.
  void render_union(t_struct* tstruct);
  void render_union_definition(const string& union_name, t_struct* tstruct);
  void render_union_impl(const string& union_name, t_struct* tstruct);
  void render_union_write_to_out_protocol(const string& union_name, t_struct* tstruct); // FIXME: remove
  void render_union_read_from_in_protocol(const string& union_name, t_struct* tstruct);

  void render_sync_client(t_service* tservice);
  void render_sync_client_lifecycle_functions(const string& client_struct);
  void render_sync_send_recv_wrapper(t_function* tfunc);

  // Render the `send` functionality for a Thrift service call represented
  // by a `t_service->t_function`.
  void render_sync_send(t_function* tfunc);

  // Render the `recv` functionality for a Thrift service call represented
  // by a `t_service->t_function`. This method is only rendered if the function
  // is *not* oneway.
  void render_sync_recv(t_function* tfunc);
  void render_sync_server(t_service* tservice);

  void render_service_sync_client_trait(t_service* tservice);
  void render_service_sync_handler_trait(t_service* tservice);
  void render_result_value_struct(t_function* tfunc);
  void render_service_server_comment(t_service* tservice);
  void render_service_process_function(t_function* tfunc);
  void render_service_processor(t_service* tservice);
  void render_handler_succeeded(t_function* tfunc);
  void render_handler_failed(t_function* tfunc);
  void render_handler_failed_user_exception_branch(t_function* tfunc);
  void render_handler_failed_default_exception_branch(t_function* tfunc);
  void render_service_call_structs(t_service* tservice);
  string handler_successful_return_struct(t_function* tfunc);
  void render_rift_error(const string& error_kind, const string& error_struct, const string& sub_error_kind, const string& error_message);
  void render_rift_error_struct(const string& error_struct, const string& sub_error_kind, const string& error_message);

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

  bool can_generate_simple_const(t_type* ttype);
  bool can_generate_const_holder(t_type* ttype);

  void render_const_struct(t_type* ttype, t_const_value* tvalue);
  void render_const_list(t_type* ttype, t_const_value* tvalue);
  void render_const_set(t_type* ttype, t_const_value* tvalue);
  void render_const_map(t_type* ttype, t_const_value* tvalue);
  void render_container_const_value(const string& insert_function, t_type* ttype, t_const_value* tvalue);

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

  bool has_non_void_args(t_function* tfunc);

  // Return `pub ` (notice trailing whitespace!) if the struct
  // should be public, `` (empty string) otherwise.
  string visibility_qualifier(t_rs_generator::e_struct_type struct_type);

  string rust_namespace(t_type* ttype);

  string rust_namespace(t_service* tservice);

  string rust_struct_name(t_struct* tstruct);

  // Return the trait name for the sync service client given a `t_service` name.
  string rust_sync_client_trait_name(t_service* tservice);

  string rust_sync_client_impl_name(t_service* tservice);

  // Return the trait name for the sync service processor given a `t_service` name.
  string rust_sync_handler_trait_name(t_service* tservice);

  string rust_sync_processor_name(t_service* tservice);

  string service_call_client_function_name(t_function* tfunc);

  string service_call_handler_function_name(t_function* tfunc);

  string service_call_sync_send_client_function_name(t_function* tfunc);

  string service_call_sync_recv_client_function_name(t_function* tfunc);

  // Return the name of the struct used to pack the return value
  // and user-defined exceptions for the thrift service call.
  string service_call_result_struct_name(t_function* tfunc);

  string rust_upper_case(const string& name);
  string rust_snake_case(const string& name);
  string rust_camel_case(const string& name);
};

void t_rs_generator::init_generator() {
  // make output directory for this thrift program
  MKDIR(gen_dir_.c_str());

  // create the file into which we're going to write the generated code
  string f_gen_name = gen_dir_ + "/" + rust_snake_case(get_program()->get_name()) + ".rs";
  f_gen_.open(f_gen_name.c_str());

  // header comment
  f_gen_ << "// " << autogen_summary() << endl;
  f_gen_ << "// DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING" << endl;
  f_gen_ << endl;

  render_attributes_and_includes();
  //render_utility_functions();
}

void t_rs_generator::render_attributes_and_includes() {
  // turn off some warnings
  f_gen_ << "#![allow(unused_imports)]" << endl; // generated code always includes BTreeMap/BTreeSet
  f_gen_ << endl;

  // add standard includes
  f_gen_ << "extern crate rift;" << endl;
  f_gen_ << "extern crate try_from;" << endl;
  f_gen_ << endl;
  f_gen_ << "use std::cell::RefCell;" << endl;
  f_gen_ << "use std::collections::{BTreeMap, BTreeSet};" << endl;
  f_gen_ << "use std::convert::From;" << endl;
  f_gen_ << "use std::error::Error;" << endl;
  f_gen_ << "use std::fmt;" << endl;
  f_gen_ << "use std::fmt::{Display, Formatter};" << endl;
  f_gen_ << "use std::rc::Rc;" << endl;
  f_gen_ << "use try_from::TryFrom;" << endl;
  f_gen_ << endl;
  f_gen_ << "use rift::{ApplicationError, ApplicationErrorKind, ProtocolError, ProtocolErrorKind};" << endl;
  f_gen_ << "use rift::protocol::{TFieldIdentifier, TListIdentifier, TMapIdentifier, TMessageIdentifier, TMessageType, TProtocol, TSetIdentifier, TStructIdentifier, TType};" << endl;
  f_gen_ << "use rift::protocol::field_id;" << endl;
  f_gen_ << "use rift::protocol::verify_expected_message_type;" << endl;
  f_gen_ << "use rift::protocol::verify_expected_sequence_number;" << endl;
  f_gen_ << "use rift::protocol::verify_expected_service_call;" << endl;
  f_gen_ << "use rift::protocol::verify_required_field_exists;" << endl;
  f_gen_ << "use rift::server::TProcessor;" << endl;
  f_gen_ << endl;

  // add thrift includes
  const vector<t_program*> includes = get_program()->get_includes();
  if (!includes.empty()) {
    vector<t_program*>::const_iterator includes_iter;
    for(includes_iter = includes.begin(); includes_iter != includes.end(); ++includes_iter) {
      f_gen_ << "pub use " << rust_snake_case((*includes_iter)->get_name()) << ";" << endl;
    }
    f_gen_ << endl;
  }
}

void t_rs_generator::close_generator() {
  f_gen_.close();
}

//-----------------------------------------------------------------------------
//
// Consts
//
//-----------------------------------------------------------------------------

// FIXME: consider using macros

// This is worse than it should be because constants
// aren't (sensibly) limited to scalar types
void t_rs_generator::generate_const(t_const* tconst) {
  string name = tconst->get_name();
  t_type* ttype = tconst->get_type();
  t_const_value* tvalue = tconst->get_value();

  if (can_generate_simple_const(ttype)) {
    render_const_value(name, ttype, tvalue);
  } else if (can_generate_const_holder(ttype)) {
    render_const_value_holder(name, ttype, tvalue);
  } else {
    throw "cannot generate const for " + name;
  }
}

void t_rs_generator::render_const_value(const string& name, t_type* ttype, t_const_value* tvalue) {
  if (!can_generate_simple_const(ttype)) {
    throw "cannot generate simple rust constant for " + ttype->get_name();
  }

  f_gen_ << "pub const " << rust_upper_case(name) << ": " << to_rust_type(ttype) << " = ";
  render_const_value(ttype, tvalue);
  f_gen_ << ";" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_const_value_holder(const string& name, t_type* ttype, t_const_value* tvalue) {
  if (!can_generate_const_holder(ttype)) {
    throw "cannot generate constant holder for " + ttype->get_name();
  }

  string holder_name("Const" + rust_camel_case(name));

  f_gen_ << indent() << "pub struct " << holder_name << ";" << endl;
  f_gen_ << indent() << "impl " << holder_name << " {" << endl;
  indent_up();

  f_gen_ << indent() << "pub fn const_value() -> " << to_rust_type(ttype) << " {" << endl;
  indent_up();
  render_const_value(ttype, tvalue);
  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_const_value(t_type* ttype, t_const_value* tvalue) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        f_gen_ << "\"" << tvalue->get_string() << "\""<<  ".to_owned().into_bytes()";
      } else {
        f_gen_ << "\"" << tvalue->get_string() << "\""<<  ".to_owned()";
      }
      break;
    case t_base_type::TYPE_BOOL:
      f_gen_ << (tvalue->get_integer() ? "true" : "false");
      break;
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      f_gen_ << tvalue->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      throw "cannot generate const value for double"; // FIXME
    default:
      throw "cannot generate const value for " + t_base_type::t_base_name(tbase_type->get_base());
    }
  } else if (ttype->is_typedef()) {
    render_const_value(get_true_type(ttype), tvalue);
  } else if (ttype->is_enum()) {
    f_gen_ << indent() << "{" << endl;
    indent_up();
    f_gen_
      << indent()
      << to_rust_type(ttype)
      << "::try_from("
      << tvalue->get_integer()
      << ").expect(\"expecting valid const value\")"
      << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  } else if (ttype->is_struct() || ttype->is_xception()) {
    render_const_struct(ttype, tvalue);
  } else if (ttype->is_container()) {
    f_gen_ << indent() << "{" << endl;
    indent_up();

    if (ttype->is_list()) {
      render_const_list(ttype, tvalue);
    } else if (ttype->is_set()) {
      render_const_set(ttype, tvalue);
    } else if (ttype->is_map()) {
      render_const_map(ttype, tvalue);
    } else {
      throw "cannot generate const container value for " + ttype->get_name();
    }

    indent_down();
    f_gen_ << indent() << "}" << endl;
  } else {
    throw "cannot generate const value for " + ttype->get_name();
  }
}

void t_rs_generator::render_const_struct(t_type* ttype, t_const_value* tvalue) {
  if (((t_struct*)ttype)->is_union()) {
    f_gen_ << indent() << "{" << endl;
    indent_up();
    f_gen_ << indent() << "unimplemented!()" << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  } else {
    f_gen_ << indent() << "{" << endl;
    indent_up();
    f_gen_ << indent() << "unimplemented!()" << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
  }
}

void t_rs_generator::render_const_list(t_type* ttype, t_const_value* tvalue) {
  t_type* elem_type = ((t_list*)ttype)->get_elem_type();
  f_gen_ << indent() << "let mut l: Vec<" << to_rust_type(elem_type) << "> = Vec::new();" << endl;
  const vector<t_const_value*>& elems = tvalue->get_list();
  vector<t_const_value*>::const_iterator elem_iter;
  for(elem_iter = elems.begin(); elem_iter != elems.end(); ++elem_iter) {
    t_const_value* elem_value = (*elem_iter);
    render_container_const_value("l.push", elem_type, elem_value);
  }
  f_gen_ << indent() << "l" << endl;
}

void t_rs_generator::render_const_set(t_type* ttype, t_const_value* tvalue) {
  t_type* elem_type = ((t_set*)ttype)->get_elem_type();
  f_gen_ << indent() << "let mut s: BTreeSet<" << to_rust_type(elem_type) << "> = BTreeSet::new();" << endl;
  const vector<t_const_value*>& elems = tvalue->get_list();
  vector<t_const_value*>::const_iterator elem_iter;
  for(elem_iter = elems.begin(); elem_iter != elems.end(); ++elem_iter) {
    t_const_value* elem_value = (*elem_iter);
    render_container_const_value("s.insert", elem_type, elem_value);
  }
  f_gen_ << indent() << "s" << endl;
}

void t_rs_generator::render_const_map(t_type* ttype, t_const_value* tvalue) {
  t_type* key_type = ((t_map*)ttype)->get_key_type();
  t_type* val_type = ((t_map*)ttype)->get_val_type();
  f_gen_
    << indent()
    << "let mut m: BTreeMap<"
    << to_rust_type(key_type) << ", " << to_rust_type(val_type)
    << "> = BTreeMap::new();"
    << endl;
  const map<t_const_value*, t_const_value*>& elems = tvalue->get_map();
  map<t_const_value*, t_const_value*>::const_iterator elem_iter;
  for (elem_iter = elems.begin(); elem_iter != elems.end(); ++elem_iter) {
    t_const_value* key_value = elem_iter->first;
    t_const_value* val_value = elem_iter->second;
    if (get_true_type(key_type)->is_base_type()) {
      f_gen_ << indent() << "let k = ";
      render_const_value(key_type, key_value);
      f_gen_ << ";" << endl;
    } else {
      f_gen_ << indent() << "let k = {" << endl;
      indent_up();
      render_const_value(key_type, key_value);
      indent_down();
      f_gen_ << indent() << "};" << endl;
    }
    if (get_true_type(val_type)->is_base_type()) {
      f_gen_ << indent() << "let v = ";
      render_const_value(val_type, val_value);
      f_gen_ << ";" << endl;
    } else {
      f_gen_ << indent() << "let v = {" << endl;
      indent_up();
      render_const_value(val_type, val_value);
      indent_down();
      f_gen_ << indent() << "};" << endl;
    }
    f_gen_ <<  indent() << "m.insert(k, v);" << endl;
  }
  f_gen_ << indent() << "m" << endl;
}

void t_rs_generator::render_container_const_value(const string& insert_function, t_type* ttype, t_const_value* tvalue) {
  if (get_true_type(ttype)->is_base_type()) {
    f_gen_ << indent() << insert_function << "(";
    render_const_value(ttype, tvalue);
    f_gen_ << ");" << endl;
  } else {
    f_gen_ << indent() << insert_function << "(" << endl;
    indent_up();
    render_const_value(ttype, tvalue);
    indent_down();
    f_gen_ << indent() << ");" << endl;
  }
}

bool t_rs_generator::can_generate_simple_const(t_type* ttype) {
  return (get_true_type(ttype))->is_base_type();
}

bool t_rs_generator::can_generate_const_holder(t_type* ttype) {
  t_type* actual_type = get_true_type(ttype);
  return !can_generate_simple_const(actual_type) && !actual_type->is_service();
}

//-----------------------------------------------------------------------------
//
// Typedefs
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_typedef(t_typedef* ttypedef) {
  std::string actual_type = to_rust_type(ttypedef->get_type());
  f_gen_ << "pub type " << ttypedef->get_symbolic() << " = " << actual_type << ";" << endl;
  f_gen_ << endl;
}

//-----------------------------------------------------------------------------
//
// Enums
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_enum(t_enum* tenum) {
  render_enum_definition(tenum);
  render_enum_impl(tenum);
  render_enum_conversion(tenum);
}

void t_rs_generator::render_enum_definition(t_enum* tenum) {
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

void t_rs_generator::render_enum_impl(t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator constants_iter;

  f_gen_ << "impl " << tenum->get_name() << " {" << endl;
  indent_up();

  f_gen_
    << indent()
    << "pub fn write_to_out_protocol(&self, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();
  f_gen_ << indent() << "o_prot.write_i32(*self as i32)" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  f_gen_
    << indent()
    << "pub fn read_from_in_protocol(i_prot: &mut TProtocol) -> rift::Result<" << tenum->get_name() << "> {"
    << endl;
  indent_up();

  f_gen_ << indent() << "let enum_value = try!(i_prot.read_i32());" << endl;
  f_gen_ << indent() << tenum->get_name() << "::try_from(enum_value)";

  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_enum_conversion(t_enum* tenum) {
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator constants_iter;

  f_gen_ << "impl TryFrom<i32> for " << tenum->get_name() << " {" << endl;
  indent_up();

  f_gen_ << indent() << "type Err = rift::Error;";

  f_gen_ << indent() << "fn try_from(i: i32) -> Result<Self, Self::Err> {" << endl;
  indent_up();

  f_gen_ << indent() << "match i {" << endl;
  indent_up();
  for (constants_iter = constants.begin(); constants_iter != constants.end(); ++constants_iter) {
    f_gen_
      << indent()
      << (*constants_iter)->get_value()
      << " => Ok(" << tenum->get_name() << "::" << (*constants_iter)->get_name() << "),"
      << endl;
  }
  f_gen_ << indent() << "_ => {" << endl;
  indent_up();
  render_rift_error(
    "Protocol",
    "ProtocolError",
    "ProtocolErrorKind::InvalidData",
    "format!(\"cannot convert enum constant {} to " + tenum->get_name() + "\", i)"
  );
  indent_down();
  f_gen_ << indent() << "}," << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

//-----------------------------------------------------------------------------
//
// Structs, Unions and Exceptions
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_xception(t_struct* txception) {
  render_struct(rust_struct_name(txception), txception, t_rs_generator::T_EXCEPTION);
}

void t_rs_generator::generate_struct(t_struct* tstruct) {
  if (tstruct->is_union()) {
    render_union(tstruct);
  } else if (tstruct->is_struct()) {
    render_struct(rust_struct_name(tstruct), tstruct, t_rs_generator::T_REGULAR);
  } else {
    throw "cannot generate struct for exception";
  }
}

void t_rs_generator::render_struct(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  render_type_comment(struct_name);
  render_struct_definition(struct_name, tstruct, struct_type);
  render_struct_impl(struct_name, tstruct, struct_type);
  if (struct_type == t_rs_generator::T_EXCEPTION) {
    render_exception_struct_error_trait_impls(struct_name, tstruct);
  }
}

void t_rs_generator::render_struct_definition(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_ << "#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]" << endl;
  f_gen_ << visibility_qualifier(struct_type) << "struct " << struct_name << " {" << endl;

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
      f_gen_ << indent() << visibility_qualifier(struct_type) << rust_snake_case(tfield->get_name()) << ": " << rust_type << "," << endl;
    }

    indent_down();
  }

  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_exception_struct_error_trait_impls(const string& struct_name, t_struct* tstruct) {
  // error::Error trait
  f_gen_ << "impl Error for " << struct_name << " {" << endl;
  indent_up();
  f_gen_ << indent() << "fn description(&self) -> &str {" << endl;
  indent_up();
  f_gen_ << indent() << "\"" << "remote service threw " << tstruct->get_name() << "\"" << endl; // use original name
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // convert::From trait
  f_gen_ << "impl From<" << struct_name << "> for rift::Error {" << endl;
  indent_up();
  f_gen_ << indent() << "fn from(e: " << struct_name << ") -> Self {" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::User(Box::new(e))" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;

  // fmt::Display trait
  f_gen_ << "impl Display for " << struct_name << " {" << endl;
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

void t_rs_generator::render_struct_impl(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_ << "impl " << struct_name << " {" << endl;
  indent_up();

  render_struct_read_from_in_protocol(struct_name, tstruct, struct_type);
  render_struct_write_to_out_protocol(tstruct, struct_type);
  if (struct_type == t_rs_generator::T_RESULT) {
    render_result_struct_to_result_method(tstruct);
  }

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_result_struct_to_result_method(t_struct* tstruct) {
  // we don't use the rust struct name in this method, just the service call name
  string service_call_name = tstruct->get_name();

  // check that we actually have a result
  size_t index = service_call_name.find(RESULT_STRUCT_SUFFIX, 0);
  if (index == std::string::npos) {
    throw "result struct " + service_call_name + " missing result suffix";
  } else {
     service_call_name.replace(index, 6, "");
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
  // maintaining a rendered branch count (while a little ugly) got me the
  // rendering I wanted with code that was reasonably understandable

  f_gen_ << indent() << "fn ok_or(self) -> rift::Result<" << rust_return_type << "> {" << endl;
  indent_up();

  int rendered_branch_count = 0;

  // render the exception branches
  for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    if (tfield->get_name() != SERVICE_CALL_RESULT_VARIABLE) {
      string field_name = "self." + rust_snake_case(tfield->get_name());
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
    render_rift_error(
      "Application",
      "ApplicationError",
      "ApplicationErrorKind::MissingResult",
      "\"no result received for " + service_call_name + "\".to_owned()"
    );
    indent_down();
    f_gen_ << indent() << "}" << endl;
  }

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_union(t_struct* tstruct) {
  string union_name(rust_camel_case(tstruct->get_name()));
  render_type_comment(union_name);
  render_union_definition(union_name, tstruct);
  render_union_impl(union_name, tstruct);
}

void t_rs_generator::render_union_definition(const string& union_name, t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_sorted_members();
  if (members.size() == 0) {
    throw "cannot generate rust enum with 0 members"; // this may be valid thrift, but it's not valid rust
  }

  f_gen_ << "#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]" << endl;
  f_gen_ << "pub enum " << union_name << " {" << endl;
  indent_up();

  vector<t_field*>::const_iterator member_iter;
  for(member_iter = members.begin(); member_iter != members.end(); ++member_iter) {
    t_field* field = (*member_iter);
    f_gen_
      << indent()
      << rust_camel_case(field->get_name())
      << "(" << to_rust_type(field->get_type()) << "),"
      << endl;
  }

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_union_impl(const string& union_name, t_struct* tstruct) {
  f_gen_ << "impl " << union_name << " {" << endl;
  indent_up();

  render_union_read_from_in_protocol(union_name, tstruct);
  render_union_write_to_out_protocol(union_name, tstruct);

  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

//-----------------------------------------------------------------------------
//
// Sync Write
//
//-----------------------------------------------------------------------------

void t_rs_generator::render_struct_write_to_out_protocol(t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_
    << indent()
    << visibility_qualifier(struct_type)
    << "fn write_to_out_protocol(&self, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();

  // write struct header to output protocol
  // note: use the *original* struct name here
  f_gen_ << indent() << "let struct_ident = TStructIdentifier { name: \"" + tstruct->get_name() + "\".to_owned() };" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_begin(&struct_ident));" << endl;

  // write struct members to output protocol
  vector<t_field*> members = tstruct->get_sorted_members();
  if (!members.empty()) {
    vector<t_field*>::iterator members_iter;
    for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
      string field_var("self." + rust_snake_case(tfield->get_name()));
      render_struct_field_write(field_var, tfield, req);
    }
  }

  // write struct footer to output protocol
  f_gen_ << indent() << "try!(o_prot.write_field_stop());" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_end());" << endl;
  f_gen_ << indent() << "o_prot.flush()" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

// FIXME: consolidate this method with above
void t_rs_generator::render_union_write_to_out_protocol(const string& union_name, t_struct* tstruct) {
  f_gen_
    << indent()
    << "pub fn write_to_out_protocol(&self, o_prot: &mut TProtocol) -> rift::Result<()> {"
    << endl;
  indent_up();

  // write struct header to output protocol
  // note: use the *original* struct name here
  f_gen_ << indent() << "let struct_ident = TStructIdentifier { name: \"" + tstruct->get_name() + "\".to_owned() };" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_begin(&struct_ident));" << endl;

  // write the enum field to the output protocol
  vector<t_field*> members = tstruct->get_sorted_members();
  if (!members.empty()) {
    f_gen_ << indent() << "match *self {" << endl;
    indent_up();
    vector<t_field*>::iterator members_iter;
    for(members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = t_field::T_REQUIRED;
      t_type* ttype = tfield->get_type();
      string match_var((ttype->is_base_type() && !ttype->is_string()) ? "f" : "ref f");
      f_gen_ << indent() << union_name << "::" << rust_camel_case(tfield->get_name()) << "(" << match_var << ") => {" << endl;
      indent_up();
      render_struct_field_write("f", tfield, req);
      indent_down();
      f_gen_ << indent() << "}," << endl;
    }
    indent_down();
    f_gen_ << indent() << "}" << endl;
  }

  // write struct footer to output protocol
  f_gen_ << indent() << "try!(o_prot.write_field_stop());" << endl;
  f_gen_ << indent() << "try!(o_prot.write_struct_end());" << endl;
  f_gen_ << indent() << "o_prot.flush()" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_struct_field_write(const string& field_var, t_field* tfield, t_field::e_req req) {
  t_type* field_type = tfield->get_type();
  t_type* actual_type = get_true_type(field_type);

  ostringstream field_stream;
  field_stream
    << "TFieldIdentifier {"
    << "name: Some(\"" << tfield->get_name() << "\".to_owned()" << "), " // note: use *original* name
    << "field_type: " << to_rust_field_type_enum(field_type) << ", "
    << "id: Some(" << tfield->get_key() << ") "
    << "}";
  string field_ident_string = field_stream.str();

  if (is_optional(req)) {
    string let_var((actual_type->is_base_type() && !actual_type->is_string()) ? "fld_var" : "ref fld_var");
    f_gen_ << indent() << "if let Some(" << let_var << ") = " << field_var << " {" << endl;
    indent_up();
    f_gen_ << indent() << "try!(o_prot.write_field_begin(&" << field_ident_string << "));" << endl;
    render_type_write("fld_var", field_type);
    f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
    f_gen_ << indent() << "()" << endl; // FIXME: remove this extraneous '()'
    indent_down();
    f_gen_ << indent() << "} else {" << endl; // FIXME: remove else branch
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
    render_type_write(field_var, tfield->get_type());
    f_gen_ << indent() << "try!(o_prot.write_field_end());" << endl;
  }
}

void t_rs_generator::render_type_write(const string& type_var, t_type* ttype) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      throw "cannot write field of type TYPE_VOID to output protocol";
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        f_gen_ << indent() << "try!(o_prot.write_bytes(&" + type_var + "));" << endl;
      } else {
        f_gen_ << indent() << "try!(o_prot.write_string(&" + type_var + "));" << endl;
      }
      return;
    case t_base_type::TYPE_BOOL:
      f_gen_ << indent() << "try!(o_prot.write_bool(" + type_var + "));" << endl;
      return;
    case t_base_type::TYPE_I8:
      f_gen_ << indent() << "try!(o_prot.write_i8(" + type_var + "));" << endl;
      return;
    case t_base_type::TYPE_I16:
      f_gen_ << indent() << "try!(o_prot.write_i16(" + type_var + "));" << endl;
      return;
    case t_base_type::TYPE_I32:
      f_gen_ << indent() << "try!(o_prot.write_i32(" + type_var + "));" << endl;
      return;
    case t_base_type::TYPE_I64:
      f_gen_ << indent() << "try!(o_prot.write_i64(" + type_var + "));" << endl;
      return;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << indent() << "try!(o_prot.write_double(" + type_var + "));" << endl;
      return;
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_type_write(type_var, ttypedef->get_type());
    return;
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << indent() << "try!(" +  type_var + ".write_to_out_protocol(o_prot));" << endl;
    return;
  } else if (ttype->is_map()) {
    render_map_write(type_var, (t_map*)ttype);
    return;
  } else if (ttype->is_set()) {
    render_set_write(type_var, (t_set*)ttype);
    return;
  } else if (ttype->is_list()) {
    render_list_write(type_var, (t_list*)ttype);
    return;
  }

  throw "cannot write unsupported type " + ttype->get_name();
}

void t_rs_generator::render_list_write(const string& list_variable, t_list* tlist) {
  t_type* elem_type = tlist->get_elem_type();

  f_gen_
    << indent()
    << "try!(o_prot.write_list_begin("
    << "&TListIdentifier {"
    << " element_type: " << to_rust_field_type_enum(elem_type)
    << ", size: " << list_variable << ".len() as i32"
    << " }"
    << "));" << endl;

  f_gen_ << indent() << "for e in " << list_variable << ".iter() {" << endl;
  indent_up();
  render_type_write(needs_deref_on_container_write(elem_type) ? "*e" : "e", elem_type);
  f_gen_ << indent() << "try!(o_prot.write_list_end());" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_set_write(const string& set_variable, t_set* tset) {
  t_type* elem_type = tset->get_elem_type();

  f_gen_
    << indent()
    << "try!(o_prot.write_set_begin("
    << "&TSetIdentifier {"
    << " element_type: " << to_rust_field_type_enum(elem_type)
    << ", size: " << set_variable << ".len() as i32"
    << " }"
    << "));" << endl;

  f_gen_ << indent() << "for e in " << set_variable << ".iter() {" << endl;
  indent_up();
  render_type_write(needs_deref_on_container_write(elem_type) ? "*e" : "e", elem_type);
  f_gen_ << indent() << "try!(o_prot.write_set_end());" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_map_write(const string& map_variable, t_map* tmap) {
  t_type* key_type = tmap->get_key_type();
  t_type* val_type = tmap->get_val_type();

  f_gen_
    << indent()
    << "try!(o_prot.write_map_begin("
    << "&TMapIdentifier {"
    << " key_type: " << to_rust_field_type_enum(key_type)
    << ", value_type: " << to_rust_field_type_enum(val_type)
    << ", size: " << map_variable << ".len() as i32"
    << " }"
    << "));" << endl;

  f_gen_ << indent() << "for (k, v) in " << map_variable << ".iter() {" << endl;
  indent_up();
  render_type_write(needs_deref_on_container_write(key_type) ? "*k" : "k", key_type);
  render_type_write(needs_deref_on_container_write(val_type) ? "*v" : "v", val_type);
  f_gen_ << indent() << "try!(o_prot.write_map_end());" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

bool t_rs_generator::needs_deref_on_container_write(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_base_type() && !ttype->is_string()) {
    return true;
  } else {
    return false;
  }
}

//-----------------------------------------------------------------------------
//
// Sync Read
//
//-----------------------------------------------------------------------------

void t_rs_generator::render_struct_read_from_in_protocol(const string& struct_name, t_struct* tstruct, t_rs_generator::e_struct_type struct_type) {
  f_gen_
    << indent()
    << visibility_qualifier(struct_type)
    << "fn read_from_in_protocol(i_prot: &mut TProtocol) -> rift::Result<" << struct_name << "> {"
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
      << "let mut " << struct_field_read_temp_variable(tfield)
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
    render_type_read("val", tfield->get_type());
    f_gen_ << indent() << struct_field_read_temp_variable(tfield) << " = Some(val);" << endl;
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
  f_gen_ << indent() << "try!(i_prot.read_struct_end());" << endl; // read message footer from the wire

  // verify that all required fields exist
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
    if (!is_optional(req)) {
      f_gen_
        << indent()
        << "try!(verify_required_field_exists("
        << "\"" << struct_name << "." << rust_snake_case(tfield->get_name()) << "\""
        << ", "
        << "&" << struct_field_read_temp_variable(tfield)
        << "));" << endl;
    }
  }

  // construct the struct
  if (members.size() == 0) {
    f_gen_ << indent() << "let ret = " << struct_name << " {};" << endl;
  } else {
    f_gen_ << indent() << "let ret = " << struct_name << " {" << endl;
    indent_up();

    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = struct_type == t_rs_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
      string field_name = rust_snake_case(tfield->get_name());
      string field_key = struct_field_read_temp_variable(tfield);
      if (is_optional(req)) {
        f_gen_ << indent() << field_name << ": " << field_key << "," << endl;
      } else {
        f_gen_
          << indent()
          << field_name
          << ": "
          << field_key
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

void t_rs_generator::render_union_read_from_in_protocol(const string& union_name, t_struct* tstruct) {
  f_gen_
    << indent()
    << "pub fn read_from_in_protocol(i_prot: &mut TProtocol) -> rift::Result<" << union_name << "> {"
    << endl;
  indent_up();

  // create temporary variables to hold the
  // completed union as well as a count of fields read
  f_gen_ << indent() << "let mut ret: Option<" << union_name << "> = None;" << endl;
  f_gen_ << indent() << "let mut received_field_count = 0;" << endl;

  // read the struct preamble
  f_gen_ << indent() << "try!(i_prot.read_struct_begin());" << endl;

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

  const vector<t_field*> members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    f_gen_ << indent() << tfield->get_key() << " => {" << endl;
    indent_up();
    render_type_read("val", tfield->get_type());
    f_gen_ << indent() << "if let None = ret {" << endl;
    indent_up();
    f_gen_ << indent() << "ret = Some(" << union_name << "::" << rust_camel_case(tfield->get_name()) << "(val));" << endl;
    indent_down();
    f_gen_ << indent() << "}" << endl;
    f_gen_ << indent() << "received_field_count += 1;" << endl;
    indent_down();
    f_gen_ << indent() << "}," << endl;
  }

  // default case (skip fields)
  f_gen_ << indent() << "_ => {" << endl;
  indent_up();
  f_gen_ << indent() << "try!(i_prot.skip(field_ident.field_type));" << endl;
  f_gen_ << indent() << "received_field_count += 1;" << endl;
  indent_down();
  f_gen_ << indent() << "}," << endl;

  indent_down();
  f_gen_ << indent() << "};" << endl; // finish match
  f_gen_ << indent() << "try!(i_prot.read_field_end());" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl; // finish loop
  f_gen_ << indent() << "try!(i_prot.read_struct_end());" << endl; // finish reading message from wire

  // return the value or an error
  f_gen_ << indent() << "if received_field_count > 1 {" << endl;
  indent_up();
  render_rift_error(
    "Protocol",
    "ProtocolError",
    "ProtocolErrorKind::InvalidData",
    "\"received multiple fields from remote for union " + union_name + "\".to_owned()"
  );
  indent_down();
  f_gen_ << indent() << "} else {" << endl;
  indent_up();
  f_gen_ << indent() << "Ok(ret.expect(\"return value should have been constructed\"))" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

// Construct the rust representation of all supported types from the wire.
void t_rs_generator::render_type_read(const string& type_var, t_type* ttype) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      throw "cannot read field of type TYPE_VOID from input protocol";
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_bytes());" << endl;
      } else {
        f_gen_ << indent() << "let " << type_var << " = try!(i_prot.read_string());" << endl;
      }
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
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_type_read(type_var, ttypedef->get_type());
    return;
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << indent() << "let " << type_var << " = try!(" <<  to_rust_type(ttype) << "::read_from_in_protocol(i_prot));" << endl;
    return;
  } else if (ttype->is_map()) {
    render_map_read((t_map*) ttype, type_var);
    return;
  } else if (ttype->is_set()) {
    render_set_read((t_set*) ttype, type_var);
    return;
  } else if (ttype->is_list()) {
    render_list_read((t_list*) ttype, type_var);
    return;
  }

  throw "cannot read unsupported type " + ttype->get_name();
}

// Construct the rust representation of a list from the wire.
void t_rs_generator::render_list_read(t_list* tlist, const string& list_var) {
  t_type* elem_type = tlist->get_elem_type();

  f_gen_ << indent() << "let list_ident = try!(i_prot.read_list_begin());" << endl;
  f_gen_ << indent() << "let mut " << list_var << ": " << to_rust_type((t_type*) tlist) << " = Vec::with_capacity(list_ident.size as usize);" << endl;
  f_gen_ << indent() << "for _ in 0..list_ident.size {" << endl;

  indent_up();

  string list_elem_var = tmp("list_elem_");
  render_type_read(list_elem_var, elem_type);
  f_gen_ << indent() << list_var << ".push(" << list_elem_var << ");" << endl;

  indent_down();

  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(i_prot.read_list_end());" << endl;
}

// Construct the rust representation of a set from the wire.
void t_rs_generator::render_set_read(t_set* tset, const string& set_var) {
  t_type* elem_type = tset->get_elem_type();

  f_gen_ << indent() << "let set_ident = try!(i_prot.read_set_begin());" << endl;
  f_gen_ << indent() << "let mut " << set_var << ": " << to_rust_type((t_type*) tset) << " = BTreeSet::new();" << endl;
  f_gen_ << indent() << "for _ in 0..set_ident.size {" << endl;

  indent_up();

  string set_elem_var = tmp("set_elem_");
  render_type_read(set_elem_var, elem_type);
  f_gen_ << indent() << set_var << ".insert(" << set_elem_var << ");" << endl;

  indent_down();

  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(i_prot.read_set_end());" << endl;
}

// Construct the rust representation of a map from the wire.
void t_rs_generator::render_map_read(t_map* tmap, const string& map_var) {
  t_type* key_type = tmap->get_key_type();
  t_type* val_type = tmap->get_val_type();

  f_gen_ << indent() << "let map_ident = try!(i_prot.read_map_begin());" << endl;
  f_gen_ << indent() << "let mut " << map_var << ": " << to_rust_type((t_type*) tmap) << " = BTreeMap::new();" << endl;
  f_gen_ << indent() << "for _ in 0..map_ident.size {" << endl;

  indent_up();

  string key_elem_var = tmp("map_key_");
  render_type_read(key_elem_var, key_type);
  string val_elem_var = tmp("map_val_");
  render_type_read(val_elem_var, val_type);
  f_gen_ << indent() << map_var << ".insert(" << key_elem_var << ", " << val_elem_var << ");" << endl;

  indent_down();

  f_gen_ << indent() << "}" << endl;
  f_gen_ << indent() << "try!(i_prot.read_map_end());" << endl;
}

string t_rs_generator::struct_field_read_temp_variable(t_field* tfield) {
  std::ostringstream foss;
  foss << "f_" << tfield->get_key();
  return foss.str();
}

//-----------------------------------------------------------------------------
//
// Sync Client
//
//-----------------------------------------------------------------------------

void t_rs_generator::generate_service(t_service* tservice) {
  render_sync_client(tservice);
  render_sync_server(tservice);
  render_service_call_structs(tservice);
}

void t_rs_generator::render_service_call_structs(t_service* tservice) {
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
    render_struct(rust_struct_name(tfunc->get_arglist()), tfunc->get_arglist(), t_rs_generator::T_ARGS);
    if (!tfunc->is_oneway()) {
      render_result_value_struct(tfunc);
    }
  }
}

void t_rs_generator::render_sync_client(t_service* tservice) {
  // service comment demarcation
  f_gen_ << "//" << endl;
  f_gen_ << "// " << tservice->get_name() << " service client "<< endl; // note: use *original* name
  f_gen_ << "//" << endl;
  f_gen_ << endl;

  // render the trait through which the service calls will be mad
  render_service_sync_client_trait(tservice);

  string client_impl_struct_name = rust_sync_client_impl_name(tservice);

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
  render_sync_client_lifecycle_functions(client_impl_struct_name);
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    render_sync_send(tfunc);
    if (!tfunc->is_oneway()) {
      render_sync_recv(tfunc);
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
    render_sync_send_recv_wrapper(func);
  }
  indent_down();
  f_gen_ << "}" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_service_sync_client_trait(t_service* tservice) {
  string extension = "";
  if (tservice->get_extends() != NULL) {
    t_service* extends = tservice->get_extends();
    extension = " : " + rust_namespace(extends) + rust_sync_client_trait_name(extends);
  }

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  f_gen_ << "pub trait " << rust_sync_client_trait_name(tservice) << extension << " {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    string func_name = service_call_client_function_name(tfunc);
    string func_args = rust_sync_service_call_args(tfunc, true);
    string func_return = to_rust_type(tfunc->get_returntype());
    f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<" << func_return << ">;" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}" << endl;

  f_gen_ << endl;
}

void t_rs_generator::render_sync_client_lifecycle_functions(const string& client_struct) {
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

void t_rs_generator::render_result_value_struct(t_function* tfunc) {
  string result_struct_name = service_call_result_struct_name(tfunc);
  t_struct result(program_, result_struct_name);

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

  render_struct(result_struct_name, &result, t_rs_generator::T_RESULT);
}

void t_rs_generator::render_sync_send_recv_wrapper(t_function* tfunc) {
  string func_name = service_call_client_function_name(tfunc);
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

void t_rs_generator::render_sync_send(t_function* tfunc) {
  string func_name = service_call_sync_send_client_function_name(tfunc);
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
    << "TMessageIdentifier { name:\"" << tfunc->get_name() << "\".to_owned(), " // note: use *original* name
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
    string field_name(rust_snake_case(tfield->get_name()));
    struct_definition << field_name << ": " << field_name << ", ";
  }
  string struct_fields = struct_definition.str();
  if (struct_fields.size() > 0) {
    struct_fields = struct_fields.substr(0, struct_fields.size() - 2); // strip trailing comma
  }
  f_gen_
    << indent()
    << "let call_args = "
    << rust_struct_name(tfunc->get_arglist())
    << " { "
    << struct_fields
    << " };"
    << endl;
  // write everything over the wire
  f_gen_ << indent() << "try!(self.o_prot.borrow_mut().write_message_begin(&message_ident));" << endl;
  f_gen_ << indent() << "try!(call_args.write_to_out_protocol(&mut **self.o_prot.borrow_mut()));" << endl; // written even if we have 0 args
  f_gen_ << indent() << "try!(self.o_prot.borrow_mut().write_message_end());" << endl;
  f_gen_ << indent() << "self.o_prot.borrow_mut().flush()" << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_sync_recv(t_function* tfunc) {
  string func_name = service_call_sync_recv_client_function_name(tfunc);
  string func_return = to_rust_type(tfunc->get_returntype());
  f_gen_ << indent() << "fn " << func_name << "(&mut self) -> rift::Result<" << func_return << "> {" << endl;
  indent_up();
  f_gen_ << indent() << "let message_ident = try!(self.i_prot.borrow_mut().read_message_begin());" << endl;
  f_gen_ << indent() << "try!(verify_expected_sequence_number(self.sequence_number, message_ident.sequence_number));" << endl;
  f_gen_ << indent() << "try!(verify_expected_service_call(\"" << tfunc->get_name() <<"\", &message_ident.name));" << endl; // note: use *original* name
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
      func_args << prefix << rust_snake_case(tfield->get_name()) << (is_declaration ? ": " + rust_type : "");
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

void t_rs_generator::render_sync_server(t_service* tservice) {
  render_service_server_comment(tservice);
  render_service_sync_handler_trait(tservice);
  render_service_processor(tservice);
}

void t_rs_generator::render_service_server_comment(t_service* tservice) {
  f_gen_ << "//" << endl;
  f_gen_ << "// " << tservice->get_name() << " service server"<< endl; // note: use *original* name
  f_gen_ << "//" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_service_sync_handler_trait(t_service* tservice) {
  string extension = "";
  if (tservice->get_extends() != NULL) {
    t_service* extends = tservice->get_extends();
    extension = " : " + rust_namespace(extends) + rust_sync_handler_trait_name(extends);
  }

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;

  f_gen_ << "pub trait " << rust_sync_handler_trait_name(tservice) << extension << " {" << endl;
  indent_up();
  for(func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    string func_name = service_call_handler_function_name(tfunc);
    string func_args = rust_sync_service_call_args(tfunc, true);
    string func_return = to_rust_type(tfunc->get_returntype());
    f_gen_ << indent() << "fn " << func_name <<  func_args << " -> rift::Result<" << func_return << ">;" << endl;
  }
  indent_down();
  f_gen_ << indent() << "}" << endl;

  f_gen_ << endl;
}

void t_rs_generator::render_service_processor(t_service* tservice) {
  string service_processor_name = rust_sync_processor_name(tservice);
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
    render_service_process_function(tfunc);
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
    f_gen_ << indent() << "\"" << tfunc->get_name() << "\"" << " => {" << endl; // note: use *original* name
    indent_up();
    f_gen_ << indent() << "self.process_" << rust_snake_case(tfunc->get_name()) << "(message_ident.sequence_number, i_prot, o_prot)" << endl;
    indent_down();
    f_gen_ << indent() << "}," << endl;
  }
  f_gen_ << indent() << "method => {" << endl;
  indent_up();
  render_rift_error("Application", "ApplicationError", "ApplicationErrorKind::UnknownMethod", "format!(\"unknown method {}\", method)");
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

void t_rs_generator::render_service_process_function(t_function* tfunc) {
  string sequence_number_param("incoming_sequence_number");
  string output_protocol_param("o_prot");

  if (tfunc->is_oneway()) {
    sequence_number_param = "_";
    output_protocol_param = "_";
  }

  f_gen_
    << indent()
    << "fn process_" << rust_snake_case(tfunc->get_name())
    << "(&mut self, "
    << sequence_number_param << ": i32, "
    << "i_prot: &mut TProtocol, "
    << output_protocol_param << ": &mut TProtocol) "
    << "-> rift::Result<()> {"
    << endl;

  indent_up();

  // *always* read arguments from the input protocol
  f_gen_
    << indent()
    << "let "
    << (has_non_void_args(tfunc) ? "args" : "_")
    << " = try!("
    << rust_struct_name(tfunc->get_arglist())
    << "::read_from_in_protocol(i_prot));"
    << endl;

  f_gen_
    << indent()
    << "match self.handler."
    << service_call_handler_function_name(tfunc)
    << rust_sync_service_call_args(tfunc, false, "args.")
    << " {"
    << endl; // start match
  indent_up();

  // handler succeeded
  string handler_return_variable = tfunc->is_oneway() || tfunc->get_returntype()->is_void() ? "_" : "handler_return";
  f_gen_ << indent() << "Ok(" << handler_return_variable << ") => {" << endl;
  indent_up();
  render_handler_succeeded(tfunc);
  indent_down();
  f_gen_ << indent() << "}," << endl;
  // handler failed
  f_gen_ << indent() << "Err(e) => {" << endl;
  indent_up();
  render_handler_failed(tfunc);
  indent_down();
  f_gen_ << indent() << "}," << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl; // end match

  indent_down();
  f_gen_ << indent() << "}" << endl; // end function
}

void t_rs_generator::render_handler_succeeded(t_function* tfunc) {
  if (tfunc->is_oneway()) {
    f_gen_ << indent() << "Ok(())" << endl;
  } else {
    f_gen_
      << indent()
      << "let message_ident = TMessageIdentifier { "
      << "name: \"" << tfunc->get_name() << "\".to_owned(), " // note: use *original* name
      << "message_type: TMessageType::Reply, "
      << "sequence_number: incoming_sequence_number };"
      << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_begin(&message_ident));" << endl;
    f_gen_ << indent() << "let ret = " << handler_successful_return_struct(tfunc) <<";" << endl;
    f_gen_ << indent() << "try!(ret.write_to_out_protocol(o_prot));" << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_end());" << endl;
    f_gen_ << indent() << "o_prot.flush()" << endl;
  }
}

void t_rs_generator::render_handler_failed(t_function* tfunc) {
  f_gen_ << indent() << "match e {" << endl;
  indent_up();

  // if there are any user-defined exceptions for this service call handle them first
  if (tfunc->get_xceptions() != NULL && tfunc->get_xceptions()->get_sorted_members().size() > 0) {
    string user_err_var("usr_err");
    f_gen_ << indent() << "rift::Error::User(" << user_err_var << ") => {" << endl;
    indent_up();
    render_handler_failed_user_exception_branch(tfunc);
    indent_down();
    f_gen_ << indent() << "}," << endl;
  }

  // default case
  f_gen_ << indent() << "_ => {" << endl;
  indent_up();
  render_handler_failed_default_exception_branch(tfunc);
  indent_down();
  f_gen_ << indent() << "}," << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_handler_failed_user_exception_branch(t_function* tfunc) {
  if (tfunc->get_xceptions() == NULL || tfunc->get_xceptions()->get_sorted_members().size() == 0) {
    throw "cannot render user exception branches if no user exceptions defined";
  }

  const vector<t_field*> txceptions = tfunc->get_xceptions()->get_sorted_members();
  vector<t_field*>::const_iterator xception_iter;
  int branches_rendered = 0;

  // run through all user-defined exceptions
  for (xception_iter = txceptions.begin(); xception_iter != txceptions.end(); ++xception_iter) {
    t_field* xception_field = (*xception_iter);

    string if_statement(branches_rendered == 0 ? "if usr_err" : "} else if usr_err");
    string exception_type(to_rust_type(xception_field->get_type()));
    f_gen_ << indent() << if_statement << ".downcast_ref::<" << exception_type << ">().is_some() {" << endl;
    indent_up();

    f_gen_
      << indent()
      << "let err = usr_err.downcast::<" << exception_type << ">().expect(\"downcast already checked\");"
      << endl;

    // render the members of the return struct
    ostringstream members;

    bool has_result_variable = !(tfunc->is_oneway() || tfunc->get_returntype()->is_void());
    if (has_result_variable) {
      members << SERVICE_CALL_RESULT_VARIABLE << ": None, ";
    }

    vector<t_field*>::const_iterator xception_members_iter;
    for(xception_members_iter = txceptions.begin(); xception_members_iter != txceptions.end(); ++xception_members_iter) {
      t_field* member = (*xception_members_iter);
      string member_name(rust_snake_case(member->get_name()));
      if (member == xception_field) {
        members << member_name << ": Some(*err), ";
      } else {
        members << member_name << ": None, ";
      }
    }

    string member_string = members.str();
    member_string.replace(member_string.size() - 2, 2, " "); // trim trailing comma

    // now write out the return struct
    f_gen_ << indent() << "let ret_err = " << service_call_result_struct_name(tfunc) << "{ " << member_string << "};" << endl;

    f_gen_
      << indent()
      << "let message_ident = "
      << "TMessageIdentifier { "
      << "name: \"" << tfunc->get_name() << "\".to_owned(), " // note: use *original* name
      << "message_type: TMessageType::Reply, "
      << "sequence_number: incoming_sequence_number };"
      << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_begin(&message_ident));" << endl;
    f_gen_ << indent() << "try!(ret_err.write_to_out_protocol(o_prot));" << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_end());" << endl;
    f_gen_ << indent() << "o_prot.flush()" << endl;

    indent_down();

    branches_rendered++;
  }

  // the catch all, if somehow it was a user exception that we don't support
  f_gen_ << indent() << "} else {" << endl;
  indent_up();

  // FIXME: same as default block below

  f_gen_ << indent() << "let ret_err = {" << endl;
  indent_up();
  render_rift_error_struct("ApplicationError", "ApplicationErrorKind::Unknown", "usr_err.description().to_owned()");
  indent_down();
  f_gen_ << indent() << "};" << endl;

  f_gen_
      << indent()
      << "let message_ident = TMessageIdentifier { "
      << "name: \"" << tfunc->get_name() << "\".to_owned(), " // note: use *original* name
      << "message_type: TMessageType::Exception, "
      << "sequence_number: incoming_sequence_number };"
      << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_begin(&message_ident));" << endl;
    f_gen_ << indent() << "try!(rift::Error::write_application_error_to_out_protocol(&ret_err, o_prot));" << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_end());" << endl;
    f_gen_ << indent() << "o_prot.flush()" << endl;

  indent_down();
  f_gen_ << indent() << "}" << endl;
}

void t_rs_generator::render_handler_failed_default_exception_branch(t_function* tfunc) {
  f_gen_ << indent() << "let ret_err = {" << endl;
  indent_up();
  render_rift_error_struct("ApplicationError", "ApplicationErrorKind::Unknown", "e.description().to_owned()");
  indent_down();
  f_gen_ << indent() << "};" << endl;
  if (tfunc->is_oneway()) {
    f_gen_ << indent() << "Err(rift::Error::Application(ret_err))" << endl;
  } else {
    f_gen_
      << indent()
      << "let message_ident = TMessageIdentifier { "
      << "name: \"" << tfunc->get_name() << "\".to_owned(), " // note: use *original* name
      << "message_type: TMessageType::Exception, "
      << "sequence_number: incoming_sequence_number };"
      << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_begin(&message_ident));" << endl;
    f_gen_ << indent() << "try!(rift::Error::write_application_error_to_out_protocol(&ret_err, o_prot));" << endl;
    f_gen_ << indent() << "try!(o_prot.write_message_end());" << endl;
    f_gen_ << indent() << "o_prot.flush()" << endl;
   }
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
      t_field* xception_field = (*members_iter);
      if (member_count > 0) { return_struct << ", "; }
      return_struct << rust_snake_case(xception_field->get_name()) << ": None";
      member_count++;
    }
  }

  return_struct << " }";

  return  return_struct.str();
}

//-----------------------------------------------------------------------------
//
// Utility
//
//-----------------------------------------------------------------------------

void t_rs_generator::render_type_comment(const string& type_name) {
  f_gen_ << "//" << endl;
  f_gen_ << "// " << type_name << endl;
  f_gen_ << "//" << endl;
  f_gen_ << endl;
}

void t_rs_generator::render_rift_error(const string& error_kind, const string& error_struct, const string& sub_error_kind, const string& error_message) {
  f_gen_ << indent() << "Err(" << endl;
  indent_up();
  f_gen_ << indent() << "rift::Error::" << error_kind << "(" << endl;
  indent_up();
  render_rift_error_struct(error_struct, sub_error_kind, error_message);
  indent_down();
  f_gen_ << indent() << ")" << endl;
  indent_down();
  f_gen_ << indent() << ")" << endl;
}

void t_rs_generator::render_rift_error_struct(const string& error_struct, const string& sub_error_kind, const string& error_message) {
  f_gen_ << indent() << error_struct << " {" << endl;
  indent_up();
  f_gen_ << indent() << "kind: " << sub_error_kind << "," << endl;
  f_gen_ << indent() << "message: " << error_message << "," << endl;
  indent_down();
  f_gen_ << indent() << "}" << endl;
}

string t_rs_generator::to_rust_type(t_type* ttype) {
  // ttype = get_true_type(ttype); <-- recurses through as many typedef layers as necessary
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = ((t_base_type*)ttype);
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      return "()";
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        return "Vec<u8>";
      } else {
        return "String";
      }
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
  } else if (ttype->is_typedef()) {
    return rust_namespace(ttype) + ((t_typedef*)ttype)->get_symbolic();
  } else if (ttype->is_enum()) {
    return rust_namespace(ttype) + ttype->get_name();
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return rust_namespace(ttype) + rust_camel_case(ttype->get_name());
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
    case t_base_type::TYPE_STRING: // both strings and binary are actually encoded as TType::String
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

bool t_rs_generator::has_non_void_args(t_function* tfunc) {
  bool has_non_void_args = false;

  const vector<t_field*> args = tfunc->get_arglist()->get_sorted_members();
  vector<t_field*>::const_iterator args_iter;
  for (args_iter = args.begin(); args_iter != args.end(); ++args_iter) {
    t_field* tfield = (*args_iter);
    if (!tfield->get_type()->is_void()) {
      has_non_void_args = true;
      break;
    }
  }

  return has_non_void_args;
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

string t_rs_generator::rust_namespace(t_type* ttype) {
  if (ttype->get_program()->get_name() != get_program()->get_name()) {
    return rust_snake_case(ttype->get_program()->get_name()) + "::";
  } else {
    return "";
  }
}

string t_rs_generator::rust_namespace(t_service* tservice) {
  if (tservice->get_program()->get_name() != get_program()->get_name()) {
    return rust_snake_case(tservice->get_program()->get_name()) + "::";
  } else {
    return "";
  }
}

string t_rs_generator::rust_struct_name(t_struct* tstruct) {
  return rust_camel_case(tstruct->get_name());
}

string t_rs_generator::rust_sync_client_trait_name(t_service* tservice) {
  return "TAbstract" + rust_camel_case(tservice->get_name()) + "SyncClient";
}

string t_rs_generator::rust_sync_client_impl_name(t_service* tservice) {
  return "T" + rust_camel_case(tservice->get_name()) + "SyncClient";
}

string t_rs_generator::rust_sync_handler_trait_name(t_service* tservice) {
  return "TAbstract" + rust_camel_case(tservice->get_name()) + "SyncHandler";
}

string t_rs_generator::rust_sync_processor_name(t_service* tservice) {
  return "T" + rust_camel_case(tservice->get_name()) + "Processor";
}

string t_rs_generator::service_call_client_function_name(t_function* tfunc) {
  return rust_snake_case(tfunc->get_name());
}

string t_rs_generator::service_call_handler_function_name(t_function* tfunc) {
  return "handle_" + rust_snake_case(tfunc->get_name());
}

string t_rs_generator::service_call_sync_send_client_function_name(t_function* tfunc) {
  return "send_" + rust_snake_case(tfunc->get_name());
}

string t_rs_generator::service_call_sync_recv_client_function_name(t_function* tfunc) {
  return "recv_" + rust_snake_case(tfunc->get_name());
}

string t_rs_generator::service_call_result_struct_name(t_function* tfunc) {
  return rust_camel_case(tfunc->get_name()) + RESULT_STRUCT_SUFFIX;
}

string t_rs_generator::rust_upper_case(const string& name) {
  string str(uppercase(underscore(name)));
  boost::replace_all(str, "__", "_");
  return str;
}

string t_rs_generator::rust_snake_case(const string& name) {
  string str(decapitalize(underscore(name)));
  boost::replace_all(str, "__", "_");
  return str;
}

string t_rs_generator::rust_camel_case(const string& name) {
  string str(capitalize(camelcase(name)));
  boost::replace_all(str, "_", "");
  return str;
}

THRIFT_REGISTER_GENERATOR(
  rs,
  "Rust",
  "   nodoc\n")
