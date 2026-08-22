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

#include <iostream>
#include <sstream>
#include <string>

#include "thrift/generate/t_generator.h"
#include "thrift/parse/t_enum.h"
#include "thrift/parse/t_field.h"
#include "thrift/parse/t_function.h"
#include "thrift/parse/t_program.h"
#include "thrift/parse/t_service.h"
#include "thrift/parse/t_struct.h"
#include "thrift/parse/t_type.h"
#include "thrift/platform.h"

using std::map;
using std::ofstream;
using std::ostringstream;
using std::pair;
using std::set;
using std::string;
using std::vector;

static const string CLIENT_RESULT_STRUCT_SUFFIX("ClientResult");
static const string RESULT_STRUCT_SUFFIX("Result");
static const string ZIG_RESERVED_WORDS[]
    = {"addrspace", "align",    "allowzero",   "and",      "anyframe",    "anytype",     "asm",
       "break",     "callconv", "catch",       "comptime", "const",       "continue",    "defer",
       "else",      "enum",     "errdefer",    "error",    "export",      "extern",      "false",
       "fn",        "for",      "if",          "inline",   "linksection", "noalias",     "noinline",
       "nosuspend", "opaque",   "or",          "orelse",   "packed",      "pub",         "resume",
       "return",    "struct",   "suspend",     "switch",   "test",        "threadlocal", "true",
       "try",       "union",    "unreachable", "var",      "volatile",    "while"};

const set<string> ZIG_RESERVED_WORDS_SET(ZIG_RESERVED_WORDS,
                                         ZIG_RESERVED_WORDS
                                             + sizeof(ZIG_RESERVED_WORDS)
                                                   / sizeof(ZIG_RESERVED_WORDS[0]));

class t_zig_generator : public t_generator {
public:
  t_zig_generator(t_program* program, const std::map<std::string, std::string>&, const std::string&)
    : t_generator(program), const_block_label_id_(0) {
    gen_dir_ = get_out_dir();
  }

  /**
   * Init and close methods
   */

  void init_generator() override;
  void close_generator() override;
  std::string display_name() const override;

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef) override;
  void generate_enum(t_enum* tenum) override;
  void generate_const(t_const* tconst) override;
  void generate_struct(t_struct* tstruct) override;
  void generate_xception(t_struct* txception) override;
  void generate_service(t_service* tservice) override;

private:
  // struct type
  // T_REGULAR: user-defined struct in the IDL
  // T_ARGS: struct used to hold all service-call parameters
  // T_RESULT: struct used to hold all service-call returns and exceptions
  // T_EXCEPTION: user-defined exception in the IDL
  enum e_struct_type { T_REGULAR, T_ARGS, T_RESULT, T_EXCEPTION };

  // Directory to which generated code is written.
  string gen_dir_;

  // Monotonic counter for unique block labels in generated const expressions.
  int const_block_label_id_;

  // File to which generated code is written.
  ofstream_with_content_based_conditional_update f_gen_;

  // Write the common compiler attributes and module includes to the top of the auto-generated file.
  void render_attributes_and_includes();

  // Create the of zig modules referenced by this one
  void compute_service_referenced_modules(t_service* tservice,
                                          set<pair<string, string>>& referenced_modules);

  // Write a simple zig const value (ie. `pub const FOO: foo...`).
  void render_const_value(const string& name, t_type* ttype, t_const_value* tvalue);

  // Write a constant list, set, map or struct. These constants require allocation and cannot be
  // defined using a 'pub const'. As a result, I create a holder struct with a single `const_value`
  // method that returns the initialized instance.
  void render_const_value_holder(const string& name, t_type* ttype, t_const_value* tvalue);

  // Write the actual const value expression (always in an allocator context).
  void render_const_value(t_type* ttype, t_const_value* tvalue);

  // Write a const struct (returned from `const_value` method).
  void render_const_struct(t_type* ttype, t_const_value* tvalue);

  // Write a const list (returned from `const_value` method).
  void render_const_list(t_type* ttype, t_const_value* tvalue);

  // Write a const set (returned from `const_value` method).
  void render_const_set(t_type* ttype, t_const_value* tvalue);

  // Write a const map (returned from `const_value` method).
  void render_const_map(t_type* ttype, t_const_value* tvalue);

  // Returns a unique block label for nested const initialization expressions.
  string next_const_block_label();

  // Returns a unique temporary variable name for const initialization expressions.
  string next_const_var_name();

  // Resets block label generation for a new const holder.
  void reset_const_block_labels();

  // Write the zig representation of a thrift struct to the generated file. Set `struct_type` to
  // `T_ARGS` if rendering the struct used to pack arguments for a service call. When `struct_type`
  // is `T_ARGS` all fields are required.
  void render_struct(const string& struct_name,
                     t_struct* tstruct,
                     t_zig_generator::e_struct_type struct_type);

  // Returns true if the given struct requires to allocate memory for it's members
  bool struct_requires_alloc(t_struct* tstruct);
  bool struct_requires_alloc(t_struct* tstruct, set<t_struct*>& active);

  // Returns if the type requires memory management, e.g. strings, maps, etc.
  bool type_requires_alloc(t_type* ttype);
  bool type_requires_alloc(t_type* ttype, set<t_struct*>& active);

  // Write the comment block preceding a type.
  void render_type_comment(const string& struct_name);

  // Writes the implementation of a struct, includes init/deinit methods, read and write
  // from protocols, utility functions (eq, ord, hash, etc).
  void render_struct_impl(const string& struct_name,
                          t_struct* tstruct,
                          t_zig_generator::e_struct_type struct_type);

  // Generates the init and deinit function for a struct
  void render_struct_lifecycle_functions(const string& struct_name,
                                         t_struct* tstruct,
                                         t_zig_generator::e_struct_type struct_type);

  // Generates the default assignment for values inside a struct during instantiation
  void render_struct_default_values(t_struct* tstruct, t_zig_generator::e_struct_type struct_type);

  // Generates default field assignments for a variable (used by initDefault).
  void render_struct_default_assignments(t_struct* tstruct,
                                         t_zig_generator::e_struct_type struct_type,
                                         const string& var_name);

  // Returns true when an InitArgs field may be omitted (optional IDL field or has IDL default).
  bool field_is_optional_in_init_args(t_field* field);

  // Returns true when the default value expression for a field requires try.
  bool field_default_value_needs_try(t_field* field, e_struct_type struct_type);

  // Returns true when initDefault uses the allocator parameter.
  bool struct_init_default_uses_allocator(t_struct* tstruct, e_struct_type struct_type);

  // Renders the RHS of a field default value assignment.
  void render_field_default_value_rhs(t_field* field, e_struct_type struct_type);

  // Renders the nested InitArgs struct for manual construction.
  void render_struct_init_args(t_struct* tstruct, e_struct_type struct_type);

  // Renders pub fn init(allocator, args: InitArgs).
  void render_struct_init_fn(t_struct* tstruct, e_struct_type struct_type);

  // Deinits a heap-owned default field value before init replaces it.
  void render_init_field_replace_deallocation(t_field* member);

  // Generates the call to allocate an empty struct member of type ttype
  void render_struct_member_empty_allocation(t_type* ttype);

  // Generates the default value for the given ttype. This is not a default value
  // as given through thrift, but one that allows
  void render_variable_default_value(t_type* ttype);

  void render_struct_member_equality_comparison(t_type* ttype, const string& member_name, const string& self_prefix = "self.", const string& other_prefix = "other.");

  // Returns whether the allocator was used. This us useful information so we know
  // if we need to discard the allocator parameter.
  bool render_struct_member_clone_invocation(t_type* ttype, const string& member_name, const string& self_prefix = "self.");

  void render_struct_member_deallocation(t_field* member, const string& var_name = "self");

  // Read an alloc-requiring struct field from the wire into a temporary, then replace the
  // existing field value. Reading first keeps the field valid if deserialization fails.
  void render_struct_sync_read_field_assignment(t_field* member, const string& var_name);

  // Write the function that serializes a struct to its wire representation. If `struct_type` is
  // `T_ARGS` then all fields are considered "required", if not, the default optionality is used.
  void render_struct_sync_write(t_struct* tstruct, t_zig_generator::e_struct_type struct_type);

  // Helper function that serializes a single struct field to its wire representation. Unpacks the
  // variable (since it may be optional) and serializes according to the optionality rules required
  // by `req`. Variables in auto-generated code are passed by reference. Since this function may be
  // called in contexts where the variable is *already* a reference you can set `field_var_is_ref`
  // to `true` to avoid generating an extra, unnecessary `&` that the compiler will have to
  // automatically dereference.
  void render_struct_field_sync_write(const string& field_var,
                                      bool field_var_is_ref,
                                      t_field* tfield,
                                      t_field::e_req req);

  // Write the zig function that serializes a single type (i.e. a i32 etc.) to its wire
  // representation. Variables in auto-generated code are passed by reference. Since this function
  // may be called in contexts where the variable is *already* a reference you can set
  // `type_var_is_ref` to `true` to avoid generating an extra, unnecessary `&` that the compiler
  // will have to automatically dereference.
  void render_type_sync_write(const string& type_var, bool type_var_is_ref, t_type* ttype);

  // Return `true` if we need to dereference ths type when writing an element from a container.
  // Iterations on zig containers are performed as follows: `for v in &values { ... }`
  // where `v` has type `&ZIG_TYPE` All defined functions take primitives by value, so, if the
  // rendered code is calling such a function it has to dereference `v`.
  bool needs_deref_on_container_write(t_type* ttype);

  // Write the code to read bytes from the wire into the given `t_struct`. `struct_name` is the
  // actual Zig name of the `t_struct`. If `struct_type` is `T_ARGS` then all struct fields are
  // necessary. Otherwise, the field's default optionality is used.
  void render_struct_sync_read(const string& struct_name,
                               t_struct* tstruct,
                               t_zig_generator::e_struct_type struct_type);

  // Write the zig function that deserializes a single type (i.e. i32 etc.) from its wire
  // representation. Set `is_boxed` to `true` if the resulting value should be wrapped in a
  // box.
  void render_type_sync_read(t_type* ttype, bool is_boxed = false);

  // Top-level function that calls the various render functions necessary to write the zig
  // representation of a thrift union (i.e. an enum).
  void render_union(t_struct* tstruct);

  // Write the enum corresponding to the Thrift union.
  void render_union_definition(const string& union_name, t_struct* tstruct);

  // Write the `writeToProtocol` method for the union.
  void render_union_sync_write(const string& union_name, t_struct* tstruct);

  // Write the `readFromProtocol` method for the union.
  void render_union_sync_read(const string& union_name, t_struct* tstruct);

  // Top-level function that calls the various render functions necessary to write the zig
  // representation of a Thrift client.
  void render_sync_client(t_service* tservice);
  void render_client_result_unions(t_service* tservice);
  void render_client_result_union(t_service* tservice, t_function* tfunc);
  void render_client_result_error_union(string union_name, t_struct* exceptions);


  // Write the code to create the Thrift service sync client struct.
  void render_sync_client_fields_and_init_fn(t_service* tservice);

  // Write the code to create the `init` functions as well as other functions
  // callers would like to use on the Thrift service sync client.
  void render_sync_client_lifecycle_functions(t_service* service);

  // Top-level function that writes the code to make the Thrift service calls.
  void render_sync_client_struct(t_service* tservice);

  // Write the actual function that calls out to the remote service and processes its response.
  void render_sync_send_recv_wrapper(t_service* tservice, t_function* tfunc);

  // Renders the creation TMessageIdentifier{...} to to f_gen_
  void message_identifier_creation(const string& allocator_var,
                                   const string& name,
                                   const string& msg_type,
                                   const string& sequence_number_var);

  // Renders the creation TStructIdentifier{...} to to f_gen_
  void struct_identifier_creation(const string& allocator_var, const string& name);

  // Renders the creation TFieldIdentifier{...} to to f_gen_
  void field_identifier_creation(const string& allocator_var,
                                 const string& name,
                                 const string& field_type,
                                 const string& id_var);

  // Write the `send` functionality for a Thrift service call represented by a
  // `t_service->t_function`.
  void render_sync_send(t_service* tservice, t_function* tfunc);

  // Write the `recv` functionality for a Thrift service call represented by a
  // `t_service->t_function`. This method is only rendered if the function is *not* oneway.
  void render_sync_recv(t_service* tservice, t_function* tfunc);

  void render_sync_processor(t_service* tservice);

  void render_sync_handler_interface(t_service* tservice);
  void render_sync_processor_definition_and_impl(t_service* tservice);
  void render_sync_process_function(t_service* tservice,
                                    t_function* tfunc,
                                    const string& handler_type);
  void render_process_match_statements(t_service* tservice);
  void render_sync_handler_succeeded(t_function* tfunc);
  void render_service_call_structs(t_service* tservice);
  void render_service_call_args_struct(t_service* tservice, t_function* tfunc);
  void render_service_call_result_value_struct(t_service* tservice, t_function* tfunc);
  void render_service_call_result_error_union(string union_name, t_struct* exceptions);

  // Writes the result of `render_thrift_error_struct` wrapped in an error.
  void render_thrift_error(const string& error_kind, const string& error_name);

  string zig_sync_service_handler_interface_call_declaration(t_function* tfunc);

  // Return a string containing all the unpacked service call args given a service call function
  // `t_function`. Prepends the args with either `&mut self` or `&self` and includes the arg types
  // in the returned string, for example:
  // `fn foo(&mut self, field_0: String)`.
  string zig_sync_service_call_declaration(t_function* tfunc, string self_type = "*@This()");

  string zig_sync_service_handler_interface_call_invocation(t_function* tfunc,
                                                            const string& self_name = "",
                                                            const string& field_prefix = "");

  // Return a string containing all the unpacked service call args given a service call function
  // `t_function`. Only includes the arg names, each of which is prefixed with the optional prefix
  // `field_prefix`, for example: `self.field_0`.
  string zig_sync_service_call_invocation(t_function* tfunc, const string& field_prefix = "");

  // Return a string containing all fields in the struct `tstruct` for use in a function
  // declaration. Each field is followed by its type, for example: `field_0: String`.
  string struct_to_declaration(t_struct* tstruct, t_zig_generator::e_struct_type struct_type);

  // Return a string containing all fields in the struct `tstruct` for use in a function call,
  // for example: `field_0: String`.
  string struct_to_invocation(t_struct* tstruct, const string& field_prefix = "");

  // Write the documentation for a struct, service-call or other documentation-annotated element.
  void render_zigdoc(t_doc* tdoc);

  // Return `true` if the true type of `ttype` is a thrift double, `false` otherwise.
  bool is_double(t_type* ttype);

  string type_format_string(t_field* tfield);

  // Return a string representing the zig type given a `t_type`.
  string to_zig_type(t_type* ttype);

  // Return a string representing the rift `protocol::TType` given a `t_type`.
  string to_zig_field_type_enum(t_type* ttype);

  // Return `true` if we can write a const of the form `pub const FOO: ...`.
  bool can_generate_simple_const(t_type* ttype);

  // Return `true` if we cannot write a standard zig constant (because the type needs some
  // allocation).
  bool can_generate_const_holder(t_type* ttype);

  // Return `true` if this type is a void, and should be represented by the zig `void` type.
  bool is_void(t_type* ttype);

  t_field::e_req actual_field_req(t_field* tfield, t_zig_generator::e_struct_type struct_type);

  // Return `true` if this `t_field::e_req` is either `t_field::T_OPTIONAL` or
  // `t_field::T_OPT_IN_REQ_OUT` and needs to be wrapped by an `Option<TYPE_NAME>`, `false`
  // otherwise.
  bool is_optional_when_reading_from_proto(t_field::e_req req);

  // Return `true` if the service call has arguments, `false` otherwise.
  bool has_args(t_function* tfunc);

  // Return `true` if a service call has non-`()` arguments, `false` otherwise.
  bool has_non_void_args(t_function* tfunc);

  // Return `pub ` (notice trailing whitespace!) if the struct should be public, `` (empty string)
  // otherwise.
  string visibility_qualifier(t_zig_generator::e_struct_type struct_type);

  // Returns the namespace prefix for a given Thrift service. If the type is defined in the
  // presently-computed Thrift program, then an empty string is returned.
  string zig_namespace(t_service* tservice);

  // Returns the namespace prefix for a given Thrift type. If the type is defined in the
  // presently-computed Thrift program, then an empty string is returned.
  string zig_namespace(t_type* ttype);

  // Returns the camel-cased name for a Zig struct type. Handles the case where
  // `tstruct->get_name()` is a reserved word.
  string zig_struct_nameb(t_struct* tstruct);

  // Returns the snake-cased name for a Zig field or local variable. Handles the case where
  // `tfield->get_name()` is a reserved word.
  string zig_field_name(t_field* tstruct);

  // Returns the camel-cased name for a Zig union type. Handles the case where
  // `tstruct->get_name()` is a reserved word.
  string zig_union_field_name(t_field* tstruct);

  // Converts any variable name into a 'safe' variant that does not clash with any Zig reserved
  // keywords.
  string zig_safe_name(const string& name);

  // Return `true` if the name is a reserved Zig keyword, `false` otherwise.
  bool is_reserved(const string& name);

  // Return the name of the function that users will invoke to make outgoing service calls.
  string service_call_client_function_name(t_function* tfunc);

  // Return the name of the function that users will have to implement to handle incoming service
  // calls.
  string service_call_handler_function_name(t_function* tfunc);

  string client_call_result_struct_name(t_service* tservice, t_function* tfunc);

  // Return the name of the struct used to pack the arguments for the thrift service call.
  string service_call_args_struct_name(t_service* tservice, t_function* tfunc);

  // Return the name of the struct used to pack the return value
  // and user-defined exceptions for the thrift service call.
  string service_call_result_struct_name(t_service* tservice, t_function* tfunc);

  // Return the struct name for the sync service client given a `t_service`.
  string zig_sync_client_trait_name(t_service* tservice);

  // Return the struct name that users will have to implement for the server half of a Thrift
  // service.
  string zig_sync_handler_interface_name(t_service* tservice);

  // Return the struct name for the  server half of a Thrift service.
  string zig_sync_processor_name(t_service* tservice);

  // Properly uppercase names for use in Zig.
  string zig_upper_case(const string& name);

  // Snake-case field, parameter and function names and make them Zig friendly.
  string zig_snake_case(const string& name);

  // Camel-case type/variant names and make them Zig friendly.
  string zig_camel_case(const string& name);

  // Replace all instances of `search_string` with `replace_string` in `target`.
  void string_replace(string& target, const string& search_string, const string& replace_string);

  // Adjust field identifier to correctly handle unspecified field identifiers
  // THRIFT-4953
  string zig_safe_field_id(int32_t id);
};

void t_zig_generator::init_generator() {
  // make output directory for this thrift program
  MKDIR(gen_dir_.c_str());

  // create the file into which we're going to write the generated code
  string f_gen_name = gen_dir_ + "/" + zig_snake_case(get_program()->get_name()) + ".zig";
  f_gen_.open(f_gen_name.c_str());

  // header comment
  f_gen_ << "// " << autogen_summary() << '\n';
  f_gen_ << "// DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING" << '\n';
  f_gen_ << '\n';

  render_attributes_and_includes();
}

void t_zig_generator::render_attributes_and_includes() {
  f_gen_ << "const std = @import(\"std\");\n";
  f_gen_ << "const thrift = @import(\"thrift\");\n";

  f_gen_ << "const Allocator = std.mem.Allocator;\n";

  f_gen_ << "const TApplicationException = thrift.TApplicationException;\n";
  f_gen_ << "const TMessageType = thrift.TMessageType;\n";
  f_gen_ << "const ApplicationError = thrift.ApplicationError;\n";

  f_gen_ << "const TProtocol = thrift.TProtocol;\n";
  f_gen_ << "const TType = thrift.protocol.TType;\n";
  f_gen_ << "const TFieldIdentifier = thrift.protocol.TFieldIdentifier;\n";
  f_gen_ << "const ProtocolError = thrift.protocol.ProtocolError;\n";
  f_gen_ << "const TMessageIdentifier = thrift.protocol.TMessageIdentifier;\n";
  f_gen_ << "const TStructIdentifier = thrift.protocol.TStructIdentifier;\n";
  f_gen_ << "const TMapIdentifier = thrift.protocol.TMapIdentifier;\n";
  f_gen_ << "const TSetIdentifier = thrift.protocol.TSetIdentifier;\n";
  f_gen_ << "const TListIdentifier = thrift.protocol.TListIdentifier;\n";

  set<pair<string, string>> referenced_modules; // set<module, namespace>

  // first, start by adding explicit thrift includes
  const vector<t_program*> includes = get_program()->get_includes();
  vector<t_program*>::const_iterator includes_iter;
  for (includes_iter = includes.begin(); includes_iter != includes.end(); ++includes_iter) {
    referenced_modules.insert(
        std::make_pair((*includes_iter)->get_name(), (*includes_iter)->get_namespace("zig")));
  }

  // next, recursively iterate through all the services and add the names of any programs they
  // reference
  const vector<t_service*> services = get_program()->get_services();
  vector<t_service*>::const_iterator service_iter;
  for (service_iter = services.begin(); service_iter != services.end(); ++service_iter) {
    compute_service_referenced_modules(*service_iter, referenced_modules);
  }

  // FIXME: haven't put much tought into namespaces, will look into it later
  if (!referenced_modules.empty()) {
    set<pair<string, string>>::iterator module_iter;
    for (module_iter = referenced_modules.begin(); module_iter != referenced_modules.end();
         ++module_iter) {
      string module_name((*module_iter).first);

      string module_namespace((*module_iter).second);

      if (module_namespace.empty()) {
        f_gen_ << "pub const " << module_name << " = @import(\"" << zig_snake_case(module_name)
               << ".zig\");" << '\n';
      } else {
        f_gen_ << "pub const " << module_name << " = @import(\"" << module_namespace << "/"
               << zig_snake_case(module_name) << "\".zig);" << '\n';
      }
    }
    f_gen_ << '\n';
  }
}

void t_zig_generator::compute_service_referenced_modules(
    t_service* tservice,
    set<pair<string, string>>& referenced_modules) {
  t_service* extends = tservice->get_extends();
  if (extends) {
    if (extends->get_program() != get_program()) {
      referenced_modules.insert(std::make_pair(extends->get_program()->get_name(),
                                               extends->get_program()->get_namespace("zig")));
    }
    compute_service_referenced_modules(extends, referenced_modules);
  }
}

void t_zig_generator::close_generator() {
  f_gen_.close();
}

//-----------------------------------------------------------------------------
//
// Consts
//
// NOTE: consider using macros to generate constants
//
//-----------------------------------------------------------------------------

// This is worse than it should be because constants
// aren't (sensibly) limited to scalar types
void t_zig_generator::generate_const(t_const* tconst) {
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

void t_zig_generator::render_const_value(const string& name, t_type* ttype, t_const_value* tvalue) {
  if (!can_generate_simple_const(ttype)) {
    throw "cannot generate simple zig constant for " + ttype->get_name();
  }

  f_gen_ << "pub const " << zig_upper_case(name) << ": " << to_zig_type(ttype) << " = ";
  render_const_value(ttype, tvalue);
  f_gen_ << ";" << '\n';
  f_gen_ << '\n';
}

void t_zig_generator::render_const_value_holder(const string& name,
                                                t_type* ttype,
                                                t_const_value* tvalue) {
  if (!can_generate_const_holder(ttype)) {
    throw "cannot generate constant holder for " + ttype->get_name();
  }

  string holder_name("Const" + zig_camel_case(name));

  reset_const_block_labels();

  f_gen_ << indent() << "pub const " << holder_name << " = struct {" << '\n';
  indent_up();

  f_gen_ << indent() << "pub fn const_value(allocator: Allocator) !" << to_zig_type(ttype) << " {"
         << '\n';
  indent_up();
  f_gen_ << indent() << "return ";
  render_const_value(ttype, tvalue);
  f_gen_ << ";" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  indent_down();
  f_gen_ << indent() << "};" << '\n';
  f_gen_ << '\n';
}

void t_zig_generator::render_const_value(t_type* ttype, t_const_value* tvalue) {
  t_type* true_type = get_true_type(ttype);
  if (true_type->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)true_type;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        f_gen_ << "try thrift.BinaryBytes.initFromSlice(allocator, \"" << tvalue->get_string()
               << "\")";
      } else {
        f_gen_ << "try thrift.String.initFromSlice(allocator, \"" << tvalue->get_string() << "\")";
      }
      break;
    case t_base_type::TYPE_UUID:
      f_gen_ << "thrift.UUID.parse(\"" << tvalue->get_string() << "\") catch unreachable";
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
      f_gen_ << tvalue->get_double();
      break;
    default:
      throw "cannot generate const value for " + t_base_type::t_base_name(tbase_type->get_base());
    }
  } else if (true_type->is_enum()) {
    f_gen_ << "." << tvalue->get_identifier_name();
  } else if (true_type->is_struct() || true_type->is_xception()) {
    render_const_struct(true_type, tvalue);
  } else if (true_type->is_container()) {
    if (true_type->is_list()) {
      render_const_list(true_type, tvalue);
    } else if (true_type->is_set()) {
      render_const_set(true_type, tvalue);
    } else if (true_type->is_map()) {
      render_const_map(true_type, tvalue);
    } else {
      throw "cannot generate const container value for " + true_type->get_name();
    }
  } else {
    throw "cannot generate const value for " + true_type->get_name();
  }
}

void t_zig_generator::render_const_struct(t_type* ttype, t_const_value* tvalue) {
  f_gen_ << "try " << to_zig_type(ttype) << ".init(allocator, .{" << '\n';
  indent_up();

  const vector<t_field*>& fields = ((t_struct*)ttype)->get_members();
  const map<t_const_value*, t_const_value*, t_const_value::value_compare>& val = tvalue->get_map();
  map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator value_iter;
  for (value_iter = val.begin(); value_iter != val.end(); ++value_iter) {
    t_field* field = nullptr;
    vector<t_field*>::const_iterator field_iter;
    for (field_iter = fields.begin(); field_iter != fields.end(); ++field_iter) {
      if ((*field_iter)->get_name() == value_iter->first->get_string()) {
        field = *field_iter;
        break;
      }
    }
    if (field == nullptr) {
      throw "type error: " + ttype->get_name() + " has no field " + value_iter->first->get_string();
    }

    f_gen_ << indent() << "." << zig_field_name(field) << " = ";
    render_const_value(field->get_type(), value_iter->second);
    f_gen_ << "," << '\n';
  }

  indent_down();
  f_gen_ << indent() << "})";
}

void t_zig_generator::render_const_list(t_type* ttype, t_const_value* tvalue) {
  t_type* elem_type = ((t_list*)ttype)->get_elem_type();
  string block_label(next_const_block_label());
  string var_name(next_const_var_name());
  f_gen_ << block_label << ": {" << '\n';
  indent_up();
  f_gen_ << indent() << "var " << var_name << ": " << to_zig_type(ttype) << " = .init(allocator);" << '\n';
  const vector<t_const_value*>& elems = tvalue->get_list();
  vector<t_const_value*>::const_iterator elem_iter;
  for (elem_iter = elems.begin(); elem_iter != elems.end(); ++elem_iter) {
    t_const_value* elem_value = (*elem_iter);
    f_gen_ << indent() << "try " << var_name << ".append(";
    render_const_value(elem_type, elem_value);
    f_gen_ << ");" << '\n';
  }
  f_gen_ << indent() << "break :" << block_label << " " << var_name << ";" << '\n';
  indent_down();
  f_gen_ << indent() << "}";
}

void t_zig_generator::render_const_set(t_type* ttype, t_const_value* tvalue) {
  t_type* elem_type = ((t_set*)ttype)->get_elem_type();
  const vector<t_const_value*>& elems = tvalue->get_list();
  string block_label(next_const_block_label());
  string var_name(next_const_var_name());
  f_gen_ << block_label << ": {" << '\n';
  indent_up();
  if (elems.empty()) {
    f_gen_ << indent() << "const " << var_name << ": " << to_zig_type(ttype) << " = .init(allocator);" << '\n';
  } else {
    f_gen_ << indent() << "var " << var_name << ": " << to_zig_type(ttype) << " = .init(allocator);" << '\n';
  }
  vector<t_const_value*>::const_iterator elem_iter;
  for (elem_iter = elems.begin(); elem_iter != elems.end(); ++elem_iter) {
    t_const_value* elem_value = (*elem_iter);
    f_gen_ << indent() << "try " << var_name << ".put(";
    render_const_value(elem_type, elem_value);
    f_gen_ << ");" << '\n';
  }
  f_gen_ << indent() << "break :" << block_label << " " << var_name << ";" << '\n';
  indent_down();
  f_gen_ << indent() << "}";
}

void t_zig_generator::render_const_map(t_type* ttype, t_const_value* tvalue) {
  t_type* key_type = ((t_map*)ttype)->get_key_type();
  t_type* val_type = ((t_map*)ttype)->get_val_type();
  const map<t_const_value*, t_const_value*, t_const_value::value_compare>& elems
      = tvalue->get_map();
  string block_label(next_const_block_label());
  string var_name(next_const_var_name());
  f_gen_ << block_label << ": {" << '\n';
  indent_up();
  if (elems.empty()) {
    f_gen_ << indent() << "const " << var_name << ": " << to_zig_type(ttype) << " = .init(allocator);" << '\n';
  } else {
    f_gen_ << indent() << "var " << var_name << ": " << to_zig_type(ttype) << " = .init(allocator);" << '\n';
  }
  map<t_const_value*, t_const_value*, t_const_value::value_compare>::const_iterator elem_iter;
  for (elem_iter = elems.begin(); elem_iter != elems.end(); ++elem_iter) {
    t_const_value* key_value = elem_iter->first;
    t_const_value* val_value = elem_iter->second;

    f_gen_ << indent() << "try " << var_name << ".put(";
    render_const_value(key_type, key_value);
    f_gen_ << ", ";
    render_const_value(val_type, val_value);
    f_gen_ << ");" << '\n';
  }
  f_gen_ << indent() << "break :" << block_label << " " << var_name << ";" << '\n';
  indent_down();
  f_gen_ << indent() << "}";
}

string t_zig_generator::next_const_block_label() {
  return "const_blk_" + std::to_string(const_block_label_id_++);
}

string t_zig_generator::next_const_var_name() {
  return "const_tmp_" + std::to_string(const_block_label_id_++);
}

void t_zig_generator::reset_const_block_labels() {
  const_block_label_id_ = 0;
}

//-----------------------------------------------------------------------------
//
// Typedefs
//
//-----------------------------------------------------------------------------

void t_zig_generator::generate_typedef(t_typedef* ttypedef) {
  std::string actual_type = to_zig_type(ttypedef->get_type());
  f_gen_ << "pub const " << zig_safe_name(ttypedef->get_symbolic()) << " = " << actual_type << ";"
         << '\n';
  f_gen_ << '\n';
}

//-----------------------------------------------------------------------------
//
// Enums
//
//-----------------------------------------------------------------------------

void t_zig_generator::generate_enum(t_enum* tenum) {
  string enum_name(tenum->get_name());
  f_gen_ << "pub const " << enum_name << " = enum(i32) {\n";

  indent_up();
  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator constants_iter;
  for (constants_iter = constants.begin(); constants_iter != constants.end(); ++constants_iter) {
    t_enum_value* val = (*constants_iter);
    render_zigdoc((t_doc*)val);
    f_gen_ << indent() << val->get_name() << " = " << val->get_value() << "," << '\n';
  }
  f_gen_ << indent() << "_," << '\n';

  f_gen_ << indent() << "pub fn writeToProtocol(self: *const @This(), o_prot: *TProtocol) !void {"
         << '\n';
  indent_up();
  f_gen_ << indent() << "try o_prot.writeI32(@intFromEnum(self.*));" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  f_gen_ << indent() << "pub fn readFromProtocol(allocator: Allocator, i_prot: *TProtocol) !"
         << enum_name << " {" << '\n';
  indent_up();
  f_gen_ << indent() << "_ = allocator;" << '\n';
  f_gen_ << indent() << "return @enumFromInt(try i_prot.readI32());" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  indent_down();

  f_gen_ << "};\n";
}

//-----------------------------------------------------------------------------
//
// Structs, Unions and Exceptions
//
//-----------------------------------------------------------------------------

void t_zig_generator::generate_xception(t_struct* txception) {
  render_struct(zig_struct_nameb(txception), txception, t_zig_generator::T_EXCEPTION);
}

void t_zig_generator::generate_struct(t_struct* tstruct) {
  if (tstruct->is_union()) {
    render_union(tstruct);
  } else if (tstruct->is_struct()) {
    render_struct(zig_struct_nameb(tstruct), tstruct, t_zig_generator::T_REGULAR);
  } else {
    throw "cannot generate struct for exception";
  }
}

void t_zig_generator::render_struct(const string& struct_name,
                                    t_struct* tstruct,
                                    t_zig_generator::e_struct_type struct_type) {
  render_type_comment(struct_name);
  render_struct_impl(struct_name, tstruct, struct_type);
}

bool t_zig_generator::type_requires_alloc(t_type* ttype) {
  set<t_struct*> active;
  return type_requires_alloc(ttype, active);
}

bool t_zig_generator::type_requires_alloc(t_type* ttype, set<t_struct*>& active) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      return false;
    case t_base_type::TYPE_STRING:
      return true;
    case t_base_type::TYPE_UUID:
      return false;
    case t_base_type::TYPE_BOOL:
      return false;
    case t_base_type::TYPE_I8:
      return false;
    case t_base_type::TYPE_I16:
      return false;
    case t_base_type::TYPE_I32:
      return false;
    case t_base_type::TYPE_I64:
      return false;
    case t_base_type::TYPE_DOUBLE:
      return false;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    return type_requires_alloc(ttypedef->get_type(), active);
  } else if (ttype->is_struct()) {
    return struct_requires_alloc((t_struct*)ttype, active);
  } else if (ttype->is_enum()) {
    return false;
  } else if (ttype->is_xception()) {
    return struct_requires_alloc((t_struct*)ttype, active);
  } else if (ttype->is_map()) {
    return true;
  } else if (ttype->is_set()) {
    return true;
  } else if (ttype->is_list()) {
    return true;
  }
  return false;
}

bool t_zig_generator::struct_requires_alloc(t_struct* tstruct) {
  set<t_struct*> active;
  return struct_requires_alloc(tstruct, active);
}

bool t_zig_generator::struct_requires_alloc(t_struct* tstruct, set<t_struct*>& active) {
  if (active.count(tstruct) > 0) {
    throw string("zig: recursive struct types are not yet supported (cycle involving struct \"")
           + tstruct->get_name() + "\")";
  }
  active.insert(tstruct);

  const vector<t_field*> members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  bool requires_alloc = false;

  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* member = (*members_iter);
    auto ttype = member->get_type();

    if (type_requires_alloc(ttype, active)) {
      requires_alloc = true;
      break;
    }
  }

  active.erase(tstruct);
  return requires_alloc;
}

void t_zig_generator::render_struct_impl(const string& struct_name,
                                         t_struct* tstruct,
                                         t_zig_generator::e_struct_type struct_type) {
  render_zigdoc((t_doc*)tstruct);
  f_gen_ << "pub const " << struct_name << " = struct {" << '\n';
  indent_up();

  const vector<t_field*> members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  bool hasMembers = !members.empty();

  // isset struct
  if (hasMembers) {
    f_gen_ << indent() << "const IsSet = struct {" << '\n';
    indent_up();

    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* member = (*members_iter);
      f_gen_ << indent() << zig_field_name(member) << ": bool = false," << '\n';
    }

    indent_down();
    f_gen_ << indent() << "};" << '\n';

    if (struct_type == T_REGULAR || struct_type == T_EXCEPTION) {
      render_struct_init_args(tstruct, struct_type);
    }

    f_gen_ << indent() << "__isset: IsSet = .{}," << '\n' << '\n';
  } else if (struct_type == T_REGULAR || struct_type == T_EXCEPTION) {
    render_struct_init_args(tstruct, struct_type);
  }

  if (struct_requires_alloc(tstruct) && struct_type != T_ARGS) {
    f_gen_ << indent() << "allocator: Allocator," << '\n';
  }

  // render the members
  if (hasMembers) {
    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* member = (*members_iter);
      t_field::e_req member_req = actual_field_req(member, struct_type);

      string zig_type = to_zig_type(member->get_type());
      zig_type = member_req == t_field::T_OPTIONAL ? "?" + zig_type : zig_type;

      render_zigdoc((t_doc*)member);
      f_gen_ << indent() << zig_field_name(member) << ": " << zig_type << "," << '\n';
    }
  }

  render_struct_lifecycle_functions(struct_name, tstruct, struct_type);

  render_struct_sync_read(struct_name, tstruct, struct_type);
  render_struct_sync_write(tstruct, struct_type);

  indent_down();
  f_gen_ << "};" << '\n';
  f_gen_ << '\n';
}


void t_zig_generator::render_struct_default_values(t_struct* tstruct,
                                                   t_zig_generator::e_struct_type struct_type) {
  const vector<t_field*>& members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  if (struct_type != T_ARGS && struct_requires_alloc(tstruct)) {
    f_gen_ << indent() << ".allocator = allocator," << '\n';
  }
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* member = (*members_iter);
    string member_name(zig_field_name(member));

    f_gen_ << indent() << "." << member_name << " = ";
    render_field_default_value_rhs(member, struct_type);
    f_gen_ << ",\n";
  }
}

void t_zig_generator::render_struct_default_assignments(t_struct* tstruct,
                                                        t_zig_generator::e_struct_type struct_type,
                                                        const string& var_name) {
  const vector<t_field*>& members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  if (struct_type != T_ARGS && struct_requires_alloc(tstruct)) {
    f_gen_ << indent() << var_name << ".allocator = allocator;" << '\n';
  }
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* member = (*members_iter);
    string member_name(zig_field_name(member));

    f_gen_ << indent() << var_name << "." << member_name << " = ";
    if (field_default_value_needs_try(member, struct_type)) {
      f_gen_ << "try ";
    }
    render_field_default_value_rhs(member, struct_type);
    f_gen_ << ";" << '\n';
    if (field_default_value_needs_try(member, struct_type)) {
      f_gen_ << indent() << "errdefer ";
      render_struct_member_deallocation(member, var_name);
      if (member->get_req() == t_field::T_OPTIONAL) {
        f_gen_ << ";";
      }
      f_gen_ << '\n';
    }
  }
}

bool t_zig_generator::field_is_optional_in_init_args(t_field* field) {
  return field->get_req() == t_field::T_OPTIONAL || field->get_value() != nullptr;
}

bool t_zig_generator::struct_init_default_uses_allocator(t_struct* tstruct,
                                                         e_struct_type struct_type) {
  if (struct_type != T_ARGS && struct_requires_alloc(tstruct)) {
    return true;
  }

  const vector<t_field*>& members = tstruct->get_sorted_members();
  for (auto member : members) {
    t_type* member_type = get_true_type(member->get_type());
    if (member_type->is_struct() || member_type->is_xception()) {
      return true;
    }
    if (type_requires_alloc(member->get_type())) {
      return true;
    }
    if (member->get_value() != nullptr) {
      t_type* true_type = get_true_type(member->get_type());
      if (true_type->is_base_type()) {
        t_base_type* tbase_type = (t_base_type*)true_type;
        if (tbase_type->get_base() == t_base_type::TYPE_STRING) {
          return true;
        }
      } else if (true_type->is_container() || true_type->is_struct() || true_type->is_xception()) {
        return true;
      }
    }
  }

  return false;
}

bool t_zig_generator::field_default_value_needs_try(t_field* field, e_struct_type struct_type) {
  (void)struct_type;
  t_type* true_type = get_true_type(field->get_type());

  if (field->get_value() != nullptr) {
    if (true_type->is_base_type()) {
      t_base_type* tbase_type = (t_base_type*)true_type;
      return tbase_type->get_base() == t_base_type::TYPE_STRING;
    }
    if (true_type->is_container()) {
      return true;
    }
    if (true_type->is_struct() || true_type->is_xception()) {
      return true;
    }
    return false;
  }

  if (field->get_req() == t_field::T_OPTIONAL) {
    return false;
  }

  if (true_type->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)true_type;
    return tbase_type->get_base() == t_base_type::TYPE_STRING;
  }
  if (true_type->is_struct() || true_type->is_xception()) {
    return true;
  }

  return false;
}

void t_zig_generator::render_field_default_value_rhs(t_field* field, e_struct_type /*struct_type*/) {
  t_type* ttype = field->get_type();
  t_const_value* tvalue = field->get_value();

  if (tvalue != nullptr) {
    t_type* true_type = get_true_type(ttype);
    if (true_type->is_base_type()) {
      t_base_type* tbase_type = (t_base_type*)true_type;
      switch (tbase_type->get_base()) {
      case t_base_type::TYPE_STRING:
        if (tbase_type->is_binary()) {
          f_gen_ << "thrift.BinaryBytes.initFromSlice(allocator, \"" << tvalue->get_string()
                 << "\")";
        } else {
          f_gen_ << "thrift.String.initFromSlice(allocator, \"" << tvalue->get_string() << "\")";
        }
        return;
      case t_base_type::TYPE_UUID:
        f_gen_ << "comptime thrift.UUID.parse(&\"" << tvalue->get_string() << "\".*) catch unreachable";
        return;
      case t_base_type::TYPE_BOOL:
        f_gen_ << (tvalue->get_integer() ? "true" : "false");
        return;
      case t_base_type::TYPE_I8:
      case t_base_type::TYPE_I16:
      case t_base_type::TYPE_I32:
      case t_base_type::TYPE_I64:
        f_gen_ << tvalue->get_integer();
        return;
      case t_base_type::TYPE_DOUBLE:
        f_gen_ << tvalue->get_double();
        return;
      default:
        throw "cannot generate field default value for " + t_base_type::t_base_name(tbase_type->get_base());
      }
    } else if (true_type->is_enum()) {
      render_const_value(true_type, tvalue);
      return;
    } else if (true_type->is_container()) {
      if (true_type->is_list()) {
        render_const_list(true_type, tvalue);
      } else if (true_type->is_set()) {
        render_const_set(true_type, tvalue);
      } else if (true_type->is_map()) {
        render_const_map(true_type, tvalue);
      } else {
        throw "cannot generate field default container value";
      }
      return;
    }
    render_variable_default_value(ttype);
    return;
  }

  if (field->get_req() == t_field::T_OPTIONAL) {
    f_gen_ << "null";
    return;
  }

  render_variable_default_value(ttype);
}

void t_zig_generator::render_struct_init_args(t_struct* tstruct, e_struct_type /*struct_type*/) {
  const vector<t_field*>& members = tstruct->get_sorted_members();

  f_gen_ << indent() << "const InitArgs = struct {" << '\n';
  indent_up();

  for (auto member : members) {
    string member_name(zig_field_name(member));
    string zig_type = to_zig_type(member->get_type());

    if (field_is_optional_in_init_args(member)) {
      f_gen_ << indent() << member_name << ": ?" << zig_type << " = null," << '\n';
    } else {
      f_gen_ << indent() << member_name << ": " << zig_type << "," << '\n';
    }
  }

  indent_down();
  f_gen_ << indent() << "};" << '\n' << '\n';
}

void t_zig_generator::render_init_field_replace_deallocation(t_field* member) {
  if (!type_requires_alloc(member->get_type())) {
    return;
  }

  string member_name(zig_field_name(member));
  if (member->get_req() == t_field::T_OPTIONAL) {
    f_gen_ << indent() << "if (ret." << member_name << ") |" << member_name << "| {" << '\n';
    indent_up();
    f_gen_ << indent() << member_name << ".deinit();" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  } else {
    f_gen_ << indent() << "ret." << member_name << ".deinit();" << '\n';
  }
}

void t_zig_generator::render_struct_init_fn(t_struct* tstruct, e_struct_type /*struct_type*/) {
  const vector<t_field*>& members = tstruct->get_sorted_members();

  f_gen_ << indent() << "pub fn init(allocator: Allocator, args: InitArgs) !@This() {" << '\n';
  indent_up();

  if (members.empty()) {
    f_gen_ << indent() << "_ = args;" << '\n';
    f_gen_ << indent() << "return try initDefault(allocator);" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
    return;
  }

  f_gen_ << indent() << "var ret = try initDefault(allocator);" << '\n';

  for (auto member : members) {
    string member_name(zig_field_name(member));

    if (field_is_optional_in_init_args(member)) {
      f_gen_ << indent() << "if (args." << member_name << ") |v| {" << '\n';
      indent_up();
      render_init_field_replace_deallocation(member);
      f_gen_ << indent() << "ret." << member_name << " = v;" << '\n';
      indent_down();
      f_gen_ << indent() << "}" << '\n';
    } else {
      render_init_field_replace_deallocation(member);
      f_gen_ << indent() << "ret." << member_name << " = args." << member_name << ";" << '\n';
    }
  }

  if (!members.empty()) {
    f_gen_ << indent() << "ret.__isset = .{" << '\n';
    indent_up();
    for (auto member : members) {
      f_gen_ << indent() << "." << zig_field_name(member) << " = true," << '\n';
    }
    indent_down();
    f_gen_ << indent() << "};" << '\n';
  }

  f_gen_ << indent() << "return ret;" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::render_struct_lifecycle_functions(
    const string& struct_name,
    t_struct* tstruct,
    t_zig_generator::e_struct_type struct_type) {
  (void)struct_name;
  const vector<t_field*>& members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;

  bool alloc_required = struct_requires_alloc(tstruct);

  // Init with default values (private — used by readFromProtocol and init)
  f_gen_ << indent() << "fn initDefault(allocator: Allocator) !@This() {" << '\n';
  indent_up();
  if (members.empty() && !alloc_required) {
    f_gen_ << indent() << "_ = allocator;" << '\n';
    f_gen_ << indent() << "return .{};" << '\n';
  } else {
    f_gen_ << indent() << "var ret: @This() = undefined;" << '\n';
    if (!struct_init_default_uses_allocator(tstruct, struct_type)) {
      f_gen_ << indent() << "_ = allocator;" << '\n';
    }
    render_struct_default_assignments(tstruct, struct_type, "ret");
    f_gen_ << indent() << "return ret;" << '\n';
  }
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  if (struct_type == T_REGULAR || struct_type == T_EXCEPTION) {
    render_struct_init_fn(tstruct, struct_type);
  }

  // Deinit
  bool discard_self = true;
  f_gen_ << indent() << "pub fn deinit(self: *@This()) void {" << '\n';
  indent_up();
  if (!members.empty()) {
    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* member = (*members_iter);
      string member_name(zig_field_name(member));
      t_type* ttype = member->get_type();

      if (!type_requires_alloc(ttype)) {
        continue;
      }
      discard_self = false;

      f_gen_ << indent();
      render_struct_member_deallocation(member);
      f_gen_ << '\n';
    }
  }
  if (discard_self) {
    f_gen_ << indent() << "_ = self;" << '\n';
  }

  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // hash
  f_gen_ << indent() << "pub fn hash(self: *const @This(), hasher: anytype) void {" << '\n';
  indent_up();
  if (members.empty()) {
    f_gen_ << indent() << "_ = self;" << '\n';
    f_gen_ << indent() << "_ = hasher;" << '\n';
  } else {
    for (auto member: members) {
      auto member_name = zig_field_name(member);
      f_gen_ << indent() << "thrift.internal.hash(hasher, self." << member_name << ");" << '\n';
    }
  }
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // eql
  f_gen_ << indent() << "pub fn eql(self: *const @This(), other: @This()) bool {" << '\n';
  indent_up();
  if (members.empty()) {
    f_gen_ << indent() << "_ = self;" << '\n';
    f_gen_ << indent() << "_ = other;" << '\n';
  } else {
    for (auto member: members) {
      string self_prefix("self.");
      string other_prefix("other.");

      auto member_name = zig_field_name(member);

      if (member->get_req() == t_field::T_OPTIONAL) {
        f_gen_ << indent() << "if (self." << member_name << ") |" << member_name << "| {" << '\n';
        indent_up();
        f_gen_ << indent() << "if (other." << member_name << " == null) { return false; }" << '\n';
        f_gen_ << indent() << "const other" << member_name << " = other." << member_name << ".?;" << '\n';
        self_prefix = "";
        other_prefix = "other";
      }

      f_gen_ << indent() << "if (!(";
      render_struct_member_equality_comparison(member->get_type(), member_name, self_prefix, other_prefix);
      f_gen_ << ")) {" << '\n';
      indent_up();
      f_gen_ << indent() << "return false;" << '\n';
      indent_down();
      f_gen_ << indent() << "}" << '\n';
      if (member->get_req() == t_field::T_OPTIONAL) {
        indent_down();
        f_gen_ << indent() << "}" << '\n';
      }
    }
  }
  f_gen_ << indent() << "return true;" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  f_gen_ << indent() << "pub fn format(self: *const @This(), writer: *std.Io.Writer) !void {" << '\n';
  indent_up();
  if (members.empty()) {
    f_gen_ << indent() << "_ = self;" << '\n';
    f_gen_ << indent() << "try writer.print(\"{}\", .{});" << '\n';
  } else {
    f_gen_ << indent() << "try writer.print(\"{{ \", .{});" << '\n';

    for (auto member: members) {
      f_gen_ << indent() << "try writer.print(\"" << zig_field_name(member) << " = {" << type_format_string(member) << "}, \", .{self." << zig_field_name(member) << "});" << '\n';
    }

    f_gen_ << indent() << "try writer.print(\"}}\", .{});" << '\n';
  }
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // clone
  bool discard_alloc = true;
  f_gen_ << indent() << "pub fn clone(self: *const @This(), allocator: Allocator) !@This() {" << '\n';
  indent_up();
  if (members.empty()) {
    f_gen_ << indent() << "_ = self;" << '\n';
    f_gen_ << indent() << "_ = allocator;" << '\n';
    f_gen_ << indent() << "return .{};" << '\n';
  } else {
    f_gen_ << indent() << "const ret: @This() = .{" << '\n';
    indent_up();
    if (alloc_required) {
      f_gen_ << indent() << ".allocator = allocator," << '\n';
    }
    for (auto member: members) {
      string self_prefix("self.");
      auto member_name = zig_field_name(member);

      f_gen_ << indent() << "." << member_name << " = ";

      if (member->get_req() == t_field::T_OPTIONAL) {
        f_gen_ << member_name << "OptClone: {" << '\n';
        indent_up();

        f_gen_ << indent() << "if (self." << member_name << ") |" << member_name << "| {" << '\n';
        indent_up();
        f_gen_ << indent() << "break :" << member_name << "OptClone ";
        self_prefix = "";
      }

      if (render_struct_member_clone_invocation(member->get_type(), member_name, self_prefix)) {
        discard_alloc = false;
      }

      if (member->get_req() == t_field::T_OPTIONAL) {
        f_gen_ << ";" << '\n';
        indent_down();
        f_gen_ << indent() << "}" << '\n';
        f_gen_ << indent() << "break :" << member_name << "OptClone null;" << '\n';

        indent_down();
        f_gen_ << indent() << "}," << '\n';
      } else {
        f_gen_ << ",\n";
      }
    }
    indent_down();
    f_gen_ << indent() << "};" << '\n';

    if (discard_alloc) {
      f_gen_ << indent() << "_ = allocator;" << '\n';
    }

    f_gen_ << indent() << "return ret;" << '\n';
  }

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::render_union(t_struct* tstruct) {
  string union_name(zig_struct_nameb(tstruct));
  render_type_comment(union_name);
  render_union_definition(union_name, tstruct);
}

void t_zig_generator::render_union_definition(const string& union_name, t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_sorted_members();
  if (members.empty()) {
    throw "cannot generate zig unions with 0 members"; // may be valid thrift, but it's invalid zig
  }
  f_gen_ << "pub const " << union_name << " = union(enum) {" << '\n';
  indent_up();

  vector<t_field*>::const_iterator member_iter;
  for (member_iter = members.begin(); member_iter != members.end(); ++member_iter) {
    t_field* tfield = (*member_iter);
    f_gen_ << indent() << zig_union_field_name(tfield) << ": " << to_zig_type(tfield->get_type())
           << "," << '\n';
  }
  f_gen_ << indent() << "_unknown: void," << '\n';

  f_gen_ << indent() << "pub fn hash(self: *const @This(), hasher: anytype) void {" << '\n';
  indent_up();
  f_gen_ << indent() << "thrift.internal.unionHash(hasher, self.*);" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  f_gen_ << indent() << "pub fn eql(self: *const @This(), other: @This()) bool {" << '\n';
  indent_up();
  f_gen_ << indent() << "return thrift.internal.unionEql(self.*, other);" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  render_union_sync_read(union_name, tstruct);
  render_union_sync_write(union_name, tstruct);

  indent_down();
  f_gen_ << "};" << '\n';
  f_gen_ << '\n';
}

//-----------------------------------------------------------------------------
//
// Sync Struct Write
//
//-----------------------------------------------------------------------------

void t_zig_generator::render_struct_sync_write(t_struct* tstruct,
                                               t_zig_generator::e_struct_type struct_type) {
  f_gen_ << indent()
         << "pub fn writeToProtocol(self: *const @This(), o_prot: *TProtocol) !void {" << '\n';
  indent_up();
  vector<t_field*> members = tstruct->get_sorted_members();

  if (members.empty()) {
    f_gen_ << indent() << "_ = self;" << '\n';
  }

  // write struct header to output protocol
  // note: use the *original* struct name here
  // f_gen_ << indent()
  //        << "let struct_ident = TStructIdentifier::new(\"" + tstruct->get_name() + "\");" <<
  //        '\n';
  f_gen_ << indent() << "try o_prot.writeStructBegin(";
  struct_identifier_creation("undefined", tstruct->get_name());
  f_gen_ << ");" << '\n';

  // write struct members to output protocol
  if (!members.empty()) {
    vector<t_field*>::iterator members_iter;
    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* member = (*members_iter);
      t_field::e_req member_req = actual_field_req(member, struct_type);
      string member_var("self." + zig_field_name(member));
      f_gen_ << indent() << "{" << '\n';
      indent_up();
      render_struct_field_sync_write(member_var, false, member, member_req);
      indent_down();
      f_gen_ << indent() << "}" << '\n';
    }
  }

  // write struct footer to output protocol
  f_gen_ << indent() << "try o_prot.writeFieldStop();" << '\n';
  f_gen_ << indent() << "try o_prot.writeStructEnd();" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::render_union_sync_write(const string& /*union_name*/, t_struct* tstruct) {
  f_gen_ << indent()
         << "pub fn writeToProtocol(self: *const @This(), o_prot: *TProtocol) !void {" << '\n';
  indent_up();

  // write struct header to output protocol
  // note: use the *original* struct name here
  f_gen_ << indent() << "try o_prot.writeStructBegin(";
  struct_identifier_creation("undefined", tstruct->get_name());
  f_gen_ << ");" << '\n';

  // write the enum field to the output protocol
  vector<t_field*> members = tstruct->get_sorted_members();
  if (!members.empty()) {
    f_gen_ << indent() << "switch(self.*) {" << '\n';
    indent_up();
    vector<t_field*>::iterator members_iter;
    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* member = (*members_iter);
      t_field::e_req member_req = t_field::T_REQUIRED;
      t_type* ttype = member->get_type();
      if (ttype->is_typedef()) {
        // get the actual type of typedef
        ttype = ((t_typedef*)ttype)->get_type();
      }
      f_gen_ << indent() << "." << zig_union_field_name(member) << " => |f| {" << '\n';
      indent_up();
      render_struct_field_sync_write("f", true, member, member_req);
      indent_down();
      f_gen_ << indent() << "}," << '\n';
    }
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  }

  // write struct footer to output protocol
  f_gen_ << indent() << "try o_prot.writeFieldStop();" << '\n';
  f_gen_ << indent() << "try o_prot.writeStructEnd();" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::render_struct_field_sync_write(const string& field_var,
                                                     bool field_var_is_ref,
                                                     t_field* tfield,
                                                     t_field::e_req req) {
  t_type* field_type = tfield->get_type();
  // t_type* actual_type = get_true_type(field_type);

  ostringstream field_stream;
  field_stream << "TFieldIdentifier.init("
               << "\"" << tfield->get_name() << "\""
               << ", " // note: use *original* name
               << to_zig_field_type_enum(field_type) << ", " << tfield->get_key() << ")";
  string field_ident_string = field_stream.str();

  if (req == t_field::T_OPTIONAL) {
    string let_var = "fld_var";

    f_gen_ << indent() << "if (" << field_var << ") |" << let_var << "| {" << '\n';
    indent_up();
    f_gen_ << indent() << "try o_prot.writeFieldBegin(";
    field_identifier_creation("undefined", tfield->get_name(), to_zig_field_type_enum(field_type),
                              std::to_string(tfield->get_key()));
    f_gen_ << ");" << '\n';
    render_type_sync_write("fld_var", true, field_type);
    f_gen_ << indent() << "try o_prot.writeFieldEnd();" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  } else {
    f_gen_ << indent() << "try o_prot.writeFieldBegin(";
    field_identifier_creation("undefined", tfield->get_name(), to_zig_field_type_enum(field_type),
                              std::to_string(tfield->get_key()));
    f_gen_ << ");" << '\n';
    render_type_sync_write(field_var, field_var_is_ref, tfield->get_type());
    f_gen_ << indent() << "try o_prot.writeFieldEnd();" << '\n';
  }
}

void t_zig_generator::render_type_sync_write(const string& type_var,
                                             bool type_var_is_ref,
                                             t_type* ttype) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      throw "cannot write field of type TYPE_VOID to output protocol";
    case t_base_type::TYPE_STRING: {
      if (tbase_type->is_binary()) {
        f_gen_ << indent() << "try o_prot.writeBytes(" + type_var + ");" << '\n';
      } else {
        f_gen_ << indent() << "try o_prot.writeString(" + type_var + ");" << '\n';
      }
      return;
    }
    case t_base_type::TYPE_UUID:
      f_gen_ << indent() << "try o_prot.writeUUID(" + type_var + ");" << '\n';
      return;
    case t_base_type::TYPE_BOOL:
      f_gen_ << indent() << "try o_prot.writeBool(" + type_var + ");" << '\n';
      return;
    case t_base_type::TYPE_I8:
      f_gen_ << indent() << "try o_prot.writeI8(" + type_var + ");" << '\n';
      return;
    case t_base_type::TYPE_I16:
      f_gen_ << indent() << "try o_prot.writeI16(" + type_var + ");" << '\n';
      return;
    case t_base_type::TYPE_I32:
      f_gen_ << indent() << "try o_prot.writeI32(" + type_var + ");" << '\n';
      return;
    case t_base_type::TYPE_I64:
      f_gen_ << indent() << "try o_prot.writeI64(" + type_var + ");" << '\n';
      return;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << indent() << "try o_prot.writeDouble(" + type_var + ");" << '\n';
      return;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_type_sync_write(type_var, type_var_is_ref, ttypedef->get_type());
    return;
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception() ||
      ttype->is_map() || ttype->is_list() || ttype->is_set()) {
    f_gen_ << indent() << "try " << type_var + ".writeToProtocol(o_prot);" << '\n';
    return;
  }

  throw "cannot write unsupported type " + ttype->get_name();
}

bool t_zig_generator::needs_deref_on_container_write(t_type* ttype) {
  ttype = get_true_type(ttype);
  return ttype->is_base_type() && !ttype->is_string();
}

//-----------------------------------------------------------------------------
//
// Sync Struct Read
//
//-----------------------------------------------------------------------------

void t_zig_generator::render_struct_sync_read(const string& struct_name,
                                              t_struct* tstruct,
                                              t_zig_generator::e_struct_type struct_type) {
  f_gen_ << indent() << "pub fn readFromProtocol(allocator: Allocator, i_prot: *TProtocol) !"
         << struct_name << " {" << '\n';

  indent_up();

  f_gen_ << indent() << "var structIdent = try i_prot.readStructBegin(allocator);" << '\n';
  f_gen_ << indent() << "defer structIdent.deinit();" << '\n';

  const vector<t_field*> members = tstruct->get_sorted_members();

  f_gen_ << indent() << "var temp_struct: " << struct_name << " = try .initDefault(allocator);\n";
  f_gen_ << indent() << "errdefer temp_struct.deinit();\n\n";

  // FIXME: when a field is already set, consider skipping the wire payload
  // with i_prot.skip instead of deinit+re-read. Although not likely, if the
  // allocator passed in is backed by an ArenaAllocator with a noop on deinit
  // (like Zig's std one), there's a risk of too many duplicates blowing the
  // heap up.

  // now loop through the fields we've received
  f_gen_ << indent() << "while (true) {" << '\n'; // start loop
  indent_up();

  // break out if you've found the Stop field
  f_gen_ << indent() << "var field_ident = try i_prot.readFieldBegin(allocator);" << '\n';
  f_gen_ << indent() << "defer field_ident.deinit();" << '\n';
  f_gen_ << indent() << "if (field_ident.fieldType == .Stop) {" << '\n';
  indent_up();
  f_gen_ << indent() << "break;" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // now read all the fields found
  // avoid clippy::match_single_binding
  vector<t_field*>::const_iterator members_iter;
  if (members.empty()) {
    f_gen_ << indent() << "try i_prot.skip(allocator, field_ident.fieldType);" << '\n';
  } else {
    f_gen_ << indent()
           << "const field_id = field_ident.id orelse return ProtocolError.MissingFieldId;" << '\n';
    f_gen_ << indent() << "switch (field_id) {" << '\n'; // start match
    indent_up();

    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      string member_name(zig_field_name(tfield));
      f_gen_ << indent() << zig_safe_field_id(tfield->get_key()) << " => {" << '\n';
      indent_up();
      f_gen_ << indent() << "if (field_ident.fieldType == "
             << to_zig_field_type_enum(tfield->get_type()) << ") {" << '\n';
      indent_up();
      if (type_requires_alloc(tfield->get_type())) {
        render_struct_sync_read_field_assignment(tfield, "temp_struct");
      } else {
        f_gen_ << indent() << "temp_struct." << member_name << " = ";
        render_type_sync_read(tfield->get_type());
        f_gen_ << ";" << '\n';
        f_gen_ << indent() << "temp_struct.__isset." << member_name << " = true;" << '\n';
      }
      indent_down();
      f_gen_ << indent() << "} else {" << '\n';
      indent_up();
      f_gen_ << indent() << "try i_prot.skip(allocator, field_ident.fieldType);" << '\n';
      indent_down();
      f_gen_ << indent() << "}" << '\n';
      indent_down();
      f_gen_ << indent() << "}," << '\n';
    }

    // default case (skip fields)
    f_gen_ << indent() << "else => {" << '\n';
    indent_up();
    f_gen_ << indent() << "try i_prot.skip(allocator, field_ident.fieldType);" << '\n';
    indent_down();
    f_gen_ << indent() << "}," << '\n';

    indent_down();
    f_gen_ << indent() << "}" << '\n'; // finish switch
  }

  f_gen_ << indent() << "try i_prot.readFieldEnd();" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';                           // finish loop
  f_gen_ << indent() << "try i_prot.readStructEnd();" << '\n'; // read message footer from the wire

  // apply IDL default values for fields not sent on the wire
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* tfield = (*members_iter);
    if (tfield->get_value() == nullptr) {
      continue;
    }
    string member_name(zig_field_name(tfield));
    f_gen_ << indent() << "if (!temp_struct.__isset." << member_name << ") {" << '\n';
    indent_up();
    f_gen_ << indent() << "temp_struct." << member_name << " = ";
    if (field_default_value_needs_try(tfield, struct_type)) {
      f_gen_ << "try ";
    }
    render_field_default_value_rhs(tfield, struct_type);
    f_gen_ << ";" << '\n';
    f_gen_ << indent() << "temp_struct.__isset." << member_name << " = true;" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  }

  // validate required variables have been assigned
  {
    vector<std::string> conditions;
    for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
      t_field* tfield = (*members_iter);
      t_field::e_req req = actual_field_req(tfield, struct_type);
      if (is_optional_when_reading_from_proto(req)) {
        continue;
      }
      conditions.push_back(string("temp_struct.__isset.") + zig_field_name(tfield));
    }

    if (!conditions.empty()) {
      f_gen_ << indent() << "if (!(";
      f_gen_ << conditions[0];

      if (conditions.size() > 1) {
        for (size_t i = 1; i < conditions.size(); ++i) {
          f_gen_ << " and " << conditions[i];
        }
      }

      f_gen_ << ")) {" << '\n';
      indent_up();
      f_gen_ << indent() << "return ProtocolError.MissingField;" << '\n';
      indent_down();
      f_gen_ << indent() << "}" << '\n';
    }
  }

  // return the constructed value
  f_gen_ << indent() << "return temp_struct;" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::render_union_sync_read(const string& union_name, t_struct* tstruct) {
  f_gen_ << indent() << "pub fn readFromProtocol(allocator: Allocator, i_prot: *TProtocol) !"
         << union_name << " {" << '\n';
  indent_up();

  // create temporary variables to hold the
  // completed union as well as a count of fields read
  f_gen_ << indent() << "var ret: ?" << union_name << " = null;" << '\n';
  f_gen_ << indent() << "var received_field_count: usize = 0;" << '\n';

  // read the struct preamble
  f_gen_ << indent() << "try i_prot.readStructBegin();" << '\n';

  // now loop through the fields we've received
  f_gen_ << indent() << "while(true) {" << '\n'; // start loop
  indent_up();

  // break out if you've found the Stop field
  f_gen_ << indent() << "const field_ident = try i_prot.readFieldBegin();" << '\n';
  f_gen_ << indent() << "if (field_ident.fieldType == TType.Stop) {" << '\n';
  indent_up();
  f_gen_ << indent() << "break;" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // now read all the fields found
  // f_gen_ << indent() << "const field_id = try field_ident.id;" << '\n';
  f_gen_ << indent() << "switch (field_ident.id) {" << '\n'; // start match
  indent_up();

  const vector<t_field*> members = tstruct->get_sorted_members();
  vector<t_field*>::const_iterator members_iter;
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* member = (*members_iter);
    f_gen_ << indent() << zig_safe_field_id(member->get_key()) << " => {" << '\n';
    indent_up();
    f_gen_ << indent() << "if (ret == null) {" << '\n';
    indent_up();
    f_gen_ << indent() << "ret = .{ ." << zig_union_field_name(member) << " = ";
    render_type_sync_read(member->get_type());
      f_gen_ << " };" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
    f_gen_ << indent() << "received_field_count += 1;" << '\n';
    indent_down();
    f_gen_ << indent() << "}," << '\n';
  }

  // default case (skip unknown fields without affecting the count)
  f_gen_ << indent() << "else => {" << '\n';
  indent_up();
  f_gen_ << indent() << "try i_prot.skip(allocator, field_ident.fieldType);" << '\n';
  indent_down();
  f_gen_ << indent() << "}," << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n'; // finish match
  f_gen_ << indent() << "try i_prot.readFieldEnd();" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';                           // finish loop
  f_gen_ << indent() << "try i_prot.readStructEnd();" << '\n'; // finish reading message from wire

  // return the value or an error
  f_gen_ << indent() << "if (received_field_count == 0) {" << '\n';
  indent_up();
  // As per thrift spec, unknown fields should be skipped
  // When a receiver doesn't recognise a new union field, the union should be initialized
  // with an unknown value. As per zig's compiler, users are forced to handle that case in
  // exhaustive swtich statements, so this is safe
  f_gen_ << indent() << "ret = .{ ._unknown = {} };" << '\n';
  indent_down();
  f_gen_ << indent() << "} else if (received_field_count > 1) {" << '\n';
  indent_up();
  render_thrift_error("Protocol", "UnionHasMultipleFields");
  indent_down();
  f_gen_ << indent() << "} else if (ret) |retval| {" << '\n';
  indent_up();
  f_gen_ << indent() << "return retval;" << '\n';
  indent_down();
  f_gen_ << indent() << "} else unreachable;" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

// Construct the zig representation of all supported types from the wire.
void t_zig_generator::render_type_sync_read(t_type* ttype, bool is_boxed) {
  (void) is_boxed;
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      throw "cannot read field of type TYPE_VOID from input protocol";
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        f_gen_ << "try i_prot.readBytes(allocator)";
      } else {
        f_gen_ << "try i_prot.readString(allocator)";
      }
      return;
    case t_base_type::TYPE_UUID:
      f_gen_ << "try i_prot.readUUID()";
      return;
    case t_base_type::TYPE_BOOL:
      f_gen_ << "try i_prot.readBool()";
      return;
    case t_base_type::TYPE_I8:
      f_gen_ << "try i_prot.readI8()";
      return;
    case t_base_type::TYPE_I16:
      f_gen_ << "try i_prot.readI16()";
      return;
    case t_base_type::TYPE_I32:
      f_gen_ << "try i_prot.readI32()";
      return;
    case t_base_type::TYPE_I64:
      f_gen_ << "try i_prot.readI64()";
      return;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << "try i_prot.readDouble()";
      return;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_type_sync_read(ttypedef->get_type(), ttypedef->is_forward_typedef());
    return;
  } else if (ttype->is_enum() || ttype->is_struct() || ttype->is_xception() ||
      ttype->is_map() || ttype->is_list() || ttype->is_set()) {
    f_gen_ << "try .readFromProtocol(allocator, i_prot)";
    return;
  }

  throw "cannot read unsupported type " + ttype->get_name();
}

//-----------------------------------------------------------------------------
//
// Sync Client
//
//-----------------------------------------------------------------------------

void t_zig_generator::generate_service(t_service* tservice) {
  render_client_result_unions(tservice);
  render_sync_client(tservice);
  render_sync_processor(tservice);
  render_service_call_structs(tservice);
}

void t_zig_generator::render_client_result_unions(t_service* tservice) {
  for (auto function: tservice->get_functions()) {
    if (function->is_oneway()) {
      continue;
    }
    render_client_result_union(tservice, function);
  }
}

void t_zig_generator::render_client_result_union(t_service* tservice, t_function* tfunc) {
  const auto service_call_result_union_name = service_call_result_struct_name(tservice, tfunc);
  const auto client_result_union_name = client_call_result_struct_name(tservice, tfunc);
  const auto error_union_name = client_result_union_name + "Error";

  t_struct* exceptions = tfunc->get_xceptions();
  const vector<t_field*>& exception_types = exceptions->get_members();
  const auto hasExceptions = !exception_types.empty();
  render_client_result_error_union(error_union_name, exceptions);

  f_gen_ << "pub const " << client_result_union_name << " = union(enum) {" << '\n';
  indent_up();
  f_gen_ << indent() << "result: "
           << to_zig_type(tfunc->get_returntype()) << "," << '\n';
  f_gen_ << indent() << "err: "
           << error_union_name << "," << '\n';

  // init()
  f_gen_ << indent() << "pub fn initFromMethodResult(val: " << service_call_result_union_name << ") @This() {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch (val) {" << '\n';
  indent_up();
  f_gen_ << indent() << ".result => |r| return .{ .result = r }," << '\n';
  f_gen_ << indent() << ".err => ";
  if (!hasExceptions) {
    f_gen_ << "unreachable" << '\n';
  } else {
    f_gen_ << "|e| {" << '\n';
    indent_up();
    f_gen_ << indent() << "switch (e) {" << '\n';
    indent_up();
    for (auto ex: exception_types) {
      f_gen_ << indent() << "." << zig_union_field_name(ex) << " => |ex| return .{ .err = .{ ." << zig_union_field_name(ex) << " = ex } }," << '\n';
    }

    indent_down();
    f_gen_ << indent() << "}" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  }
  // end switch
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  // end init function
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // deinit()
  f_gen_ << indent() << "pub fn deinit(self: *@This()) void {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch (self.*) {" << '\n';
  indent_up();
  f_gen_ << indent() << ".result => ";
  if (type_requires_alloc(tfunc->get_returntype())) {
    f_gen_ << "|*r| r.deinit()," << '\n';
  } else {
    f_gen_ << "{}," << '\n';
  }
  f_gen_ << indent() << ".err => |*e| {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch (e.*) {" << '\n';
  indent_up();
  f_gen_ << indent() << "inline else => |*v| { v.deinit(); }" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  // end switch
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  // end deinit function
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // get()
  f_gen_ << indent() << "pub fn get(self: *@This()) !" << to_zig_type(tfunc->get_returntype()) << " {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch (self.*) {" << '\n';
  indent_up();
  f_gen_ << indent() << ".result => |r| return r," << '\n';
  f_gen_ << indent() << ".err => return error.ErrorResult," << '\n';
  // end switch
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  // end get function
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // unwrapError()
  f_gen_ << indent() << "pub fn unwrapError(self: *@This()) " << error_union_name << " {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch (self.*) {" << '\n';
  indent_up();
  f_gen_ << indent() << ".err => |e| return e," << '\n';
  f_gen_ << indent() << ".result => unreachable," << '\n';
  // end switch
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  // end get function
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // end union
  indent_down();
  f_gen_ << indent() << "};" << '\n' << '\n';
}

void t_zig_generator::render_client_result_error_union(string union_name,
                                                             t_struct* exceptions) {
  f_gen_ << "const " << union_name << " = union(enum) {" << '\n';
  indent_up();

  // Possible values
  const vector<t_field*>& exception_types = exceptions->get_members();
  vector<t_field*>::const_iterator exception_iter;
  for (exception_iter = exception_types.begin(); exception_iter != exception_types.end();
       ++exception_iter) {
    t_field* exception_type = *exception_iter;
    f_gen_ << indent() << zig_union_field_name(exception_type) << ": "
           << to_zig_type(exception_type->get_type()) << "," << '\n';
  }
  f_gen_ << indent() << "TApplicationException: thrift.TApplicationException," << '\n';

  f_gen_ << indent() << "pub fn deinit(self: *@This()) void {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch(self.*) {" << '\n';
  indent_up();
  f_gen_ << indent() << "inline else => |*e| { e.deinit(); }" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  indent_down();
  f_gen_ << indent() << "};" << '\n' << '\n';
}

void t_zig_generator::render_service_call_structs(t_service* tservice) {
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
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    render_service_call_args_struct(tservice, tfunc);
    if (!tfunc->is_oneway()) {
      render_service_call_result_value_struct(tservice, tfunc);
    }
  }
}

void t_zig_generator::render_sync_client(t_service* tservice) {
  render_type_comment(tservice->get_name() + " service client"); // note: use *original* name
  render_sync_client_struct(tservice);
}

void t_zig_generator::render_sync_client_fields_and_init_fn(t_service* tservice) {

  // render the definition for the client struct
  f_gen_ << indent() << "allocator: Allocator," << '\n';
  f_gen_ << indent() << "i_prot: *TProtocol," << '\n';
  f_gen_ << indent() << "o_prot: *TProtocol," << '\n';
  f_gen_ << indent() << "sequence_number: i32," << '\n';
  f_gen_ << '\n';

  render_sync_client_lifecycle_functions(tservice);
  f_gen_ << '\n';
}

void t_zig_generator::render_sync_client_lifecycle_functions(t_service* tservice) {
  f_gen_ << indent()
         << "pub fn init(allocator: Allocator, input_protocol: *TProtocol, output_protocol: "
            "*TProtocol) "
         << zig_sync_client_trait_name(tservice) << " {" << '\n';
  indent_up();
  f_gen_ << indent()
         << "return .{ .allocator = allocator, .i_prot = input_protocol, .o_prot = "
            "output_protocol, .sequence_number = 0 };"
         << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  f_gen_ << '\n';

  f_gen_ << indent() << "fn increment_sequence_number(self: *@This()) i32 {" << '\n';
  indent_up();
  f_gen_ << indent() << "self.sequence_number = @addWithOverflow(self.sequence_number, 1)[0];"
         << '\n';
  f_gen_ << indent() << "return self.sequence_number;" << '\n';
  indent_down();
  f_gen_ << indent() << '}' << '\n';
}

void t_zig_generator::render_sync_client_struct(t_service* tservice) {
  f_gen_ << "pub const " << zig_sync_client_trait_name(tservice) << " = struct {" << '\n';
  indent_up();

  render_sync_client_fields_and_init_fn(tservice);

  const std::vector<t_function*> functions = tservice->get_functions();
  std::vector<t_function*>::const_iterator func_iter;
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* func = (*func_iter);
    render_sync_send_recv_wrapper(tservice, func);
  }

  t_service* extends = tservice->get_extends();
  if (extends) {
    const std::vector<t_function*> functions = extends->get_functions();
    std::vector<t_function*>::const_iterator func_iter;
    for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
      t_function* func = (*func_iter);
      render_sync_send_recv_wrapper(extends, func);
    }
  }

  indent_down();
  f_gen_ << "};" << '\n';
  f_gen_ << '\n';
}

void t_zig_generator::render_sync_send_recv_wrapper(t_service* tservice, t_function* tfunc) {
  string func_name = service_call_client_function_name(tfunc);
  string func_decl_args = zig_sync_service_call_declaration(tfunc);
  string func_call_args = zig_sync_service_call_invocation(tfunc);

  f_gen_ << indent() << "pub fn " << func_name << func_decl_args << " ";
  if (tfunc->is_oneway()) {
    f_gen_ << "!void";
  } else {
    const auto client_result_union_name = client_call_result_struct_name(tservice, tfunc);
    f_gen_ << "!" << client_result_union_name;
  }
  f_gen_ << " {" << '\n';
  indent_up();

  render_sync_send(tservice, tfunc);
  if (!tfunc->is_oneway()) {
    render_sync_recv(tservice, tfunc);
  }

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::message_identifier_creation(const string& allocator_var,
                                                  const string& name,
                                                  const string& msg_type,
                                                  const string& sequence_number_var) {
  f_gen_ << "TMessageIdentifier{\n";
  indent_up();
  f_gen_ << indent() << ".allocator = " << allocator_var << "," << '\n'
         << indent() << ".name = &\"" << name << "\".*," << '\n'
         << indent() << ".msgType = ." << msg_type << "," << '\n'
         << indent() << ".sequenceNumber = " << sequence_number_var << '\n';
  indent_down();
  f_gen_ << indent() << "}";
}

void t_zig_generator::struct_identifier_creation(const string& allocator_var, const string& name) {
  f_gen_ << "TStructIdentifier{\n";
  indent_up();
  f_gen_ << indent() << ".allocator = " << allocator_var << "," << '\n'
         << indent() << ".name = &\"" << name << "\".*," << '\n';
  indent_down();
  f_gen_ << indent() << "}";
}

void t_zig_generator::field_identifier_creation(const string& allocator_var,
                                                const string& name,
                                                const string& field_type,
                                                const string& id_var) {
  f_gen_ << "TFieldIdentifier{\n";
  indent_up();
  f_gen_ << indent() << ".allocator = " << allocator_var << "," << '\n'
         << indent() << ".name = &\"" << name << "\".*," << '\n'
         << indent() << ".fieldType = " << field_type << "," << '\n'
         << indent() << ".id = " << id_var << '\n';
  indent_down();
  f_gen_ << indent() << "}";
}

void t_zig_generator::render_sync_send(t_service* tservice, t_function* tfunc) {
  f_gen_ << indent() << "{" << '\n';
  indent_up();

  // increment the sequence number and generate the call header
  string message_type = tfunc->is_oneway() ? "OneWay" : "Call";
  f_gen_ << indent() << "const sequenceNumber = self.increment_sequence_number();" << '\n';
  f_gen_ << indent() << "const message_ident = ";
  message_identifier_creation("self.allocator", tfunc->get_name(), message_type, "sequenceNumber");
  f_gen_ << ";" << '\n';
  // pack the arguments into the containing struct that we'll write out over the wire
  // note that this struct is generated even if we have 0 args
  ostringstream struct_definition;
  vector<t_field*> members = tfunc->get_arglist()->get_sorted_members();
  vector<t_field*>::iterator members_iter;
  for (members_iter = members.begin(); members_iter != members.end(); ++members_iter) {
    t_field* member = (*members_iter);
    string member_name(zig_field_name(member));
    struct_definition << "." << member_name << " = " << member_name << ", ";
  }
  string struct_fields = struct_definition.str();
  if (struct_fields.size() > 0) {
    struct_fields = struct_fields.substr(0, struct_fields.size() - 2); // strip trailing comma
  }
  f_gen_ << indent() << "var call_args = " << service_call_args_struct_name(tservice, tfunc) << "{ "
         << struct_fields << " };" << '\n';
  // write everything over the wire
  f_gen_ << indent() << "try self.o_prot.writeMessageBegin(message_ident);" << '\n';
  f_gen_ << indent() << "try call_args.writeToProtocol(self.o_prot);"
         << '\n'; // written even if we have 0 args
  f_gen_ << indent() << "try self.o_prot.writeMessageEnd();" << '\n';
  f_gen_ << indent() << "try self.o_prot.flush();" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

void t_zig_generator::render_sync_recv(t_service* tservice, t_function* tfunc) {
  f_gen_ << indent() << "{" << '\n';
  indent_up();

  f_gen_ << indent() << "var message_ident = try self.i_prot.readMessageBegin(self.allocator);"
         << '\n';
  f_gen_ << indent() << "defer message_ident.deinit();" << '\n';
  f_gen_ << indent()
         << "try thrift.internal.verifyExpectedSequenceNumber(self.sequence_number, "
            "message_ident.sequenceNumber);"
         << '\n';
  f_gen_ << indent() << "try thrift.internal.verifyExpectedServiceCall(&\"" << tfunc->get_name()
         << "\".*, message_ident.name);" << '\n'; // note: use *original* name
  f_gen_ << indent() << "if (message_ident.msgType == TMessageType.Exception) {" << '\n';
  indent_up();
  f_gen_ << indent()
         << "const remote_error = "
            "try TApplicationException.readFromProtocol(self.allocator, self.i_prot);"
         << '\n';
  f_gen_ << indent() << "try self.i_prot.readMessageEnd();" << '\n';
  f_gen_ << indent() << "return .{ .err = .{ .TApplicationException = remote_error } };" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  f_gen_ << indent()
         << "try thrift.internal.verifyExpectedMessageType(TMessageType.Reply, message_ident.msgType);"
         << '\n';
  f_gen_ << indent() << "const result = try ";
  f_gen_ << service_call_result_struct_name(tservice, tfunc)
         << ".readFromProtocol(self.allocator, self.i_prot);" << '\n';
  f_gen_ << indent() << "try self.i_prot.readMessageEnd();" << '\n';
  f_gen_ << indent() << "return .initFromMethodResult(result);" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';
}

string t_zig_generator::zig_sync_service_handler_interface_call_declaration(t_function* tfunc) {
  ostringstream func_args;

  func_args << "(self: *anyopaque";

  if (has_args(tfunc)) {
    func_args << ", "; // put comma after "self"
    func_args << struct_to_declaration(tfunc->get_arglist(), T_ARGS);
  }

  func_args << ")";
  return func_args.str();
}

string t_zig_generator::zig_sync_service_call_declaration(t_function* tfunc, string self_type) {
  ostringstream func_args;

  func_args << "(self: " << self_type;

  if (has_args(tfunc)) {
    func_args << ", "; // put comma after "self"
    func_args << struct_to_declaration(tfunc->get_arglist(), T_ARGS);
  }

  func_args << ")";
  return func_args.str();
}

string t_zig_generator::zig_sync_service_handler_interface_call_invocation(
    t_function* tfunc,
    const string& self_name,
    const string& field_prefix) {
  ostringstream func_args;
  func_args << "(" << self_name;

  if (has_args(tfunc)) {
    func_args << ", ";
    func_args << struct_to_invocation(tfunc->get_arglist(), field_prefix);
  }

  func_args << ")";
  return func_args.str();
}

string t_zig_generator::zig_sync_service_call_invocation(t_function* tfunc,
                                                         const string& field_prefix) {
  ostringstream func_args;
  func_args << "(";

  if (has_args(tfunc)) {
    func_args << struct_to_invocation(tfunc->get_arglist(), field_prefix);
  }

  func_args << ")";
  return func_args.str();
}

string t_zig_generator::struct_to_declaration(t_struct* tstruct,
                                              t_zig_generator::e_struct_type struct_type) {
  ostringstream args;

  bool first_arg = true;
  std::vector<t_field*> fields = tstruct->get_sorted_members();
  std::vector<t_field*>::iterator field_iter;
  for (field_iter = fields.begin(); field_iter != fields.end(); ++field_iter) {
    t_field* tfield = (*field_iter);
    t_field::e_req field_req = actual_field_req(tfield, struct_type);
    string zig_type = to_zig_type(tfield->get_type());
    zig_type
        = is_optional_when_reading_from_proto(field_req) ? "?" + zig_type : zig_type;

    if (first_arg) {
      first_arg = false;
    } else {
      args << ", ";
    }

    args << zig_field_name(tfield) << ": " << zig_type;
  }

  return args.str();
}

string t_zig_generator::struct_to_invocation(t_struct* tstruct, const string& field_prefix) {
  ostringstream args;

  bool first_arg = true;
  std::vector<t_field*> fields = tstruct->get_sorted_members();
  std::vector<t_field*>::iterator field_iter;
  for (field_iter = fields.begin(); field_iter != fields.end(); ++field_iter) {
    t_field* tfield = (*field_iter);

    if (first_arg) {
      first_arg = false;
    } else {
      args << ", ";
    }

    args << field_prefix << zig_field_name(tfield);
  }

  return args.str();
}

void t_zig_generator::render_service_call_args_struct(t_service* tservice, t_function* tfunc) {
  string args_struct_name(service_call_args_struct_name(tservice, tfunc));
  render_struct(args_struct_name, tfunc->get_arglist(), t_zig_generator::T_ARGS);
}

void t_zig_generator::render_service_call_result_error_union(string union_name,
                                                             t_struct* exceptions) {
  f_gen_ << "const " << union_name << " = union(enum) {" << '\n';
  indent_up();

  // Possible values
  const vector<t_field*>& exception_types = exceptions->get_members();
  vector<t_field*>::const_iterator exception_iter;
  for (exception_iter = exception_types.begin(); exception_iter != exception_types.end();
       ++exception_iter) {
    t_field* exception_type = *exception_iter;
    f_gen_ << indent() << zig_union_field_name(exception_type) << ": "
           << to_zig_type(exception_type->get_type()) << "," << '\n';
  }

  f_gen_ << indent() << "pub fn deinit(self: *@This()) void {" << '\n';
  indent_up();
  f_gen_ << indent() << "switch(self.*) {" << '\n';
  indent_up();
  f_gen_ << indent() << "inline else => |*e| { e.deinit(); }" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // read function
  f_gen_ << indent()
         << "pub fn readFromProtocol(allocator: Allocator, fieldId: i16, iProt: *TProtocol) "
            "!@This() {"
         << '\n';
  indent_up();
  f_gen_ << indent() << "switch (fieldId) {" << '\n';
  indent_up();
  for (exception_iter = exception_types.begin(); exception_iter != exception_types.end();
       ++exception_iter) {
    t_field* exception_type = *exception_iter;
    f_gen_ << indent() << exception_type->get_key() << " => {" << '\n';
    indent_up();
    f_gen_ << indent() << "return .{" << '\n';
    indent_up();
    f_gen_ << indent() << "." << zig_union_field_name(exception_type) << " = try "
           << to_zig_type(exception_type->get_type()) << ".readFromProtocol(allocator, iProt),"
           << '\n';
    indent_down();
    f_gen_ << indent() << "};" << '\n';
    indent_down();
    f_gen_ << indent() << "}," << '\n';
  }
  f_gen_ << indent() << "else => {" << '\n';
  indent_up();
  f_gen_ << indent() << "return ProtocolError.UnknownField;" << '\n';
  indent_down();
  f_gen_ << indent() << "}," << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // write function
  f_gen_ << indent() << "pub fn writeToProtocol(self: *const @This(), oProt: *TProtocol) !void {"
         << '\n';
  indent_up();
  f_gen_ << indent() << "switch (self.*) {" << '\n';
  indent_up();
  for (exception_iter = exception_types.begin(); exception_iter != exception_types.end();
       ++exception_iter) {
    t_field* exception_type = *exception_iter;
    f_gen_ << indent() << "." << zig_union_field_name(exception_type) << " => |*eVal| {" << '\n';
    indent_up();
    f_gen_ << indent() << "try oProt.writeFieldBegin(";
    field_identifier_creation("undefined", exception_type->get_name(), "TType.Struct",
                              std::to_string(exception_type->get_key()));
    f_gen_ << ");" << '\n';
    f_gen_ << indent() << "try eVal.writeToProtocol(oProt);" << '\n';
    f_gen_ << indent() << "try oProt.writeFieldEnd();" << '\n';
    // end case
    indent_down();
    f_gen_ << indent() << "}," << '\n';
  }
  // end switch
  indent_down();
  f_gen_ << indent() << "}" << '\n';
  // end function
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  indent_down();
  f_gen_ << "};" << '\n' << '\n';
}

void t_zig_generator::render_service_call_result_value_struct(t_service* tservice,
                                                              t_function* tfunc) {
  string result_struct_name = service_call_result_struct_name(tservice, tfunc);
  string result_value_name = to_zig_type(tfunc->get_returntype());
  string result_error_union_name = result_struct_name + "Error";

  t_struct* exceptions = tfunc->get_xceptions();
  const vector<t_field*>& exception_types = exceptions->get_members();
  const auto hasExceptions = !exception_types.empty();
  if (hasExceptions) {
    render_service_call_result_error_union(result_error_union_name, exceptions);
  }

  f_gen_ << "pub const " << result_struct_name << " = thrift.ServiceCallResult(";
  f_gen_ << result_value_name << ", ";
  if (hasExceptions) {
    f_gen_ << result_error_union_name;
  } else {
    f_gen_ << "void";
  }
  f_gen_ << ", &\"" << result_struct_name << "\".*);" << '\n' << '\n';
}

//-----------------------------------------------------------------------------
//
// Sync Processor
//
//-----------------------------------------------------------------------------

void t_zig_generator::render_sync_processor(t_service* tservice) {
  render_type_comment(tservice->get_name() + " service processor"); // note: use *original* name
  render_sync_handler_interface(tservice);
  render_sync_processor_definition_and_impl(tservice);
}

void t_zig_generator::render_sync_handler_interface(t_service* tservice) {
  const std::vector<std::pair<t_service*, t_function*>> functions = [&tservice]() {
    std::vector<std::pair<t_service*, t_function*>> functions;

    for (auto f : tservice->get_functions()) {
      functions.emplace_back(tservice, f);
    }

    if (tservice->get_extends() != nullptr) {
      t_service* extends = tservice->get_extends();
      for (auto f : extends->get_functions()) {
        functions.emplace_back(extends, f);
      }
    }

    return functions;
  }();
  std::vector<std::pair<t_service*, t_function*>>::const_iterator func_iter;

  render_zigdoc((t_doc*)tservice);
  f_gen_ << "pub const " << zig_sync_handler_interface_name(tservice) << " = struct {" << '\n';
  indent_up();
  f_gen_ << indent() << "ptr: *anyopaque," << '\n';
  // Fn pointers
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter).second;
    string func_name = service_call_handler_function_name(tfunc);
    string func_args = zig_sync_service_handler_interface_call_declaration(tfunc);
    string func_return
        = tfunc->is_oneway() ? "void" : service_call_result_struct_name(func_iter->first, tfunc);
    f_gen_ << indent() << func_name << "Fn: *const fn" << func_args << " anyerror!" << func_return
           << "," << '\n';
  }

  // init function
  f_gen_ << indent() << "pub fn init(ptr: anytype) @This() {" << '\n';
  indent_up();
  f_gen_ << indent() << "const T = @TypeOf(ptr);" << '\n';
  f_gen_ << indent() << "const ptr_info = @typeInfo(T);" << '\n';

  f_gen_ << indent() << "const gen = struct {" << '\n';
  indent_up();
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = func_iter->second;
    string func_name = service_call_handler_function_name(tfunc);
    string func_args = zig_sync_service_call_declaration(tfunc, "*anyopaque");
    string call_parameters
        = zig_sync_service_handler_interface_call_invocation(tfunc, "realSelf", "");
    string func_return
        = tfunc->is_oneway() ? "void" : service_call_result_struct_name(func_iter->first, tfunc);
    f_gen_ << indent() << "fn " << func_name << func_args << " !" << func_return << " {" << '\n';
    indent_up();
    /* const self: T = @ptrCast(@alignCast(pointer));
       return ptr_info.@"pointer".child.deinit(self); */
    f_gen_ << indent() << "const realSelf: T = @ptrCast(@alignCast(self));" << '\n';
    f_gen_ << indent() << "return try ptr_info.@\"pointer\".child." << func_name << call_parameters
           << ';' << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  }
  // end gen struct
  indent_down();
  f_gen_ << indent() << "};" << '\n';

  f_gen_ << indent() << "return .{" << '\n';
  indent_up();
  f_gen_ << indent() << ".ptr = ptr," << '\n';
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = func_iter->second;
    string func_name = service_call_handler_function_name(tfunc);
    f_gen_ << indent() << "." << func_name << "Fn" << " = gen." << func_name << ',' << '\n';
  }
  // end return
  indent_down();
  f_gen_ << indent() << "};" << '\n';

  // end init
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // Methods
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = func_iter->second;
    string func_name = service_call_handler_function_name(tfunc);
    string func_args = zig_sync_service_call_declaration(tfunc);
    string call_parameters
        = zig_sync_service_handler_interface_call_invocation(tfunc, "self.ptr", "");
    string func_return
        = tfunc->is_oneway() ? "void" : service_call_result_struct_name(func_iter->first, tfunc);
    ;
    render_zigdoc((t_doc*)tfunc);
    f_gen_ << indent() << "fn " << func_name << func_args << " !" << func_return << " {" << '\n';
    indent_up();
    f_gen_ << indent() << "return try self." << func_name << "Fn" << call_parameters << ";" << '\n';
    indent_down();
    f_gen_ << indent() << "}" << '\n';
  }
  indent_down();
  f_gen_ << indent() << "};" << '\n';
  f_gen_ << '\n';
}

void t_zig_generator::render_sync_processor_definition_and_impl(t_service* tservice) {
  string service_processor_name = zig_sync_processor_name(tservice);
  string handler_interface_name = zig_sync_handler_interface_name(tservice);

  // struct
  f_gen_ << indent() << "pub const " << service_processor_name << " = struct {" << '\n';
  indent_up();
  f_gen_ << indent() << "allocator: Allocator" << ",\n";
  f_gen_ << indent() << "handler: *" << handler_interface_name << ",\n";

  {
    vector<t_function*> functions = tservice->get_functions();
    vector<t_function*>::iterator func_iter;
    for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
      t_function* tfunc = (*func_iter);
      render_sync_process_function(tservice, tfunc, handler_interface_name);
    }
    if (tservice->get_extends() != nullptr) {
      t_service* extends = tservice->get_extends();
      functions = extends->get_functions();
      for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
        t_function* tfunc = (*func_iter);
        render_sync_process_function(extends, tfunc, handler_interface_name);
      }
    }
  }

  f_gen_ << '\n';

  f_gen_ << indent()
         << "pub fn process(self: *@This(), i_prot: *TProtocol, o_prot: *TProtocol) "
            "!void {"
         << '\n';
  indent_up();

  f_gen_ << indent() << "var message_ident = try i_prot.readMessageBegin(self.allocator);" << '\n';
  f_gen_ << indent() << "defer message_ident.deinit();" << '\n';

  f_gen_ << indent() << "const res = result: {" << '\n'; // [sigh] explicit deref coercion
  indent_up();
  f_gen_ << indent();
  render_process_match_statements(tservice);
  f_gen_ << "{" << '\n';
  indent_up();
  render_thrift_error("Application", "UnknownMethod");
  indent_down();
  f_gen_ << indent() << "}" << '\n';

  indent_down();
  f_gen_ << indent() << "};" << '\n';
  f_gen_ << indent()
         << "try thrift.server.handleProcessResult(self.allocator, message_ident, res, o_prot);"
         << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';

  // interface fn
  f_gen_ << indent() << "pub fn interface(self: *@This()) thrift.TProcessor {" << '\n';
  indent_up();

  f_gen_ << indent() << "return .init(self);" << '\n';

  indent_down();
  f_gen_ << indent() << "}" << '\n';

  indent_down();
  f_gen_ << indent() << "};" << '\n';
  f_gen_ << '\n';
}

void t_zig_generator::render_process_match_statements(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator func_iter;
  for (func_iter = functions.begin(); func_iter != functions.end(); ++func_iter) {
    t_function* tfunc = (*func_iter);
    f_gen_ << "if (std.mem.eql(u8, message_ident.name, \"" << tfunc->get_name() << "\")) {"
           << '\n'; // note: use *original* name
    indent_up();
    f_gen_ << indent() << "break :result self.process_" << zig_snake_case(tfunc->get_name())
           << "(message_ident.sequenceNumber, i_prot, o_prot);" << '\n';
    indent_down();
    f_gen_ << indent() << "} else ";
  }

  t_service* extends = tservice->get_extends();
  if (extends) {
    render_process_match_statements(extends);
  }
}

void t_zig_generator::render_sync_process_function(t_service* tservice,
                                                   t_function* tfunc,
                                                   const string& handler_type) {
  (void)handler_type;

  string sequence_number_param("incoming_sequence_number");
  string output_protocol_param("o_prot");
  string method_result_type = service_call_result_struct_name(tservice, tfunc);

  if (tfunc->is_oneway()) {
    sequence_number_param = "_";
    output_protocol_param = "_";
  }

  f_gen_ << indent() << "pub fn process_" << zig_snake_case(tfunc->get_name())
         << "(self: *@This(), " << sequence_number_param << ": i32, "
         << "i_prot: *TProtocol, " << output_protocol_param << ": *TProtocol) "
         << "!void {" << '\n';

  indent_up();

  // *always* read arguments from the input protocol
  f_gen_ << indent() << (has_non_void_args(tfunc) ? "var args" : "_") << " = try "
         << service_call_args_struct_name(tservice, tfunc)
         << ".readFromProtocol(self.allocator, i_prot);" << '\n';

  f_gen_ << indent() << "try i_prot.readMessageEnd();" << '\n';

  if (has_non_void_args(tfunc)) {
    f_gen_ << indent() << "defer args.deinit();" << '\n';
  }

  f_gen_ << indent() << (tfunc->is_oneway() ? "" : "var result = ") << "try self.handler."
         << service_call_handler_function_name(tfunc)
         << zig_sync_service_call_invocation(tfunc, "args.") << ";" << '\n';

  if (!tfunc->is_oneway()) {
    f_gen_ << indent() << "defer result.deinit();" << '\n';
    f_gen_ << indent() << "const message_ident = ";
    message_identifier_creation("self.allocator", tfunc->get_name(), "Reply",
                                "incoming_sequence_number");
    f_gen_ << ";" << '\n';
    f_gen_ << indent() << "try o_prot.writeMessageBegin(message_ident);" << '\n';
    f_gen_ << indent() << "try result.writeToProtocol(o_prot);" << '\n';
    f_gen_ << indent() << "try o_prot.writeMessageEnd();" << '\n';
    f_gen_ << indent() << "try o_prot.flush();" << '\n';
  }

  indent_down();
  f_gen_ << indent() << "}" << '\n'; // end function
}

void t_zig_generator::render_sync_handler_succeeded(t_function* tfunc) {
  if (tfunc->is_oneway()) {
    return;
  } else {
    f_gen_ << indent() << "const message_ident = ";
    message_identifier_creation("self.allocator", tfunc->get_name(), "Reply",
                                "incoming_sequence_number");
    f_gen_ << ";" << '\n';
    f_gen_ << indent() << "try o_prot.writeMessageBegin(message_ident);" << '\n';
    f_gen_ << indent() << "try result.writeToProtocol(o_prot);" << '\n';
    f_gen_ << indent() << "try o_prot.writeMessageEnd();" << '\n';
    f_gen_ << indent() << "try o_prot.flush();" << '\n';
  }
}

//-----------------------------------------------------------------------------
//
// Utility
//
//-----------------------------------------------------------------------------

void t_zig_generator::render_struct_member_empty_allocation(t_type* ttype) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        f_gen_ << "thrift.BinaryBytes.initDefault(allocator)";
      } else {
        f_gen_ << "thrift.String.initDefault(allocator)";
      }
      break;
    default:
      throw "compiler error: unhandled type empty alloc";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_struct_member_empty_allocation(ttypedef->get_type());
  } else if (ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << to_zig_type(ttype) << ".initDefault(allocator)";
  } else if (ttype->is_map() || ttype->is_set() || ttype->is_list()) {
    f_gen_ << to_zig_type(ttype) << ".init(allocator)";
  } else {
    throw "compiler error: unhandled type empty alloc";
  }
}

void t_zig_generator::render_variable_default_value(t_type* ttype) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      f_gen_ << "{}";
      return;
    case t_base_type::TYPE_STRING:
      render_struct_member_empty_allocation(ttype);
      return;
    case t_base_type::TYPE_UUID:
      f_gen_ << ".empty";
      return;
    case t_base_type::TYPE_BOOL:
      f_gen_ << "false";
      return;
    case t_base_type::TYPE_I8:
      f_gen_ << "0";
      return;
    case t_base_type::TYPE_I16:
      f_gen_ << "0";
      return;
    case t_base_type::TYPE_I32:
      f_gen_ << "0";
      return;
    case t_base_type::TYPE_I64:
      f_gen_ << "0";
      return;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << "0.0";
      return;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_variable_default_value(ttypedef->get_type());
    return;
  } else if (ttype->is_enum()) {
    t_enum* e = (t_enum*)ttype;
    auto enum_constants = e->get_constants();
    if (enum_constants.empty()) {
      f_gen_ << "@enumFromInt(0)";
    } else {
      f_gen_ << "." << enum_constants[0]->get_name();
    }
    return;
  } else if (ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << to_zig_type(ttype) << ".initDefault(allocator)";
    return;
  } else if (ttype->is_map() || ttype->is_set() || ttype->is_list()) {
    render_struct_member_empty_allocation(ttype);
    return;
  } else {
    throw "compiler error: unhandled type" + ttype->get_name();
  }
}

void t_zig_generator::render_struct_member_equality_comparison(t_type* ttype, const string& member_name, const string& self_prefix, const string& other_prefix) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      return;
    case t_base_type::TYPE_STRING:
    case t_base_type::TYPE_UUID:
      f_gen_ << self_prefix << member_name << ".eql(" << other_prefix << member_name << ")";
      return;
    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << self_prefix << member_name << " == " << other_prefix << member_name;
      return;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    render_struct_member_equality_comparison(ttypedef->get_type(), member_name, self_prefix, other_prefix);
    return;
  } else if (ttype->is_enum()) {
    f_gen_ << self_prefix << member_name << " == " << other_prefix << member_name;
    return;
  } else if (ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << self_prefix << member_name << ".eql(" << other_prefix << member_name << ")";
    return;
  } else if (ttype->is_map() || ttype->is_set() || ttype->is_list()) {
    f_gen_ << self_prefix << member_name << ".eql(" << other_prefix << member_name << ")";
    return;
  } else {
    throw "compiler error: unhandled type" + ttype->get_name();
  }
}

bool t_zig_generator::render_struct_member_clone_invocation(t_type* ttype, const string& member_name, const string& self_prefix) {
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      f_gen_ << "{}";
      return false;
    case t_base_type::TYPE_STRING:
    case t_base_type::TYPE_UUID:
      f_gen_ << "try " << self_prefix << member_name << ".clone(allocator)";
      return true;
    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << self_prefix << member_name;
      return false;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    return render_struct_member_clone_invocation(ttypedef->get_type(), member_name, self_prefix);
  } else if (ttype->is_enum()) {
    f_gen_ << self_prefix << member_name;
    return false;
  } else if (ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << "try " << self_prefix << member_name << ".clone(allocator)";
    return true;
  } else if (ttype->is_map() || ttype->is_set() || ttype->is_list()) {
    f_gen_ << "try " << self_prefix << member_name << ".clone(allocator)";
    return true;
  } else {
    throw "compiler error: unhandled type" + ttype->get_name();
  }
}

void t_zig_generator::render_struct_sync_read_field_assignment(t_field* member,
                                                               const string& var_name) {
  string member_name(zig_field_name(member));
  string temp_var = "read_" + member_name;
  t_type* true_type = get_true_type(member->get_type());

  f_gen_ << indent() << "const " << temp_var << " = ";
  if (true_type->is_base_type()) {
    render_type_sync_read(member->get_type());
  } else {
    f_gen_ << "try " << to_zig_type(member->get_type()) << ".readFromProtocol(allocator, i_prot)";
  }
  f_gen_ << ";" << '\n';
  f_gen_ << indent();
  render_struct_member_deallocation(member, var_name);
  f_gen_ << '\n';
  f_gen_ << indent() << var_name << "." << member_name << " = " << temp_var << ";" << '\n';
  f_gen_ << indent() << var_name << ".__isset." << member_name << " = true;" << '\n';
}

void t_zig_generator::render_struct_member_deallocation(t_field* member, const string& var_name) {
  string member_name(zig_field_name(member));
  t_type* ttype = get_true_type(member->get_type());

  if (member->get_req() == t_field::T_OPTIONAL) {
    f_gen_ << "if (" << var_name << "." << member_name << ") |" << member_name << "| {" << '\n';
    indent_up();
    f_gen_ << indent();
  } else {
    member_name = var_name + "." + member_name;
  }

  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_STRING:
      f_gen_ << member_name << ".deinit();";
      break;
    default:
      throw "compiler error: unhandled type empty alloc";
    }
  } else if (ttype->is_struct()) {
    f_gen_ << member_name << ".deinit();";
  } else if (ttype->is_xception()) {
    f_gen_ << member_name << ".deinit();";
  } else if (ttype->is_map()) {
    f_gen_ << member_name << ".deinit();";
  } else if (ttype->is_set()) {
    f_gen_ << member_name << ".deinit();";
  } else if (ttype->is_list()) {
    f_gen_ << member_name << ".deinit();";
  } else {
    throw "compiler error: unhandled type empty alloc";
  }

  if (member->get_req() == t_field::T_OPTIONAL) {
    f_gen_ << '\n';
    indent_down();
    f_gen_ << indent() << "}";
  }
}

void t_zig_generator::render_type_comment(const string& type_name) {
  f_gen_ << "//" << '\n';
  f_gen_ << "// " << type_name << '\n';
  f_gen_ << "//" << '\n';
  f_gen_ << '\n';
}

void t_zig_generator::render_zigdoc(t_doc* tdoc) {
  if (!tdoc->has_doc()) {
    return;
  }

  generate_docstring_comment(f_gen_, "", "// ", tdoc->get_doc(), "");
}

void t_zig_generator::render_thrift_error(const string& error_kind, const string& error_name) {
  f_gen_ << indent() << "return " << error_kind << "Error." << error_name << ';' << '\n';
}

bool t_zig_generator::is_double(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    if (tbase == t_base_type::TYPE_DOUBLE) {
      return true;
    }
  }

  return false;
}

string t_zig_generator::type_format_string(t_field* tfield) {
  ostringstream ss;
  if (tfield->get_req() == t_field::T_OPTIONAL) {
    ss << "?";
  }
  auto ttype = get_true_type(tfield->get_type());

  if (ttype->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)ttype;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      ss << "s";
      break;
    case t_base_type::TYPE_STRING:
    case t_base_type::TYPE_UUID:
      ss << "f";
      break;
    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      f_gen_ << "";
      break;
    case t_base_type::TYPE_DOUBLE:
      f_gen_ << "f";
      break;
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_enum()) {
    f_gen_ << "f";
  } else if (ttype->is_struct() || ttype->is_xception()) {
    f_gen_ << "f";
  } else if (ttype->is_map() || ttype->is_set() || ttype->is_list()) {
    f_gen_ << "f";
  } else {
    throw "compiler error: unhandled type" + ttype->get_name();
  }
  return ss.str();
}

string t_zig_generator::to_zig_type(t_type* ttype) {
  // ttype = get_true_type(ttype); <-- recurses through as many typedef layers as necessary
  if (ttype->is_base_type()) {
    t_base_type* tbase_type = ((t_base_type*)ttype);
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_VOID:
      return "void";
    case t_base_type::TYPE_STRING:
      if (tbase_type->is_binary()) {
        return "thrift.BinaryBytes";
      } else {
        return "thrift.String";
      }
    case t_base_type::TYPE_UUID:
      return "thrift.UUID";
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
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_typedef()) {
    t_typedef* ttypedef = (t_typedef*)ttype;
    string zig_type = zig_namespace(ttype) + ttypedef->get_symbolic();
    return zig_type;
  } else if (ttype->is_enum()) {
    return zig_namespace(ttype) + ttype->get_name();
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return zig_namespace(ttype) + ttype->get_name();
  } else if (ttype->is_map()) {
    t_map* tmap = (t_map*)ttype;
    return "thrift.Map(" + to_zig_type(tmap->get_key_type()) + ", " + to_zig_type(tmap->get_val_type()) + ")";
  } else if (ttype->is_set()) {
    t_set* tset = (t_set*)ttype;
    return "thrift.Set(" + to_zig_type(tset->get_elem_type()) + ")";
  } else if (ttype->is_list()) {
    t_list* tlist = (t_list*)ttype;
    return "thrift.List(" + to_zig_type(tlist->get_elem_type()) + ")";
  }

  throw "cannot find zig type for " + ttype->get_name();
}

string t_zig_generator::to_zig_field_type_enum(t_type* ttype) {
  ttype = get_true_type(ttype);
  if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "will not generate protocol::TType for TYPE_VOID";
    case t_base_type::TYPE_STRING: // both strings and binary are actually encoded as TType::String
      return "TType.String";
    case t_base_type::TYPE_UUID:
      return "TType.Uuid";
    case t_base_type::TYPE_BOOL:
      return "TType.Bool";
    case t_base_type::TYPE_I8:
      return "TType.I8";
    case t_base_type::TYPE_I16:
      return "TType.I16";
    case t_base_type::TYPE_I32:
      return "TType.I32";
    case t_base_type::TYPE_I64:
      return "TType.I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType.Double";
    default:
      throw "compiler error: unhandled type";
    }
  } else if (ttype->is_enum()) {
    return "TType.I32";
  } else if (ttype->is_struct() || ttype->is_xception()) {
    return "TType.Struct";
  } else if (ttype->is_map()) {
    return "TType.Map";
  } else if (ttype->is_set()) {
    return "TType.Set";
  } else if (ttype->is_list()) {
    return "TType.List";
  }

  throw "cannot find TType for " + ttype->get_name();
}

bool t_zig_generator::can_generate_simple_const(t_type* ttype) {
  t_type* actual_type = get_true_type(ttype);
  if (actual_type->is_enum()) {
    return true;
  }
  if (actual_type->is_base_type()) {
    t_base_type* tbase_type = (t_base_type*)actual_type;
    switch (tbase_type->get_base()) {
    case t_base_type::TYPE_BOOL:
    case t_base_type::TYPE_I8:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
    case t_base_type::TYPE_DOUBLE:
    case t_base_type::TYPE_UUID:
      return true;
    case t_base_type::TYPE_STRING:
      return false;
    default:
      return false;
    }
  }
  return false;
}

bool t_zig_generator::can_generate_const_holder(t_type* ttype) {
  t_type* actual_type = get_true_type(ttype);
  return !can_generate_simple_const(actual_type) && !actual_type->is_service();
}

bool t_zig_generator::is_void(t_type* ttype) {
  return ttype->is_base_type() && ((t_base_type*)ttype)->get_base() == t_base_type::TYPE_VOID;
}

bool t_zig_generator::is_optional_when_reading_from_proto(t_field::e_req req) {
  return req == t_field::T_OPTIONAL || req == t_field::T_OPT_IN_REQ_OUT;
}

t_field::e_req t_zig_generator::actual_field_req(t_field* tfield,
                                                 t_zig_generator::e_struct_type struct_type) {
  return struct_type == t_zig_generator::T_ARGS ? t_field::T_REQUIRED : tfield->get_req();
}

bool t_zig_generator::has_args(t_function* tfunc) {
  return tfunc->get_arglist() != nullptr && !tfunc->get_arglist()->get_sorted_members().empty();
}

bool t_zig_generator::has_non_void_args(t_function* tfunc) {
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

string t_zig_generator::visibility_qualifier(t_zig_generator::e_struct_type struct_type) {
  switch (struct_type) {
  case t_zig_generator::T_ARGS:
  case t_zig_generator::T_RESULT:
    return "";
  default:
    return "pub ";
  }
}

string t_zig_generator::zig_namespace(t_service* tservice) {
  if (tservice->get_program()->get_name() != get_program()->get_name()) {
    return zig_snake_case(tservice->get_program()->get_name()) + ".";
  } else {
    return "";
  }
}

string t_zig_generator::zig_namespace(t_type* ttype) {
  if (ttype->get_program()->get_name() != get_program()->get_name()) {
    return zig_snake_case(ttype->get_program()->get_name()) + ".";
  } else {
    return "";
  }
}

bool t_zig_generator::is_reserved(const string& name) {
  return ZIG_RESERVED_WORDS_SET.find(name) != ZIG_RESERVED_WORDS_SET.end();
}

string t_zig_generator::zig_struct_nameb(t_struct* tstruct) {
  return zig_safe_name(tstruct->get_name());
}

string t_zig_generator::zig_field_name(t_field* tfield) {
  return zig_safe_name(tfield->get_name());
}

string t_zig_generator::zig_union_field_name(t_field* tfield) {
  return zig_safe_name(tfield->get_name());
}

string t_zig_generator::zig_safe_name(const string& name) {
  if (is_reserved(name)) {
    return name + "_";
  } else {
    return name;
  }
}

string t_zig_generator::service_call_client_function_name(t_function* tfunc) {
  return tfunc->get_name();
}

string t_zig_generator::service_call_handler_function_name(t_function* tfunc) {
  return tfunc->get_name();
}

string t_zig_generator::client_call_result_struct_name(t_service* tservice, t_function* tfunc) {
  return zig_namespace(tservice) + zig_camel_case(tservice->get_name())
         + zig_camel_case(tfunc->get_name()) + CLIENT_RESULT_STRUCT_SUFFIX;
}

string t_zig_generator::service_call_args_struct_name(t_service* tservice, t_function* tfunc) {
  // Thrift automatically appends `Args` to the arglist name. No need to do it here.
  return zig_namespace(tservice) + zig_camel_case(tservice->get_name())
         + zig_camel_case(tfunc->get_arglist()->get_name());
}

string t_zig_generator::service_call_result_struct_name(t_service* tservice, t_function* tfunc) {
  return zig_namespace(tservice) + zig_camel_case(tservice->get_name())
         + zig_camel_case(tfunc->get_name()) + RESULT_STRUCT_SUFFIX;
}

string t_zig_generator::zig_sync_client_trait_name(t_service* tservice) {
  return "T" + zig_camel_case(tservice->get_name()) + "Client";
}

string t_zig_generator::zig_sync_handler_interface_name(t_service* tservice) {
  return zig_camel_case(tservice->get_name()) + "Handler";
}

string t_zig_generator::zig_sync_processor_name(t_service* tservice) {
  return zig_camel_case(tservice->get_name()) + "Processor";
}

string t_zig_generator::zig_upper_case(const string& name) {
  bool all_uppercase = true;

  for (char i : name) {
    if (isalpha(i) && islower(i)) {
      all_uppercase = false;
      break;
    }
  }

  if (all_uppercase) {
    return name;
  } else {
    string str(uppercase(underscore(name)));
    string_replace(str, "__", "_");
    return str;
  }
}

string t_zig_generator::zig_snake_case(const string& name) {
  string str(decapitalize(underscore(name)));
  string_replace(str, "__", "_");
  return str;
}

string t_zig_generator::zig_camel_case(const string& name) {
  string str(capitalize(camelcase(name)));
  string_replace(str, "_", "");
  return str;
}

string t_zig_generator::zig_safe_field_id(int32_t id) {
  string id_str = std::to_string(abs(id));
  if (id >= 0) {
    return id_str;
  } else {
    string str("-");
    str += id_str;
    return str;
  }
}

void t_zig_generator::string_replace(string& target,
                                     const string& search_string,
                                     const string& replace_string) {
  if (target.empty()) {
    return;
  }

  size_t match_len = search_string.length();
  size_t replace_len = replace_string.length();

  size_t search_idx = 0;
  size_t match_idx;
  while ((match_idx = target.find(search_string, search_idx)) != string::npos) {
    target.replace(match_idx, match_len, replace_string);
    search_idx = match_idx + replace_len;
  }
}

std::string t_zig_generator::display_name() const {
  return "Zig";
}

THRIFT_REGISTER_GENERATOR(zig, "Zig", "\n")
