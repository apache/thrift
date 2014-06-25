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
#include <sstream>
#include "t_oop_generator.h"
#include "platform.h"

using std::map;
using std::ofstream;
using std::ostringstream;
using std::string;
using std::stringstream;
using std::vector;

static const string endl = "\n";  // avoid ostream << std::endl flushes

/**
 * Objective-C code generator.
 *
 * mostly copy/pasting/tweaking from mcslee's work.
 */
class t_cocoa_generator : public t_oop_generator {
 public:
  t_cocoa_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_oop_generator(program)
  {
    (void) option_string;
    std::map<std::string, std::string>::const_iterator iter;
    
    iter = parsed_options.find("log_unexpected");
    log_unexpected_ = (iter != parsed_options.end());    
    
    iter = parsed_options.find("validate_required");
    validate_required_ = (iter != parsed_options.end());    
    
    out_dir_base_ = "gen-cocoa";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  void generate_consts(std::vector<t_const*> consts);

  /**
   * Program-level generation functions
   */

  void generate_typedef (t_typedef*  ttypedef);
  void generate_enum    (t_enum*     tenum);
  void generate_struct  (t_struct*   tstruct);
  void generate_xception(t_struct*   txception);
  void generate_service (t_service*  tservice);

  void print_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value, bool defval=false, bool is_property=false);
  std::string render_const_value(ofstream& out, t_type* type, t_const_value* value, bool containerize_it=false);

  void generate_cocoa_struct(t_struct* tstruct, bool is_exception);
  void generate_cocoa_struct_interface(std::ofstream& out, t_struct* tstruct, bool is_xception=false);
  void generate_cocoa_struct_implementation(std::ofstream& out, t_struct* tstruct, bool is_xception=false, bool is_result=false);
  void generate_cocoa_struct_initializer_signature(std::ofstream& out,
                                                   t_struct* tstruct);
  void generate_cocoa_struct_init_with_coder_method(ofstream &out,
                                                    t_struct* tstruct,
                                                    bool is_exception);
  void generate_cocoa_struct_encode_with_coder_method(ofstream &out,
                                                    t_struct* tstruct,
                                                    bool is_exception);
  void generate_cocoa_struct_field_accessor_declarations(std::ofstream& out,
                                                         t_struct* tstruct,
                                                         bool is_exception);
  void generate_cocoa_struct_field_accessor_implementations(std::ofstream& out,
                                                            t_struct* tstruct,
                                                            bool is_exception);
  void generate_cocoa_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_result_writer(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_validator(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_description(std::ofstream& out, t_struct* tstruct);

  std::string function_result_helper_struct_type(t_function* tfunction);
  std::string function_args_helper_struct_type(t_function* tfunction);
  void generate_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_cocoa_service_protocol (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_client_interface (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_client_implementation (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_server_interface (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_server_implementation (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_helpers   (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string fieldName);

  void generate_deserialize_struct       (std::ofstream& out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_deserialize_container    (std::ofstream& out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_deserialize_set_element  (std::ofstream& out,
                                          t_set*      tset,
                                          std::string prefix="");

  void generate_deserialize_map_element  (std::ofstream& out,
                                          t_map*      tmap,
                                          std::string prefix="");

  void generate_deserialize_list_element (std::ofstream& out,
                                          t_list*     tlist,
                                          std::string prefix="");

  void generate_serialize_field          (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string prefix="");

  void generate_serialize_struct         (std::ofstream& out,
                                          t_struct*   tstruct,
                                          std::string fieldName="");

  void generate_serialize_container      (std::ofstream& out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (std::ofstream& out,
                                          t_map*      tmap,
                                          std::string iter,
                                          std::string map);

  void generate_serialize_set_element    (std::ofstream& out,
                                          t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (std::ofstream& out,
                                          t_list*     tlist,
                                          std::string index,
                                          std::string listName);

  /**
   * Helper rendering functions
   */

  std::string cocoa_prefix();
  std::string cocoa_imports();
  std::string cocoa_thrift_imports();
  std::string type_name(t_type* ttype, bool class_ref=false);
  std::string base_type_name(t_base_type* tbase);
  std::string declare_field(t_field* tfield);
  std::string declare_property(t_field* tfield);
  std::string function_signature(t_function* tfunction);
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);
  std::string format_string_for_type(t_type* type);
  std::string call_field_setter(t_field* tfield, std::string fieldName);
  std::string containerize(t_type * ttype, std::string fieldName);
  std::string decontainerize(t_field * tfield, std::string fieldName);

  bool type_can_be_null(t_type* ttype) {
    ttype = get_true_type(ttype);

    return
      ttype->is_container() ||
      ttype->is_struct() ||
      ttype->is_xception() ||
      ttype->is_string();
  }

 private:

  std::string cocoa_prefix_;
  std::string constants_declarations_;

  /**
   * File streams
   */

  std::ofstream f_header_;
  std::ofstream f_impl_;

  bool log_unexpected_;
  bool validate_required_;
};


/**
 * Prepares for file generation by opening up the necessary file output
 * streams.
 */
void t_cocoa_generator::init_generator() {
  // Make output directory
  MKDIR(get_out_dir().c_str());
  cocoa_prefix_ = program_->get_namespace("cocoa");

  // we have a .h header file...
  string f_header_name = program_name_+".h";
  string f_header_fullname = get_out_dir()+f_header_name;
  f_header_.open(f_header_fullname.c_str());

  f_header_ <<
    autogen_comment() <<
    endl;

  f_header_ <<
    cocoa_imports() <<
    cocoa_thrift_imports();

  // ...and a .m implementation file
  string f_impl_name = get_out_dir()+program_name_+".m";
  f_impl_.open(f_impl_name.c_str());

  f_impl_ <<
    autogen_comment() <<
    endl;

  f_impl_ <<
    cocoa_imports() <<
    cocoa_thrift_imports() <<
    "#import \"" << f_header_name << "\"" << endl <<
    endl;

}

/**
 * Prints standard Cocoa imports
 *
 * @return List of imports for Cocoa libraries
 */
string t_cocoa_generator::cocoa_imports() {
  return
    string() +
    "#import <Foundation/Foundation.h>\n" +
    "\n";
}

/**
 * Prints thrift runtime imports
 *
 * @return List of imports necessary for thrift runtime
 */
string t_cocoa_generator::cocoa_thrift_imports() {
  string result = string() +
    "#import \"TProtocol.h\"\n" +
    "#import \"TApplicationException.h\"\n" +
    "#import \"TProtocolException.h\"\n" +
    "#import \"TProtocolUtil.h\"\n" +
    "#import \"TProcessor.h\"\n" +
    "#import \"TObjective-C.h\"\n" +
    "#import \"TBase.h\"\n" +
    "\n";

  // Include other Thrift includes
  const vector<t_program*>& includes = program_->get_includes();
  for (size_t i = 0; i < includes.size(); ++i) {
    result += "#import \"" + includes[i]->get_name() + ".h\"" + "\n";
  }
  result += "\n";

  return result;
}


/**
 * Finish up generation.
 */
void t_cocoa_generator::close_generator()
{
  // stick our constants declarations at the end of the header file
  // since they refer to things we are defining.
  f_header_ << constants_declarations_ << endl;
}

/**
 * Generates a typedef. This is just a simple 1-liner in objective-c
 *
 * @param ttypedef The type definition
 */
void t_cocoa_generator::generate_typedef(t_typedef* ttypedef) {
  f_header_ <<
    indent() << "typedef " << type_name(ttypedef->get_type()) << " " << cocoa_prefix_ << ttypedef->get_symbolic() << ";" << endl <<
    endl;
}

/**
 * Generates code for an enumerated type. In Objective-C, this is
 * essentially the same as the thrift definition itself, using the
 * enum keyword in Objective-C.  For namespace purposes, the name of
 * the enum plus an underscore is prefixed onto each element.
 *
 * @param tenum The enumeration
 */
void t_cocoa_generator::generate_enum(t_enum* tenum) {
  f_header_ <<
    indent() << "enum " << cocoa_prefix_ << tenum->get_name() << " {" << endl;
  indent_up();

  vector<t_enum_value*> constants = tenum->get_constants();
  vector<t_enum_value*>::iterator c_iter;
  bool first = true;
  for (c_iter = constants.begin(); c_iter != constants.end(); ++c_iter) {
    if (first) {
      first = false;
    } else {
      f_header_ <<
        "," << endl;
    }
    f_header_ <<
      indent() << tenum->get_name() << "_" << (*c_iter)->get_name();
    f_header_ <<
      " = " << (*c_iter)->get_value();
  }

  indent_down();
  f_header_ <<
    endl <<
    "};" << endl <<
    endl;
}

/**
 * Generates a class that holds all the constants.  Primitive values
 * could have been placed outside this class, but I just put
 * everything in for consistency.
 */
void t_cocoa_generator::generate_consts(std::vector<t_const*> consts) {
  std::ostringstream const_interface;
  string constants_class_name = cocoa_prefix_ + program_name_ + "Constants";

  const_interface << "@interface " << constants_class_name << " : NSObject ";
  scope_up(const_interface);
  scope_down(const_interface);

  // getter method for each constant defined.
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    string name = (*c_iter)->get_name();
    t_type* type = (*c_iter)->get_type();
    const_interface <<
      "+ (" << type_name(type) << ") " << name << ";" << endl;
  }

  const_interface << "@end";

  // this gets spit into the header file in ::close_generator
  constants_declarations_ = const_interface.str();

  // static variables in the .m hold all constant values
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    string name = (*c_iter)->get_name();
    t_type* type = (*c_iter)->get_type();
    f_impl_ <<
      "static " << type_name(type) << " " << cocoa_prefix_ << name;
    if (!type->is_container() && !type->is_struct()) {
      f_impl_ << " = " << render_const_value(f_impl_, type, (*c_iter)->get_value());
    }
    f_impl_ << ";" << endl;
  }
  f_impl_ << endl;

  f_impl_ << "@implementation " << constants_class_name << endl;

  // initialize complex constants when the class is loaded
  f_impl_ << "+ (void) initialize ";
  scope_up(f_impl_);

  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    if ((*c_iter)->get_type()->is_container() ||
        (*c_iter)->get_type()->is_struct()) {
      print_const_value(f_impl_, 
           cocoa_prefix_+(*c_iter)->get_name(),
           (*c_iter)->get_type(),
           (*c_iter)->get_value(),
           false, false);
      f_impl_ << ";" << endl;
    }
  }
  scope_down(f_impl_);

  // getter method for each constant
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    string name = (*c_iter)->get_name();
    t_type* type = (*c_iter)->get_type();
    f_impl_ <<
      "+ (" << type_name(type) << ") " << name;
    scope_up(f_impl_);
    indent(f_impl_) << "return " << cocoa_prefix_ << name << ";" << endl;
    scope_down(f_impl_);
  }

  f_impl_ << "@end" << endl << endl;
}


/**
 * Generates a struct definition for a thrift data type. This is a class
 * with protected data members, read(), write(), and getters and setters.
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_struct(t_struct* tstruct) {
  generate_cocoa_struct_interface(f_header_, tstruct, false);
  generate_cocoa_struct_implementation(f_impl_, tstruct, false);
}

/**
 * Exceptions are structs, but they inherit from NSException
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_xception(t_struct* txception) {
  generate_cocoa_struct_interface(f_header_, txception, true);
  generate_cocoa_struct_implementation(f_impl_, txception, true);
}


/**
 * Generate the interface for a struct
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_interface(ofstream &out,
                                                      t_struct* tstruct,
                                                      bool is_exception) {
  out << "@interface " << cocoa_prefix_ << tstruct->get_name() << " : ";

  if (is_exception) {
    out << "NSException ";
  } else {
    out << "NSObject ";
  }
  out << "<TBase, NSCoding> ";

  scope_up(out);

  // members are protected.  this is redundant, but explicit.
  //  f_header_ << endl << "@protected:" << endl;

  const vector<t_field*>& members = tstruct->get_members();

  // member varialbes
  vector<t_field*>::const_iterator m_iter;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    out << indent() << declare_field(*m_iter) << endl;
  }

  if (members.size() > 0) {
    out << endl;
    // isset fields
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      indent(out) <<
        "BOOL __" << (*m_iter)->get_name() << "_isset;" <<  endl;
    }
  }

  scope_down(out);
  out << endl;

  // properties
  if (members.size() > 0) {
    out << "#if TARGET_OS_IPHONE || (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)" << endl;
    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      out << indent() << declare_property(*m_iter) << endl;
    }
    out << "#endif" << endl << endl;
  }

  // default initializer
  out << indent() << "- (id) init;" << endl;

  // initializer for all fields
  if (!members.empty()) {
    generate_cocoa_struct_initializer_signature(out, tstruct);
    out << ";" << endl;
  }
  out << endl;

  // read and write
  out << "- (void) read: (id <TProtocol>) inProtocol;" << endl;
  out << "- (void) write: (id <TProtocol>) outProtocol;" << endl;
  out << endl;

  // validator
  out << "- (void) validate;" << endl << endl;

  // getters and setters
  generate_cocoa_struct_field_accessor_declarations(out, tstruct, is_exception);

  out << "@end" << endl << endl;
}


/**
 * Generate signature for initializer of struct with a parameter for
 * each field.
 */
void t_cocoa_generator::generate_cocoa_struct_initializer_signature(ofstream &out,
                                                                  t_struct* tstruct) {
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;
  indent(out) << "- (id) initWith";
  for (m_iter = members.begin(); m_iter != members.end(); ) {
    if (m_iter == members.begin()) {
      out << capitalize((*m_iter)->get_name());
    } else {
      out << (*m_iter)->get_name();
    }
    out << ": (" << type_name((*m_iter)->get_type()) << ") " <<
      (*m_iter)->get_name();
    ++m_iter;
    if (m_iter != members.end()) {
      out << " ";
    }
  }
}

/**
 * Generate getter and setter declarations for all fields, plus an
 * IsSet getter.
 */
void t_cocoa_generator::generate_cocoa_struct_field_accessor_declarations(ofstream &out,
                                                                          t_struct* tstruct,
                                                                          bool is_exception) {
  (void) is_exception;
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    out << indent() << "#if !__has_feature(objc_arc)" << endl;
    out << indent() << "- (" << type_name((*m_iter)->get_type()) << ") " << decapitalize((*m_iter)->get_name()) << ";" << endl;
    out << indent() << "- (void) set" << capitalize((*m_iter)->get_name()) <<
      ": (" << type_name((*m_iter)->get_type()) << ") " << (*m_iter)->get_name() << ";" << endl;
    out << indent() << "#endif" << endl;
    out << indent() << "- (BOOL) " << (*m_iter)->get_name() << "IsSet;" << endl << endl;
  }
}


/**
 * Generate the initWithCoder method for this struct so it's compatible with
 * the NSCoding protocol
 */
void t_cocoa_generator::generate_cocoa_struct_init_with_coder_method(ofstream &out,
                                                                     t_struct* tstruct,
                                                                     bool is_exception) 
{
  indent(out) << "- (id) initWithCoder: (NSCoder *) decoder" << endl;
  scope_up(out);
  if (is_exception) {
    // NSExceptions conform to NSCoding, so we can call super
    out << indent() << "self = [super initWithCoder: decoder];" << endl;
  } else {
    out << indent() << "self = [super init];" << endl;
  }

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type((*m_iter)->get_type());
    out << indent() << "if ([decoder containsValueForKey: @\""<< (*m_iter)->get_name() <<"\"])" << endl;
    scope_up(out);
    out << indent() << "__" << (*m_iter)->get_name() << " = ";
    if (type_can_be_null(t)) 
    {
      out << "[[decoder decodeObjectForKey: @\"" << (*m_iter)->get_name() << "\"] retain_stub];" << endl;
    }
    else if (t->is_enum()) 
    {
      out << "[decoder decodeIntForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
    }
    else 
    {
      t_base_type::t_base tbase = ((t_base_type *) t)->get_base();
      switch (tbase)
      {
        case t_base_type::TYPE_BOOL:
          out << "[decoder decodeBoolForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
          break;
        case t_base_type::TYPE_BYTE:
          out << "[decoder decodeIntForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
          break;
        case t_base_type::TYPE_I16:
          out << "[decoder decodeIntForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
          break;
        case t_base_type::TYPE_I32:
          out << "[decoder decodeInt32ForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
          break;
        case t_base_type::TYPE_I64:
          out << "[decoder decodeInt64ForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
          break;
        case t_base_type::TYPE_DOUBLE:
          out << "[decoder decodeDoubleForKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;      
          break;
        default:    
          throw "compiler error: don't know how to decode thrift type: " + t_base_type::t_base_name(tbase);
      }
    }
    out << indent() << "__" << (*m_iter)->get_name() << "_isset = YES;" << endl;
    scope_down(out);
  }
  
  out << indent() << "return self;" << endl;
  scope_down(out);
  out << endl;
}


/**
 * Generate the encodeWithCoder method for this struct so it's compatible with
 * the NSCoding protocol
 */
void t_cocoa_generator::generate_cocoa_struct_encode_with_coder_method(ofstream &out,
                                                                       t_struct* tstruct,
                                                                       bool is_exception) 
{
  indent(out) << "- (void) encodeWithCoder: (NSCoder *) encoder" << endl;
  scope_up(out);
  if (is_exception) {
    // NSExceptions conform to NSCoding, so we can call super
    out << indent() << "[super encodeWithCoder: encoder];" << endl;
  }
  
  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;
  
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    t_type* t = get_true_type((*m_iter)->get_type());
    out << indent() << "if (__"<< (*m_iter)->get_name() <<"_isset)" << endl;
    scope_up(out);
    //out << indent() << "__" << (*m_iter)->get_name() << " = ";
    if (type_can_be_null(t)) 
    {
      out << indent() << "[encoder encodeObject: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
    }
    else if (t->is_enum()) 
    {
      out << indent() << "[encoder encodeInt: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
    }
    else 
    {
      t_base_type::t_base tbase = ((t_base_type *) t)->get_base();
      switch (tbase)
      {
        case t_base_type::TYPE_BOOL:
          out << indent() << "[encoder encodeBool: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
          break;
        case t_base_type::TYPE_BYTE:
          out << indent() << "[encoder encodeInt: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
          break;
        case t_base_type::TYPE_I16:
          out << indent() << "[encoder encodeInt: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
          break;
        case t_base_type::TYPE_I32:
          out << indent() << "[encoder encodeInt32: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
          break;
        case t_base_type::TYPE_I64:
          out << indent() << "[encoder encodeInt64: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
          break;
        case t_base_type::TYPE_DOUBLE:
          out << indent() << "[encoder encodeDouble: __" << (*m_iter)->get_name() << " forKey: @\"" << (*m_iter)->get_name() << "\"];" << endl;
          break;
        default:    
          throw "compiler error: don't know how to encode thrift type: " + t_base_type::t_base_name(tbase);
      }
    }
    scope_down(out);
  }
  
  scope_down(out);
  out << endl;
}


/**
 * Generate struct implementation.
 *
 * @param tstruct      The struct definition
 * @param is_exception Is this an exception?
 * @param is_result    If this is a result it needs a different writer
 */
void t_cocoa_generator::generate_cocoa_struct_implementation(ofstream &out,
                                                             t_struct* tstruct,
                                                             bool is_exception,
                                                             bool is_result) {
  indent(out) <<
    "@implementation " << cocoa_prefix_ << tstruct->get_name() << endl << endl;

  const vector<t_field*>& members = tstruct->get_members();
  vector<t_field*>::const_iterator m_iter;

  // exceptions need to call the designated initializer on NSException
  if (is_exception) {
    out << indent() << "- (id) init" << endl;
    scope_up(out);
    out << indent() << "return [super initWithName: @\"" << cocoa_prefix_ << tstruct->get_name() <<
        "\" reason: @\"unknown\" userInfo: nil];" << endl;
    scope_down(out);
    out << endl;
  } else {
    // struct

    // default initializer
    // setup instance variables with default values
    indent(out) << "- (id) init" << endl;
    scope_up(out);
    indent(out) << "self = [super init];" << endl;
    if (members.size() > 0) {
      out << "#if TARGET_OS_IPHONE || (MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_5)" << endl;
      for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
        t_type* t = get_true_type((*m_iter)->get_type());
        if ((*m_iter)->get_value() != NULL) {
          print_const_value(out, "self."+(*m_iter)->get_name(), t, (*m_iter)->get_value(), false, true);
        }
      }
      out << "#endif" << endl;
    }
    indent(out) << "return self;" << endl;
    scope_down(out);
    out << endl;
  }

  // initializer with all fields as params
  if (!members.empty()) {
    generate_cocoa_struct_initializer_signature(out, tstruct);
    out << endl;
    scope_up(out);
    if (is_exception) {
      out << indent() << "self = [self init];" << endl;
    } else {
      out << indent() << "self = [super init];" << endl;
    }

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      t_type* t = get_true_type((*m_iter)->get_type());
      out << indent() << "__" << (*m_iter)->get_name() << " = ";
      if (type_can_be_null(t)) {
        out << "[" << (*m_iter)->get_name() << " retain_stub];" << endl;
      } else {
        out << (*m_iter)->get_name() << ";" << endl;
      }
      out << indent() << "__" << (*m_iter)->get_name() << "_isset = YES;" << endl;
    }

    out << indent() << "return self;" << endl;
    scope_down(out);
    out << endl;
  }
  
  // initWithCoder for NSCoding
  generate_cocoa_struct_init_with_coder_method(out, tstruct, is_exception);
  // encodeWithCoder for NSCoding
  generate_cocoa_struct_encode_with_coder_method(out, tstruct, is_exception);  

  // dealloc
  if (!members.empty()) {
    out << "- (void) dealloc" << endl;
    scope_up(out);

    for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
      t_type* t = get_true_type((*m_iter)->get_type());
      if (type_can_be_null(t)) {
        indent(out) << "[__" << (*m_iter)->get_name() << " release_stub];" << endl;
      }
    }

    out << indent() << "[super dealloc_stub];" << endl;
    scope_down(out);
    out << endl;
  }

  // the rest of the methods
  generate_cocoa_struct_field_accessor_implementations(out, tstruct, is_exception);
  generate_cocoa_struct_reader(out, tstruct);
  if (is_result) {
    generate_cocoa_struct_result_writer(out, tstruct);
  } else {
    generate_cocoa_struct_writer(out, tstruct);
  }
  generate_cocoa_struct_validator(out, tstruct);
  generate_cocoa_struct_description(out, tstruct);

  out << "@end" << endl << endl;
}


/**
 * Generates a function to read all the fields of the struct.
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_reader(ofstream& out,
                                                     t_struct* tstruct) {
  out <<
    "- (void) read: (id <TProtocol>) inProtocol" << endl;
  scope_up(out);

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  // Declare stack tmp variables
  indent(out) << "NSString * fieldName;" << endl;
  indent(out) << "int fieldType;" << endl;
  indent(out) << "int fieldID;" << endl;
  out << endl;

  indent(out) << "[inProtocol readStructBeginReturningName: NULL];" << endl;

  // Loop over reading in fields
  indent(out) <<
    "while (true)" << endl;
    scope_up(out);

    // Read beginning field marker
    indent(out) <<
      "[inProtocol readFieldBeginReturningName: &fieldName type: &fieldType fieldID: &fieldID];" << endl;

    // Check for field STOP marker and break
    indent(out) <<
      "if (fieldType == TType_STOP) { " << endl;
    indent_up();
    indent(out) <<
      "break;" << endl;
    indent_down();
    indent(out) <<
      "}" << endl;

    // Switch statement on the field we are reading
    indent(out) <<
      "switch (fieldID)" << endl;

      scope_up(out);

      // Generate deserialization code for known cases
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        indent(out) <<
          "case " << (*f_iter)->get_key() << ":" << endl;
        indent_up();
        indent(out) <<
          "if (fieldType == " << type_to_enum((*f_iter)->get_type()) << ") {" << endl;
        indent_up();

        generate_deserialize_field(out, *f_iter, "fieldValue");
        indent(out) << call_field_setter(*f_iter, "fieldValue") << endl;
        // if this is an allocated field, release it since the struct
        // is now retaining it
        if (type_can_be_null((*f_iter)->get_type())) {
          // deserialized strings are autorelease, so don't release them
          if (!(get_true_type((*f_iter)->get_type())->is_string())) {
            indent(out) << "[fieldValue release_stub];" << endl;
          }
        }

        indent_down();
        out << indent() << "} else { " << endl;
        if (log_unexpected_) {
          out << indent() << "  NSLog(@\"%s: field ID %i has unexpected type %i.  Skipping.\", __PRETTY_FUNCTION__, fieldID, fieldType);" << endl;
        }
        out << indent() << "  [TProtocolUtil skipType: fieldType onProtocol: inProtocol];" << endl <<
          indent() << "}" << endl <<
          indent() << "break;" << endl;
        indent_down();
      }

      // In the default case we skip the field
      out << indent() << "default:" << endl;
      if (log_unexpected_) {
        out << indent() << "  NSLog(@\"%s: unexpected field ID %i with type %i.  Skipping.\", __PRETTY_FUNCTION__, fieldID, fieldType);" << endl;
      }
      out << indent() << "  [TProtocolUtil skipType: fieldType onProtocol: inProtocol];" << endl <<
        indent() << "  break;" << endl;

      scope_down(out);

    // Read field end marker
    indent(out) <<
      "[inProtocol readFieldEnd];" << endl;

    scope_down(out);

    out <<
      indent() << "[inProtocol readStructEnd];" << endl;

    // performs various checks (e.g. check that all required fields are set)
    if (validate_required_) {
      out <<
        indent() << "[self validate];" << endl;
    }

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a function to write all the fields of the struct
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_writer(ofstream& out,
                                                     t_struct* tstruct) {
  out <<
    indent() << "- (void) write: (id <TProtocol>) outProtocol {" << endl;
  indent_up();

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<
    indent() << "[outProtocol writeStructBeginWithName: @\"" << name << "\"];" << endl;

  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    out <<
      indent() << "if (__" << (*f_iter)->get_name() << "_isset) {" << endl;
    indent_up();
    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    if (null_allowed) {
      out <<
        indent() << "if (__" << (*f_iter)->get_name() << " != nil) {" << endl;
      indent_up();
    }

    indent(out) << "[outProtocol writeFieldBeginWithName: @\"" <<
      (*f_iter)->get_name() << "\" type: " << type_to_enum((*f_iter)->get_type()) <<
      " fieldID: " << (*f_iter)->get_key() << "];" << endl;

    // Write field contents
    generate_serialize_field(out, *f_iter, "__"+(*f_iter)->get_name());

    // Write field closer
    indent(out) <<
      "[outProtocol writeFieldEnd];" << endl;

    if (null_allowed) {
      scope_down(out);
    }
    scope_down(out);
  }
  // Write the struct map
  out <<
    indent() << "[outProtocol writeFieldStop];" << endl <<
    indent() << "[outProtocol writeStructEnd];" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a function to write all the fields of the struct, which
 * is a function result. These fields are only written if they are
 * set, and only one of them can be set at a time.
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_result_writer(ofstream& out,
                                                            t_struct* tstruct) {
  out <<
    indent() << "- (void) write: (id <TProtocol>) outProtocol {" << endl;
  indent_up();

  string name = tstruct->get_name();
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;

  out <<
    indent() << "[outProtocol writeStructBeginWithName: @\"" << name << "\"];" << endl;

  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      out <<
        endl <<
        indent() << "if ";
    } else {
      out <<
        " else if ";
    }

    out <<
      "(__" << (*f_iter)->get_name() << "_isset) {" << endl;
    indent_up();

    bool null_allowed = type_can_be_null((*f_iter)->get_type());
    if (null_allowed) {
      out <<
        indent() << "if (__" << (*f_iter)->get_name() << " != nil) {" << endl;
      indent_up();
    }

    indent(out) << "[outProtocol writeFieldBeginWithName: @\"" <<
      (*f_iter)->get_name() << "\" type: " << type_to_enum((*f_iter)->get_type()) <<
      " fieldID: " << (*f_iter)->get_key() << "];" << endl;

    // Write field contents
    generate_serialize_field(out, *f_iter, "__"+(*f_iter)->get_name());

    // Write field closer
    indent(out) <<
      "[outProtocol writeFieldEnd];" << endl;

    if (null_allowed) {
      indent_down();
      indent(out) << "}" << endl;
    }

    indent_down();
    indent(out) << "}";
  }
  // Write the struct map
  out <<
    endl <<
    indent() << "[outProtocol writeFieldStop];" << endl <<
    indent() << "[outProtocol writeStructEnd];" << endl;

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generates a function to perform various checks
 * (e.g. check that all required fields are set)
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_validator(ofstream& out,
                                                        t_struct* tstruct) {
  out <<
    indent() << "- (void) validate {" << endl;
  indent_up();

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
    
  out << indent() << "// check for required fields" << endl;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_field* field = (*f_iter);
    if ((*f_iter)->get_req() == t_field::T_REQUIRED) {
      out <<
        indent() << "if (!__" << field->get_name() << "_isset) {" << endl <<
        indent() << "  @throw [TProtocolException exceptionWithName: @\"TProtocolException\"" << endl <<
        indent() << "                             reason: @\"Required field '" << (*f_iter)->get_name() << "' is not set.\"];" << endl <<
        indent() << "}" << endl;
    }
  }

  indent_down();
  out <<
    indent() << "}" << endl <<
    endl;
}

/**
 * Generate property accessor methods for all fields in the struct.
 * getter, setter, isset getter.
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_field_accessor_implementations(ofstream& out,
                                                                             t_struct* tstruct,
                                                                             bool is_exception) {
  (void) is_exception;
  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    t_field* field = *f_iter;
    t_type* type = get_true_type(field->get_type());
    std::string field_name = field->get_name();
    std::string cap_name = field_name;
    cap_name[0] = toupper(cap_name[0]);

    // Simple getter
    indent(out) << "- (" << type_name(type) << ") ";
    out << field_name << " {" << endl;
    indent_up();
    if (!type_can_be_null(type)) {
      indent(out) << "return __" << field_name << ";" << endl;
    } else {
      indent(out) << "return [[__" << field_name << " retain_stub] autorelease_stub];" << endl;
    }
    indent_down();
    indent(out) << "}" << endl << endl;

    // Simple setter
    indent(out) << "- (void) set" << cap_name << ": (" << type_name(type) <<
      ") " << field_name << " {" << endl;
    indent_up();
    if (!type_can_be_null(type)) {
      indent(out) << "__" << field_name << " = " << field_name << ";" << endl;
    } else {
      indent(out) << "[" << field_name << " retain_stub];" << endl;
      indent(out) << "[__" << field_name << " release_stub];" << endl;
      indent(out) << "__" << field_name << " = " << field_name << ";" << endl;
    }
    indent(out) << "__" << field_name << "_isset = YES;" << endl;
    indent_down();
    indent(out) << "}" << endl << endl;

    // IsSet
    indent(out) << "- (BOOL) " << field_name << "IsSet {" << endl;
    indent_up();
    indent(out) << "return __" << field_name << "_isset;" << endl;
    indent_down();
    indent(out) << "}" << endl << endl;

    // Unsetter - do we need this?
    indent(out) << "- (void) unset" << cap_name << " {" << endl;
    indent_up();
    if (type_can_be_null(type)) {
      indent(out) << "[__" << field_name << " release_stub];" << endl;
      indent(out) << "__" << field_name << " = nil;" << endl;
    }
    indent(out) << "__" << field_name << "_isset = NO;" << endl;
    indent_down();
    indent(out) << "}" << endl << endl;
  }
}

/**
 * Generates a description method for the given struct
 *
 * @param tstruct The struct definition
 */
void t_cocoa_generator::generate_cocoa_struct_description(ofstream& out,
                                                          t_struct* tstruct) {
  out <<
    indent() << "- (NSString *) description {" << endl;
  indent_up();

  out <<
    indent() << "NSMutableString * ms = [NSMutableString stringWithString: @\"" <<
    cocoa_prefix_ << tstruct->get_name() << "(\"];" << endl;

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    if (first) {
      first = false;
      indent(out) << "[ms appendString: @\"" << (*f_iter)->get_name() << ":\"];" << endl;
    } else {
      indent(out) << "[ms appendString: @\"," << (*f_iter)->get_name() << ":\"];" << endl;
    }
    t_type* ttype = (*f_iter)->get_type();
    indent(out) << "[ms appendFormat: @\"" << format_string_for_type(ttype) << "\", __" <<
      (*f_iter)->get_name() << "];" << endl;
  }
  out <<
    indent() << "[ms appendString: @\")\"];" << endl <<
    indent() << "return [NSString stringWithString: ms];" << endl;

  indent_down();
  indent(out) << "}" << endl <<
    endl;
}


/**
 * Generates a thrift service.  In Objective-C this consists of a
 * protocol definition, a client interface and a client implementation.
 *
 * @param tservice The service definition
 */
void t_cocoa_generator::generate_service(t_service* tservice) {
  generate_cocoa_service_protocol(f_header_, tservice);
  generate_cocoa_service_client_interface(f_header_, tservice);
  generate_cocoa_service_server_interface(f_header_, tservice);
  generate_cocoa_service_helpers(tservice);
  generate_cocoa_service_client_implementation(f_impl_, tservice);
  generate_cocoa_service_server_implementation(f_impl_, tservice);
}


/**
 * Generates structs for all the service return types
 *
 * @param tservice The service
 */
void t_cocoa_generator::generate_cocoa_service_helpers(t_service* tservice) {
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    t_struct* ts = (*f_iter)->get_arglist();
    generate_cocoa_struct_interface(f_impl_, ts, false);
    generate_cocoa_struct_implementation(f_impl_, ts, false, false);  
    generate_function_helpers(*f_iter);
  }
}

string t_cocoa_generator::function_result_helper_struct_type(t_function* tfunction) {
  if (tfunction->is_oneway()) {
    return capitalize(tfunction->get_name());
  } else {
    return capitalize(tfunction->get_name()) + "_result";
  }
}


string t_cocoa_generator::function_args_helper_struct_type(t_function* tfunction) {
  return tfunction->get_name() + "_args";
}


/**
 * Generates a struct and helpers for a function.
 *
 * @param tfunction The function
 */
void t_cocoa_generator::generate_function_helpers(t_function* tfunction) {
  if (tfunction->is_oneway()) {
    return;
  }

  // create a result struct with a success field of the return type,
  // and a field for each type of exception thrown
  t_struct result(program_, function_result_helper_struct_type(tfunction));
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

  // generate the result struct
  generate_cocoa_struct_interface(f_impl_, &result, false);
  generate_cocoa_struct_implementation(f_impl_, &result, false, true);  
}


/**
 * Generates a service protocol definition.
 *
 * @param tservice The service to generate a protocol definition for
 */
void t_cocoa_generator::generate_cocoa_service_protocol(ofstream& out,
                                                        t_service* tservice) {
  out << "@protocol " << cocoa_prefix_ << tservice->get_name() << " <NSObject>" << endl;

  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    out << "- " << function_signature(*f_iter) << ";" <<
      "  // throws ";
    t_struct* xs = (*f_iter)->get_xceptions();
    const std::vector<t_field*>& xceptions = xs->get_members();
    vector<t_field*>::const_iterator x_iter;
    for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
      out << type_name((*x_iter)->get_type()) + ", ";
    }
    out << "TException" << endl;
  }
  out << "@end" << endl << endl;
}


/**
 * Generates a service client interface definition.
 *
 * @param tservice The service to generate a client interface definition for
 */
void t_cocoa_generator::generate_cocoa_service_client_interface(ofstream& out,
                                                                t_service* tservice) {
  out << "@interface " << cocoa_prefix_ << tservice->get_name() << "Client : NSObject <" <<
    cocoa_prefix_ << tservice->get_name() << "> ";

  scope_up(out);
  out << indent() << "id <TProtocol> inProtocol;" << endl;
  out << indent() << "id <TProtocol> outProtocol;" << endl;
  scope_down(out);

  out << "- (id) initWithProtocol: (id <TProtocol>) protocol;" << endl;
  out << "- (id) initWithInProtocol: (id <TProtocol>) inProtocol outProtocol: (id <TProtocol>) outProtocol;" << endl;
  out << "@end" << endl << endl;
}


/**
 * Generates a service server interface definition. In other words, the TProcess implementation for the
 * service definition.
 *
 * @param tservice The service to generate a client interface definition for
 */
void t_cocoa_generator::generate_cocoa_service_server_interface(ofstream& out,
                                                                t_service* tservice) {
  out << "@interface " << cocoa_prefix_ << tservice->get_name() << "Processor : NSObject <TProcessor> ";
  
  scope_up(out);
  out << indent() << "id <" << cocoa_prefix_ << tservice->get_name() <<"> mService;" << endl;
  out << indent() << "NSDictionary * mMethodMap;" << endl;
  scope_down(out);
  
  out << "- (id) initWith" << tservice->get_name() << ": (id <" << cocoa_prefix_ << tservice->get_name() << ">) service;" << endl;
  out << "- (id<"<<cocoa_prefix_ << tservice->get_name() << ">) service;" << endl;

  out << "@end" << endl << endl;
}


/**
 * Generates a service client implementation.
 *
 * @param tservice The service to generate an implementation for
 */
void t_cocoa_generator::generate_cocoa_service_client_implementation(ofstream& out,
                                                                     t_service* tservice) {
  out << "@implementation " << cocoa_prefix_ << tservice->get_name() << "Client" << endl;

  // initializers
  out << "- (id) initWithProtocol: (id <TProtocol>) protocol" << endl;
  scope_up(out);
  out << indent() << "return [self initWithInProtocol: protocol outProtocol: protocol];" << endl;
  scope_down(out);
  out << endl;

  out << "- (id) initWithInProtocol: (id <TProtocol>) anInProtocol outProtocol: (id <TProtocol>) anOutProtocol" << endl;
  scope_up(out);
  out << indent() << "self = [super init];" << endl;
  out << indent() << "inProtocol = [anInProtocol retain_stub];" << endl;
  out << indent() << "outProtocol = [anOutProtocol retain_stub];" << endl;
  out << indent() << "return self;" << endl;
  scope_down(out);
  out << endl;

  // dealloc
  out << "- (void) dealloc" << endl;
  scope_up(out);
  out << indent() << "[inProtocol release_stub];" << endl;
  out << indent() << "[outProtocol release_stub];" << endl;
  out << indent() << "[super dealloc_stub];" << endl;
  scope_down(out);
  out << endl;

  // generate client method implementations
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();

    t_function send_function(g_type_void,
                             string("send_") + (*f_iter)->get_name(),
                             (*f_iter)->get_arglist());

    string argsname = (*f_iter)->get_name() + "_args";

    // Open function
    indent(out) <<
      "- " << function_signature(&send_function) << endl;
    scope_up(out);

    // Serialize the request
    out <<
      indent() << "[outProtocol writeMessageBeginWithName: @\"" << funname << "\"" <<
      " type: TMessageType_CALL" <<
      " sequenceID: 0];" << endl;

    out <<
      indent() << "[outProtocol writeStructBeginWithName: @\"" << argsname << "\"];" << endl;

    // write out function parameters
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      string fieldName = (*fld_iter)->get_name();
      if (type_can_be_null((*fld_iter)->get_type())) {
        out << indent() << "if (" << fieldName << " != nil)";
        scope_up(out);
      }
      out <<
        indent() << "[outProtocol writeFieldBeginWithName: @\"" << fieldName << "\""
        " type: " << type_to_enum((*fld_iter)->get_type()) <<
        " fieldID: " << (*fld_iter)->get_key() << "];" << endl;

      generate_serialize_field(out, *fld_iter, fieldName);

      out <<
        indent() << "[outProtocol writeFieldEnd];" << endl;

      if (type_can_be_null((*fld_iter)->get_type())) {
        scope_down(out);
      }
    }

    out <<
      indent() << "[outProtocol writeFieldStop];" << endl;
    out <<
      indent() << "[outProtocol writeStructEnd];" << endl;

    out <<
      indent() << "[outProtocol writeMessageEnd];" << endl <<
      indent() << "[[outProtocol transport] flush];" << endl;

    scope_down(out);
    out << endl;

    if (!(*f_iter)->is_oneway()) {
      t_struct noargs(program_);
      t_function recv_function((*f_iter)->get_returntype(),
                               string("recv_") + (*f_iter)->get_name(),
                               &noargs,
                               (*f_iter)->get_xceptions());
      // Open function
      indent(out) <<
        "- " << function_signature(&recv_function) << endl;
      scope_up(out);

      // TODO(mcslee): Message validation here, was the seqid etc ok?

      // check for an exception
      out <<
        indent() << "int msgType = 0;" << endl <<
        indent() << "[inProtocol readMessageBeginReturningName: nil type: &msgType sequenceID: NULL];" << endl <<
        indent() << "if (msgType == TMessageType_EXCEPTION) {" << endl <<
        indent() << "  TApplicationException * x = [TApplicationException read: inProtocol];" << endl <<
        indent() << "  [inProtocol readMessageEnd];" << endl <<
        indent() << "  @throw x;" << endl <<
        indent() << "}" << endl;

      // FIXME - could optimize here to reduce creation of temporary objects.
      string resultname = function_result_helper_struct_type(*f_iter);
      out <<
        indent() << cocoa_prefix_ << resultname << " * result = [[[" << cocoa_prefix_ <<
        resultname << " alloc] init] autorelease_stub];" << endl;
      indent(out) << "[result read: inProtocol];" << endl;
      indent(out) << "[inProtocol readMessageEnd];" << endl;

      // Careful, only return _result if not a void function
      if (!(*f_iter)->get_returntype()->is_void()) {
        out <<
          indent() << "if ([result successIsSet]) {" << endl <<
          indent() << "  return [result success];" << endl <<
          indent() << "}" << endl;
      }

      t_struct* xs = (*f_iter)->get_xceptions();
      const std::vector<t_field*>& xceptions = xs->get_members();
      vector<t_field*>::const_iterator x_iter;
      for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
        out <<
          indent() << "if ([result " << (*x_iter)->get_name() << "IsSet]) {" << endl <<
          indent() << "  @throw [result " << (*x_iter)->get_name() << "];" << endl <<
          indent() << "}" << endl;
      }

      // If you get here it's an exception, unless a void function
      if ((*f_iter)->get_returntype()->is_void()) {
        indent(out) <<
          "return;" << endl;
      } else {
        out <<
          indent() << "@throw [TApplicationException exceptionWithType: TApplicationException_MISSING_RESULT" << endl <<
          indent() << "                                         reason: @\"" << (*f_iter)->get_name() << " failed: unknown result\"];" << endl;
      }

      // Close function
      scope_down(out);
      out << endl;
    }

    // Open function
    indent(out) <<
      "- " << function_signature(*f_iter) << endl;
    scope_up(out);
    indent(out) <<
      "[self send_" << funname;

    // Declare the function arguments
    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      string fieldName = (*fld_iter)->get_name();
      out << " ";
      if (first) {
        first = false;
        out << ": " << fieldName;
      } else {
        out << fieldName << ": " << fieldName;
      }
    }
    out << "];" << endl;

    if (!(*f_iter)->is_oneway()) {
      out << indent();
      if (!(*f_iter)->get_returntype()->is_void()) {
        out << "return ";
      }
      out <<
        "[self recv_" << funname << "];" << endl;
    }
    scope_down(out);
    out << endl;
  }

  indent_down();

  out << "@end" << endl << endl;
}


/**
 * Generates a service server implementation.  In other words the actual TProcessor implementation
 * for the service.
 *
 * @param tservice The service to generate an implementation for
 */
void t_cocoa_generator::generate_cocoa_service_server_implementation(ofstream& out,
                                                                     t_service* tservice) {
  out << "@implementation " << cocoa_prefix_ << tservice->get_name() << "Processor" << endl;
  indent_up();
  
  // initializer
  out << endl;
  out << "- (id) initWith" << tservice->get_name() << ": (id <" << cocoa_prefix_ << tservice->get_name() << ">) service" << endl;
  scope_up(out);
  out << indent() << "self = [super init];" << endl;
  out << indent() << "if (!self) {" << endl;
  out << indent() << "  return nil;" << endl;
  out << indent() << "}" << endl;
  out << indent() << "mService = [service retain_stub];" << endl;
  out << indent() << "mMethodMap = [[NSMutableDictionary dictionary] retain_stub];" << endl;
  
  // generate method map for routing incoming calls
  vector<t_function*> functions = tservice->get_functions();
  vector<t_function*>::const_iterator f_iter;
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    string funname = (*f_iter)->get_name();
    scope_up(out);
    out << indent() << "SEL s = @selector(process_" << funname << "_withSequenceID:inProtocol:outProtocol:);" << endl;
    out << indent() << "NSMethodSignature * sig = [self methodSignatureForSelector: s];" << endl;
    out << indent() << "NSInvocation * invocation = [NSInvocation invocationWithMethodSignature: sig];" << endl;
    out << indent() << "[invocation setSelector: s];" << endl;
    out << indent() << "[invocation retainArguments];" << endl;
    out << indent() << "[mMethodMap setValue: invocation forKey: @\"" << funname << "\"];" << endl;
    scope_down(out);
  }
  out << indent() << "return self;" << endl;
  scope_down(out);
  
  // implementation of the 'service' method which returns the service associated with this
  // processor
  out << endl;
  out << indent() << "- (id<"<<cocoa_prefix_ << tservice->get_name() << ">) service" << endl;
  out << indent() << "{" << endl;
  out << indent() << "  return [[mService retain_stub] autorelease_stub];" << endl;
  out << indent() << "}" << endl;
  
  // implementation of the TProcess method, which dispatches the incoming call using the method map
  out << endl;
  out << indent() << "- (BOOL) processOnInputProtocol: (id <TProtocol>) inProtocol" << endl;
  out << indent() << "                 outputProtocol: (id <TProtocol>) outProtocol" <<endl;
  out << indent() << "{" << endl;
  out << indent() << "  NSString * messageName;" << endl;
  out << indent() << "  int messageType;" << endl;
  out << indent() << "  int seqID;" << endl;
  out << indent() << "  [inProtocol readMessageBeginReturningName: &messageName" << endl;
  out << indent() << "                                       type: &messageType" << endl;
  out << indent() << "                                 sequenceID: &seqID];" << endl;
  out << indent() << "  NSInvocation * invocation = [mMethodMap valueForKey: messageName];" << endl;
  out << indent() << "  if (invocation == nil) {" << endl;
  out << indent() << "    [TProtocolUtil skipType: TType_STRUCT onProtocol: inProtocol];" << endl;
  out << indent() << "    [inProtocol readMessageEnd];" << endl;
  out << indent() << "    TApplicationException * x = [TApplicationException exceptionWithType: TApplicationException_UNKNOWN_METHOD reason: [NSString stringWithFormat: @\"Invalid method name: '%@'\", messageName]];" << endl;
  out << indent() << "    [outProtocol writeMessageBeginWithName: messageName" << endl;
  out << indent() << "                                      type: TMessageType_EXCEPTION" << endl;
  out << indent() << "                                sequenceID: seqID];" << endl;
  out << indent() << "    [x write: outProtocol];" << endl;
  out << indent() << "    [outProtocol writeMessageEnd];" << endl;
  out << indent() << "    [[outProtocol transport] flush];" << endl;
  out << indent() << "    return YES;" << endl;
  out << indent() << "  }" << endl;
  out << indent() << "  // NSInvocation does not conform to NSCopying protocol" << endl;
  out << indent() << "  NSInvocation * i = [NSInvocation invocationWithMethodSignature: [invocation methodSignature]];" << endl;
  out << indent() << "  [i setSelector: [invocation selector]];" << endl;
  out << indent() << "  [i setArgument: &seqID atIndex: 2];" << endl;
  out << indent() << "  [i setArgument: &inProtocol atIndex: 3];" << endl;
  out << indent() << "  [i setArgument: &outProtocol atIndex: 4];" << endl;
  out << indent() << "  [i setTarget: self];" << endl;
  out << indent() << "  [i invoke];" << endl;
  out << indent() << "  return YES;" << endl;
  out << indent() << "}" << endl;
  
  // generate a process_XXXX method for each service function, which reads args, calls the service, and writes results
  functions = tservice->get_functions();
  for (f_iter = functions.begin(); f_iter != functions.end(); ++f_iter) {
    out << endl;
    string funname = (*f_iter)->get_name();
    out << indent() << "- (void) process_" << funname << "_withSequenceID: (int32_t) seqID inProtocol: (id<TProtocol>) inProtocol outProtocol: (id<TProtocol>) outProtocol" << endl;
    scope_up(out);
    string argstype = cocoa_prefix_ + function_args_helper_struct_type(*f_iter);
    out << indent() << argstype << " * args = [[" << argstype << " alloc] init];" << endl;
    out << indent() << "[args read: inProtocol];" << endl;
    out << indent() << "[inProtocol readMessageEnd];" << endl;
    
    // prepare the result if not oneway
    if (!(*f_iter)->is_oneway()) {
        string resulttype = cocoa_prefix_ + function_result_helper_struct_type(*f_iter);
        out << indent() << resulttype << " * result = [[" << resulttype << " alloc] init];" << endl;
    }

    // make the call to the actual service object
    out << indent();
    if (!(*f_iter)->get_returntype()->is_void()) {
      out << "[result setSuccess: ";
    }
    out << "[mService " << funname;
    // supplying arguments
    t_struct* arg_struct = (*f_iter)->get_arglist();
    const vector<t_field*>& fields = arg_struct->get_members();
    vector<t_field*>::const_iterator fld_iter;
    bool first = true;
    for (fld_iter = fields.begin(); fld_iter != fields.end(); ++fld_iter) {
      string fieldName = (*fld_iter)->get_name();
      if (first) {
        first = false;
        out << ": [args " << fieldName << "]";
      } else {
        out << " " << fieldName << ": [args " << fieldName << "]";
      }
    }
    out << "]";
    if (!(*f_iter)->get_returntype()->is_void()) {
      out << "]";
    }
    out << ";" << endl;
    
    // write out the result if not oneway
    if (!(*f_iter)->is_oneway()) {
        out << indent() << "[outProtocol writeMessageBeginWithName: @\"" << funname << "\"" << endl;
        out << indent() << "                                  type: TMessageType_REPLY" << endl;
        out << indent() << "                            sequenceID: seqID];" << endl;
        out << indent() << "[result write: outProtocol];" << endl;
        out << indent() << "[outProtocol writeMessageEnd];" << endl;
        out << indent() << "[[outProtocol transport] flush];" << endl;
        out << indent() << "[result release_stub];" << endl;
    }
    out << indent() << "[args release_stub];" << endl;
    
    scope_down(out);
  }
  
  // dealloc
  out << endl;
  out << "- (void) dealloc" << endl;
  scope_up(out);
  out << indent() << "[mService release_stub];" << endl;
  out << indent() << "[mMethodMap release_stub];" << endl;
  out << indent() << "[super dealloc_stub];" << endl;
  scope_down(out);
  out << endl;

  indent_down();

  out << "@end" << endl << endl;
}


/**
 * Deserializes a field of any type.
 *
 * @param tfield The field
 * @param fieldName The variable name for this field
 */
void t_cocoa_generator::generate_deserialize_field(ofstream& out,
                                                   t_field* tfield,
                                                   string fieldName) {
  t_type* type = get_true_type(tfield->get_type());

  if (type->is_void()) {
    throw "CANNOT GENERATE DESERIALIZE CODE FOR void TYPE: " +
      tfield->get_name();
  }

  if (type->is_struct() || type->is_xception()) {
    generate_deserialize_struct(out,
                                (t_struct*)type,
                                fieldName);
  } else if (type->is_container()) {
    generate_deserialize_container(out, type, fieldName);
  } else if (type->is_base_type() || type->is_enum()) {
    indent(out) <<
      type_name(type) << " " << fieldName << " = [inProtocol ";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw "compiler error: cannot serialize void field in a struct: " +
          tfield->get_name();
        break;
      case t_base_type::TYPE_STRING:
        if (((t_base_type*)type)->is_binary()) {
          out << "readBinary];";
        } else {
          out << "readString];";
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "readBool];";
        break;
      case t_base_type::TYPE_BYTE:
        out << "readByte];";
        break;
      case t_base_type::TYPE_I16:
        out << "readI16];";
        break;
      case t_base_type::TYPE_I32:
        out << "readI32];";
        break;
      case t_base_type::TYPE_I64:
        out << "readI64];";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "readDouble];";
        break;
      default:
        throw "compiler error: no Objective-C name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "readI32];";
    }
    out <<
      endl;
  } else {
    printf("DO NOT KNOW HOW TO DESERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(), type_name(type).c_str());
  }
}

/**
 * Generates an unserializer for a struct, allocates the struct and invokes read:
 */
void t_cocoa_generator::generate_deserialize_struct(ofstream& out,
                                                    t_struct* tstruct,
                                                    string fieldName) {
  indent(out) << type_name(tstruct) << fieldName << " = [[" <<
    type_name(tstruct, true) << " alloc] init];" << endl;
  indent(out) << "[" << fieldName << " read: inProtocol];" << endl;
}

/**
 * Deserializes a container by reading its size and then iterating
 */
void t_cocoa_generator::generate_deserialize_container(ofstream& out,
                                                       t_type* ttype,
                                                       string fieldName) {
  string size = tmp("_size");
  indent(out) << "int " << size << ";" << endl;

  // Declare variables, read header
  if (ttype->is_map()) {
    indent(out)
      << "[inProtocol readMapBeginReturningKeyType: NULL valueType: NULL size: &" <<
      size << "];" << endl;
    indent(out) << "NSMutableDictionary * " << fieldName <<
      " = [[NSMutableDictionary alloc] initWithCapacity: " << size << "];" << endl;
  } else if (ttype->is_set()) {
    indent(out)
      << "[inProtocol readSetBeginReturningElementType: NULL size: &" << size << "];" << endl;
    indent(out) << "NSMutableSet * " << fieldName <<
      " = [[NSMutableSet alloc] initWithCapacity: " << size << "];" << endl;
  } else if (ttype->is_list()) {
    indent(out)
      << "[inProtocol readListBeginReturningElementType: NULL size: &" << size << "];" << endl;
    indent(out) << "NSMutableArray * " << fieldName <<
      " = [[NSMutableArray alloc] initWithCapacity: " << size << "];" << endl;
  }
  // FIXME - the code above does not verify that the element types of
  // the containers being read match the element types of the
  // containers we are reading into.  Does that matter?

  // For loop iterates over elements
  string i = tmp("_i");
  indent(out) << "int " << i << ";" << endl <<
    indent() << "for (" << i << " = 0; " <<
    i << " < " << size << "; " <<
    "++" << i << ")" << endl;

    scope_up(out);

    if (ttype->is_map()) {
      generate_deserialize_map_element(out, (t_map*)ttype, fieldName);
    } else if (ttype->is_set()) {
      generate_deserialize_set_element(out, (t_set*)ttype, fieldName);
    } else if (ttype->is_list()) {
      generate_deserialize_list_element(out, (t_list*)ttype, fieldName);
    }

    scope_down(out);

  // Read container end
  if (ttype->is_map()) {
    indent(out) << "[inProtocol readMapEnd];" << endl;
  } else if (ttype->is_set()) {
    indent(out) << "[inProtocol readSetEnd];" << endl;
  } else if (ttype->is_list()) {
    indent(out) << "[inProtocol readListEnd];" << endl;
  }

}


/**
 * Take a variable of a given type and wrap it in code to make it
 * suitable for putting into a container, if necessary.  Basically,
 * wrap scaler primitives in NSNumber objects.
 */
string t_cocoa_generator::containerize(t_type * ttype,
                                       string fieldName)
{
  // FIXME - optimize here to avoid autorelease pool?
  ttype = get_true_type(ttype);
  if (ttype->is_enum()) {
    return "[NSNumber numberWithInt: " + fieldName + "]";
  } else if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "can't containerize void";
    case t_base_type::TYPE_BOOL:
      return "[NSNumber numberWithBool: " + fieldName + "]";
    case t_base_type::TYPE_BYTE:
      return "[NSNumber numberWithUnsignedChar: " + fieldName + "]";
    case t_base_type::TYPE_I16:
      return "[NSNumber numberWithShort: " + fieldName + "]";
    case t_base_type::TYPE_I32:
      return "[NSNumber numberWithLong: " + fieldName + "]";
    case t_base_type::TYPE_I64:
      return "[NSNumber numberWithLongLong: " + fieldName + "]";
    case t_base_type::TYPE_DOUBLE:
      return "[NSNumber numberWithDouble: " + fieldName + "]";
    default:
      break;
    }
  }

  // do nothing
  return fieldName;
}


/**
 * Generates code to deserialize a map element
 */
void t_cocoa_generator::generate_deserialize_map_element(ofstream& out,
                                                         t_map* tmap,
                                                         string fieldName) {
  string key = tmp("_key");
  string val = tmp("_val");
  t_type* keyType = tmap->get_key_type();
  t_type* valType = tmap->get_val_type();
  t_field fkey(keyType, key);
  t_field fval(valType, val);

  generate_deserialize_field(out, &fkey, key);
  generate_deserialize_field(out, &fval, val);

  indent(out) <<
    "[" << fieldName << " setObject: " << containerize(valType, val) <<
    " forKey: " << containerize(keyType, key) << "];" << endl;

  if (type_can_be_null(keyType)) {
    if (!(get_true_type(keyType)->is_string())) {
      indent(out) << "[" << containerize(keyType, key) << " release_stub];" << endl;
    }
  }

  if (type_can_be_null(valType)) {
    if (!(get_true_type(valType)->is_string())) {
      indent(out) << "[" << containerize(valType, val) << " release_stub];" << endl;
    }
  }
}

/**
 * Deserializes a set element
 */
void t_cocoa_generator::generate_deserialize_set_element(ofstream& out,
                                                         t_set* tset,
                                                         string fieldName) {
  string elem = tmp("_elem");
  t_type* type = tset->get_elem_type();
  t_field felem(type, elem);

  generate_deserialize_field(out, &felem, elem);

  indent(out) <<
    "[" << fieldName << " addObject: " << containerize(type, elem) << "];" << endl;

  if (type_can_be_null(type)) {
    // deserialized strings are autorelease, so don't release them
    if (!(get_true_type(type)->is_string())) {
      indent(out) << "[" << containerize(type, elem) << " release_stub];" << endl;
    }
  }
}

/**
 * Deserializes a list element
 */
void t_cocoa_generator::generate_deserialize_list_element(ofstream& out,
                                                          t_list* tlist,
                                                          string fieldName) {
  string elem = tmp("_elem");
  t_type* type = tlist->get_elem_type();
  t_field felem(type, elem);

  generate_deserialize_field(out, &felem, elem);

  indent(out) <<
    "[" << fieldName << " addObject: " << containerize(type, elem) << "];" << endl;

  if (type_can_be_null(type)) {
    if (!(get_true_type(type)->is_string())) {
      indent(out) << "[" << containerize(type, elem) << " release_stub];" << endl;
    }
  }
}


/**
 * Serializes a field of any type.
 *
 * @param tfield The field to serialize
 * @param fieldName Name to of the variable holding the field
 */
void t_cocoa_generator::generate_serialize_field(ofstream& out,
                                                 t_field* tfield,
                                                 string fieldName) {
  t_type* type = get_true_type(tfield->get_type());

  // Do nothing for void types
  if (type->is_void()) {
    throw "CANNOT GENERATE SERIALIZE CODE FOR void TYPE: " +
      tfield->get_name();
  }

  if (type->is_struct() || type->is_xception()) {
    generate_serialize_struct(out,
                              (t_struct*)type,
                              fieldName);
  } else if (type->is_container()) {
    generate_serialize_container(out,
                                 type,
                                 fieldName);
  } else if (type->is_base_type() || type->is_enum()) {
    indent(out) <<
      "[outProtocol ";

    if (type->is_base_type()) {
      t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
      switch (tbase) {
      case t_base_type::TYPE_VOID:
        throw
          "compiler error: cannot serialize void field in a struct: " + fieldName;
        break;
      case t_base_type::TYPE_STRING:
        if (((t_base_type*)type)->is_binary()) {
          out << "writeBinary: " << fieldName << "];";
        } else {
          out << "writeString: " << fieldName << "];";
        }
        break;
      case t_base_type::TYPE_BOOL:
        out << "writeBool: " << fieldName << "];";
        break;
      case t_base_type::TYPE_BYTE:
        out << "writeByte: " << fieldName << "];";
        break;
      case t_base_type::TYPE_I16:
        out << "writeI16: " << fieldName << "];";
        break;
      case t_base_type::TYPE_I32:
        out << "writeI32: " << fieldName << "];";
        break;
      case t_base_type::TYPE_I64:
        out << "writeI64: " << fieldName << "];";
        break;
      case t_base_type::TYPE_DOUBLE:
        out << "writeDouble: " << fieldName << "];";
        break;
      default:
        throw "compiler error: no Objective-C name for base type " + t_base_type::t_base_name(tbase);
      }
    } else if (type->is_enum()) {
      out << "writeI32: " << fieldName << "];";
    }
    out << endl;
  } else {
    printf("DO NOT KNOW HOW TO SERIALIZE FIELD '%s' TYPE '%s'\n",
           tfield->get_name().c_str(),
           type_name(type).c_str());
  }
}

/**
 * Serialize a struct.
 *
 * @param tstruct The struct to serialize
 * @param fieldName Name of variable holding struct
 */
void t_cocoa_generator::generate_serialize_struct(ofstream& out,
                                                  t_struct* tstruct,
                                                  string fieldName) {
  (void) tstruct;
  out <<
    indent() << "[" << fieldName << " write: outProtocol];" << endl;
}

/**
 * Serializes a container by writing its size then the elements.
 *
 * @param ttype  The type of container
 * @param fieldName Name of variable holding container
 */
void t_cocoa_generator::generate_serialize_container(ofstream& out,
                                                     t_type* ttype,
                                                     string fieldName) {
  scope_up(out);

  if (ttype->is_map()) {
    indent(out) <<
      "[outProtocol writeMapBeginWithKeyType: " <<
      type_to_enum(((t_map*)ttype)->get_key_type()) << " valueType: " <<
      type_to_enum(((t_map*)ttype)->get_val_type()) << " size: [" <<
      fieldName << " count]];" << endl;
  } else if (ttype->is_set()) {
    indent(out) <<
      "[outProtocol writeSetBeginWithElementType: " <<
      type_to_enum(((t_set*)ttype)->get_elem_type()) << " size: [" <<
      fieldName << " count]];" << endl;
  } else if (ttype->is_list()) {
    indent(out) <<
      "[outProtocol writeListBeginWithElementType: " <<
      type_to_enum(((t_list*)ttype)->get_elem_type()) << " size: [" <<
      fieldName << " count]];" << endl;
  }

  string iter = tmp("_iter");
  string key;
  if (ttype->is_map()) {
    key = tmp("key");
    indent(out) << "NSEnumerator * " << iter << " = [" << fieldName << " keyEnumerator];" << endl;
    indent(out) << "id " << key << ";" << endl;
    indent(out) << "while ((" << key << " = [" << iter << " nextObject]))" << endl;
  } else if (ttype->is_set()) {
    key = tmp("obj");
    indent(out) << "NSEnumerator * " << iter << " = [" << fieldName << " objectEnumerator];" << endl;
    indent(out) << "id " << key << ";" << endl;
    indent(out) << "while ((" << key << " = [" << iter << " nextObject]))" << endl;
  } else if (ttype->is_list()) {
    key = tmp("i");
    indent(out) << "int " << key << ";" << endl;
    indent(out) <<
      "for (" << key << " = 0; " << key << " < [" << fieldName << " count]; " << key << "++)" << endl;
  }

    scope_up(out);

    if (ttype->is_map()) {
      generate_serialize_map_element(out, (t_map*)ttype, key, fieldName);
    } else if (ttype->is_set()) {
      generate_serialize_set_element(out, (t_set*)ttype, key);
    } else if (ttype->is_list()) {
      generate_serialize_list_element(out, (t_list*)ttype, key, fieldName);
    }

    scope_down(out);

    if (ttype->is_map()) {
      indent(out) <<
        "[outProtocol writeMapEnd];" << endl;
    } else if (ttype->is_set()) {
      indent(out) <<
        "[outProtocol writeSetEnd];" << endl;
    } else if (ttype->is_list()) {
      indent(out) <<
        "[outProtocol writeListEnd];" << endl;
    }

  scope_down(out);
}

/**
 * Given a field variable name, wrap it in code that converts it to a
 * primitive type, if necessary.
 */
string t_cocoa_generator::decontainerize(t_field * tfield,
                                         string fieldName)
{
  t_type * ttype = get_true_type(tfield->get_type());
  if (ttype->is_enum()) {
    return "[" + fieldName + " intValue]";
  } else if (ttype->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)ttype)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "can't decontainerize void";
    case t_base_type::TYPE_BOOL:
      return "[" + fieldName + " boolValue]";
    case t_base_type::TYPE_BYTE:
      return "[" + fieldName + " unsignedCharValue]";
    case t_base_type::TYPE_I16:
      return "[" + fieldName + " shortValue]";
    case t_base_type::TYPE_I32:
      return "[" + fieldName + " longValue]";
    case t_base_type::TYPE_I64:
      return "[" + fieldName + " longLongValue]";
    case t_base_type::TYPE_DOUBLE:
      return "[" + fieldName + " doubleValue]";
    default:
      break;
    }
  }

  // do nothing
  return fieldName;
}


/**
 * Serializes the members of a map.
 */
void t_cocoa_generator::generate_serialize_map_element(ofstream& out,
                                                       t_map* tmap,
                                                       string key,
                                                       string mapName) {
  t_field kfield(tmap->get_key_type(), key);
  generate_serialize_field(out, &kfield, decontainerize(&kfield, key));
  t_field vfield(tmap->get_val_type(), "[" + mapName + " objectForKey: " + key + "]");
  generate_serialize_field(out, &vfield, decontainerize(&vfield, vfield.get_name()));
}

/**
 * Serializes the members of a set.
 */
void t_cocoa_generator::generate_serialize_set_element(ofstream& out,
                                                       t_set* tset,
                                                       string elementName) {
  t_field efield(tset->get_elem_type(), elementName);
  generate_serialize_field(out, &efield, decontainerize(&efield, elementName));
}

/**
 * Serializes the members of a list.
 */
void t_cocoa_generator::generate_serialize_list_element(ofstream& out,
                                                        t_list* tlist,
                                                        string index,
                                                        string listName) {
  t_field efield(tlist->get_elem_type(), "[" + listName + " objectAtIndex: " + index + "]");
  generate_serialize_field(out, &efield, decontainerize(&efield, efield.get_name()));
}


/**
 * Returns an Objective-C name
 *
 * @param ttype The type
 * @param class_ref Do we want a Class reference istead of a type reference?
 * @return Java type name, i.e. HashMap<Key,Value>
 */
string t_cocoa_generator::type_name(t_type* ttype, bool class_ref) {
  if (ttype->is_typedef()) {
    return cocoa_prefix_ + ttype->get_name();
  }

  string result;
  if (ttype->is_base_type()) {
    return base_type_name((t_base_type*)ttype);
  } else if (ttype->is_enum()) {
    return "int";
  } else if (ttype->is_map()) {
    result = "NSMutableDictionary";
  } else if (ttype->is_set()) {
    result = "NSMutableSet";
  } else if (ttype->is_list()) {
    result = "NSMutableArray";
  } else {
    // Check for prefix
    t_program* program = ttype->get_program();
    if (program != NULL) {
      result = program->get_namespace("cocoa") + ttype->get_name();
    } else {
      result = ttype->get_name();
    }
  }

  if (!class_ref) {
    result += " *";
  }
  return result;
}

/**
 * Returns the Objective-C type that corresponds to the thrift type.
 *
 * @param tbase The base type
 */
string t_cocoa_generator::base_type_name(t_base_type* type) {
  t_base_type::t_base tbase = type->get_base();

  switch (tbase) {
  case t_base_type::TYPE_VOID:
    return "void";
  case t_base_type::TYPE_STRING:
    if (type->is_binary()) {
      return "NSData *";
    } else {
      return "NSString *";
    }
  case t_base_type::TYPE_BOOL:
    return "BOOL";
  case t_base_type::TYPE_BYTE:
    return "uint8_t";
  case t_base_type::TYPE_I16:
    return"int16_t";
  case t_base_type::TYPE_I32:
    return "int32_t";
  case t_base_type::TYPE_I64:
    return"int64_t";
  case t_base_type::TYPE_DOUBLE:
    return "double";
  default:
    throw "compiler error: no Objective-C name for base type " + t_base_type::t_base_name(tbase);
  }
}

/**
 * Prints the value of a constant with the given type. Note that type checking
 * is NOT performed in this function as it is always run beforehand using the
 * validate_types method in main.cc
 */
void t_cocoa_generator::print_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value, bool defval, bool is_property) {
  type = get_true_type(type);

  indent(out);
  if (type->is_base_type()) {
    string v2 = render_const_value(out, type, value);
    if (defval)
      out << type_name(type) << " ";
    out << name << " = " << v2 << ";" << endl << endl;
  } else if (type->is_enum()) {
    if (defval)
      out << type_name(type) << " ";
    out << name << " = " << render_const_value(out, type, value) << ";" << endl << endl;
  } else if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    if (defval)
      out << type_name(type) << " ";
    if (defval || is_property)
      out << name << " = [[[" << type_name(type, true) << " alloc] init] autorelease_stub];" << endl;
    else
      out << name << " = [[" << type_name(type, true) << " alloc] init];" << endl;
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
      string val = render_const_value(out, field_type, v_iter->second);
      std::string cap_name = capitalize(v_iter->first->get_string());
      indent(out) << "[" << name << " set" << cap_name << ":" << val << "];" << endl;
    }
    out << endl;
  } else if (type->is_map()) {
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    if (defval)
      out << "NSMutableDictionary *";
    if (defval || is_property)
      out << name << " = [[[NSMutableDictionary alloc] initWithCapacity:" << val.size() << "] autorelease_stub]; " << endl;
    else
      out << name << " = [[NSMutableDictionary alloc] initWithCapacity:" << val.size() << "]; " << endl;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(out, ktype, v_iter->first, true);
      string val = render_const_value(out, vtype, v_iter->second, true);
      indent(out) << "[" << name << " setObject:" << val << " forKey:" << key << "];" << endl;
    }
    out << endl;
  } else if (type->is_list()) {
    t_type* etype = ((t_list*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    if (defval)
      out << "NSMutableArray *";
    if (defval || is_property)
      out << name << " = [[[NSMutableArray alloc] initWithCapacity:" << val.size() <<"] autorelease_stub];" << endl;
    else
      out << name << " = [[NSMutableArray alloc] initWithCapacity:" << val.size() <<"];" << endl;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(out, etype, *v_iter, true);
      indent(out) << "[" << name << " addObject:" << val << "];" << endl;
    }
    out << endl;
  } else if (type->is_set()) {
    t_type* etype = ((t_set*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    if (defval)
      out << "NSMutableSet *";
    if (defval || is_property)
      out << name << " = [[[NSMutableSet alloc] initWithCapacity:" << val.size() << "] autorelease_stub];" << endl;
    else
      out << name << " = [[NSMutableSet alloc] initWithCapacity:" << val.size() << "];" << endl;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string val = render_const_value(out, etype, *v_iter, true);
      indent(out) << "[" << name << " addObject:" << val << "];" << endl;
    }
    out << endl;
  } else {
    throw "compiler error: no const of type " + type->get_name();
  }
}

string t_cocoa_generator::render_const_value(ofstream& out, t_type* type, t_const_value* value, bool containerize_it) {
  type = get_true_type(type);
  std::ostringstream render;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      // We must handle binary constant but the syntax of IDL defines 
      // nothing about binary constant.
      //   if ((t_base_type*)type)->is_binary())
      //      // binary code
      render << "@\"" << get_escaped_string(value) << '"';
      break;
    case t_base_type::TYPE_BOOL:
      render << ((value->get_integer() > 0) ? "YES" : "NO");
      break;
    case t_base_type::TYPE_BYTE:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      render << value->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        render << value->get_integer();
      } else {
        render << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    render << value->get_integer();
  } else {
    string t = tmp("tmp");
    print_const_value(out, t, type, value, true, false);
    render << t;
  }

  if (containerize_it) {
    return containerize(type, render.str());
  }
  return render.str();
}

#if 0
/**
ORIGINAL
 * Spit out code that evaluates to the specified constant value.
 */
string t_cocoa_generator::render_const_value(string name,
                                             t_type* type,
                                             t_const_value* value,
                                             bool containerize_it) {
  type = get_true_type(type);
  std::ostringstream render;

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      render << "@\"" << get_escaped_string(value) << '"';
      break;
    case t_base_type::TYPE_BOOL:
      render << ((value->get_integer() > 0) ? "YES" : "NO");
      break;
    case t_base_type::TYPE_BYTE:
    case t_base_type::TYPE_I16:
    case t_base_type::TYPE_I32:
    case t_base_type::TYPE_I64:
      render << value->get_integer();
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() == t_const_value::CV_INTEGER) {
        render << value->get_integer();
      } else {
        render << value->get_double();
      }
      break;
    default:
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase);
    }
  } else if (type->is_enum()) {
    render << value->get_integer();
  } else if (type->is_struct() || type->is_xception()) {
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    if (val.size() > 0)
      render << "[[" << type_name(type, true) << " alloc] initWith";
    else
      render << "[[" << type_name(type, true) << " alloc] init";
    bool first = true;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      // FIXME The generated code does not match with initWithXXX 
      //       initializer and causes compile error.
      //       Try: test/DebugProtoTest.thrift and test/SmallTest.thrift
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
        render << capitalize(v_iter->first->get_string());
        first = false;
      } else {
        render << " " << v_iter->first->get_string();
      }
      render << ": " << render_const_value(name, field_type, v_iter->second);
    }
    render << "]";
  } else if (type->is_map()) {
    render << "[[NSDictionary alloc] initWithObjectsAndKeys: ";
    t_type* ktype = ((t_map*)type)->get_key_type();
    t_type* vtype = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    bool first = true;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      string key = render_const_value(name, ktype, v_iter->first, true);
      string val = render_const_value(name, vtype, v_iter->second, true);
      if (first) {
        first = false;
      } else {
        render << ", ";
      }
      render << val << ", " << key;
    }
    if (first)
      render << " nil]";
    else
      render << ", nil]";
  } else if (type->is_list()) {
    render << "[[NSArray alloc] initWithObjects: ";
    t_type * etype = ((t_list*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    bool first = true;
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (first) {
        first = false;
      } else {
        render << ", ";
      }
      render << render_const_value(name, etype, *v_iter, true);
    }
    if (first)
      render << " nil]";
    else
      render << ", nil]";
  } else if (type->is_set()) {
    render << "[[NSSet alloc] initWithObjects: ";
    t_type * etype = ((t_set*)type)->get_elem_type();
    const vector<t_const_value*>& val = value->get_list();
    bool first = true;
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (first) {
        first = false;
      } else {
        render << ", ";
      }
      render << render_const_value(name, etype, *v_iter, true);
    }
    if (first)
      render << " nil]";
    else
      render << ", nil]";
  } else {
    throw "don't know how to render constant for type: " + type->get_name();
  }

  if (containerize_it) {
    return containerize(type, render.str());
  }

  return render.str();
}
#endif

/**
 * Declares a field.
 *
 * @param ttype The type
 */
string t_cocoa_generator::declare_field(t_field* tfield) {
  return type_name(tfield->get_type()) + " __" + tfield->get_name() + ";";
}

/**
 * Declares an Objective-C 2.0 property.
 *
 * @param tfield The field to declare a property for
 */
string t_cocoa_generator::declare_property(t_field* tfield) {
  std::ostringstream render;
  render << "@property (nonatomic, ";

  if (type_can_be_null(tfield->get_type()))
    render << "retain, ";
  
  render << "getter=" << decapitalize(tfield->get_name()) <<
    ", setter=set" << capitalize(tfield->get_name()) + ":) " <<
    type_name(tfield->get_type()) << " " << tfield->get_name() << ";";

  return render.str();
}

/**
 * Renders a function signature
 *
 * @param tfunction Function definition
 * @return String of rendered function definition
 */
string t_cocoa_generator::function_signature(t_function* tfunction) {
  t_type* ttype = tfunction->get_returntype();
  std::string result =
    "(" + type_name(ttype) + ") " + tfunction->get_name() + argument_list(tfunction->get_arglist());
  return result;
}


/**
 * Renders a colon separated list of types and names, suitable for an
 * objective-c parameter list
 */
string t_cocoa_generator::argument_list(t_struct* tstruct) {
  string result = "";

  const vector<t_field*>& fields = tstruct->get_members();
  vector<t_field*>::const_iterator f_iter;
  bool first = true;
  for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
    string argPrefix = "";
    if (first) {
      first = false;
    } else {
      argPrefix = (*f_iter)->get_name();
      result += " ";
    }
    result += argPrefix + ": (" + type_name((*f_iter)->get_type()) + ") " + (*f_iter)->get_name();
  }
  return result;
}


/**
 * Converts the parse type to an Objective-C enum string for the given type.
 */
string t_cocoa_generator::type_to_enum(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "TType_STRING";
    case t_base_type::TYPE_BOOL:
      return "TType_BOOL";
    case t_base_type::TYPE_BYTE:
      return "TType_BYTE";
    case t_base_type::TYPE_I16:
      return "TType_I16";
    case t_base_type::TYPE_I32:
      return "TType_I32";
    case t_base_type::TYPE_I64:
      return "TType_I64";
    case t_base_type::TYPE_DOUBLE:
      return "TType_DOUBLE";
    }
  } else if (type->is_enum()) {
    return "TType_I32";
  } else if (type->is_struct() || type->is_xception()) {
    return "TType_STRUCT";
  } else if (type->is_map()) {
    return "TType_MAP";
  } else if (type->is_set()) {
    return "TType_SET";
  } else if (type->is_list()) {
    return "TType_LIST";
  }

  throw "INVALID TYPE IN type_to_enum: " + type->get_name();
}


/**
 * Returns a format string specifier for the supplied parse type.
 */
string t_cocoa_generator::format_string_for_type(t_type* type) {
  type = get_true_type(type);

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_VOID:
      throw "NO T_VOID CONSTRUCT";
    case t_base_type::TYPE_STRING:
      return "\\\"%@\\\"";
    case t_base_type::TYPE_BOOL:
      return "%i";
    case t_base_type::TYPE_BYTE:
      return "%i";
    case t_base_type::TYPE_I16:
      return "%hi";
    case t_base_type::TYPE_I32:
      return "%i";
    case t_base_type::TYPE_I64:
      return "%qi";
    case t_base_type::TYPE_DOUBLE:
      return "%f";
    }
  } else if (type->is_enum()) {
    return "%i";
  } else if (type->is_struct() || type->is_xception()) {
    return "%@";
  } else if (type->is_map()) {
    return "%@";
  } else if (type->is_set()) {
    return "%@";
  } else if (type->is_list()) {
    return "%@";
  }

  throw "INVALID TYPE IN format_string_for_type: " + type->get_name();
}

/**
 * Generate a call to a field's setter.
 *
 * @param tfield Field the setter is being called on
 * @param fieldName Name of variable to pass to setter
 */

string t_cocoa_generator::call_field_setter(t_field* tfield, string fieldName) {
  return "[self set" + capitalize(tfield->get_name()) + ": " + fieldName + "];";
}


THRIFT_REGISTER_GENERATOR(cocoa, "Cocoa",
"    log_unexpected:  Log every time an unexpected field ID or type is encountered.\n"
"    validate_required:\n"
"                     Throws exception if any required field is not set.\n"
)

