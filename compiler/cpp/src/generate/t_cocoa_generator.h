// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_COCOA_GENERATOR_H
#define T_COCOA_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_oop_generator.h"

/**
 * Objective-C code generator.
 *
 * @author Andrew McGeachie <geechorama@gmail.com>, although it was
 * mostly copy/pasting/tweaking from mcslee's work.
 */
class t_cocoa_generator : public t_oop_generator {
 public:
  t_cocoa_generator(t_program* program) :
    t_oop_generator(program) {
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

  void print_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value);
  std::string render_const_value(std::string name, t_type* type, t_const_value* value,
                                 bool containerize_it=false);

  void generate_cocoa_struct(t_struct* tstruct, bool is_exception);
  void generate_cocoa_struct_interface(std::ofstream& out, t_struct* tstruct, bool is_xception=false);
  void generate_cocoa_struct_implementation(std::ofstream& out, t_struct* tstruct, bool is_xception=false, bool is_result=false);
  void generate_cocoa_struct_initializer_signature(std::ofstream& out,
                                                   t_struct* tstruct);
  void generate_cocoa_struct_field_accessor_declarations(std::ofstream& out,
                                                         t_struct* tstruct,
                                                         bool is_exception);
  void generate_cocoa_struct_field_accessor_implementations(std::ofstream& out,
                                                            t_struct* tstruct,
                                                            bool is_exception);
  void generate_cocoa_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_result_writer(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_cocoa_struct_description(std::ofstream& out, t_struct* tstruct);

  std::string function_result_helper_struct_type(t_function* tfunction);
  void generate_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_cocoa_service_protocol (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_client_interface (std::ofstream& out, t_service* tservice);
  void generate_cocoa_service_client_implementation (std::ofstream& out, t_service* tservice);
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

};

#endif
