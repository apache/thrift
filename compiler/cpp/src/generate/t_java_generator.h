// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_JAVA_GENERATOR_H
#define T_JAVA_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_oop_generator.h"

/**
 * Java code generator.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_java_generator : public t_oop_generator {
 public:
  t_java_generator(t_program* program, bool bean_style=false) :
    t_oop_generator(program),
    bean_style_(bean_style) {

    out_dir_base_ = (bean_style_ ? "gen-javabean" : "gen-java");
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

  void print_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value, bool in_static, bool defval=false);
  std::string render_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value);

  /**
   * Service-level generation functions
   */

  void generate_java_struct(t_struct* tstruct, bool is_exception);

  void generate_java_struct_definition(std::ofstream& out, t_struct* tstruct, bool is_xception=false, bool in_class=false, bool is_result=false);
  void generate_java_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_java_struct_result_writer(std::ofstream& out, t_struct* tstruct);
  void generate_java_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_java_struct_tostring(std::ofstream& out, t_struct* tstruct);
  void generate_java_bean_boilerplate(std::ofstream& out, t_struct* tstruct);

  void generate_function_helpers(t_function* tfunction);

  void generate_service_interface (t_service* tservice);
  void generate_service_helpers   (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string prefix="");

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
                                          std::string prefix="");

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
                                          std::string iter);

  void generate_java_doc                 (std::ofstream& out,
                                          t_doc*     tdoc);


  /**
   * Helper rendering functions
   */

  std::string java_package();
  std::string java_type_imports();
  std::string java_thrift_imports();
  std::string type_name(t_type* ttype, bool in_container=false, bool in_init=false);
  std::string base_type_name(t_base_type* tbase, bool in_container=false);
  std::string declare_field(t_field* tfield, bool init=false);
  std::string function_signature(t_function* tfunction, std::string prefix="");
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);

  bool type_can_be_null(t_type* ttype) {
    ttype = get_true_type(ttype);

    return
      ttype->is_container() ||
      ttype->is_struct() ||
      ttype->is_xception() ||
      ttype->is_string();
  }


 private:

  /**
   * File streams
   */

  std::string package_name_;
  std::ofstream f_service_;
  std::string package_dir_;

  bool bean_style_;

};

#endif
