// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_HS_GENERATOR_H
#define T_HS_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_oop_generator.h"

/**
 * Haskell code generator.
 *
 * @author Iain Proctor <iproctor@facebook.com>
 */
class t_hs_generator : public t_oop_generator {
 public:
  t_hs_generator(t_program* program) :
    t_oop_generator(program) {

    out_dir_base_ = "gen-hs";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */
  void generate_program  ();
  void generate_typedef  (t_typedef*  ttypedef);
  void generate_enum     (t_enum*     tenum);
  void generate_const    (t_const*    tconst);
  void generate_struct   (t_struct*   tstruct);
  void generate_xception (t_struct*   txception);
  void generate_service  (t_service*  tservice);

  std::string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_hs_struct(t_struct* tstruct, bool is_exception);
  void generate_hs_struct_definition(std::ofstream &out,t_struct* tstruct, bool is_xception=false,bool helper=false);
  void generate_hs_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_hs_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_hs_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_service_helpers   (t_service*  tservice);
  void generate_service_interface (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string prefix);
  
  void generate_deserialize_struct       (std::ofstream &out,
                                          t_struct*   tstruct);
  
  void generate_deserialize_container    (std::ofstream &out,
                                          t_type*     ttype);
  
  void generate_deserialize_set_element  (std::ofstream &out,
                                          t_set*      tset);


  void generate_deserialize_list_element (std::ofstream &out,
                                          t_list*     tlist,
                                          std::string prefix="");
  void generate_deserialize_type          (std::ofstream &out,
                                           t_type* type);

  void generate_serialize_field          (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string name= "");

  void generate_serialize_struct         (std::ofstream &out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_serialize_container      (std::ofstream &out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (std::ofstream &out,
                                          t_map*      tmap,
                                          std::string kiter,
                                          std::string viter);

  void generate_serialize_set_element    (std::ofstream &out,
                                          t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (std::ofstream &out,
                                          t_list*     tlist,
                                          std::string iter);

  /**
   * Helper rendering functions
   */

  std::string hs_autogen_comment();
  std::string hs_imports();
  std::string type_name(t_type* ttype);
  std::string function_type(t_function* tfunc, bool options = false, bool io = false, bool method = false);
  std::string type_to_enum(t_type* ttype);
  std::string render_hs_type(t_type* type);


 private:

  /**
   * File streams
   */

  std::ofstream f_types_;
  std::ofstream f_consts_;
  std::ofstream f_service_;
  std::ofstream f_iface_;
  std::ofstream f_client_;

};

#endif
