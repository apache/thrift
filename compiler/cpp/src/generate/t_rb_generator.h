// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_RB_GENERATOR_H
#define T_RB_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <boost/tokenizer.hpp>

#include "t_oop_generator.h"

/**
 * Ruby code generator.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_rb_generator : public t_oop_generator {
 public:
  t_rb_generator(t_program* program) :
    t_oop_generator(program) {

    out_dir_base_ = "gen-rb";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */

  void generate_typedef     (t_typedef*  ttypedef);
  void generate_enum        (t_enum*     tenum);
  void generate_const       (t_const*    tconst);
  void generate_struct      (t_struct*   tstruct);
  void generate_xception    (t_struct*   txception);
  void generate_service     (t_service*  tservice);

  std::string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_rb_struct(std::ofstream& out, t_struct* tstruct, bool is_exception);
  void generate_rb_struct_reader(std::ofstream& out, t_struct* tstruct);
  void generate_rb_struct_writer(std::ofstream& out, t_struct* tstruct);
  void generate_rb_function_helpers(t_function* tfunction);
  void generate_rb_simple_exception_constructor(std::ofstream& out, t_struct* tstruct);
  void generate_accessors   (std::ofstream& out, t_struct* tstruct);
  void generate_field_defns (std::ofstream& out, t_struct* tstruct);
  void generate_field_data  (std::ofstream& out, t_type* field_type, const std::string& field_name, t_const_value* field_value);

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
                                          std::string prefix="",
                                          bool inclass=false);
  
  void generate_deserialize_struct       (std::ofstream &out,
                                          t_struct*   tstruct,
                                          std::string prefix="");
  
  void generate_deserialize_container    (std::ofstream &out,
                                          t_type*     ttype,
                                          std::string prefix="");
  
  void generate_deserialize_set_element  (std::ofstream &out,
                                          t_set*      tset,
                                          std::string prefix="");

  void generate_deserialize_map_element  (std::ofstream &out,
                                          t_map*      tmap,
                                          std::string prefix="");

  void generate_deserialize_list_element (std::ofstream &out,
                                          t_list*     tlist,
                                          std::string prefix="");

  void generate_serialize_field          (std::ofstream &out,
                                          t_field*    tfield,
                                          std::string prefix="");

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

  std::string rb_autogen_comment();
  std::string rb_imports();
  std::string render_includes();
  std::string declare_field(t_field* tfield);
  std::string type_name(t_type* ttype);
  std::string function_signature(t_function* tfunction, std::string prefix="");
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);


  
  std::string ruby_namespace(t_program* p) {
    std::string ns = p->get_ruby_namespace();
    return ns.size() ? ns : "";
  }
  
  std::vector<std::string> ruby_modules(t_program* p) {
    std::string ns = p->get_ruby_namespace();
    boost::tokenizer<> tok(ns);
    std::vector<std::string> modules;
    
    for(boost::tokenizer<>::iterator beg=tok.begin(); beg != tok.end(); ++beg) {
      modules.push_back(*beg);
    }
    
    return modules;
  }
  
  void begin_namespace(std::ofstream&, std::vector<std::string>);
  void end_namespace(std::ofstream&, std::vector<std::string>);

 private:

  /**
   * File streams
   */

  std::ofstream f_types_;
  std::ofstream f_consts_; 
  std::ofstream f_service_;

};

#endif
