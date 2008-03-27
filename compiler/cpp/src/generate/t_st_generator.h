// Copyright (c) 2007- Patrick Collison <patrick@collison.ie>
// Copyright (c) 2006- Facebook
//
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_ST_GENERATOR_H
#define T_ST_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <boost/tokenizer.hpp>

#include "t_oop_generator.h"

/**
 * Smalltalk code generator.
 *
 * @author Patrick Collison <patrick@collison.ie>
 */
class t_st_generator : public t_oop_generator {
 public:
  t_st_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string)
    : t_oop_generator(program)
  {
    out_dir_base_ = "gen-st";
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
  void generate_class_side_definition ();
  void generate_force_consts ();


  std::string render_const_value(t_type* type, t_const_value* value);

  /**
   * Struct generation code
   */

  void generate_st_struct (std::ofstream& out, t_struct* tstruct, bool is_exception);
  void generate_accessors   (std::ofstream& out, t_struct* tstruct);

  /**
   * Service-level generation functions
   */

  void generate_service_client    (t_service* tservice);

  void generate_send_method (t_function* tfunction);
  void generate_recv_method (t_function* tfunction);

  std::string map_reader (t_map *tmap);
  std::string list_reader (t_list *tlist);
  std::string set_reader (t_set *tset);
  std::string struct_reader (t_struct *tstruct, std::string clsName);

  std::string map_writer (t_map *tmap, std::string name);
  std::string list_writer (t_list *tlist, std::string name);
  std::string set_writer (t_set *tset, std::string name);
  std::string struct_writer (t_struct *tstruct, std::string fname);

  std::string write_val (t_type *t, std::string fname);
  std::string read_val (t_type *t);

  /**
   * Helper rendering functions
   */

  std::string st_autogen_comment();

  void st_class_def(std::ofstream &out, std::string name);
  void st_method(std::ofstream &out, std::string cls, std::string name);
  void st_method(std::ofstream &out, std::string cls, std::string name, std::string category);
  void st_close_method(std::ofstream &out);
  void st_class_method(std::ofstream &out, std::string cls, std::string name);
  void st_class_method(std::ofstream &out, std::string cls, std::string name, std::string category);
  void st_setter(std::ofstream &out, std::string cls, std::string name, std::string type);
  void st_getter(std::ofstream &out, std::string cls, std::string name);
  void st_accessors(std::ofstream &out, std::string cls, std::string name, std::string type);

  std::string class_name();
  std::string client_class_name();
  std::string prefix(std::string name);
  std::string declare_field(t_field* tfield);
  std::string sanitize(std::string s);
  std::string type_name(t_type* ttype);

  std::string function_signature(t_function* tfunction);
  std::string argument_list(t_struct* tstruct);
  std::string function_types_comment(t_function* fn);

  std::string type_to_enum(t_type* ttype);
  std::string a_type(t_type* type);
  bool is_vowel(char c);
  std::string temp_name();
  std::string generated_category();

 private:

  /**
   * File streams
   */
  int temporary_var;
  std::ofstream f_;

};

#endif
