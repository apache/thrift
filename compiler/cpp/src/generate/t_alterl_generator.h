#ifndef T_ALTERL_GENERATOR_H
#define T_ALTERL_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_oop_generator.h"

/**
 * Erlang code generator.
 *
 * @author
 */
class t_alterl_generator : public t_generator {
 public:
  t_alterl_generator(t_program* program) :
    t_generator(program) 
  {
    program_name_[0] = tolower(program_name_[0]);
    service_name_[0] = tolower(service_name_[0]);
    out_dir_base_ = "gen-erl";
  }

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */

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

  void generate_erl_struct(t_struct* tstruct, bool is_exception);
  void generate_erl_struct_definition(std::ostream& out, std::ostream& hrl_out, t_struct* tstruct, bool is_xception=false, bool is_result=false);
  void generate_erl_struct_info(std::ostream& out, t_struct* tstruct);
  void generate_erl_function_helpers(t_function* tfunction);

  /**
   * Service-level generation functions
   */

  void generate_service_helpers   (t_service*  tservice);
  void generate_service_interface (t_service* tservice);
  void generate_function_info     (t_service* tservice, t_function* tfunction);

  /**
   * Helper rendering functions
   */

  std::string erl_autogen_comment();
  std::string erl_imports();
  std::string render_includes();
  std::string declare_field(t_field* tfield);
  std::string type_name(t_type* ttype);

  std::string function_signature(t_function* tfunction, std::string prefix="");


  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);
  std::string generate_type_term(t_type* ttype, bool expand_structs);
  std::string type_module(t_type* ttype);

  std::string capitalize(std::string in) {
    in[0] = toupper(in[0]);
    return in;
  }

  std::string uncapitalize(std::string in) {
    in[0] = tolower(in[0]);
    return in;
  }

 private:

  /**
   * add function to export list
   */

  void export_function(t_function* tfunction, std::string prefix="");
  void export_string(std::string name, int num);

  void export_types_function(t_function* tfunction, std::string prefix="");
  void export_types_string(std::string name, int num);

  /**
   * write out headers and footers for hrl files
   */
  
  void hrl_header(std::ostream& out, std::string name);
  void hrl_footer(std::ostream& out, std::string name); 

  /**
   * stuff to spit out at the top of generated files
   */

  bool export_lines_first_;
  std::ostringstream export_lines_;

  bool export_types_lines_first_;
  std::ostringstream export_types_lines_;

  /**
   * File streams
   */

  std::ostringstream f_types_;
  std::ofstream f_types_file_;
  std::ofstream f_types_hrl_file_;

  std::ofstream f_consts_; 
  std::ostringstream f_service_;
  std::ofstream f_service_file_;
  std::ofstream f_service_hrl_;

};

#endif
