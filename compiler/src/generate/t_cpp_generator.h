#ifndef T_CPP_GENERATOR_H
#define T_CPP_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_generator.h"

#define T_CPP_DIR "gen-cpp"

/**
 * C++ code generator.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_cpp_generator : public t_generator {
 public:
  t_cpp_generator() {}
  ~t_cpp_generator() {}

  /** Init and close methods */

  void init_generator(t_program *tprogram);
  void close_generator();

  /** Program-level generation functions */

  void generate_typedef (t_typedef*  ttypedef);
  void generate_enum    (t_enum*     tenum);
  void generate_struct  (t_struct*   tstruct);
  void generate_service (t_service*  tservice);

  /** Service-level generation functions */

  void generate_service_interface (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_server    (t_service* tservice);
  void generate_dispatch_function (t_service* tservice, t_function* tfunction);

  /** Serialization constructs */

  void generate_deserialize_field        (std::string src,
                                          t_field*    tfield, 
                                          std::string prefix="");
  
  void generate_deserialize_struct       (std::string src,
                                          t_struct*   tstruct,
                                          std::string prefix="");
  
  void generate_deserialize_container    (std::string src,
                                          t_type*     ttype,
                                          std::string prefix="");
  
  void generate_deserialize_set_element  (std::string src,
                                          t_set*      tset,
                                          std::string prefix="");

  void generate_deserialize_map_element  (std::string src,
                                          t_map*      tmap,
                                          std::string prefix="");

  void generate_deserialize_list_element (std::string src,
                                          t_list*     tlist,
                                          std::string prefix="");

  void generate_serialize_field          (std::string dest,
                                          t_field*    tfield,
                                          std::string prefix="");

  void generate_serialize_struct         (std::string dest,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_serialize_container      (std::string dest,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (std::string dest,
                                          t_map*      tmap,
                                          std::string iter);

  void generate_serialize_set_element    (std::string dest,
                                          t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (std::string dest,
                                          t_list*     tlist,
                                          std::string iter);

  /** Scoping */

  void scope_up(std::ostream& out) {
    indent(out) << "{" << std::endl;
    indent_up();
  }

  void scope_down(std::ostream& out) {
    indent_down();
    indent(out) << "}" << std::endl;
  }

  /** Helper rendering functions */

  std::string autogen_comment();
  std::string type_name(t_type* ttype);
  std::string base_type_name(t_base_type::t_base tbase);
  std::string declare_field(t_field* tfield, bool init=false);
  std::string function_signature(t_function* tfunction, std::string prefix="");
  std::string argument_list(t_struct* tstruct);
  
 private:
  /** File streams */

  std::ofstream f_types_;
  std::ofstream f_header_;
  std::ofstream f_service_;
};

#endif
