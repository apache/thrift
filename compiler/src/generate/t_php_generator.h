#ifndef T_PHP_GENERATOR_H
#define T_PHP_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_oop_generator.h"

// TODO(mcslee: Paramaterize the output dir
#define T_PHP_DIR "gen-php"

/**
 * Java code generator.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_php_generator : public t_oop_generator {
 public:
  t_php_generator() {}
  ~t_php_generator() {}

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

  /** Serialization constructs */

  void generate_deserialize_field        (t_field*    tfield, 
                                          std::string prefix="");
  
  void generate_deserialize_struct       (t_struct*   tstruct,
                                          std::string prefix="");
  
  void generate_deserialize_container    (t_type*     ttype,
                                          std::string prefix="");
  
  void generate_deserialize_set_element  (t_set*      tset,
                                          std::string prefix="");

  void generate_deserialize_map_element  (t_map*      tmap,
                                          std::string prefix="");

  void generate_deserialize_list_element (t_list*     tlist,
                                          std::string prefix="");

  void generate_serialize_field          (t_field*    tfield,
                                          std::string prefix="");

  void generate_serialize_struct         (t_struct*   tstruct,
                                          std::string prefix="");

  void generate_serialize_container      (t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (t_map*      tmap,
                                          std::string kiter,
                                          std::string viter);

  void generate_serialize_set_element    (t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (t_list*     tlist,
                                          std::string iter);

  /** Helper rendering functions */

  std::string php_includes();
  std::string type_name(t_type* ttype);
  std::string base_type_name(t_base_type::t_base tbase);
  std::string declare_field(t_field* tfield, bool init=false, bool obj=false);
  std::string function_signature(t_function* tfunction, std::string prefix="");
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);

 private:

  /** File streams */

  std::ofstream f_types_;
  std::ofstream f_service_;
};

#endif
