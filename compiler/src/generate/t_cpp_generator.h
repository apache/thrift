#ifndef T_CPP_GENERATOR_H
#define T_CPP_GENERATOR_H

#include <string>
#include <fstream>

#include "t_generator.h"

#define T_CPP_DIR "gen-cpp"

/**
 * C++ code generator
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
  void generate_service_server    (t_service* tservice);
  void generate_service_client    (t_service* tservice);

  /** Serialization constructs */
  void generate_dispatch_function (t_service* tservice, t_function* tfunction);
  void generate_deserialize_struct(t_field* tfield);
  void generate_deserialize_field(t_field* tfield);

  /** Helper rendering functions */
  std::string autogen_comment();
  std::string type_name(t_type* ttype);
  std::string base_type_name(t_base_type::t_base tbase);
  std::string declare_field(t_field* tfield);
  std::string function_signature(t_function* tfunction);
  std::string field_list(t_list* tlist);
  
 private:
  /** File streams */
  std::ofstream f_types_;
  std::ofstream f_header_;
  std::ofstream f_service_;
};

#endif
