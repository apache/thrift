// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_CPP_GENERATOR_H
#define T_CPP_GENERATOR_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>

#include "t_oop_generator.h"

/**
 * C++ code generator. This is legitimacy incarnate.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_cpp_generator : public t_oop_generator {
 public:
  t_cpp_generator(t_program* program, bool gen_dense) :
    t_oop_generator(program),
    gen_dense_(gen_dense),
    use_include_prefix_(false) {

    out_dir_base_ = "gen-cpp";
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

  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum);
  void generate_struct(t_struct* tstruct) {
    generate_cpp_struct(tstruct, false);
  }
  void generate_xception(t_struct* txception) {
    generate_cpp_struct(txception, true);
  }
  void generate_cpp_struct(t_struct* tstruct, bool is_exception);

  void generate_service(t_service* tservice);

  void print_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value);
  std::string render_const_value(std::ofstream& out, std::string name, t_type* type, t_const_value* value);

  void generate_struct_definition    (std::ofstream& out, t_struct* tstruct, bool is_exception=false, bool pointers=false, bool read=true, bool write=true);
  void generate_struct_fingerprint   (std::ofstream& out, t_struct* tstruct, bool is_definition);
  void generate_struct_reader        (std::ofstream& out, t_struct* tstruct, bool pointers=false);
  void generate_struct_writer        (std::ofstream& out, t_struct* tstruct, bool pointers=false);
  void generate_struct_result_writer (std::ofstream& out, t_struct* tstruct, bool pointers=false);

  /**
   * Service-level generation functions
   */

  void generate_service_interface (t_service* tservice);
  void generate_service_null      (t_service* tservice);
  void generate_service_multiface (t_service* tservice);
  void generate_service_helpers   (t_service* tservice);
  void generate_service_client    (t_service* tservice);
  void generate_service_processor (t_service* tservice);
  void generate_service_skeleton  (t_service* tservice);
  void generate_process_function  (t_service* tservice, t_function* tfunction);
  void generate_function_helpers  (t_service* tservice, t_function* tfunction);

  void generate_service_limited_reflector(t_service* tservice);
  bool generate_type_limited_reflection(t_type* ttype, std::string target);
  bool generate_simple_type_limited_reflection(std::ostream& out, t_type* ttype, std::string target);

  /**
   * Serialization constructs
   */

  void generate_deserialize_field        (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string prefix="",
                                          std::string suffix="");

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
                                          std::string prefix,
                                          std::string index);

  void generate_serialize_field          (std::ofstream& out,
                                          t_field*    tfield,
                                          std::string prefix="",
                                          std::string suffix="");

  void generate_serialize_struct         (std::ofstream& out,
                                          t_struct*   tstruct,
                                          std::string prefix="");

  void generate_serialize_container      (std::ofstream& out,
                                          t_type*     ttype,
                                          std::string prefix="");

  void generate_serialize_map_element    (std::ofstream& out,
                                          t_map*      tmap,
                                          std::string iter);

  void generate_serialize_set_element    (std::ofstream& out,
                                          t_set*      tmap,
                                          std::string iter);

  void generate_serialize_list_element   (std::ofstream& out,
                                          t_list*     tlist,
                                          std::string iter);

  /**
   * Helper rendering functions
   */

  std::string namespace_prefix(std::string ns);
  std::string namespace_open(std::string ns);
  std::string namespace_close(std::string ns);
  std::string type_name(t_type* ttype, bool in_typedef=false, bool arg=false);
  std::string base_type_name(t_base_type::t_base tbase);
  std::string declare_field(t_field* tfield, bool init=false, bool pointer=false, bool constant=false, bool reference=false);
  std::string function_signature(t_function* tfunction, std::string prefix="");
  std::string argument_list(t_struct* tstruct);
  std::string type_to_enum(t_type* ttype);
  std::string local_reflection_name(const char*, t_type* ttype);

  // These handles checking gen_dense_ and checking for duplicates.
  void generate_local_reflection(std::ofstream& out, t_type* ttype, bool is_definition);
  void generate_local_reflection_pointer(std::ofstream& out, t_type* ttype);

  bool is_complex_type(t_type* ttype) {
    ttype = get_true_type(ttype);

    return
      ttype->is_container() ||
      ttype->is_struct() ||
      ttype->is_xception() ||
      (ttype->is_base_type() && (((t_base_type*)ttype)->get_base() == t_base_type::TYPE_STRING));
  }

  void set_use_include_prefix(bool use_include_prefix) {
    use_include_prefix_ = use_include_prefix;
  }

 private:
  /**
   * Returns the include prefix to use for a file generated by program, or the
   * empty string if no include prefix should be used.
   */
  std::string get_include_prefix(const t_program& program) const;

  /**
   * True iff we should generate local reflection metadata for TDenseProtocol.
   */
  bool gen_dense_;

  /**
   * True iff we should use a path prefix in our #include statements for other
   * thrift-generated header files.
   */
  bool use_include_prefix_;

  /**
   * Strings for namespace, computed once up front then used directly
   */

  std::string ns_open_;
  std::string ns_close_;

  /**
   * File streams, stored here to avoid passing them as parameters to every
   * function.
   */

  std::ofstream f_types_;
  std::ofstream f_types_impl_;
  std::ofstream f_header_;
  std::ofstream f_service_;

  /**
   * When generating local reflections, make sure we don't generate duplicates.
   */
  std::set<std::string> reflected_fingerprints_;
};

#endif
