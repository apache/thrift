// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_XSD_GENERATOR_H
#define T_XSD_GENERATOR_H

#include <fstream>
#include <iostream>
#include <sstream>
#include "t_generator.h"

/**
 * XSD generator, creates an XSD for the base types etc.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_xsd_generator : public t_generator {
 public:
  t_xsd_generator(t_program* program) :
    t_generator(program) {
    out_dir_base_ = "gen-xsd";
  }

  virtual ~t_xsd_generator() {}

  /**
   * Init and close methods
   */

  void init_generator();
  void close_generator();

  /**
   * Program-level generation functions
   */

  void generate_typedef(t_typedef* ttypedef);
  void generate_enum(t_enum* tenum) {}

  void generate_service(t_service* tservice);
  void generate_struct(t_struct* tstruct);

 private:

  void generate_element(std::ostream& out, std::string name, t_type* ttype, t_struct* attrs=NULL, bool optional=false, bool nillable=false, bool list_element=false);

  std::string ns(std::string in, std::string ns) {
    return ns + ":" + in;
  }

  std::string xsd(std::string in) {
    return ns(in, "xsd");
  }

  std::string type_name(t_type* ttype);
  std::string base_type_name(t_base_type::t_base tbase);

  /**
   * Output xsd/php file
   */
  std::ofstream f_xsd_;
  std::ofstream f_php_;

  /**
   * Output string stream
   */
  std::ostringstream s_xsd_types_;

};

#endif
