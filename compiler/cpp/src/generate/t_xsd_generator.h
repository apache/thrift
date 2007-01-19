#ifndef T_XSD_GENERATOR_H
#define T_XSD_GENERATOR_H

#include <fstream>
#include <iostream>
#include <sstream>
#include "t_generator.h"

// TODO(mcslee): Paramaterize the output dir
#define T_XSD_DIR "gen-xsd"

/**
 * XSD generator, creates an XSD for the base types etc.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_xsd_generator : public t_generator {
 public:
  t_xsd_generator(t_program* program) :
    t_generator(program) {}

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

  void generate_element(std::ostream& out, std::string name, t_type* ttype, bool optional=false);

  std::string ns(std::string in, std::string ns) {
    return ns + ":" + in;
  }

  std::string xsd(std::string in) {
    return ns(in, "xsd");
  }

  std::string type_name(t_type* ttype);
  std::string base_type_name(t_base_type::t_base tbase);

  /**
   * Output xsd file
   */
  std::ofstream f_xsd_;

  /**
   * Output string stream
   */
  std::ostringstream s_xsd_types_;

};

#endif
