#include "t_generator.h"
using namespace std;

/**
 * Top level program generation function. Calls the generator subclass methods
 * for preparing file streams etc. then iterates over all the parts of the
 * program to perform the correct actions.
 *
 * @param program The thrift program to compile into C++ source
 * @author Mark Slee <mcslee@facebook.com>
 */
void t_generator::generate_program() {
  // Initialize the generator
  init_generator();

  // Generate typedefs
  vector<t_typedef*> typedefs = program_->get_typedefs();
  vector<t_typedef*>::iterator td_iter;
  for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
    generate_typedef(*td_iter);
  }

  // Generate enums
  vector<t_enum*> enums = program_->get_enums();
  vector<t_enum*>::iterator en_iter;
  for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
    generate_enum(*en_iter);
  }

  // Generate constants
  vector<t_const*> consts = program_->get_consts();
  generate_consts(consts);

  // Generate structs
  vector<t_struct*> structs = program_->get_structs();
  vector<t_struct*>::iterator st_iter;
  for (st_iter = structs.begin(); st_iter != structs.end(); ++st_iter) {
    generate_struct(*st_iter);
  }

  // Generate xceptions
  vector<t_struct*> xceptions = program_->get_xceptions();
  vector<t_struct*>::iterator x_iter;
  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    generate_xception(*x_iter);
  }

  // Generate services
  vector<t_service*> services = program_->get_services();
  vector<t_service*>::iterator sv_iter;
  for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
    service_name_ = get_service_name(*sv_iter);
    generate_service(*sv_iter);
  }

  // Close the generator
  close_generator();
}

void t_generator::generate_consts(vector<t_const*> consts) {
  vector<t_const*>::iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    generate_const(*c_iter);
  }
}
