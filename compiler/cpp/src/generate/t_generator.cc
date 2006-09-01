#include "t_generator.h"
using namespace std;

/**
 * Top level program generation function. Calls the generator subclass methods
 * for preparing file streams etc. then iterates over all the parts of the
 * program to perform the correct actions.
 *
 * @param program The thrift program to compile into C++ source
 */
void t_generator::generate_program(t_program *tprogram) {
  // Set program name
  program_name_ = get_program_name(tprogram);

  // Initialize the generator
  init_generator(tprogram);

  // Generate typedefs
  vector<t_typedef*> typedefs = tprogram->get_typedefs();
  vector<t_typedef*>::iterator td_iter;
  for (td_iter = typedefs.begin(); td_iter != typedefs.end(); ++td_iter) {
    generate_typedef(*td_iter);
  }

  // Generate enums
  vector<t_enum*> enums = tprogram->get_enums();
  vector<t_enum*>::iterator en_iter;
  for (en_iter = enums.begin(); en_iter != enums.end(); ++en_iter) {
    generate_enum(*en_iter);
  }

  // Generate structs
  vector<t_struct*> structs = tprogram->get_structs();
  vector<t_struct*>::iterator st_iter;
  for (st_iter = structs.begin(); st_iter != structs.end(); ++st_iter) {
    generate_struct(*st_iter);
  }

  // Generate xceptions
  vector<t_struct*> xceptions = tprogram->get_xceptions();
  vector<t_struct*>::iterator x_iter;
  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    generate_xception(*x_iter);
  }

  // Generate services
  vector<t_service*> services = tprogram->get_services();
  vector<t_service*>::iterator sv_iter;
  for (sv_iter = services.begin(); sv_iter != services.end(); ++sv_iter) {
    service_name_ = get_service_name(*sv_iter);
    generate_service(*sv_iter);
  }

  // Close the generator
  close_generator(tprogram);
}
