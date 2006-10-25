#ifndef T_SERVICE_H
#define T_SERVICE_H

#include "t_function.h"
#include <vector>

class t_program;

/**
 * A service consists of a set of functions.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_service : public t_type {
 public:
  t_service(t_program* program) :
    t_type(program),
    extends_(NULL) {}

  bool is_service() const {
    return true;
  }

  void set_extends(t_service* extends) {
    extends_ = extends;
  }

  void add_function(t_function* func) {
    functions_.push_back(func);
  }

  const std::vector<t_function*>& get_functions() const {
    return functions_;
  }

  t_service* get_extends() {
    return extends_;
  }

 private:
  std::vector<t_function*> functions_;
  t_service* extends_;
};

#endif
