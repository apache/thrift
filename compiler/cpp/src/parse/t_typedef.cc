#include <cstdio>

#include "t_typedef.h"
#include "t_program.h"

t_type* t_typedef::get_type() const {
  if (type_ == NULL) {
    type_ = get_program()->scope()->get_type(symbolic_);
    if (type_ == NULL) {
      printf("Type \"%s\" not defined\n", symbolic_.c_str());
      exit(1);
    }
  }
  return type_;
}
