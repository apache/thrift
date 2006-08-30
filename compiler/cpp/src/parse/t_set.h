#ifndef T_SET_H
#define T_SET_H

#include "t_type.h"

class t_set : public t_type {
 public:
  t_set(t_type* elem_type) : elem_type_(elem_type) {}
  ~t_set() {}

  t_type* get_elem_type() const { return elem_type_; }
  bool is_set() const { return true; }

 private:
  t_type* elem_type_;
};

#endif
