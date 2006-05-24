#ifndef T_STRUCT_H
#define T_STRUCT_H

#include <vector>
#include <string>

#include "t_type.h"
#include "t_list.h"

class t_struct : public t_type {
 public:
  t_struct(std::string name, t_list* members) :
    t_type(name), members_(members) {}
  ~t_struct() {}

  t_list* get_members() { return members_; }
  bool is_struct() { return true; }

 private:
  t_list* members_;
};

#endif
