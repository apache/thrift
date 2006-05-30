#ifndef T_STRUCT_H
#define T_STRUCT_H

#include <vector>
#include <string>

#include "t_type.h"
#include "t_field.h"

class t_struct : public t_type {
 public:
  t_struct() {}
  ~t_struct() {}

  /** Set the struct name */
  void set_name(const std::string& name) { name_ = name; }

  /** Add a new field to the list */
  void append(t_field* elem) { members_.push_back(elem); }

  const std::vector<t_field*>& get_members() { return members_; }
  bool is_struct() const { return true; }

 private:
  std::vector<t_field*> members_;
};

#endif
