#ifndef T_STRUCT_H
#define T_STRUCT_H

#include <vector>
#include <string>

#include "t_type.h"
#include "t_field.h"

class t_struct : public t_type {
 public:
  t_struct() : is_xception_(false) {}
  t_struct(const std::string& name) : t_type(name), is_xception_(false) {}

  ~t_struct() {}

  /** Set the struct name */
  void set_name(const std::string& name) {
    name_ = name;
  }

  /** Mark as an exception */
  void set_xception(bool is_xception) {
    is_xception_ = is_xception;
  }

  /** Add a new field to the list */
  void append(t_field* elem) {
    members_.push_back(elem);
  }

  const std::vector<t_field*>& get_members() {
    return members_;
  }

  bool is_struct() const {
    return !is_xception_;
  }

  bool is_xception() const {
    return is_xception_;
  }

 private:
  std::vector<t_field*> members_;
  bool is_xception_;
};

#endif
