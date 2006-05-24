#ifndef T_SERVICE_H
#define T_SERVICE_H

#include "t_function.h"
#include <vector>

class t_service {
 public:
  t_service() {}
  ~t_service() {}

  void set_name(std::string name) { name_ = name; }
  void add_function(t_function* func) { functions_.push_back(func); }

  const std::string& get_name() const { return name_; }
  const std::vector<t_function*>& get_functions() const { return functions_; }

 private:
  std::string name_;
  std::vector<t_function*> functions_;
};

#endif
