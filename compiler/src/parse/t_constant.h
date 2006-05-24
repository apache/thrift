#ifndef T_CONSTANT_H
#define T_CONSTANT_H

#include <string>

class t_constant {
 public:
  t_constant(std::string name) :
    name_(name), has_value_(false), value_(0) {}

  t_constant(std::string name, int value) :
    name_(name), has_value_(true), value_(value) {}

  ~t_constant() {}

  const std::string& get_name() { return name_; }
  bool has_value() { return has_value_; }
  int get_value() { return value_; }
  
 private:
  std::string name_;
  bool has_value_;
  int value_;
};  

#endif
