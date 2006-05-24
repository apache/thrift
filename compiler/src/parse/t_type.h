#ifndef T_TYPE_H
#define T_TYPE_H

#include <string>

/**
 * Generic representation of a thrift type.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_type {
 public:
  virtual ~t_type() {}

  virtual const std::string& get_name() const { return name_; }

  virtual bool is_base_type() const { return false; }
  virtual bool is_typedef()   const { return false; }
  virtual bool is_enum()      const { return false; }
  virtual bool is_struct()    const { return false; }

 protected:
  t_type() {}

  t_type(std::string name) :
    name_(name) {}

  std::string name_;
};

#endif
