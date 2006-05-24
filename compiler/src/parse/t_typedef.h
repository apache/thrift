#ifndef T_TYPEDEF_H
#define T_TYPEDEF_H

#include <string>
#include "t_type.h"

/**
 * A typedef is a mapping from a symbolic name to another type.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_typedef : public t_type {
 public:
  t_typedef(t_type* type, std::string symbolic) :
    t_type(symbolic), type_(type), symbolic_(symbolic) {}

  ~t_typedef() {}

  t_type* get_type() const { return type_; }
  const std::string& get_symbolic() const { return symbolic_; }
  bool is_typedef() const { return true; }

 private:
  /** Type that this definition inherits from */
  t_type* type_;

  /** Symbolic name for this type */
  std::string symbolic_;
};

#endif
