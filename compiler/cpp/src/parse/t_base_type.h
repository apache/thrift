#ifndef T_BASE_TYPE_H
#define T_BASE_TYPE_H

#include "t_type.h"

/**
 * A thrift base type, which must be one of the defined enumerated types.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_base_type : public t_type {
 public:
  /** Enumeration of thrift base types */
  enum t_base {
    TYPE_VOID,
    TYPE_STRING,
    TYPE_BYTE,
    TYPE_I16,
    TYPE_I32,
    TYPE_I64
  };

  t_base_type(std::string name, t_base base) :
    t_type(name), base_(base) {}
    
  t_base get_base() const { return base_; }
  bool is_void() const { return base_ == TYPE_VOID; }
  bool is_base_type() const { return true; }
    
 private:
  t_base base_;
};

#endif
