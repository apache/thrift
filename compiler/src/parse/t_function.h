#ifndef T_FUNCTION_H
#define T_FUNCTION_H

#include <string>
#include "t_type.h"
#include "t_struct.h"

/**
 * Representation of a function. Key parst are return type, function name,
 * optional modifiers, and an argument list. Each function also has a
 * hash signature that is used in the network protocol.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_function {
 public:
  t_function(t_type* returntype, std::string name, t_struct* arglist) :
    returntype_(returntype), name_(name), arglist_(arglist) {}

  ~t_function() {}

  /**
   * Implementation of the Fowler / Noll / Vo (FNV) Hash 
   *	http://www.isthe.com/chongo/tech/comp/fnv/
   */
  static uint32_t fnv32(const char *s) {
    uint32_t hash = (uint32_t)216613626;
    while (*s) {
      hash +=
        (hash << 1) +
        (hash << 4) +
        (hash << 7) +
        (hash << 8) +
        (hash << 24);
      hash ^= *s++;
    }
    return hash;
  }

  t_type*      get_returntype() const { return returntype_; }
  const std::string& get_name() const { return name_; }
  t_struct*    get_arglist()    const { return arglist_; }
  uint32_t     get_hash()       const { return fnv32(name_.c_str()); }
 
 private:
  t_type* returntype_;
  std::string name_;
  t_struct* arglist_;
};

#endif
