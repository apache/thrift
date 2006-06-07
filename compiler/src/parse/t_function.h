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

  t_type*      get_returntype() const { return returntype_; }
  const std::string& get_name() const { return name_; }
  t_struct*    get_arglist()    const { return arglist_; }

 private:
  t_type* returntype_;
  std::string name_;
  t_struct* arglist_;
};

#endif
