// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_FUNCTION_H
#define T_FUNCTION_H

#include <string>
#include "t_type.h"
#include "t_struct.h"
#include "t_doc.h"

/**
 * Representation of a function. Key parts are return type, function name,
 * optional modifiers, and an argument list, which is implemented as a thrift
 * struct.
 *
 */
class t_function : public t_doc {
 public:
  t_function(t_type* returntype,
             std::string name,
             t_struct* arglist,
             bool async=false) :
    returntype_(returntype),
    name_(name),
    arglist_(arglist),
    async_(async) {
    xceptions_ = new t_struct(NULL);
  }

  t_function(t_type* returntype,
             std::string name,
             t_struct* arglist,
             t_struct* xceptions,
             bool async=false) :
    returntype_(returntype),
    name_(name),
    arglist_(arglist),
    xceptions_(xceptions),
    async_(async)
  {
    if (async_ && !xceptions_->get_members().empty()) {
      throw std::string("Async methods can't throw exceptions.");
    }
  }

  ~t_function() {}

  t_type* get_returntype() const {
    return returntype_;
  }

  const std::string& get_name() const {
    return name_;
  }

  t_struct* get_arglist() const {
    return arglist_;
  }

  t_struct* get_xceptions() const {
    return xceptions_;
  }

  bool is_async() const {
    return async_;
  }

 private:
  t_type* returntype_;
  std::string name_;
  t_struct* arglist_;
  t_struct* xceptions_;
  bool async_;
};

#endif
