#ifndef T_TYPE_H
#define T_TYPE_H

#include <string>
#include "t_doc.h"

class t_program;

/**
 * Generic representation of a thrift type. These objects are used by the
 * parser module to build up a tree of object that are all explicitly typed.
 * The generic t_type class exports a variety of useful methods that are
 * used by the code generator to branch based upon different handling for the
 * various types.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_type : public t_doc {
 public:
  virtual ~t_type() {}

  virtual void set_name(std::string name) {
    name_ = name;
  }

  virtual const std::string& get_name() const {
    return name_;
  }

  virtual bool is_void()      const { return false; }
  virtual bool is_base_type() const { return false; }
  virtual bool is_string()    const { return false; }
  virtual bool is_typedef()   const { return false; }
  virtual bool is_enum()      const { return false; }
  virtual bool is_struct()    const { return false; }
  virtual bool is_xception()  const { return false; }
  virtual bool is_container() const { return false; }
  virtual bool is_list()      const { return false; }
  virtual bool is_set()       const { return false; }
  virtual bool is_map()       const { return false; }
  virtual bool is_service()   const { return false; }

  t_program* get_program() {
    return program_;
  }

 protected:
  t_type() {}

  t_type(t_program* program) :
    program_(program) {}

  t_type(t_program* program, std::string name) :
    program_(program),
    name_(name) {}

  t_type(std::string name) :
    name_(name) {}

  t_program* program_;
  std::string name_;

};

#endif
