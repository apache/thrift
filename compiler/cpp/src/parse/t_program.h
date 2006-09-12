#ifndef T_PROGRAM_H
#define T_PROGRAM_H

#include <map>
#include <string>
#include <vector>

#include "t_base_type.h"
#include "t_typedef.h"
#include "t_enum.h"
#include "t_struct.h"
#include "t_service.h"
#include "t_list.h"
#include "t_map.h"
#include "t_set.h"

/**
 * Top level class representing an entire thrift program. A program consists
 * fundamentally of the following:
 *
 *   Typedefs
 *   Enumerations
 *   Structs
 *   Exceptions
 *   Services
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_program {
 public:
  t_program(std::string name) :
    name_(name), namespace_() {
    type_void   = new t_base_type("void",   t_base_type::TYPE_VOID);
    type_string = new t_base_type("string", t_base_type::TYPE_STRING);
    type_bool   = new t_base_type("bool",   t_base_type::TYPE_BOOL);
    type_byte   = new t_base_type("byte",   t_base_type::TYPE_BYTE);
    type_i16    = new t_base_type("i16",    t_base_type::TYPE_I16);
    type_i32    = new t_base_type("i32",    t_base_type::TYPE_I32);
    type_i64    = new t_base_type("i64",    t_base_type::TYPE_I64);
    type_double = new t_base_type("double", t_base_type::TYPE_DOUBLE);
  }

  ~t_program() {
    delete type_string;
    delete type_bool;
    delete type_byte;
    delete type_i16;
    delete type_i32;
    delete type_i64;
    delete type_double;
  }

  // Name accessor
  const std::string& get_name() const { return name_; }

  // Namespace
  const std::string& get_namespace() const { return namespace_; }

  // Accessors for program elements
  const std::vector<t_typedef*>& get_typedefs()  const { return typedefs_;  }
  const std::vector<t_enum*>&    get_enums()     const { return enums_;     }
  const std::vector<t_struct*>&  get_structs()   const { return structs_;   }
  const std::vector<t_struct*>&  get_xceptions() const { return xceptions_; }
  const std::vector<t_service*>& get_services()  const { return services_;  }

  // Accessors for global types
  t_type* get_void_type()   const { return type_void;   }
  t_type* get_string_type() const { return type_string; }
  t_type* get_bool_type()   const { return type_bool;   }
  t_type* get_byte_type()   const { return type_byte;   }
  t_type* get_i16_type()    const { return type_i16;    }
  t_type* get_i32_type()    const { return type_i32;    }
  t_type* get_i64_type()    const { return type_i64;    }
  t_type* get_double_type() const { return type_double; }

  // Custom data type lookup
  t_type* get_custom_type(std::string name) {
    return custom_types_[name];
  }

  // New program element addition
  void set_namespace(std::string name) {
    namespace_ = name;
  }

  void add_typedef(t_typedef* td) {
    typedefs_.push_back(td);
    add_custom_type(td->get_symbolic(), td);
  }
  void add_enum(t_enum* te) {
    enums_.push_back(te);
    add_custom_type(te->get_name(), te);
  }
  void add_struct(t_struct* ts) {
    structs_.push_back(ts);
    add_custom_type(ts->get_name(), ts);
  }
  void add_xception(t_struct* tx) {
    xceptions_.push_back(tx);
    add_custom_type(tx->get_name(), tx);
  }
  void add_service(t_service* ts) {
    services_.push_back(ts);
  }

 private:
  // Add custom type for lookup
  void add_custom_type(std::string name, t_type* type) {
    custom_types_[name] = type;
  }

  // Name
  std::string name_;

  // Namespace
  std::string namespace_;

  // Components
  std::vector<t_typedef*>  typedefs_;
  std::vector<t_enum*>     enums_;
  std::vector<t_struct*>   structs_;
  std::vector<t_struct*>   xceptions_;
  std::vector<t_service*>  services_;

  // Type map
  std::map<std::string, t_type*> custom_types_;

  // Global base types
  t_type* type_void;
  t_type* type_string;
  t_type* type_bool;
  t_type* type_byte;
  t_type* type_i16;
  t_type* type_i32;
  t_type* type_i64;
  t_type* type_double;
};

#endif
