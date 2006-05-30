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
 *   Services
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_program {
 public:
  t_program(std::string name) :
    name_(name) {
    type_void   = new t_base_type("void",   t_base_type::TYPE_VOID);
    type_string = new t_base_type("string", t_base_type::TYPE_STRING);
    type_byte   = new t_base_type("byte",   t_base_type::TYPE_BYTE);
    type_i32    = new t_base_type("i32",    t_base_type::TYPE_I32);
    type_u32    = new t_base_type("u32",    t_base_type::TYPE_U32);
    type_i64    = new t_base_type("i64",    t_base_type::TYPE_I64);
    type_u64    = new t_base_type("u64",    t_base_type::TYPE_U64);
  }

  ~t_program() {
    delete type_string;
    delete type_byte;
    delete type_i32;
    delete type_u32;
    delete type_i64;
    delete type_u64;
  }

  // Name accessor
  const std::string& get_name() const { return name_; }

  // Accessors for program elements
  const std::vector<t_typedef*>& get_typedefs() const { return typedefs_; }
  const std::vector<t_enum*>&    get_enums()    const { return enums_;    }
  const std::vector<t_struct*>&  get_structs()  const { return structs_;  }
  const std::vector<t_service*>& get_services() const { return services_; }

  // Accessors for global types
  t_type* get_void_type()   const { return type_void;   }
  t_type* get_string_type() const { return type_string; }
  t_type* get_byte_type()   const { return type_byte;   }
  t_type* get_i32_type()    const { return type_i32;    }
  t_type* get_u32_type()    const { return type_u32;    }
  t_type* get_i64_type()    const { return type_i64;    }
  t_type* get_u64_type()    const { return type_u64;    }

  // Custom data type lookup
  void add_custom_type(std::string name, t_type* type) {
    custom_types_[name] = type;
  }
  t_type* get_custom_type(std::string name) {
    return custom_types_[name];
  }

  // New program element addition
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
  void add_service(t_service* ts) {
    services_.push_back(ts);
  }

 private:
  // Name
  std::string name_;

  // Components
  std::vector<t_typedef*> typedefs_;
  std::vector<t_enum*>    enums_;
  std::vector<t_struct*>  structs_;
  std::vector<t_service*> services_;

  // Type map
  std::map<std::string, t_type*> custom_types_;

  // Global base types
  t_type* type_void;
  t_type* type_string;
  t_type* type_byte;
  t_type* type_i32;
  t_type* type_u32;
  t_type* type_i64;
  t_type* type_u64;  
};

#endif
