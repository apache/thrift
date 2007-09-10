// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_PROGRAM_H
#define T_PROGRAM_H

#include <map>
#include <string>
#include <vector>

// For program_name()
#include "main.h"

#include "t_doc.h"
#include "t_scope.h"
#include "t_base_type.h"
#include "t_typedef.h"
#include "t_enum.h"
#include "t_const.h"
#include "t_struct.h"
#include "t_service.h"
#include "t_list.h"
#include "t_map.h"
#include "t_set.h"
//#include "t_doc.h"

/**
 * Top level class representing an entire thrift program. A program consists
 * fundamentally of the following:
 *
 *   Typedefs
 *   Enumerations
 *   Constants
 *   Structs
 *   Exceptions
 *   Services
 *
 * The program module also contains the definitions of the base types.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_program : public t_doc {
 public:
  t_program(std::string path, std::string name) :
    path_(path),
    name_(name) {
    scope_ = new t_scope();
  }

  t_program(std::string path) :
    path_(path) {
    name_ = program_name(path);
    scope_ = new t_scope();
  }

  // Path accessor
  const std::string& get_path() const { return path_; }

  // Name accessor
  const std::string& get_name() const { return name_; }

  // Namespace
  const std::string& get_namespace() const { return namespace_; }

  // Accessors for program elements
  const std::vector<t_typedef*>& get_typedefs()  const { return typedefs_;  }
  const std::vector<t_enum*>&    get_enums()     const { return enums_;     }
  const std::vector<t_const*>&   get_consts()    const { return consts_;    }
  const std::vector<t_struct*>&  get_structs()   const { return structs_;   }
  const std::vector<t_struct*>&  get_xceptions() const { return xceptions_; }
  const std::vector<t_service*>& get_services()  const { return services_;  }

  // Program elements
  void add_typedef  (t_typedef* td) { typedefs_.push_back(td);  }
  void add_enum     (t_enum*    te) { enums_.push_back(te);     }
  void add_const    (t_const*   tc) { consts_.push_back(tc);    }
  void add_struct   (t_struct*  ts) { structs_.push_back(ts);   }
  void add_xception (t_struct*  tx) { xceptions_.push_back(tx); }
  void add_service  (t_service* ts) { services_.push_back(ts);  }

  // Programs to include
  const std::vector<t_program*>& get_includes() const { return includes_; }

  // Scoping and namespacing
  void set_namespace(std::string name) {
    namespace_ = name;
  }

  // Scope accessor
  t_scope* scope() {
    return scope_;
  }

  // Includes

  void add_include(std::string path) {
    includes_.push_back(new t_program(path));
  }

  std::vector<t_program*>& get_includes() {
    return includes_;
  }

  // Language specific namespace / packaging

  void set_cpp_namespace(std::string cpp_namespace) {
    cpp_namespace_ = cpp_namespace;
  }

  const std::string& get_cpp_namespace() const {
    return cpp_namespace_;
  }

  void add_cpp_include(std::string path) {
    cpp_includes_.push_back(path);
  }

  const std::vector<std::string>& get_cpp_includes() {
    return cpp_includes_;
  }

  void set_php_namespace(std::string php_namespace) {
    php_namespace_ = php_namespace;
  }

  const std::string& get_php_namespace() const {
    return php_namespace_;
  }

  void set_java_package(std::string java_package) {
    java_package_ = java_package;
  }

  const std::string& get_java_package() const {
    return java_package_;
  }

  void set_xsd_namespace(std::string xsd_namespace) {
    xsd_namespace_ = xsd_namespace;
  }

  const std::string& get_xsd_namespace() const {
    return xsd_namespace_;
  }

  void set_ruby_namespace(std::string ruby_namespace) {
    ruby_namespace_ = ruby_namespace;
  }

  const std::string& get_ruby_namespace() const {
    return ruby_namespace_;
  }

  void set_py_module(std::string py_module) {
    py_module_ = py_module;
  }

  const std::string& get_py_module() const {
    return py_module_;
  }

  void set_perl_package(std::string perl_package) {
    perl_package_ = perl_package;
  }

  const std::string& get_perl_package() const {
    return perl_package_;
  }

  void set_cocoa_prefix(std::string cocoa_prefix) {
    cocoa_prefix_ = cocoa_prefix;
  }

  const std::string& get_cocoa_prefix() const {
    return cocoa_prefix_;
  }

 private:

  // File path
  std::string path_;

  // Name
  std::string name_;

  // Namespace
  std::string namespace_;

  // Included programs
  std::vector<t_program*> includes_;

  // Identifier lookup scope
  t_scope* scope_;

  // Components to generate code for
  std::vector<t_typedef*> typedefs_;
  std::vector<t_enum*>    enums_;
  std::vector<t_const*>   consts_;
  std::vector<t_struct*>  structs_;
  std::vector<t_struct*>  xceptions_;
  std::vector<t_service*> services_;

  // C++ namespace
  std::string cpp_namespace_;

  // C++ extra includes
  std::vector<std::string> cpp_includes_;

  // PHP namespace
  std::string php_namespace_;

  // Java package
  std::string java_package_;

  // XSD namespace
  std::string xsd_namespace_;

  // Ruby namespace
  std::string ruby_namespace_;

  // Python namespace
  std::string py_module_;

  // Perl namespace
  std::string perl_package_;

  // Cocoa/Objective-C naming prefix
  std::string cocoa_prefix_;


};

#endif
