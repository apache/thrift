/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

#ifndef T_GENERATOR_H
#define T_GENERATOR_H

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include "parse/t_program.h"
#include "globals.h"

/**
 * Base class for a thrift code generator. This class defines the basic
 * routines for code generation and contains the top level method that
 * dispatches code generation across various components.
 *
 */
class t_generator {
 public:
  t_generator(t_program* program) {
    tmp_ = 0;
    indent_ = 0;
    program_ = program;
    program_name_ = get_program_name(program);
    escape_['\n'] = "\\n";
    escape_['\r'] = "\\r";
    escape_['\t'] = "\\t";
    escape_['"']  = "\\\"";
    escape_['\\'] = "\\\\";
  }

  virtual ~t_generator() {}

  /**
   * Framework generator method that iterates over all the parts of a program
   * and performs general actions. This is implemented by the base class and
   * should not normally be overwritten in the subclasses.
   */
  virtual void generate_program();

  const t_program* get_program() const { return program_; }

  void generate_docstring_comment(std::ofstream& out,
                                  const std::string& comment_start,
                                  const std::string& line_prefix,
                                  const std::string& contents,
                                  const std::string& comment_end);

  /**
   * Escape string to use one in generated sources.
   */
  virtual std::string escape_string(const std::string &in) const;

  std::string get_escaped_string(t_const_value* constval) {
    return escape_string(constval->get_string());
  }

 protected:

  /**
   * Optional methods that may be imlemented by subclasses to take necessary
   * steps at the beginning or end of code generation.
   */

  virtual void init_generator() {}
  virtual void close_generator() {}

  virtual void generate_consts(std::vector<t_const*> consts);

  /**
   * Pure virtual methods implemented by the generator subclasses.
   */

  virtual void generate_typedef  (t_typedef*  ttypedef)  = 0;
  virtual void generate_enum     (t_enum*     tenum)     = 0;
  virtual void generate_const    (t_const*    tconst) {}
  virtual void generate_struct   (t_struct*   tstruct)   = 0;
  virtual void generate_service  (t_service*  tservice)  = 0;
  virtual void generate_xception (t_struct*   txception) {
    // By default exceptions are the same as structs
    generate_struct(txception);
  }

  /**
   * Method to get the program name, may be overridden
   */
  virtual std::string get_program_name(t_program* tprogram) {
    return tprogram->get_name();
  }

  /**
   * Method to get the service name, may be overridden
   */
  virtual std::string get_service_name(t_service* tservice) {
    return tservice->get_name();
  }

  /**
   * Get the current output directory
   */
  virtual std::string get_out_dir() const {
    return program_->get_out_path() + out_dir_base_ + "/";
  }

  /**
   * Creates a unique temporary variable name, which is just "name" with a
   * number appended to it (i.e. name35)
   */
  std::string tmp(std::string name) {
    std::ostringstream out;
    out << name << tmp_++;
    return out.str();
  }

  /**
   * Indentation level modifiers
   */

  void indent_up(){
    ++indent_;
  }

  void indent_down() {
    --indent_;
  }

  /**
   * Indentation print function
   */
  std::string indent() {
    std::string ind = "";
    int i;
    for (i = 0; i < indent_; ++i) {
      ind += "  ";
    }
    return ind;
  }

  /**
   * Indentation utility wrapper
   */
  std::ostream& indent(std::ostream &os) {
    return os << indent();
  }

  /**
   * Capitalization helpers
   */
  std::string capitalize(std::string in) {
    in[0] = toupper(in[0]);
    return in;
  }
  std::string decapitalize(std::string in) {
    in[0] = tolower(in[0]);
    return in;
  }
  std::string lowercase(std::string in) {
    for (size_t i = 0; i < in.size(); ++i) {
      in[i] = tolower(in[i]);
    }
    return in;
  }
  std::string underscore(std::string in) {
    in[0] = tolower(in[0]);
    for (size_t i = 1; i < in.size(); ++i) {
      if (isupper(in[i])) {
        in[i] = tolower(in[i]);
        in.insert(i, "_");
      }
    }
    return in;
  }

  /**
   * Get the true type behind a series of typedefs.
   */
  static t_type* get_true_type(t_type* type) {
    while (type->is_typedef()) {
      type = ((t_typedef*)type)->get_type();
    }
    return type;
  }

 protected:
  /**
   * The program being generated
   */
  t_program* program_;

  /**
   * Quick accessor for formatted program name that is currently being
   * generated.
   */
  std::string program_name_;

  /**
   * Quick accessor for formatted service name that is currently being
   * generated.
   */
  std::string service_name_;

  /**
   * Output type-specifc directory name ("gen-*")
   */
  std::string out_dir_base_;

  /**
   * Map of characters to escape in string literals.
   */
  std::map<char, std::string> escape_;

 private:
  /**
   * Current code indentation level
   */
  int indent_;

  /**
   * Temporary variable counter, for making unique variable names
   */
  int tmp_;
};


/**
 * A factory for producing generator classes of a particular language.
 *
 * This class is also responsible for:
 *  - Registering itself with the generator registry.
 *  - Providing documentation for the generators it produces.
 */
class t_generator_factory {
 public:
  t_generator_factory(const std::string& short_name,
                      const std::string& long_name,
                      const std::string& documentation);

  virtual ~t_generator_factory() {}

  virtual t_generator* get_generator(
      // The program to generate.
      t_program* program,
      // Note: parsed_options will not exist beyond the call to get_generator.
      const std::map<std::string, std::string>& parsed_options,
      // Note: option_string might not exist beyond the call to get_generator.
      const std::string& option_string)
    = 0;

  std::string get_short_name() { return short_name_; }
  std::string get_long_name() { return long_name_; }
  std::string get_documentation() { return documentation_; }

 private:
  std::string short_name_;
  std::string long_name_;
  std::string documentation_;
};

template <typename generator>
class t_generator_factory_impl : public t_generator_factory {
 public:
  t_generator_factory_impl(const std::string& short_name,
                           const std::string& long_name,
                           const std::string& documentation)
    : t_generator_factory(short_name, long_name, documentation)
  {}

  virtual t_generator* get_generator(
      t_program* program,
      const std::map<std::string, std::string>& parsed_options,
      const std::string& option_string) {
    return new generator(program, parsed_options, option_string);
  }
};

class t_generator_registry {
 public:
  static void register_generator(t_generator_factory* factory);

  static t_generator* get_generator(t_program* program,
                                    const std::string& options);

  typedef std::map<std::string, t_generator_factory*> gen_map_t;
  static gen_map_t& get_generator_map();

 private:
  t_generator_registry();
  t_generator_registry(const t_generator_registry&);
};

#define THRIFT_REGISTER_GENERATOR(language, long_name, doc)        \
  class t_##language##_generator_factory_impl                      \
    : public t_generator_factory_impl<t_##language##_generator>    \
  {                                                                \
   public:                                                         \
    t_##language##_generator_factory_impl()                        \
      : t_generator_factory_impl<t_##language##_generator>(        \
          #language, long_name, doc)                               \
    {}                                                             \
  };                                                               \
  static t_##language##_generator_factory_impl _registerer;

#endif
