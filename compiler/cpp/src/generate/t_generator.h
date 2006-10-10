#ifndef T_GENERATOR_H
#define T_GENERATOR_H

#include <string>
#include <iostream>
#include <sstream>
#include "parse/t_program.h"

/**
 * Base class for a thrift code generator. This class defines the basic
 * routines for code generation and contains the top level method that
 * dispatches code generation across various components.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
class t_generator {
 public:
  t_generator() {
    tmp_ = 0;
  }

  virtual ~t_generator() {}

  /**
   * Framework generator method that iterates over all the parts of a program
   * and performs general actions. This is implemented by the base class and
   * should not be overwritten in the subclasses.
   */
  void generate_program  (t_program*  tprogram);

 protected:

  /**
   * Optional methods that may be imlemented by subclasses to take necessary
   * steps at the beginning or end of code generation.
   */

  virtual void init_generator    (t_program*  tprogram) {}
  virtual void close_generator   (t_program*  tprogram) {}

  /**
   * Pure virtual methods implemented by the generator subclasses.
   */

  virtual void generate_typedef  (t_typedef*  ttypedef)  = 0;
  virtual void generate_enum     (t_enum*     tenum)     = 0;
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

 protected:
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

#endif
