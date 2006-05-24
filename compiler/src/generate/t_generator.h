#ifndef T_GENERATOR_H
#define T_GENERATOR_H

#include <string>
#include <iostream>
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
  t_generator() {}
  virtual ~t_generator() {}

  /**
   * Framework generator method that iterates over all the parts of a program
   * and performs general actions. This is implemented by the base class and
   * should not be overwritten in the subclasses.
   */
  void generate_program  (t_program*  tprogram);

 protected:
  /** Optional methods that may be imlemented by subclasses. */
  virtual void init_generator    (t_program*  tprogram) {}
  virtual void close_generator   () {}

  /** Pure virtual methods implemented by the generator subclasses. */
  virtual void generate_typedef  (t_typedef*  ttypedef) = 0;
  virtual void generate_enum     (t_enum*     tenum)    = 0;
  virtual void generate_struct   (t_struct*   tstruct)  = 0;
  virtual void generate_service  (t_service*  tservice) = 0;

  /** Indentation level modifiers */
  void indent_up()   { ++indent_; }
  void indent_down() { --indent_; }

  /** Indentation print function */
  std::string indent() {
    std::string ind = "";
    int i;
    for (i = 0; i < indent_; ++i) {
      ind += "  ";
    }
    return ind;
  }

  /** Indentation wrapper */
  std::ostream& indent(std::ostream &os) {
    return os << indent();
  }

 private:
  /** Indentation level */
  int indent_;

};

#endif
