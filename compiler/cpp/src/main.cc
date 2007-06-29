// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

/**
 * thrift - a lightweight cross-language rpc/serialization tool
 *
 * This file contains the main compiler engine for Thrift, which invokes the
 * scanner/parser to build the thrift object tree. The interface generation
 * code for each language lives in a file by the language name under the
 * generate/ folder, and all parse structures live in parse/
 *
 * @author Mark Slee <mcslee@facebook.com>
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>

// Careful: must include globals first for extern definitions
#include "globals.h"

#include "main.h"
#include "parse/t_program.h"
#include "parse/t_scope.h"
#include "generate/t_cpp_generator.h"
#include "generate/t_java_generator.h"
#include "generate/t_php_generator.h"
#include "generate/t_py_generator.h"
#include "generate/t_rb_generator.h"
#include "generate/t_xsd_generator.h"
#include "generate/t_perl_generator.h"
#include "generate/t_erl_generator.h"

using namespace std;

/**
 * Global program tree
 */
t_program* g_program;

/**
 * Global types
 */

t_type* g_type_void;
t_type* g_type_string;
t_type* g_type_binary;
t_type* g_type_slist;
t_type* g_type_bool;
t_type* g_type_byte;
t_type* g_type_i16;
t_type* g_type_i32;
t_type* g_type_i64;
t_type* g_type_double;

/**
 * Global scope
 */
t_scope* g_scope;

/**
 * Parent scope to also parse types
 */
t_scope* g_parent_scope;

/**
 * Prefix for putting types in parent scope
 */
string g_parent_prefix;

/**
 * Parsing pass
 */
PARSE_MODE g_parse_mode;

/**
 * Current directory of file being parsed
 */
string g_curdir;

/**
 * Current file being parsed
 */
string g_curpath;

/**
 * Search path for inclusions
 */
vector<string> g_incl_searchpath;

/**
 * Global debug state
 */
int g_debug = 0;

/**
 * Warning level
 */
int g_warn = 1;

/**
 * Verbose output
 */
int g_verbose = 0;

/**
 * Global time string
 */
char* g_time_str;

/**
 * Flags to control code generation
 */
bool gen_cpp = false;
bool gen_java = false;
bool gen_rb = false;
bool gen_py = false;
bool gen_xsd = false;
bool gen_php = false;
bool gen_phpi = false;
bool gen_perl = false;
bool gen_erl = false;
bool gen_recurse = false;

/**
 * Report an error to the user. This is called yyerror for historical
 * reasons (lex and yacc expect the error reporting routine to be called
 * this). Call this function to report any errors to the user.
 * yyerror takes printf style arguments.
 *
 * @param fmt C format string followed by additional arguments
 */
void yyerror(char* fmt, ...) {
  va_list args;
  fprintf(stderr,
          "[ERROR:%s:%d] (last token was '%s')\n",
          g_curpath.c_str(),
          yylineno,
          yytext);

  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);

  fprintf(stderr, "\n");
}

/**
 * Prints a debug message from the parser.
 *
 * @param fmt C format string followed by additional arguments
 */
void pdebug(char* fmt, ...) {
  if (g_debug == 0) {
    return;
  }
  va_list args;
  printf("[PARSE:%d] ", yylineno);
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
}

/**
 * Prints a verbose output mode message
 *
 * @param fmt C format string followed by additional arguments
 */
void pverbose(char* fmt, ...) {
  if (g_verbose == 0) {
    return;
  }
  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
}

/**
 * Prints a warning message
 *
 * @param fmt C format string followed by additional arguments
 */
void pwarning(int level, char* fmt, ...) {
  if (g_warn < level) {
    return;
  }
  va_list args;
  printf("[WARNING:%s:%d] ", g_curpath.c_str(), yylineno);
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);
  printf("\n");
}

/**
 * Prints a failure message and exits
 *
 * @param fmt C format string followed by additional arguments
 */
void failure(const char* fmt, ...) {
  va_list args;
  fprintf(stderr, "[FAILURE:%s:%d] ", g_curpath.c_str(), yylineno);
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  printf("\n");
  exit(1);
}

/**
 * Converts a string filename into a thrift program name
 */
string program_name(string filename) {
  string::size_type slash = filename.rfind("/");
  if (slash != string::npos) {
    filename = filename.substr(slash+1);
  }
  string::size_type dot = filename.rfind(".");
  if (dot != string::npos) {
    filename = filename.substr(0, dot);
  }
  return filename;
}

/**
 * Gets the directory path of a filename
 */
string directory_name(string filename) {
  string::size_type slash = filename.rfind("/");
  // No slash, just use the current directory
  if (slash == string::npos) {
    return ".";
  }
  return filename.substr(0, slash);
}

/**
 * Finds the appropriate file path for the given filename
 */
string include_file(string filename) {
  // Absolute path? Just try that
  if (filename[0] == '/') {
    // Realpath!
    char rp[PATH_MAX];
    if (realpath(filename.c_str(), rp) == NULL) {
      pwarning(0, "Cannot open include file %s\n", filename.c_str());
      return std::string();
    }

    // Stat this file
    struct stat finfo;
    if (stat(rp, &finfo) == 0) {
      return rp;
    }
  } else { // relative path, start searching
    // new search path with current dir global
    vector<string> sp = g_incl_searchpath;
    sp.insert(sp.begin(), g_curdir);

    // iterate through paths
    vector<string>::iterator it;
    for (it = sp.begin(); it != sp.end(); it++) {
      string sfilename = *(it) + "/" + filename;

      // Realpath!
      char rp[PATH_MAX];
      if (realpath(sfilename.c_str(), rp) == NULL) {
        continue;
      }

      // Stat this files
      struct stat finfo;
      if (stat(rp, &finfo) == 0) {
        return rp;
      }
    }
  }

  // Uh oh
  pwarning(0, "Could not find include file %s\n", filename.c_str());
  return std::string();
}

/**
 * Diplays the usage message and then exits with an error code.
 */
void usage() {
  fprintf(stderr, "Usage: thrift [options] file\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -cpp        Generate C++ output files\n");
  fprintf(stderr, "  -java       Generate Java output files\n");
  fprintf(stderr, "  -php        Generate PHP output files\n");
  fprintf(stderr, "  -phpi       Generate PHP inlined files\n");
  fprintf(stderr, "  -py         Generate Python output files\n");
  fprintf(stderr, "  -rb         Generate Ruby output files\n");
  fprintf(stderr, "  -xsd        Generate XSD output files\n");
  fprintf(stderr, "  -perl       Generate Perl output files\n");
  fprintf(stderr, "  -erl        Generate Erlang output files\n");
  fprintf(stderr, "  -I dir      Add a directory to the list of directories \n");
  fprintf(stderr, "                searched for include directives\n");
  fprintf(stderr, "  -nowarn     Suppress all compiler warnings (BAD!)\n");
  fprintf(stderr, "  -strict     Strict compiler warnings on\n");
  fprintf(stderr, "  -v[erbose]  Verbose mode\n");
  fprintf(stderr, "  -r[ecurse]  Also generate included files\n");
  fprintf(stderr, "  -debug      Parse debug trace to stdout\n");
  exit(1);
}

/**
 * You know, when I started working on Thrift I really thought it wasn't going
 * to become a programming language because it was just a generator and it
 * wouldn't need runtime type information and all that jazz. But then we
 * decided to add constants, and all of a sudden that means runtime type
 * validation and inference, except the "runtime" is the code generator
 * runtime. Shit. I've been had.
 */
void validate_const_rec(std::string name, t_type* type, t_const_value* value) {
  if (type->is_void()) {
    throw "type error: cannot declare a void const: " + name;
  }

  if (type->is_base_type()) {
    t_base_type::t_base tbase = ((t_base_type*)type)->get_base();
    switch (tbase) {
    case t_base_type::TYPE_STRING:
      if (value->get_type() != t_const_value::CV_STRING) {
        throw "type error: const \"" + name + "\" was declared as string";
      }
      break;
    case t_base_type::TYPE_BOOL:
      if (value->get_type() != t_const_value::CV_INTEGER) {
        throw "type error: const \"" + name + "\" was declared as bool";
      }
      break;
    case t_base_type::TYPE_BYTE:
      if (value->get_type() != t_const_value::CV_INTEGER) {
        throw "type error: const \"" + name + "\" was declared as byte";
      }
      break;
    case t_base_type::TYPE_I16:
      if (value->get_type() != t_const_value::CV_INTEGER) {
        throw "type error: const \"" + name + "\" was declared as i16";
      }
      break;
    case t_base_type::TYPE_I32:
      if (value->get_type() != t_const_value::CV_INTEGER) {
        throw "type error: const \"" + name + "\" was declared as i32";
      }
      break;
    case t_base_type::TYPE_I64:
      if (value->get_type() != t_const_value::CV_INTEGER) {
        throw "type error: const \"" + name + "\" was declared as i64";
      }
      break;
    case t_base_type::TYPE_DOUBLE:
      if (value->get_type() != t_const_value::CV_INTEGER &&
          value->get_type() != t_const_value::CV_DOUBLE) {
        throw "type error: const \"" + name + "\" was declared as double";
      }
      break;
    default:
      throw "compiler error: no const of base type " + tbase + name;
    }
  } else if (type->is_enum()) {
    if (value->get_type() != t_const_value::CV_INTEGER) {
      throw "type error: const \"" + name + "\" was declared as enum";
    }
  } else if (type->is_struct() || type->is_xception()) {
    if (value->get_type() != t_const_value::CV_MAP) {
      throw "type error: const \"" + name + "\" was declared as struct/xception";
    }
    const vector<t_field*>& fields = ((t_struct*)type)->get_members();
    vector<t_field*>::const_iterator f_iter;

    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      if (v_iter->first->get_type() != t_const_value::CV_STRING) {
        throw "type error: " + name + " struct key must be string";
      }
      t_type* field_type = NULL;
      for (f_iter = fields.begin(); f_iter != fields.end(); ++f_iter) {
        if ((*f_iter)->get_name() == v_iter->first->get_string()) {
          field_type = (*f_iter)->get_type();
        }
      }
      if (field_type == NULL) {
        throw "type error: " + type->get_name() + " has no field " + v_iter->first->get_string();
      }

      validate_const_rec(name + "." + v_iter->first->get_string(), field_type, v_iter->second);
    }
  } else if (type->is_map()) {
    t_type* k_type = ((t_map*)type)->get_key_type();
    t_type* v_type = ((t_map*)type)->get_val_type();
    const map<t_const_value*, t_const_value*>& val = value->get_map();
    map<t_const_value*, t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      validate_const_rec(name + "<key>", k_type, v_iter->first);
      validate_const_rec(name + "<val>", v_type, v_iter->second);
    }
  } else if (type->is_list() || type->is_set()) {
    t_type* e_type;
    if (type->is_list()) {
      e_type = ((t_list*)type)->get_elem_type();
    } else {
      e_type = ((t_set*)type)->get_elem_type();
    }
    const vector<t_const_value*>& val = value->get_list();
    vector<t_const_value*>::const_iterator v_iter;
    for (v_iter = val.begin(); v_iter != val.end(); ++v_iter) {
      validate_const_rec(name + "<elem>", e_type, *v_iter);
    }
  }
}

/**
 * Check the type of the parsed const information against its declared type
 */
void validate_const_type(t_const* c) {
  validate_const_rec(c->get_name(), c->get_type(), c->get_value());
}

/**
 * Check the type of a default value assigned to a field.
 */
void validate_field_value(t_field* field, t_const_value* cv) {
  validate_const_rec(field->get_name(), field->get_type(), cv);
}

/**
 * Parses a program
 */
void parse(t_program* program, t_program* parent_program) {
  // Get scope file path
  string path = program->get_path();

  // Set current dir global, which is used in the include_file function
  g_curdir = directory_name(path);
  g_curpath = path;

  // Open the file
  yyin = fopen(path.c_str(), "r");
  if (yyin == 0) {
    failure("Could not open input file: \"%s\"", path.c_str());
  }

  // Create new scope and scan for includes
  pverbose("Scanning %s for includes\n", path.c_str());
  g_parse_mode = INCLUDES;
  g_program = program;
  g_scope = program->scope();
  try {
    yylineno = 1;
    if (yyparse() != 0) {
      failure("Parser error during include pass.");
    }
  } catch (string x) {
    failure(x.c_str());
  }
  fclose(yyin);

  // Recursively parse all the include programs
  vector<t_program*>& includes = program->get_includes();
  vector<t_program*>::iterator iter;
  for (iter = includes.begin(); iter != includes.end(); ++iter) {
    parse(*iter, program);
  }

  // Parse the program the file
  g_parse_mode = PROGRAM;
  g_program = program;
  g_scope = program->scope();
  g_parent_scope = (parent_program != NULL) ? parent_program->scope() : NULL;
  g_parent_prefix = program->get_name() + ".";
  g_curpath = path;
  yyin = fopen(path.c_str(), "r");
  if (yyin == 0) {
    failure("Could not open input file: \"%s\"", path.c_str());
  }
  pverbose("Parsing %s for types\n", path.c_str());
  yylineno = 1;
  if (yyparse() != 0) {
    failure("Parser error during types pass.");
  }
  fclose(yyin);
}

/**
 * Generate code
 */
void generate(t_program* program) {
  // Oooohh, recursive code generation, hot!!
  if (gen_recurse) {
    const vector<t_program*>& includes = program->get_includes();
    for (size_t i = 0; i < includes.size(); ++i) {
      generate(includes[i]);
    }
  }

  // Generate code!
  try {
    pverbose("Program: %s\n", program->get_path().c_str());

    if (gen_cpp) {
      pverbose("Generating C++\n");
      t_cpp_generator* cpp = new t_cpp_generator(program);
      cpp->generate_program();
      delete cpp;
    }

    if (gen_java) {
      pverbose("Generating Java\n");
      t_java_generator* java = new t_java_generator(program);
      java->generate_program();
      delete java;
    }

    if (gen_php) {
      pverbose("Generating PHP\n");
      t_php_generator* php = new t_php_generator(program, false);
      php->generate_program();
      delete php;
    }

    if (gen_phpi) {
      pverbose("Generating PHP-inline\n");
      t_php_generator* phpi = new t_php_generator(program, true);
      phpi->generate_program();
      delete phpi;
    }

    if (gen_py) {
      pverbose("Generating Python\n");
      t_py_generator* py = new t_py_generator(program);
      py->generate_program();
      delete py;
    }

    if (gen_rb) {
      pverbose("Generating Ruby\n");
      t_rb_generator* rb = new t_rb_generator(program);
      rb->generate_program();
      delete rb;
    }

    if (gen_xsd) {
      pverbose("Generating XSD\n");
      t_xsd_generator* xsd = new t_xsd_generator(program);
      xsd->generate_program();
      delete xsd;
    }

    if (gen_perl) {
      pverbose("Generating PERL\n");
      t_perl_generator* perl = new t_perl_generator(program);
      perl->generate_program();
      delete perl;
    }

    if (gen_erl) {
      pverbose("Generating Erlang\n");
      t_erl_generator* erl = new t_erl_generator(program);
      erl->generate_program();
      delete erl;
    }

  } catch (string s) {
    printf("Error: %s\n", s.c_str());
  } catch (const char* exc) {
    printf("Error: %s\n", exc);
  }

}

/**
 * Parse it up.. then spit it back out, in pretty much every language. Alright
 * not that many languages, but the cool ones that we care about.
 */
int main(int argc, char** argv) {
  int i;

  // Setup time string
  time_t now = time(NULL);
  g_time_str = ctime(&now);

  // Check for necessary arguments, you gotta have at least a filename and
  // an output language flag
  if (argc < 2) {
    usage();
  }

  // Hacky parameter handling... I didn't feel like using a library sorry!
  for (i = 1; i < argc-1; i++) {
    char* arg;

    arg = strtok(argv[i], " ");
    while (arg != NULL) {
      // Treat double dashes as single dashes
      if (arg[0] == '-' && arg[1] == '-') {
        ++arg;
      }

      if (strcmp(arg, "-debug") == 0) {
        g_debug = 1;
      } else if (strcmp(arg, "-nowarn") == 0) {
        g_warn = 0;
      } else if (strcmp(arg, "-strict") == 0) {
        g_warn = 2;
      } else if (strcmp(arg, "-v") == 0 || strcmp(arg, "-verbose") == 0 ) {
        g_verbose = 1;
      } else if (strcmp(arg, "-r") == 0 || strcmp(arg, "-recurse") == 0 ) {
        gen_recurse = true;
      } else if (strcmp(arg, "-cpp") == 0) {
        gen_cpp = true;
      } else if (strcmp(arg, "-java") == 0) {
        gen_java = true;
      } else if (strcmp(arg, "-php") == 0) {
        gen_php = true;
      } else if (strcmp(arg, "-phpi") == 0) {
        gen_phpi = true;
      } else if (strcmp(arg, "-py") == 0) {
        gen_py = true;
      } else if (strcmp(arg, "-rb") == 0) {
        gen_rb = true;
      } else if (strcmp(arg, "-xsd") == 0) {
        gen_xsd = true;
      } else if (strcmp(arg, "-perl") == 0) {
        gen_perl = true;
      } else if (strcmp(arg, "-erl") == 0) {
        gen_erl = true;
      } else if (strcmp(arg, "-I") == 0) {
        // An argument of "-I\ asdf" is invalid and has unknown results
        arg = argv[++i];

        if (arg == NULL) {
          fprintf(stderr, "!!! Missing Include directory");
          usage();
        }
        g_incl_searchpath.push_back(arg);
      } else {
        fprintf(stderr, "!!! Unrecognized option: %s\n", arg);
        usage();
      }

      // Tokenize more
      arg = strtok(NULL, " ");
    }
  }

  // You gotta generate something!
  if (!gen_cpp && !gen_java && !gen_php && !gen_phpi && !gen_py && !gen_rb && !gen_xsd && !gen_perl && !gen_erl) {
    fprintf(stderr, "!!! No output language(s) specified\n\n");
    usage();
  }

  // Real-pathify it
  char rp[PATH_MAX];
  if (realpath(argv[i], rp) == NULL) {
    failure("Could not open input file: %s", argv[i]);
  }
  string input_file(rp);

  // Instance of the global parse tree
  t_program* program = new t_program(input_file);

  // Initialize global types
  g_type_void   = new t_base_type("void",   t_base_type::TYPE_VOID);
  g_type_string = new t_base_type("string", t_base_type::TYPE_STRING);
  g_type_binary = new t_base_type("string", t_base_type::TYPE_STRING);
  ((t_base_type*)g_type_binary)->set_binary(true);
  g_type_slist  = new t_base_type("string", t_base_type::TYPE_STRING);
  ((t_base_type*)g_type_slist)->set_string_list(true);
  g_type_bool   = new t_base_type("bool",   t_base_type::TYPE_BOOL);
  g_type_byte   = new t_base_type("byte",   t_base_type::TYPE_BYTE);
  g_type_i16    = new t_base_type("i16",    t_base_type::TYPE_I16);
  g_type_i32    = new t_base_type("i32",    t_base_type::TYPE_I32);
  g_type_i64    = new t_base_type("i64",    t_base_type::TYPE_I64);
  g_type_double = new t_base_type("double", t_base_type::TYPE_DOUBLE);

  // Parse it!
  parse(program, NULL);

  // Generate it!
  generate(program);

  // Clean up. Who am I kidding... this program probably orphans heap memory
  // all over the place, but who cares because it is about to exit and it is
  // all referenced and used by this wacky parse tree up until now anyways.

  delete program;
  delete g_type_void;
  delete g_type_string;
  delete g_type_bool;
  delete g_type_byte;
  delete g_type_i16;
  delete g_type_i32;
  delete g_type_i64;
  delete g_type_double;

  // Finished
  return 0;
}
