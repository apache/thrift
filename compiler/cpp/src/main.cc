/**
 * thrift - a lightweight cross-language rpc/serialization tool
 *
 * This file contains the main compiler engine for Thrift, which invokes the
 * scanner/parser to build the thrift object tree. The interface generation
 * code for each language lives in a file by the language name.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string>

// Careful: must include globals first
#include "globals.h"

#include "main.h"
#include "parse/t_program.h"
#include "generate/t_cpp_generator.h"
#include "generate/t_java_generator.h"
#include "generate/t_php_generator.h"
#include "generate/t_py_generator.h"

using namespace std;

/** Global program tree */
t_program* g_program;

/** Global debug state */
int g_debug = 0;

/** Global time string */
char* g_time_str;


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
          "\n!!! Error: line %d (last token was '%s')",
          yylineno,
          yytext);
  fprintf(stderr, "\n!!! ");

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
  printf("[Parse] ");
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
void failure(char* fmt, ...) {
  va_list args; 
  fprintf(stderr, "\n!!! Failure: ");
  va_start(args, fmt);
  vfprintf(stderr, fmt, args);
  va_end(args);
  printf("\n");
  exit(1);
}

/**
 * Diplays the usage message and then exits with an error code.
 */
void usage() {
  fprintf(stderr, "Usage: thrift [options] file\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  --cpp    Generate C++ output files\n");
  fprintf(stderr, "  --java   Generate Java output files\n");
  fprintf(stderr, "  --php    Generate PHP output files\n");
  fprintf(stderr, "  --phpi   Generate PHP inlined files\n");
  fprintf(stderr, "  --py     Generate Python output files\n");
  fprintf(stderr, "  --debug  Print parse debugging to standard output\n");
  exit(1);
}

/**
 * Parse it up.. then spit it back out, in pretty much every language
 */
int main(int argc, char** argv) {
  int i;
  bool gen_cpp = false;
  bool gen_java = false;
  bool gen_py = false;
  bool gen_php = false;
  bool php_inline = false;

  // Setup time string
  time_t now = time(NULL);
  g_time_str = ctime(&now);

  // Check for necessary arguments
  if (argc < 2) {
    usage();
  }

  for (i = 1; i < argc-1; i++) {
    char* arg;
    arg = strtok(argv[i], " ");
    while (arg != NULL) {
      if (strcmp(arg, "--debug") == 0) {
        g_debug = 1;
      } else if (strcmp(arg, "--cpp") == 0) {
        gen_cpp = true;
      } else if (strcmp(arg, "--java") == 0) {
        gen_java = true;
      } else if (strcmp(arg, "--php") == 0) {
        gen_php = true;
        php_inline = false;
      } else if (strcmp(arg, "--phpi") == 0) {
        gen_php = true;
        php_inline = true;
      } else if (strcmp(arg, "--py") == 0) {
        gen_py = true;
      } else {
        fprintf(stderr, "!!! Unrecognized option: %s\n", arg);
        usage();
      }

      // Tokenize more
      arg = strtok(NULL, " ");
    }
  }
  
  if (!gen_cpp && !gen_java && !gen_php && !gen_py) {
    fprintf(stderr, "!!! No output language(s) specified\n\n");
    usage();
  }
  
  // Open input file
  char* input_file = argv[i];
  yyin = fopen(input_file, "r");
  if (yyin == 0) {
    failure("Could not open input file: \"%s\"", input_file);
  }
  
  // Extract program name by dropping directory and .thrift from filename
  string name = input_file;
  string::size_type slash = name.rfind("/");
  if (slash != string::npos) {
    name = name.substr(slash+1);
  }
  string::size_type dot = name.find(".");
  if (dot != string::npos) {
    name = name.substr(0, dot);
  }
  
  // Parse it
  g_program = new t_program(name);

  if (yyparse() != 0) {
    failure("Parser error.");
  }

  // Generate code
  try {
    if (gen_cpp) {
      t_cpp_generator* cpp = new t_cpp_generator();
      cpp->generate_program(g_program);
      delete cpp;
    }

    if (gen_java) {
      t_java_generator* java = new t_java_generator();
      java->generate_program(g_program);
      delete java;
    }

    if (gen_php) {
      t_php_generator* php = new t_php_generator(php_inline);
      php->generate_program(g_program);
      delete php;
    }

    if (gen_py) {
      t_py_generator* py = new t_py_generator();
      py->generate_program(g_program);
      delete py;
    }
  } catch (string s) {
    printf("Error: %s\n", s.c_str());
  } catch (const char* exc) {
    printf("Error: %s\n", exc);
  }

  // Clean up
  delete g_program;

  // Finished
  return 0;
}
