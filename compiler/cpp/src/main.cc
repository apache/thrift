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

/**
 * thrift - a lightweight cross-language rpc/serialization tool
 *
 * This file contains the main compiler engine for Thrift, which invokes the
 * scanner/parser to build the thrift object tree. The interface generation
 * code for each language lives in a file by the language name under the
 * generate/ folder, and all parse structures live in parse/
 *
 */

#include <cassert>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string>
#include <algorithm>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <limits.h>

#ifdef MINGW
# include <windows.h> /* for GetFullPathName */
#endif

// Careful: must include globals first for extern definitions
#include "globals.h"

#include "main.h"
#include "parse/t_program.h"
#include "parse/t_scope.h"
#include "generate/t_generator.h"

#include "version.h"

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
 * Should C++ include statements use path prefixes for other thrift-generated
 * header files
 */
bool g_cpp_use_include_prefix = false;

/**
 * Global debug state
 */
int g_debug = 0;

/**
 * Strictness level
 */
int g_strict = 127;

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
 * The last parsed doctext comment.
 */
char* g_doctext;

/**
 * The location of the last parsed doctext comment.
 */
int g_doctext_lineno;

/**
 * Flags to control code generation
 */
bool gen_cpp = false;
bool gen_dense = false;
bool gen_java = false;
bool gen_javabean = false;
bool gen_rb = false;
bool gen_py = false;
bool gen_py_newstyle = false;
bool gen_xsd = false;
bool gen_php = false;
bool gen_phpi = false;
bool gen_phps = true;
bool gen_phpa = false;
bool gen_phpo = false;
bool gen_rest = false;
bool gen_perl = false;
bool gen_erl = false;
bool gen_ocaml = false;
bool gen_hs = false;
bool gen_cocoa = false;
bool gen_csharp = false;
bool gen_st = false;
bool gen_recurse = false;

/**
 * MinGW doesn't have realpath, so use fallback implementation in that case,
 * otherwise this just calls through to realpath
 */
char *saferealpath(const char *path, char *resolved_path) {
#ifdef MINGW
  char buf[MAX_PATH];
  char* basename;
  DWORD len = GetFullPathName(path, MAX_PATH, buf, &basename);
  if (len == 0 || len > MAX_PATH - 1){
    strcpy(resolved_path, path);
  } else {
    CharLowerBuff(buf, len);
    strcpy(resolved_path, buf);
  }
  return resolved_path;
#else
  return realpath(path, resolved_path);
#endif
}


/**
 * Report an error to the user. This is called yyerror for historical
 * reasons (lex and yacc expect the error reporting routine to be called
 * this). Call this function to report any errors to the user.
 * yyerror takes printf style arguments.
 *
 * @param fmt C format string followed by additional arguments
 */
void yyerror(const char* fmt, ...) {
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
void pdebug(const char* fmt, ...) {
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
void pverbose(const char* fmt, ...) {
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
void pwarning(int level, const char* fmt, ...) {
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
    if (saferealpath(filename.c_str(), rp) == NULL) {
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
      if (saferealpath(sfilename.c_str(), rp) == NULL) {
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
 * Clears any previously stored doctext string.
 * Also prints a warning if we are discarding information.
 */
void clear_doctext() {
  if (g_doctext != NULL) {
    pwarning(2, "Uncaptured doctext at on line %d.", g_doctext_lineno);
  }
  free(g_doctext);
  g_doctext = NULL;
}

/**
 * Cleans up text commonly found in doxygen-like comments
 *
 * Warning: if you mix tabs and spaces in a non-uniform way,
 * you will get what you deserve.
 */
char* clean_up_doctext(char* doctext) {
  // Convert to C++ string, and remove Windows's carriage returns.
  string docstring = doctext;
  docstring.erase(
      remove(docstring.begin(), docstring.end(), '\r'),
      docstring.end());

  // Separate into lines.
  vector<string> lines;
  string::size_type pos = string::npos;
  string::size_type last;
  while (true) {
    last = (pos == string::npos) ? 0 : pos+1;
    pos = docstring.find('\n', last);
    if (pos == string::npos) {
      // First bit of cleaning.  If the last line is only whitespace, drop it.
      string::size_type nonwhite = docstring.find_first_not_of(" \t", last);
      if (nonwhite != string::npos) {
        lines.push_back(docstring.substr(last));
      }
      break;
    }
    lines.push_back(docstring.substr(last, pos-last));
  }

  // A very profound docstring.
  if (lines.empty()) {
    return NULL;
  }

  // Clear leading whitespace from the first line.
  pos = lines.front().find_first_not_of(" \t");
  lines.front().erase(0, pos);

  // If every nonblank line after the first has the same number of spaces/tabs,
  // then a star, remove them.
  bool have_prefix = true;
  bool found_prefix = false;
  string::size_type prefix_len = 0;
  vector<string>::iterator l_iter;
  for (l_iter = lines.begin()+1; l_iter != lines.end(); ++l_iter) {
    if (l_iter->empty()) {
      continue;
    }

    pos = l_iter->find_first_not_of(" \t");
    if (!found_prefix) {
      if (pos != string::npos) {
        if (l_iter->at(pos) == '*') {
          found_prefix = true;
          prefix_len = pos;
        } else {
          have_prefix = false;
          break;
        }
      } else {
        // Whitespace-only line.  Truncate it.
        l_iter->clear();
      }
    } else if (l_iter->size() > pos
        && l_iter->at(pos) == '*'
        && pos == prefix_len) {
      // Business as usual.
    } else if (pos == string::npos) {
      // Whitespace-only line.  Let's truncate it for them.
      l_iter->clear();
    } else {
      // The pattern has been broken.
      have_prefix = false;
      break;
    }
  }

  // If our prefix survived, delete it from every line.
  if (have_prefix) {
    // Get the star too.
    prefix_len++;
    for (l_iter = lines.begin()+1; l_iter != lines.end(); ++l_iter) {
      l_iter->erase(0, prefix_len);
    }
  }

  // Now delete the minimum amount of leading whitespace from each line.
  prefix_len = string::npos;
  for (l_iter = lines.begin()+1; l_iter != lines.end(); ++l_iter) {
    if (l_iter->empty()) {
      continue;
    }
    pos = l_iter->find_first_not_of(" \t");
    if (pos != string::npos
        && (prefix_len == string::npos || pos < prefix_len)) {
      prefix_len = pos;
    }
  }

  // If our prefix survived, delete it from every line.
  if (prefix_len != string::npos) {
    for (l_iter = lines.begin()+1; l_iter != lines.end(); ++l_iter) {
      l_iter->erase(0, prefix_len);
    }
  }

  // Remove trailing whitespace from every line.
  for (l_iter = lines.begin(); l_iter != lines.end(); ++l_iter) {
    pos = l_iter->find_last_not_of(" \t");
    if (pos != string::npos && pos != l_iter->length()-1) {
      l_iter->erase(pos+1);
    }
  }

  // If the first line is empty, remove it.
  // Don't do this earlier because a lot of steps skip the first line.
  if (lines.front().empty()) {
    lines.erase(lines.begin());
  }

  // Now rejoin the lines and copy them back into doctext.
  docstring.clear();
  for (l_iter = lines.begin(); l_iter != lines.end(); ++l_iter) {
    docstring += *l_iter;
    docstring += '\n';
  }

  assert(docstring.length() <= strlen(doctext));
  strcpy(doctext, docstring.c_str());
  return doctext;
}

/** Set to true to debug docstring parsing */
static bool dump_docs = false;

/**
 * Dumps docstrings to stdout
 * Only works for top-level definitions and the whole program doc
 * (i.e., not enum constants, struct fields, or functions.
 */
void dump_docstrings(t_program* program) {
  string progdoc = program->get_doc();
  if (!progdoc.empty()) {
    printf("Whole program doc:\n%s\n", progdoc.c_str());
  }
  const vector<t_typedef*>& typedefs = program->get_typedefs();
  vector<t_typedef*>::const_iterator t_iter;
  for (t_iter = typedefs.begin(); t_iter != typedefs.end(); ++t_iter) {
    t_typedef* td = *t_iter;
    if (td->has_doc()) {
      printf("typedef %s:\n%s\n", td->get_name().c_str(), td->get_doc().c_str());
    }
  }
  const vector<t_enum*>& enums = program->get_enums();
  vector<t_enum*>::const_iterator e_iter;
  for (e_iter = enums.begin(); e_iter != enums.end(); ++e_iter) {
    t_enum* en = *e_iter;
    if (en->has_doc()) {
      printf("enum %s:\n%s\n", en->get_name().c_str(), en->get_doc().c_str());
    }
  }
  const vector<t_const*>& consts = program->get_consts();
  vector<t_const*>::const_iterator c_iter;
  for (c_iter = consts.begin(); c_iter != consts.end(); ++c_iter) {
    t_const* co = *c_iter;
    if (co->has_doc()) {
      printf("const %s:\n%s\n", co->get_name().c_str(), co->get_doc().c_str());
    }
  }
  const vector<t_struct*>& structs = program->get_structs();
  vector<t_struct*>::const_iterator s_iter;
  for (s_iter = structs.begin(); s_iter != structs.end(); ++s_iter) {
    t_struct* st = *s_iter;
    if (st->has_doc()) {
      printf("struct %s:\n%s\n", st->get_name().c_str(), st->get_doc().c_str());
    }
  }
  const vector<t_struct*>& xceptions = program->get_xceptions();
  vector<t_struct*>::const_iterator x_iter;
  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    t_struct* xn = *x_iter;
    if (xn->has_doc()) {
      printf("xception %s:\n%s\n", xn->get_name().c_str(), xn->get_doc().c_str());
    }
  }
  const vector<t_service*>& services = program->get_services();
  vector<t_service*>::const_iterator v_iter;
  for (v_iter = services.begin(); v_iter != services.end(); ++v_iter) {
    t_service* sv = *v_iter;
    if (sv->has_doc()) {
      printf("service %s:\n%s\n", sv->get_name().c_str(), sv->get_doc().c_str());
    }
  }
}

/**
 * Call generate_fingerprint for every structure and enum.
 */
void generate_all_fingerprints(t_program* program) {
  const vector<t_struct*>& structs = program->get_structs();
  vector<t_struct*>::const_iterator s_iter;
  for (s_iter = structs.begin(); s_iter != structs.end(); ++s_iter) {
    t_struct* st = *s_iter;
    st->generate_fingerprint();
  }

  const vector<t_struct*>& xceptions = program->get_xceptions();
  vector<t_struct*>::const_iterator x_iter;
  for (x_iter = xceptions.begin(); x_iter != xceptions.end(); ++x_iter) {
    t_struct* st = *x_iter;
    st->generate_fingerprint();
  }

  const vector<t_enum*>& enums = program->get_enums();
  vector<t_enum*>::const_iterator e_iter;
  for (e_iter = enums.begin(); e_iter != enums.end(); ++e_iter) {
    t_enum* e = *e_iter;
    e->generate_fingerprint();
  }

  g_type_void->generate_fingerprint();

  // If you want to generate fingerprints for implicit structures, start here.
  /*
  const vector<t_service*>& services = program->get_services();
  vector<t_service*>::const_iterator v_iter;
  for (v_iter = services.begin(); v_iter != services.end(); ++v_iter) {
    t_service* sv = *v_iter;
  }
  */
}

/**
 * Prints the version number
 */
void version() {
  printf("Thrift version %s-%s\n", THRIFT_VERSION, THRIFT_REVISION);
}

/**
 * Diplays the usage message and then exits with an error code.
 */
void usage() {
  fprintf(stderr, "Usage: thrift [options] file\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "  -version    Print the compiler version\n");
  fprintf(stderr, "  -o dir      Set the output directory for gen-* packages\n");
  fprintf(stderr, "               (default: current directory)\n");
  fprintf(stderr, "  -I dir      Add a directory to the list of directories\n");
  fprintf(stderr, "                searched for include directives\n");
  fprintf(stderr, "  -nowarn     Suppress all compiler warnings (BAD!)\n");
  fprintf(stderr, "  -strict     Strict compiler warnings on\n");
  fprintf(stderr, "  -v[erbose]  Verbose mode\n");
  fprintf(stderr, "  -r[ecurse]  Also generate included files\n");
  fprintf(stderr, "  -debug      Parse debug trace to stdout\n");
  fprintf(stderr, "  --gen STR   Generate code with a dynamically-registered generator.\n");
  fprintf(stderr, "                STR has the form language[:key1=val1[,key2,[key3=val3]]].\n");
  fprintf(stderr, "                Keys and values are options passed to the generator.\n");
  fprintf(stderr, "                Many options will not require values.\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Available generators (and options):\n");

  t_generator_registry::gen_map_t gen_map = t_generator_registry::get_generator_map();
  t_generator_registry::gen_map_t::iterator iter;
  for (iter = gen_map.begin(); iter != gen_map.end(); ++iter) {
    fprintf(stderr, "  %s (%s):\n",
        iter->second->get_short_name().c_str(),
        iter->second->get_long_name().c_str());
    fprintf(stderr, "%s", iter->second->get_documentation().c_str());
  }
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
      throw "compiler error: no const of base type " + t_base_type::t_base_name(tbase) + name;
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
 * Check that all the elements of a throws block are actually exceptions.
 */
bool validate_throws(t_struct* throws) {
  const vector<t_field*>& members = throws->get_members();
  vector<t_field*>::const_iterator m_iter;
  for (m_iter = members.begin(); m_iter != members.end(); ++m_iter) {
    if (!(*m_iter)->get_type()->is_xception()) {
      return false;
    }
  }
  return true;
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

  // Parse the program file
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
  try {
    if (yyparse() != 0) {
      failure("Parser error during types pass.");
    }
  } catch (string x) {
    failure(x.c_str());
  }
  fclose(yyin);
}

/**
 * Generate code
 */
void generate(t_program* program, const vector<string>& generator_strings) {
  // Oooohh, recursive code generation, hot!!
  if (gen_recurse) {
    const vector<t_program*>& includes = program->get_includes();
    for (size_t i = 0; i < includes.size(); ++i) {
      // Propogate output path from parent to child programs
      includes[i]->set_out_path(program->get_out_path());

      generate(includes[i], generator_strings);
    }
  }

  // Generate code!
  try {
    pverbose("Program: %s\n", program->get_path().c_str());

    // Compute fingerprints.
    generate_all_fingerprints(program);

    if (dump_docs) {
      dump_docstrings(program);
    }

    vector<string>::const_iterator iter;
    for (iter = generator_strings.begin(); iter != generator_strings.end(); ++iter) {
      t_generator* generator = t_generator_registry::get_generator(program, *iter);

      if (generator == NULL) {
        pwarning(1, "Unable to get a generator for \"%s\".\n", iter->c_str());
      } else {
        pverbose("Generating \"%s\"\n", iter->c_str());
        generator->generate_program();
        delete generator;
      }
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
  std::string out_path;

  // Setup time string
  time_t now = time(NULL);
  g_time_str = ctime(&now);

  // Check for necessary arguments, you gotta have at least a filename and
  // an output language flag
  if (argc < 2) {
    usage();
  }

  vector<string> generator_strings;

  // Set the current path to a dummy value to make warning messages clearer.
  g_curpath = "arguments";

  // Hacky parameter handling... I didn't feel like using a library sorry!
  for (i = 1; i < argc-1; i++) {
    char* arg;

    arg = strtok(argv[i], " ");
    while (arg != NULL) {
      // Treat double dashes as single dashes
      if (arg[0] == '-' && arg[1] == '-') {
        ++arg;
      }

      if (strcmp(arg, "-version") == 0) {
        version();
        exit(1);
      } else if (strcmp(arg, "-debug") == 0) {
        g_debug = 1;
      } else if (strcmp(arg, "-nowarn") == 0) {
        g_warn = 0;
      } else if (strcmp(arg, "-strict") == 0) {
        g_strict = 255;
        g_warn = 2;
      } else if (strcmp(arg, "-v") == 0 || strcmp(arg, "-verbose") == 0 ) {
        g_verbose = 1;
      } else if (strcmp(arg, "-r") == 0 || strcmp(arg, "-recurse") == 0 ) {
        gen_recurse = true;
      } else if (strcmp(arg, "-gen") == 0) {
        arg = argv[++i];
        if (arg == NULL) {
          fprintf(stderr, "!!! Missing generator specification\n");
          usage();
        }
        generator_strings.push_back(arg);
      } else if (strcmp(arg, "-dense") == 0) {
        gen_dense = true;
      } else if (strcmp(arg, "-cpp") == 0) {
        gen_cpp = true;
      } else if (strcmp(arg, "-javabean") == 0) {
        gen_javabean = true;
      } else if (strcmp(arg, "-java") == 0) {
        gen_java = true;
      } else if (strcmp(arg, "-php") == 0) {
        gen_php = true;
      } else if (strcmp(arg, "-phpi") == 0) {
        gen_phpi = true;
      } else if (strcmp(arg, "-phps") == 0) {
        gen_php = true;
        gen_phps = true;
      } else if (strcmp(arg, "-phpl") == 0) {
        gen_php = true;
        gen_phps = false;
      } else if (strcmp(arg, "-phpa") == 0) {
        gen_php = true;
        gen_phps = false;
        gen_phpa = true;
      } else if (strcmp(arg, "-phpo") == 0) {
        gen_php = true;
        gen_phpo = true;
      } else if (strcmp(arg, "-rest") == 0) {
        gen_rest = true;
      } else if (strcmp(arg, "-py") == 0) {
        gen_py = true;
      } else if (strcmp(arg, "-pyns") == 0) {
        gen_py = true;
        gen_py_newstyle = true;
      } else if (strcmp(arg, "-rb") == 0) {
        gen_rb = true;
      } else if (strcmp(arg, "-xsd") == 0) {
        gen_xsd = true;
      } else if (strcmp(arg, "-perl") == 0) {
        gen_perl = true;
      } else if (strcmp(arg, "-erl") == 0) {
        gen_erl = true;
      } else if (strcmp(arg, "-ocaml") == 0) {
        gen_ocaml = true;
      } else if (strcmp(arg, "-hs") == 0) {
        gen_hs = true;
      } else if (strcmp(arg, "-cocoa") == 0) {
        gen_cocoa = true;
      } else if (strcmp(arg, "-st") == 0) {
        gen_st = true;
      } else if (strcmp(arg, "-csharp") == 0) {
        gen_csharp = true;
      } else if (strcmp(arg, "-cpp_use_include_prefix") == 0) {
        g_cpp_use_include_prefix = true;
      } else if (strcmp(arg, "-I") == 0) {
        // An argument of "-I\ asdf" is invalid and has unknown results
        arg = argv[++i];

        if (arg == NULL) {
          fprintf(stderr, "!!! Missing Include directory\n");
          usage();
        }
        g_incl_searchpath.push_back(arg);
      } else if (strcmp(arg, "-o") == 0) {
        arg = argv[++i];
        if (arg == NULL) {
          fprintf(stderr, "-o: missing output directory\n");
          usage();
        }
        out_path = arg;

#ifdef MINGW
        //strip out trailing \ on Windows
        int last = out_path.length()-1;
        if (out_path[last] == '\\')
        {
          out_path.erase(last);
        }
#endif

        struct stat sb;
        if (stat(out_path.c_str(), &sb) < 0) {
          fprintf(stderr, "Output directory %s is unusable: %s\n", out_path.c_str(), strerror(errno));
          return -1;
        }
        if (! S_ISDIR(sb.st_mode)) {
          fprintf(stderr, "Output directory %s exists but is not a directory\n", out_path.c_str());
          return -1;
        }
      } else {
        fprintf(stderr, "!!! Unrecognized option: %s\n", arg);
        usage();
      }

      // Tokenize more
      arg = strtok(NULL, " ");
    }
  }

  // if you're asking for version, you have a right not to pass a file
  if (strcmp(argv[argc-1], "-version") == 0) {
    version();
    exit(1);
  }

  // TODO(dreiss): Delete these when everyone is using the new hotness.
  if (gen_cpp) {
    pwarning(1, "-cpp is deprecated.  Use --gen cpp");
    string gen_string = "cpp:";
    if (gen_dense) {
      gen_string.append("dense,");
    }
    if (g_cpp_use_include_prefix) {
      gen_string.append("include_prefix,");
    }
    generator_strings.push_back(gen_string);
  }
  if (gen_java) {
    pwarning(1, "-java is deprecated.  Use --gen java");
    generator_strings.push_back("java");
  }
  if (gen_javabean) {
    pwarning(1, "-javabean is deprecated.  Use --gen java:beans");
    generator_strings.push_back("java:beans");
  }
  if (gen_csharp) {
    pwarning(1, "-csharp is deprecated.  Use --gen csharp");
    generator_strings.push_back("csharp");
  }
  if (gen_py) {
    pwarning(1, "-py is deprecated.  Use --gen py");
    generator_strings.push_back("py");
  }
  if (gen_rb) {
    pwarning(1, "-rb is deprecated.  Use --gen rb");
    generator_strings.push_back("rb");
  }
  if (gen_perl) {
    pwarning(1, "-perl is deprecated.  Use --gen perl");
    generator_strings.push_back("perl");
  }
  if (gen_php || gen_phpi) {
    pwarning(1, "-php is deprecated.  Use --gen php");
    string gen_string = "php:";
    if (gen_phpi) {
      gen_string.append("inlined,");
    } else if(gen_phps) {
      gen_string.append("server,");
    } else if(gen_phpa) {
      gen_string.append("autoload,");
    } else if(gen_phpo) {
      gen_string.append("oop,");
    } else if(gen_rest) {
      gen_string.append("rest,");
    }
    generator_strings.push_back(gen_string);
  }
  if (gen_cocoa) {
    pwarning(1, "-cocoa is deprecated.  Use --gen cocoa");
    generator_strings.push_back("cocoa");
  }
  if (gen_erl) {
    pwarning(1, "-erl is deprecated.  Use --gen erl");
    generator_strings.push_back("erl");
  }
  if (gen_st) {
    pwarning(1, "-st is deprecated.  Use --gen st");
    generator_strings.push_back("st");
  }
  if (gen_ocaml) {
    pwarning(1, "-ocaml is deprecated.  Use --gen ocaml");
    generator_strings.push_back("ocaml");
  }
  if (gen_hs) {
    pwarning(1, "-hs is deprecated.  Use --gen hs");
    generator_strings.push_back("hs");
  }
  if (gen_xsd) {
    pwarning(1, "-xsd is deprecated.  Use --gen xsd");
    generator_strings.push_back("xsd");
  }

  // You gotta generate something!
  if (generator_strings.empty()) {
    fprintf(stderr, "!!! No output language(s) specified\n\n");
    usage();
  }

  // Real-pathify it
  char rp[PATH_MAX];
  if (argv[i] == NULL) {
    fprintf(stderr, "!!! Missing file name\n");
    usage();
  }
  if (saferealpath(argv[i], rp) == NULL) {
    failure("Could not open input file with realpath: %s", argv[i]);
  }
  string input_file(rp);

  // Instance of the global parse tree
  t_program* program = new t_program(input_file);
  if (out_path.size()) {
    program->set_out_path(out_path);
  }

  // Compute the cpp include prefix.
  // infer this from the filename passed in
  string input_filename = argv[i];
  string include_prefix;

  string::size_type last_slash = string::npos;
  if ((last_slash = input_filename.rfind("/")) != string::npos) {
    include_prefix = input_filename.substr(0, last_slash);
  }

  program->set_include_prefix(include_prefix);

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

  // The current path is not really relevant when we are doing generation.
  // Reset the variable to make warning messages clearer.
  g_curpath = "generation";
  // Reset yylineno for the heck of it.  Use 1 instead of 0 because
  // That is what shows up during argument parsing.
  yylineno = 1;

  // Generate it!
  generate(program, generator_strings);

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
