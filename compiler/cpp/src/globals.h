// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

#ifndef T_GLOBALS_H
#define T_GLOBALS_H

#include <set>
#include <queue>
#include <stack>
#include <vector>
#include <string>

/**
 * This module contains all the global variables (slap on the wrist) that are
 * shared throughout the program. The reason for this is to facilitate simple
 * interaction between the parser and the rest of the program. Before calling
 * yyparse(), the main.cc program will make necessary adjustments to these
 * global variables such that the parser does the right thing and puts entries
 * into the right containers, etc.
 *
 */

/**
 * Hooray for forward declaration of types!
 */

class t_program;
class t_scope;
class t_type;

/**
 * Parsing mode, two passes up in this gin rummy!
 */

enum PARSE_MODE {
  INCLUDES = 1,
  PROGRAM = 2
};

/**
 * The master program parse tree. This is accessed from within the parser code
 * to build up the program elements.
 */
extern t_program* g_program;

/**
 * Global types for the parser to be able to reference
 */

extern t_type* g_type_void;
extern t_type* g_type_string;
extern t_type* g_type_binary;
extern t_type* g_type_slist;
extern t_type* g_type_bool;
extern t_type* g_type_byte;
extern t_type* g_type_i16;
extern t_type* g_type_i32;
extern t_type* g_type_i64;
extern t_type* g_type_double;

/**
 * The scope that we are currently parsing into
 */
extern t_scope* g_scope;

/**
 * The parent scope to also load symbols into
 */
extern t_scope* g_parent_scope;

/**
 * The prefix for the parent scope entries
 */
extern std::string g_parent_prefix;

/**
 * The parsing pass that we are on. We do different things on each pass.
 */
extern PARSE_MODE g_parse_mode;

/**
 * Global time string, used in formatting error messages etc.
 */
extern char* g_time_str;

/**
 * The last parsed doctext comment.
 */
extern char* g_doctext;

/**
 * The location of the last parsed doctext comment.
 */
extern int g_doctext_lineno;

#endif
