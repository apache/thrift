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
 * Thrift scanner.
 *
 * Tokenizes a thrift definition file.
 */

%{

/* This is redundant with some of the flags in Makefile.am, but it works
 * when people override CXXFLAGS without being careful. The pragmas are
 * the 'right' way to do it, but don't work on old-enough GCC (in particular
 * the GCC that ship on Mac OS X 10.6.5, *counter* to what the GNU docs say)
 *
 * We should revert the Makefile.am changes once Apple ships a reasonable
 * GCC.
 */
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-label"

#include <string>
#include <errno.h>
#include <stdlib.h>

#include "main.h"
#include "globals.h"
#include "parse/t_program.h"

/**
 * Must be included AFTER parse/t_program.h, but I can't remember why anymore
 * because I wrote this a while ago.
 */
#include "thrifty.h"

void thrift_reserved_keyword(char* keyword) {
  yyerror("Cannot use reserved language keyword: \"%s\"\n", keyword);
  exit(1);
}

void integer_overflow(char* text) {
  yyerror("This integer is too big: \"%s\"\n", text);
  exit(1);
}

void unexpected_token(char* text) {
  yyerror("Unexpected token in input: \"%s\"\n", text);
  exit(1);
}

%}

/**
 * Provides the yylineno global, useful for debugging output
 */
%option lex-compat

/**
 * Our inputs are all single files, so no need for yywrap
 */
%option noyywrap

/**
 * We don't use it, and it fires up warnings at -Wall
 */
%option nounput

/**
 * Helper definitions, comments, constants, and whatnot
 */

intconstant   ([+-]?[0-9]+)
hexconstant   ("0x"[0-9A-Fa-f]+)
dubconstant   ([+-]?[0-9]*(\.[0-9]+)?([eE][+-]?[0-9]+)?)
identifier    ([a-zA-Z_](\.[a-zA-Z_0-9]|[a-zA-Z_0-9])*)
whitespace    ([ \t\r\n]*)
sillycomm     ("/*""*"*"*/")
multicomm     ("/*"[^*]"/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
doctext       ("/**"([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
comment       ("//"[^\n]*)
unixcomment   ("#"[^\n]*)
symbol        ([:;\,\{\}\(\)\=<>\[\]])
st_identifier ([a-zA-Z-](\.[a-zA-Z_0-9-]|[a-zA-Z_0-9-])*)
literal_begin (['\"])

%%

{whitespace}         { /* do nothing */                 }
{sillycomm}          { /* do nothing */                 }
{multicomm}          { /* do nothing */                 }
{comment}            { /* do nothing */                 }
{unixcomment}        { /* do nothing */                 }

{symbol}             { return yytext[0];                }
"*"                  { return yytext[0];                }

"false"              { yylval.iconst=0; return tok_int_constant; }
"true"               { yylval.iconst=1; return tok_int_constant; }

"namespace"          { return tok_namespace;            }
"cpp_namespace"      { return tok_cpp_namespace;        }
"cpp_include"        { return tok_cpp_include;          }
"cpp_type"           { return tok_cpp_type;             }
"java_package"       { return tok_java_package;         }
"cocoa_prefix"       { return tok_cocoa_prefix;         }
"csharp_namespace"   { return tok_csharp_namespace;     }
"delphi_namespace"   { return tok_delphi_namespace;     }
"php_namespace"      { return tok_php_namespace;        }
"py_module"          { return tok_py_module;            }
"perl_package"       { return tok_perl_package;         }
"ruby_namespace"     { return tok_ruby_namespace;       }
"smalltalk_category" { return tok_smalltalk_category;   }
"smalltalk_prefix"   { return tok_smalltalk_prefix;     }
"xsd_all"            { return tok_xsd_all;              }
"xsd_optional"       { return tok_xsd_optional;         }
"xsd_nillable"       { return tok_xsd_nillable;         }
"xsd_namespace"      { return tok_xsd_namespace;        }
"xsd_attrs"          { return tok_xsd_attrs;            }
"include"            { return tok_include;              }
"void"               { return tok_void;                 }
"bool"               { return tok_bool;                 }
"byte"               { return tok_byte;                 }
"i16"                { return tok_i16;                  }
"i32"                { return tok_i32;                  }
"i64"                { return tok_i64;                  }
"double"             { return tok_double;               }
"string"             { return tok_string;               }
"binary"             { return tok_binary;               }
"slist" {
  pwarning(0, "\"slist\" is deprecated and will be removed in a future compiler version.  This type should be replaced with \"string\".\n");
  return tok_slist;
}
"senum" {
  pwarning(0, "\"senum\" is deprecated and will be removed in a future compiler version.  This type should be replaced with \"string\".\n");
  return tok_senum;
}
"map"                { return tok_map;                  }
"list"               { return tok_list;                 }
"set"                { return tok_set;                  }
"oneway"             { return tok_oneway;               }
"typedef"            { return tok_typedef;              }
"struct"             { return tok_struct;               }
"union"              { return tok_union;                }
"exception"          { return tok_xception;             }
"extends"            { return tok_extends;              }
"throws"             { return tok_throws;               }
"service"            { return tok_service;              }
"enum"               { return tok_enum;                 }
"const"              { return tok_const;                }
"required"           { return tok_required;             }
"optional"           { return tok_optional;             }
"async" {
  pwarning(0, "\"async\" is deprecated.  It is called \"oneway\" now.\n");
  return tok_oneway;
}


"BEGIN"              { thrift_reserved_keyword(yytext); }
"END"                { thrift_reserved_keyword(yytext); }
"__CLASS__"          { thrift_reserved_keyword(yytext); }
"__DIR__"            { thrift_reserved_keyword(yytext); }
"__FILE__"           { thrift_reserved_keyword(yytext); }
"__FUNCTION__"       { thrift_reserved_keyword(yytext); }
"__LINE__"           { thrift_reserved_keyword(yytext); }
"__METHOD__"         { thrift_reserved_keyword(yytext); }
"__NAMESPACE__"      { thrift_reserved_keyword(yytext); }
"abstract"           { thrift_reserved_keyword(yytext); }
"alias"              { thrift_reserved_keyword(yytext); }
"and"                { thrift_reserved_keyword(yytext); }
"args"               { thrift_reserved_keyword(yytext); }
"as"                 { thrift_reserved_keyword(yytext); }
"assert"             { thrift_reserved_keyword(yytext); }
"begin"              { thrift_reserved_keyword(yytext); }
"break"              { thrift_reserved_keyword(yytext); }
"case"               { thrift_reserved_keyword(yytext); }
"catch"              { thrift_reserved_keyword(yytext); }
"class"              { thrift_reserved_keyword(yytext); }
"clone"              { thrift_reserved_keyword(yytext); }
"continue"           { thrift_reserved_keyword(yytext); }
"declare"            { thrift_reserved_keyword(yytext); }
"def"                { thrift_reserved_keyword(yytext); }
"default"            { thrift_reserved_keyword(yytext); }
"del"                { thrift_reserved_keyword(yytext); }
"delete"             { thrift_reserved_keyword(yytext); }
"do"                 { thrift_reserved_keyword(yytext); }
"dynamic"            { thrift_reserved_keyword(yytext); }
"elif"               { thrift_reserved_keyword(yytext); }
"else"               { thrift_reserved_keyword(yytext); }
"elseif"             { thrift_reserved_keyword(yytext); }
"elsif"              { thrift_reserved_keyword(yytext); }
"end"                { thrift_reserved_keyword(yytext); }
"enddeclare"         { thrift_reserved_keyword(yytext); }
"endfor"             { thrift_reserved_keyword(yytext); }
"endforeach"         { thrift_reserved_keyword(yytext); }
"endif"              { thrift_reserved_keyword(yytext); }
"endswitch"          { thrift_reserved_keyword(yytext); }
"endwhile"           { thrift_reserved_keyword(yytext); }
"ensure"             { thrift_reserved_keyword(yytext); }
"except"             { thrift_reserved_keyword(yytext); }
"exec"               { thrift_reserved_keyword(yytext); }
"finally"            { thrift_reserved_keyword(yytext); }
"float"              { thrift_reserved_keyword(yytext); }
"for"                { thrift_reserved_keyword(yytext); }
"foreach"            { thrift_reserved_keyword(yytext); }
"function"           { thrift_reserved_keyword(yytext); }
"global"             { thrift_reserved_keyword(yytext); }
"goto"               { thrift_reserved_keyword(yytext); }
"if"                 { thrift_reserved_keyword(yytext); }
"implements"         { thrift_reserved_keyword(yytext); }
"import"             { thrift_reserved_keyword(yytext); }
"in"                 { thrift_reserved_keyword(yytext); }
"inline"             { thrift_reserved_keyword(yytext); }
"instanceof"         { thrift_reserved_keyword(yytext); }
"interface"          { thrift_reserved_keyword(yytext); }
"is"                 { thrift_reserved_keyword(yytext); }
"lambda"             { thrift_reserved_keyword(yytext); }
"module"             { thrift_reserved_keyword(yytext); }
"native"             { thrift_reserved_keyword(yytext); }
"new"                { thrift_reserved_keyword(yytext); }
"next"               { thrift_reserved_keyword(yytext); }
"nil"                { thrift_reserved_keyword(yytext); }
"not"                { thrift_reserved_keyword(yytext); }
"or"                 { thrift_reserved_keyword(yytext); }
"pass"               { thrift_reserved_keyword(yytext); }
"public"             { thrift_reserved_keyword(yytext); }
"print"              { thrift_reserved_keyword(yytext); }
"private"            { thrift_reserved_keyword(yytext); }
"protected"          { thrift_reserved_keyword(yytext); }
"public"             { thrift_reserved_keyword(yytext); }
"raise"              { thrift_reserved_keyword(yytext); }
"redo"               { thrift_reserved_keyword(yytext); }
"rescue"             { thrift_reserved_keyword(yytext); }
"retry"              { thrift_reserved_keyword(yytext); }
"register"           { thrift_reserved_keyword(yytext); }
"return"             { thrift_reserved_keyword(yytext); }
"self"               { thrift_reserved_keyword(yytext); }
"sizeof"             { thrift_reserved_keyword(yytext); }
"static"             { thrift_reserved_keyword(yytext); }
"super"              { thrift_reserved_keyword(yytext); }
"switch"             { thrift_reserved_keyword(yytext); }
"synchronized"       { thrift_reserved_keyword(yytext); }
"then"               { thrift_reserved_keyword(yytext); }
"this"               { thrift_reserved_keyword(yytext); }
"throw"              { thrift_reserved_keyword(yytext); }
"transient"          { thrift_reserved_keyword(yytext); }
"try"                { thrift_reserved_keyword(yytext); }
"undef"              { thrift_reserved_keyword(yytext); }
"union"              { thrift_reserved_keyword(yytext); }
"unless"             { thrift_reserved_keyword(yytext); }
"unsigned"           { thrift_reserved_keyword(yytext); }
"until"              { thrift_reserved_keyword(yytext); }
"use"                { thrift_reserved_keyword(yytext); }
"var"                { thrift_reserved_keyword(yytext); }
"virtual"            { thrift_reserved_keyword(yytext); }
"volatile"           { thrift_reserved_keyword(yytext); }
"when"               { thrift_reserved_keyword(yytext); }
"while"              { thrift_reserved_keyword(yytext); }
"with"               { thrift_reserved_keyword(yytext); }
"xor"                { thrift_reserved_keyword(yytext); }
"yield"              { thrift_reserved_keyword(yytext); }

{intconstant} {
  errno = 0;
  yylval.iconst = strtoll(yytext, NULL, 10);
  if (errno == ERANGE) {
    integer_overflow(yytext);
  }
  return tok_int_constant;
}

{hexconstant} {
  errno = 0;
  yylval.iconst = strtoll(yytext+2, NULL, 16);
  if (errno == ERANGE) {
    integer_overflow(yytext);
  }
  return tok_int_constant;
}

{dubconstant} {
  yylval.dconst = atof(yytext);
  return tok_dub_constant;
}

{identifier} {
  yylval.id = strdup(yytext);
  return tok_identifier;
}

{st_identifier} {
  yylval.id = strdup(yytext);
  return tok_st_identifier;
}

{literal_begin} {
  char mark = yytext[0];
  std::string result;
  for(;;)
  {
    int ch = yyinput();
    switch (ch) {
      case EOF:
        yyerror("End of file while read string at %d\n", yylineno);
        exit(1);
      case '\n':
        yyerror("End of line while read string at %d\n", yylineno - 1);
        exit(1);
      case '\\':
        ch = yyinput();
        switch (ch) {
          case 'r':
            result.push_back('\r');
            continue;
          case 'n':
            result.push_back('\n');
            continue;
          case 't':
            result.push_back('\t');
            continue;
          case '"':
            result.push_back('"');
            continue;
          case '\'':
            result.push_back('\'');
            continue;
          case '\\':
            result.push_back('\\');
            continue;
          default:
            yyerror("Bad escape character\n");
            return -1;
        }
        break;
      default:
        if (ch == mark) {
          yylval.id = strdup(result.c_str());
          return tok_literal;
        } else {
          result.push_back(ch);
        }
    }
  }
}


{doctext} {
 /* This does not show up in the parse tree. */
 /* Rather, the parser will grab it out of the global. */
  if (g_parse_mode == PROGRAM) {
    clear_doctext();
    g_doctext = strdup(yytext + 3);
    g_doctext[strlen(g_doctext) - 2] = '\0';
    g_doctext = clean_up_doctext(g_doctext);
    g_doctext_lineno = yylineno;
  }
}

. {
  unexpected_token(yytext);
}


. {
  /* Catch-all to let us catch "*" in the parser. */
  return (int) yytext[0];
}

%%

/* vim: filetype=lex
*/
