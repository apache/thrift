/**
 * Thrift scanner.
 * 
 * Tokenizes a thrift definition file.
 * @author Mark Slee <mcslee@facebook.com>
 */

%{

#include "main.h"
#include "parse/t_program.h"

/**
 * Must be included AFTER parse/t_program.h, but I can't remember why anymore
 * because I wrote this a while ago.
 */
#include "thrift.tab.hh"

void thrift_reserved_keyword(char* keyword) {
  yyerror("Cannot use reserved language keyword: \"%s\"\n", keyword);
  exit(1);
}

%}

/**
 * Provides the yylineno global, useful for debugging output
 */
%option lex-compat 	

/** 
 * Helper definitions, comments, constants, and whatnot
 */

intconstant  ([+-]?[0-9]+)
hexconstant  ("0x"[0-9A-Fa-f]+)
dubconstant  ([+-]?[0-9]*(\.[0-9]+)?([eE][+-]?[0-9]+)?)
identifier   ([a-zA-Z_][\.a-zA-Z_0-9]*)
whitespace   ([ \t\r\n]*)
multicomm    ("/*""/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
comment      ("//"[^\n]*)
unixcomment  ("#"[^\n]*)
symbol       ([:;\,\{\}\(\)\=<>\[\]])
dliteral      ("\""[^"]*"\"")
sliteral      ("'"[^']*"'")

%%

{whitespace}  { /* do nothing */ }
{multicomm}   { /* do nothing */ }
{comment}     { /* do nothing */ }
{unixcomment} { /* do nothing */ }

{symbol}      { return yytext[0]; }

"namespace"     { return tok_namespace;     }
"cpp_namespace" { return tok_cpp_namespace; }
"cpp_include"   { return tok_cpp_include;   }
"cpp_type"      { return tok_cpp_type;      }
"java_package"  { return tok_java_package;  }
"include"       { return tok_include;       }

"void"          { return tok_void;          }
"bool"          { return tok_bool;          }
"byte"          { return tok_byte;          }
"i16"           { return tok_i16;           }
"i32"           { return tok_i32;           }
"i64"           { return tok_i64;           }
"double"        { return tok_double;        }
"string"        { return tok_string;        }
"map"           { return tok_map;           }
"list"          { return tok_list;          }
"set"           { return tok_set;           }
"async"         { return tok_async;         }
"typedef"       { return tok_typedef;       }
"struct"        { return tok_struct;        }
"exception"     { return tok_xception;      }
"extends"       { return tok_extends;       }
"throws"        { return tok_throws;        }
"service"       { return tok_service;       }
"enum"          { return tok_enum;          }
"const"         { return tok_const;         }

"abstract" { thrift_reserved_keyword(yytext); }
"and" { thrift_reserved_keyword(yytext); }
"as" { thrift_reserved_keyword(yytext); }
"assert" { thrift_reserved_keyword(yytext); }
"break" { thrift_reserved_keyword(yytext); }
"case" { thrift_reserved_keyword(yytext); }
"class" { thrift_reserved_keyword(yytext); }
"continue" { thrift_reserved_keyword(yytext); }
"declare" { thrift_reserved_keyword(yytext); }
"def" { thrift_reserved_keyword(yytext); }
"default" { thrift_reserved_keyword(yytext); }
"del" { thrift_reserved_keyword(yytext); }
"delete" { thrift_reserved_keyword(yytext); }
"do" { thrift_reserved_keyword(yytext); }
"elif" { thrift_reserved_keyword(yytext); }
"else" { thrift_reserved_keyword(yytext); }
"elseif" { thrift_reserved_keyword(yytext); }
"except" { thrift_reserved_keyword(yytext); }
"exec" { thrift_reserved_keyword(yytext); }
"false" { thrift_reserved_keyword(yytext); }
"final" { thrift_reserved_keyword(yytext); }
"finally" { thrift_reserved_keyword(yytext); }
"float" { thrift_reserved_keyword(yytext); }
"for" { thrift_reserved_keyword(yytext); }
"foreach" { thrift_reserved_keyword(yytext); }
"function" { thrift_reserved_keyword(yytext); }
"global" { thrift_reserved_keyword(yytext); }
"goto" { thrift_reserved_keyword(yytext); }
"if" { thrift_reserved_keyword(yytext); }
"implements" { thrift_reserved_keyword(yytext); }
"import" { thrift_reserved_keyword(yytext); }
"in" { thrift_reserved_keyword(yytext); }
"inline" { thrift_reserved_keyword(yytext); }
"instanceof" { thrift_reserved_keyword(yytext); }
"interface" { thrift_reserved_keyword(yytext); }
"is" { thrift_reserved_keyword(yytext); }
"lambda" { thrift_reserved_keyword(yytext); }
"native" { thrift_reserved_keyword(yytext); }
"new" { thrift_reserved_keyword(yytext); }
"not" { thrift_reserved_keyword(yytext); }
"or" { thrift_reserved_keyword(yytext); }
"pass" { thrift_reserved_keyword(yytext); }
"public" { thrift_reserved_keyword(yytext); }
"print" { thrift_reserved_keyword(yytext); }
"private" { thrift_reserved_keyword(yytext); }
"protected" { thrift_reserved_keyword(yytext); }
"raise" { thrift_reserved_keyword(yytext); }
"return" { thrift_reserved_keyword(yytext); }
"sizeof" { thrift_reserved_keyword(yytext); }
"static" { thrift_reserved_keyword(yytext); }
"switch" { thrift_reserved_keyword(yytext); }
"synchronized" { thrift_reserved_keyword(yytext); }
"this" { thrift_reserved_keyword(yytext); }
"throw" { thrift_reserved_keyword(yytext); }
"transient" { thrift_reserved_keyword(yytext); }
"true" { thrift_reserved_keyword(yytext); }
"try" { thrift_reserved_keyword(yytext); }
"unsigned" { thrift_reserved_keyword(yytext); }
"var" { thrift_reserved_keyword(yytext); }
"virtual" { thrift_reserved_keyword(yytext); }
"volatile" { thrift_reserved_keyword(yytext); }
"while" { thrift_reserved_keyword(yytext); }
"with" { thrift_reserved_keyword(yytext); }
"union" { thrift_reserved_keyword(yytext); }
"yield" { thrift_reserved_keyword(yytext); }

{intconstant} {
  yylval.iconst = atoi(yytext);
  return tok_int_constant;
}

{hexconstant} {
  sscanf(yytext+2, "%x", &yylval.iconst);
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

{dliteral} {
  yylval.id = strdup(yytext+1);
  yylval.id[strlen(yylval.id)-1] = '\0';
  return tok_literal;
}

{sliteral} {
  yylval.id = strdup(yytext+1);
  yylval.id[strlen(yylval.id)-1] = '\0';
  return tok_literal;
}

%%
