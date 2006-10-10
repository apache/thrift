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

%}

/**
 * Provides the yylineno global, useful for debugging output
 */
%option lex-compat 	

/** 
 * Helper definitions, comments, constants, and whatnot
 */

intconstant  ([0-9]+)
identifier   ([a-zA-Z_][\.a-zA-Z_0-9]*)
whitespace   ([ \t\r\n]*)
multicomm    ("/*""/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
comment      ("//"[^\n]*)
unixcomment  ("#"[^\n]*)
symbol       ([\,\{\}\(\)\=<>])

%%

{whitespace}  { /* do nothing */ }
{multicomm}   { /* do nothing */ }
{comment}     { /* do nothing */ }
{unixcomment} { /* do nothing */ }

{symbol}      { return yytext[0]; }

"namespace"   { return tok_namespace; }
"void"        { return tok_void;      }
"bool"        { return tok_bool;      }
"byte"        { return tok_byte;      }
"i16"         { return tok_i16;       }
"i32"         { return tok_i32;       }
"i64"         { return tok_i64;       }
"double"      { return tok_double;    }
"string"      { return tok_string;    }
"map"         { return tok_map;       }
"list"        { return tok_list;      }
"set"         { return tok_set;       }
"async"       { return tok_async;     }
"typedef"     { return tok_typedef;   }
"struct"      { return tok_struct;    }
"exception"   { return tok_xception;  }
"throws"      { return tok_throws;    }
"service"     { return tok_service;   }
"enum"        { return tok_enum;      }

{intconstant} { yylval.iconst = atoi(yytext); return tok_int_constant; }

{identifier}  { yylval.id = strdup(yytext); return tok_identifier; }

%%
