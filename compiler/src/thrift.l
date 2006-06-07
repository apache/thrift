/**
 * Thrift scanner.
 * 
 * Tokenizes a thrift definition file.
 * @author Mark Slee <mcslee@facebook.com>
 */
%{

#include "main.h"
#include "parse/t_program.h"

/** Must be included AFTER parse/t_program.h */
#include "thrift.tab.hh"

%}

/** Provides yylineno global */
%option lex-compat 	

/** Helper definitions */
intconstant  ([0-9]+)
identifier   ([a-zA-Z_][a-zA-Z_0-9]*)
whitespace   ([ \t\r\n]*)
multicomm    ("/*""/"*([^*/]|[^*]"/"|"*"[^/])*"*"*"*/")
comment      ("//"[^\n]*)
symbol       ([\,\{\}\(\)\=<>])

%%

{whitespace}  { /* do nothing */ }
{multicomm}   { /* do nothing */ }
{comment}     { /* do nothing */ }

{symbol}      { return yytext[0];   }

"byte"        { return tok_byte;     }
"string"      { return tok_string;   }
"i32"         { return tok_i32;      }
"u32"         { return tok_u32;      }
"i64"         { return tok_i64;      }
"u64"         { return tok_u64;      }

"map"         { return tok_map;      }
"list"        { return tok_list;     }
"set"         { return tok_set;      }

"void"        { return tok_void;     }
"async"       { return tok_async;    }

"typedef"     { return tok_typedef;  }
"struct"      { return tok_struct;   }
"service"     { return tok_service;  }
"enum"        { return tok_enum;     }

{intconstant} { yylval.iconst = atoi(yytext) ; return tok_int_constant; }
{identifier}  { yylval.id = strdup(yytext); return tok_identifier; }

%%
