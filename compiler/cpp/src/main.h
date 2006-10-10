#ifndef T_MAIN_H
#define T_MAIN_H

/**
 * Defined in the flex library
 */

extern int yylex(void);
extern int yyparse(void);

/**
 * Expected to be defined by Flex/Bison
 */
extern void yyerror(char* fmt, ...);

/**
 * Parse debugging output, used to print warnings etc.
 */
extern void pdebug(char* fmt, ...);

/**
 * Flex utilities
 */

extern int   yylineno;
extern char  yytext[];
extern FILE* yyin;

#endif
