#ifndef T_MAIN_H
#define T_MAIN_H

#include <string>

/**
 * Defined in the flex library
 */

int yylex(void);

int yyparse(void);

/**
 * Expected to be defined by Flex/Bison
 */
void yyerror(char* fmt, ...);

/**
 * Parse debugging output, used to print helpful info
 */
void pdebug(char* fmt, ...);

/**
 * Parser warning
 */
void pwarning(int level, char* fmt, ...);

/**
 * Failure!
 */
void failure(char* fmt, ...);

/**
 * Converts a string filename into a thrift program name
 */
std::string program_name(std::string filename);

/**
 * Gets the directory path of a filename
 */
std::string directory_name(std::string filename);

/**
 * Get the absolute path for an include file
 */
std::string include_file(std::string filename);

/**
 * Flex utilities
 */

extern int   yylineno;
extern char  yytext[];
extern FILE* yyin;

#endif
