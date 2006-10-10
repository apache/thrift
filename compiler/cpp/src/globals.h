#ifndef T_GLOBALS_H
#define T_GLOBALS_H

class t_program;

/**
 * The master program parse tree. This is accessed from within the parser code
 * to build up the program elements.
 */
extern t_program* g_program;

/**
 * Global debug state
 */
extern int g_debug;

/**
 * Global time string, used in formatting error messages etc.
 */
extern char* g_time_str;

#endif
