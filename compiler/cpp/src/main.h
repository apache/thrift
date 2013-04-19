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

#ifndef T_MAIN_H
#define T_MAIN_H

#include <string>
#include "parse/t_const.h"
#include "parse/t_field.h"

/**
 * Defined in the flex library
 */

int yylex(void);

int yyparse(void);

/**
 * Expected to be defined by Flex/Bison
 */
void yyerror(const char* fmt, ...);

/**
 * Parse debugging output, used to print helpful info
 */
void pdebug(const char* fmt, ...);

/**
 * Parser warning
 */
void pwarning(int level, const char* fmt, ...);

/**
 * Print verbose output message
 */
void pverbose(const char* fmt, ...);

/**
 * Failure!
 */
void failure(const char* fmt, ...);

/**
 * Check constant types
 */
void validate_const_type(t_const* c);

/**
 * Check constant types
 */
void validate_field_value(t_field* field, t_const_value* cv);

/**
 * Check members of a throws block
 */
bool validate_throws(t_struct* throws);

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
 * Clears any previously stored doctext string.
 */
void clear_doctext();

/**
 * Cleans up text commonly found in doxygen-like comments
 */
char* clean_up_doctext(char* doctext);

/**
 * Flex utilities
 */

extern int   yylineno;
extern char  yytext[];
extern FILE* yyin;

#endif
