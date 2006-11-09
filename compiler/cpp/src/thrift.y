%{

/**
 * Thrift parser.
 *
 * This parser is used on a thrift definition file.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */

#include <stdio.h>
#include "main.h"
#include "globals.h"
#include "parse/t_program.h"
#include "parse/t_scope.h"

/**
 * This global variable is used for automatic numbering of field indices etc.
 * when parsing the members of a struct. Field values are automatically
 * assigned starting from -1 and working their way down.
 */
int y_field_val = -1;

%}

/**
 * This structure is used by the parser to hold the data types associated with
 * various parse nodes.
 */
%union {
  char*       id;
  int         iconst;
  bool        tbool;
  t_type*     ttype;
  t_typedef*  ttypedef;
  t_enum*     tenum;
  t_struct*   tstruct;
  t_service*  tservice;
  t_function* tfunction;
  t_field*    tfield;
  t_constant* tconstant;
}

/**
 * Strings identifier
 */
%token<id>     tok_identifier
%token<id>     tok_literal

/**
 * Integer constant value
 */
%token<iconst> tok_int_constant

/**
 * Header keywoards
 */
%token tok_include
%token tok_namespace
%token tok_cpp_namespace
%token tok_cpp_include
%token tok_cpp_type
%token tok_java_package

/**
 * Base datatype keywords
 */
%token tok_void
%token tok_bool
%token tok_byte
%token tok_string
%token tok_i16
%token tok_i32
%token tok_i64
%token tok_double

/**
 * Complex type keywords
 */
%token tok_map
%token tok_list
%token tok_set

/**
 * Function modifiers
 */ 
%token tok_async

/**
 * Thrift language keywords
 */
%token tok_typedef
%token tok_struct
%token tok_xception
%token tok_throws
%token tok_extends
%token tok_service
%token tok_enum

/**
 * Grammar nodes
 */

%type<ttype>     BaseType
%type<ttype>     ContainerType
%type<ttype>     MapType
%type<ttype>     SetType
%type<ttype>     ListType

%type<ttype>     TypeDefinition

%type<ttypedef>  Typedef
%type<ttype>     DefinitionType

%type<tfield>    Field
%type<ttype>     FieldType
%type<tstruct>   FieldList

%type<tenum>     Enum
%type<tenum>     EnumDefList
%type<tconstant> EnumDef

%type<tstruct>   Struct
%type<tstruct>   Xception
%type<tservice>  Service

%type<tfunction> Function
%type<ttype>     FunctionType
%type<tservice>  FunctionList

%type<tstruct>   ThrowsOptional
%type<tservice>  ExtendsOptional
%type<tbool>     AsyncOptional
%type<id>        CppTypeOptional

%%

/**
 * Thrift Grammar Implementation.
 *
 * For the most part this source file works its way top down from what you
 * might expect to find in a typical .thrift file, i.e. type definitions and
 * namespaces up top followed by service definitions using those types.
 */

Program:
  HeaderList DefinitionList
    {
      pdebug("Program -> Headers DefinitionList");
    }

HeaderList:
  HeaderList Header
    {
      pdebug("HeaderList -> HeaderList Header");
    }
|
    {
      pdebug("HeaderList -> ");
    }

Header:
  Include
    {
      pdebug("Header -> Include");
    }
| tok_namespace tok_identifier
    {
      pwarning(1, "'namespace' is deprecated. Use 'cpp_namespace' and/or 'java_package' instead");
      if (g_parse_mode == PROGRAM) {
        g_program->set_cpp_namespace($2);
        g_program->set_java_package($2);
      }
    }
| tok_cpp_namespace tok_identifier
    {
      pdebug("Header -> tok_cpp_namespace tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_cpp_namespace($2);
      }
    }
| tok_cpp_include tok_literal
    {
      pdebug("Header -> tok_cpp_include tok_literal");
      if (g_parse_mode == PROGRAM) {
        g_program->add_cpp_include($2);
      }
    }
| tok_java_package tok_identifier
    {
      pdebug("Header -> tok_java_package tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_java_package($2);
      }
    }

Include:
  tok_include tok_literal
    {
      pdebug("Include -> tok_include tok_literal");     
      if (g_parse_mode == INCLUDES) {
        std::string path = include_file(std::string($2));
        if (!path.empty()) {
          g_program->add_include(path);
        }
      }
    }

DefinitionList:
  DefinitionList Definition
    {
      pdebug("DefinitionList -> DefinitionList Definition");
    }
|
    {
      pdebug("DefinitionList -> ");
    }

Definition:
  TypeDefinition
    {
      pdebug("Definition -> TypeDefinition");
      if (g_parse_mode == PROGRAM) {
        g_scope->add_type($1->get_name(), $1);
        if (g_parent_scope != NULL) {
          g_parent_scope->add_type(g_parent_prefix + $1->get_name(), $1);
        }
      }
    }
| Service
    {
      pdebug("Definition -> Service");
      if (g_parse_mode == PROGRAM) {
        g_scope->add_service($1->get_name(), $1);
        if (g_parent_scope != NULL) {
          g_parent_scope->add_service(g_parent_prefix + $1->get_name(), $1);
        }
        g_program->add_service($1);
      }
    }

TypeDefinition:
  Typedef
    {
      pdebug("TypeDefinition -> Typedef");
      if (g_parse_mode == PROGRAM) {
        g_program->add_typedef($1);
      }
    }
| Enum
    {
      pdebug("TypeDefinition -> Enum");
      if (g_parse_mode == PROGRAM) {
        g_program->add_enum($1);
      }
    }
| Struct
    {
      pdebug("TypeDefinition -> Struct");
      if (g_parse_mode == PROGRAM) {
        g_program->add_struct($1);
      }
    }
| Xception
    { 
      pdebug("TypeDefinition -> Xception");
      if (g_parse_mode == PROGRAM) {
        g_program->add_xception($1);
      }
    }

Typedef:
  tok_typedef DefinitionType tok_identifier
    {
      pdebug("TypeDef -> tok_typedef DefinitionType tok_identifier");
      t_typedef *td = new t_typedef(g_program, $2, $3);
      $$ = td;
    }

Enum:
  tok_enum tok_identifier '{' EnumDefList '}'
    {
      pdebug("Enum -> tok_enum tok_identifier { EnumDefList }");
      $$ = $4;
      $$->set_name($2);
    }

CommaOrSemicolonOptional:
  ','
    {}
| ';'
    {}
|
    {}

EnumDefList:
  EnumDefList EnumDef
    {
      pdebug("EnumDefList -> EnumDefList EnumDef");
      $$ = $1;
      $$->append($2);
    }
|
    {
      pdebug("EnumDefList -> ");
      $$ = new t_enum(g_program);
    }

EnumDef:
  tok_identifier '=' tok_int_constant CommaOrSemicolonOptional
    {
      pdebug("EnumDef => tok_identifier = tok_int_constant");
      if ($3 < 0) {
        pwarning(1, "Negative value supplied for enum %s.\n", $1);
      }
      $$ = new t_constant($1, $3);
    }
|
  tok_identifier
    {
      pdebug("EnumDef => tok_identifier");
      $$ = new t_constant($1);
    }

Struct:
  tok_struct tok_identifier '{' FieldList '}'
    {
      pdebug("Struct -> tok_struct tok_identifier { FieldList }");
      $4->set_name($2);
      $$ = $4;
      y_field_val = -1;
    }

Xception:
  tok_xception tok_identifier '{' FieldList '}'
    {
      pdebug("Xception -> tok_xception tok_identifier { FieldList }");
      $4->set_name($2);
      $4->set_xception(true);
      $$ = $4;
      y_field_val = -1;
    }

Service:
  tok_service tok_identifier ExtendsOptional '{' FunctionList '}'
    {
      pdebug("Service -> tok_service tok_identifier { FunctionList }");
      $$ = $5;
      $$->set_name($2);
      $$->set_extends($3);
    }

ExtendsOptional:
  tok_extends tok_identifier
    {
      pdebug("ExtendsOptional -> tok_extends tok_identifier");
      $$ = NULL;
      if (g_parse_mode == PROGRAM) {
        $$ = g_scope->get_service($2);
        if ($$ == NULL) {
          yyerror("Service \"%s\" has not been defined.", $2);
          exit(1);
        }
      }
    }
|
    {
      $$ = NULL;
    }

FunctionList:
  FunctionList Function
    {
      pdebug("FunctionList -> FunctionList Function");
      $$ = $1;
      $1->add_function($2);
    }
|
    {
      pdebug("FunctionList -> ");
      $$ = new t_service(g_program);
    }

Function:
  AsyncOptional FunctionType tok_identifier '(' FieldList ')' ThrowsOptional CommaOrSemicolonOptional
    {
      $5->set_name(std::string($3) + "_args");
      $$ = new t_function($2, $3, $5, $7, $1);
      y_field_val = -1;
    }

AsyncOptional:
  tok_async
    {
      $$ = true;
    }
|
    {
      $$ = false;
    }

ThrowsOptional:
  tok_throws '(' FieldList ')'
    {
      pdebug("ThrowsOptional -> tok_throws ( FieldList )");
      $$ = $3;
    }
|
    {
      $$ = new t_struct(g_program);
    }

FieldList:
  FieldList Field
    {
      pdebug("FieldList -> FieldList , Field");
      $$ = $1;
      $$->append($2);
    }
|
    {
      pdebug("FieldList -> ");
      $$ = new t_struct(g_program);
    }

Field:
  tok_int_constant ':' FieldType tok_identifier CommaOrSemicolonOptional
    {
      pdebug("tok_int_constant : Field -> FieldType tok_identifier");
      if ($1 <= 0) {
        pwarning(1, "Nonpositive value (%d) not allowed as a field key for '%s'.\n", $1, $4);
        $1 = y_field_val--;
      }
      $$ = new t_field($3, $4, $1);
    }
| FieldType tok_identifier CommaOrSemicolonOptional
    {
      pdebug("Field -> FieldType tok_identifier");
      pwarning(2, "No field key specified for '%s', resulting protocol may have conflicts or not be backwards compatible!\n", $2);
      $$ = new t_field($1, $2, y_field_val--);
    }
| FieldType tok_identifier '=' tok_int_constant CommaOrSemicolonOptional
    {
      pwarning(1, "Trailing = id notation is deprecated. Use 'Id: Type Name' notation instead"); 
      pdebug("Field -> FieldType tok_identifier = tok_int_constant");
      if ($4 <= 0) {
        pwarning(1, "Nonpositive value (%d) not allowed as a field key for '%s'.\n", $4, $2);
        $4 = y_field_val--;
      }
      $$ = new t_field($1, $2, $4);
    }

DefinitionType:
  BaseType
    {
      pdebug("DefinitionType -> BaseType");
      $$ = $1;
    }
| ContainerType
    {
      pdebug("DefinitionType -> ContainerType");
      $$ = $1;
    }

FunctionType:
  FieldType
    {
      pdebug("FunctionType -> FieldType");
      $$ = $1;
    }
| tok_void
    {
      pdebug("FunctionType -> tok_void");
      $$ = g_type_void;
    }

FieldType:
  tok_identifier
    {
      pdebug("FieldType -> tok_identifier");
      if (g_parse_mode == INCLUDES) {
        // Ignore identifiers in include mode
        $$ = NULL;
      } else {
        // Lookup the identifier in the current scope
        $$ = g_scope->get_type($1);
        if ($$ == NULL) {
          yyerror("Type \"%s\" has not been defined.", $1);
          exit(1);
        }
      }
    }
| BaseType
    {
      pdebug("FieldType -> BaseType");
      $$ = $1;
    }
| ContainerType
    {
      pdebug("FieldType -> ContainerType");
      $$ = $1;
    }

BaseType:
  tok_string
    {
      pdebug("BaseType -> tok_string");
      $$ = g_type_string;
    }
| tok_bool
    {
      pdebug("BaseType -> tok_bool");
      $$ = g_type_bool;
    }
| tok_byte
    {
      pdebug("BaseType -> tok_byte");
      $$ = g_type_byte;
    }
| tok_i16
    {
      pdebug("BaseType -> tok_i16");
      $$ = g_type_i16;
    }
| tok_i32
    {
      pdebug("BaseType -> tok_i32");
      $$ = g_type_i32;
    }
| tok_i64
    {
      pdebug("BaseType -> tok_i64");
      $$ = g_type_i64;
    }
| tok_double
    {
      pdebug("BaseType -> tok_double");
      $$ = g_type_double;
    }

ContainerType:
  MapType
    {
      pdebug("ContainerType -> MapType");
      $$ = $1;
    }
| SetType
    {
      pdebug("ContainerType -> SetType");
      $$ = $1;
    }
| ListType
    {
      pdebug("ContainerType -> ListType");
      $$ = $1;
    }

MapType:
  tok_map CppTypeOptional '<' FieldType ',' FieldType '>'
    {
      pdebug("MapType -> tok_map <FieldType, FieldType>");
      $$ = new t_map($4, $6);
      if ($2 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

SetType:
  tok_set CppTypeOptional '<' FieldType '>'
    {
      pdebug("SetType -> tok_set<FieldType>");
      $$ = new t_set($4);
      if ($2 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

ListType:
  tok_list '<' FieldType '>' CppTypeOptional
    {
      pdebug("ListType -> tok_list<FieldType>");
      $$ = new t_list($3);
      if ($5 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($5));
      }
    }

CppTypeOptional:
  '[' tok_cpp_type tok_literal ']'
    {
      $$ = $3;
    }
|
    {
      $$ = NULL;
    }

%%
