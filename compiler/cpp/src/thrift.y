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
%token<id>     tok_cpptype

/**
 * Integer constant value
 */
%token<iconst> tok_int_constant

/**
 * Namespace keyword
 */
%token tok_namespace

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
%token tok_service
%token tok_enum

/**
 * Grammar nodes
 */

%type<id>        Namespace

%type<ttype>     BaseType
%type<ttype>     ContainerType
%type<ttype>     MapType
%type<ttype>     SetType
%type<ttype>     ListType

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
  DefinitionList
  {
    pdebug("Program -> DefinitionList");
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
  Namespace
    {
      pdebug("Definition -> Namespace");
      g_program->set_namespace($1);
    }
| Typedef
    {
      pdebug("Definition -> Typedef");
      g_program->add_typedef($1);
    }
| Enum
    {
      pdebug("Definition -> Enum");
      g_program->add_enum($1);
    }
| Struct
    {
      pdebug("Definition -> Struct");
      g_program->add_struct($1);
    }
| Xception
    { 
      pdebug("Definition -> Xception");
      g_program->add_xception($1);     
    }
| Service
    {
      pdebug("Definition -> Service");
      g_program->add_service($1);
    }

Namespace:
  tok_namespace tok_identifier
    {
      pdebug("Namespace -> tok_namespace tok_identifier");
      $$ = $2;
    }

Typedef:
  tok_typedef DefinitionType tok_identifier
    {
      pdebug("TypeDef -> tok_typedef DefinitionType tok_identifier");
      t_typedef *td = new t_typedef($2, $3);
      $$ = td;
    }

Enum:
  tok_enum tok_identifier '{' EnumDefList '}'
    {
      pdebug("Enum -> tok_enum tok_identifier { EnumDefList }");
      $$ = $4;
      $$->set_name($2);
    }

EnumDefList:
  EnumDefList ',' EnumDef
    {
      pdebug("EnumDefList -> EnumDefList EnumDef");
      $$ = $1;
      $$->append($3);
    }
| EnumDef
    {
      pdebug("EnumDefList -> EnumDef");
      $$ = new t_enum;
      $$->append($1);
    }
|
    {
      pdebug("EnumDefList -> ");
      $$ = new t_enum;
    }

EnumDef:
  tok_identifier '=' tok_int_constant
    {
      pdebug("EnumDef => tok_identifier = tok_int_constant");
      if ($3 < 0) {
        printf("WARNING (%d): Negative value supplied for enum %s.\n", yylineno, $1);
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
  tok_service tok_identifier '{' FunctionList '}'
    {
      pdebug("Service -> tok_service tok_identifier { FunctionList }");
      $$ = $4;
      $$->set_name($2);
    }

FunctionList:
  FunctionList Function CommaOptional
    {
      pdebug("FunctionList -> FunctionList Function");
      $$ = $1;
      $1->add_function($2);
    }
|
    {
      pdebug("FunctionList -> ");
      $$ = new t_service;
    }

CommaOptional:
  ','
    {}
|
    {}

Function:
  AsyncOptional FunctionType tok_identifier '(' FieldList ')' ThrowsOptional
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
      $$ = $3;
    }
|
    {
      $$ = new t_struct;
    }

FieldList:
  FieldList ',' Field
    {
      pdebug("FieldList -> FieldList , Field");
      $$ = $1;
      $$->append($3);
    }
| Field
    {
      pdebug("FieldList -> Field");
      $$ = new t_struct;
      $$->append($1);
    }
|
    {
      pdebug("FieldList -> ");
      $$ = new t_struct;
    }

Field:
  FieldType tok_identifier '=' tok_int_constant
    {
      pdebug("Field -> FieldType tok_identifier = tok_int_constant");
      if ($4 <= 0) {
        printf("WARNING (%d): Nonpositive value (%d) not allowed as a field key for '%s'.\n", yylineno, $4, $2);
        $4 = y_field_val--;
      }
      $$ = new t_field($1, $2, $4);
    }
| FieldType tok_identifier
    {
      pdebug("Field -> FieldType tok_identifier");
      printf("WARNING (%d): No field key specified for '%s', resulting protocol may have conflicts or not be backwards compatible!\n", yylineno, $2);
      $$ = new t_field($1, $2, y_field_val--);
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
      $$ = g_program->get_void_type();
    }

FieldType:
  tok_identifier
    {
      pdebug("FieldType -> tok_identifier");
      $$ = g_program->get_custom_type($1);
      if ($$ == NULL) {
        yyerror("Type \"%s\" has not been defined.", $1);
        exit(1);
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
      $$ = g_program->get_string_type();
    }
| tok_bool
    {
      pdebug("BaseType -> tok_bool");
      $$ = g_program->get_bool_type();
    }
| tok_byte
    {
      pdebug("BaseType -> tok_byte");
      $$ = g_program->get_byte_type();
    }
| tok_i16
    {
      pdebug("BaseType -> tok_i16");
      $$ = g_program->get_i16_type();
    }
| tok_i32
    {
      pdebug("BaseType -> tok_i32");
      $$ = g_program->get_i32_type();
    }
| tok_i64
    {
      pdebug("BaseType -> tok_i64");
      $$ = g_program->get_i64_type();
    }
| tok_double
    {
      pdebug("BaseType -> tok_double");
      $$ = g_program->get_double_type();
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
  tok_list CppTypeOptional '<' FieldType '>'
    {
      pdebug("ListType -> tok_list<FieldType>");
      $$ = new t_list($4);
      if ($2 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

CppTypeOptional:
  tok_cpptype
    {
      $$ = $1;
    }
|
    {
      $$ = NULL;
    }

%%
