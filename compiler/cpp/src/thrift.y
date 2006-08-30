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

int y_field_val = 0;

%}

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

/** Strings and constants */
%token<id>     tok_identifier
%token<iconst> tok_int_constant

/** Base datatypes */
%token tok_byte
%token tok_string
%token tok_i32
%token tok_u32
%token tok_i64
%token tok_u64

/** Complex Types */
%token tok_map
%token tok_list
%token tok_set

/** Function types */
%token tok_void

/** Modifiers */ 
%token tok_async

/** Thrift actions */
%token tok_typedef
%token tok_struct
%token tok_service
%token tok_enum

/** Types */
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

%type<tservice>  Service

%type<tfunction> Function
%type<ttype>     FunctionType
%type<tservice>  FunctionList

%type<tbool>     AsyncOptional

%%

/** Thrift Grammar */

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
  Typedef
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
| Service
    {
      pdebug("Definition -> Service");
      g_program->add_service($1);
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
      y_field_val = 0;
    }

Service:
  tok_service tok_identifier '{' FunctionList '}'
    {
      pdebug("Service -> tok_service tok_identifier { FunctionList }");
      $$ = $4;
      $$->set_name($2);
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
      $$ = new t_service;
    }

Function:
  FunctionType AsyncOptional tok_identifier '(' FieldList ')'
    {
      $5->set_name(std::string($3) + "_args");
      $$ = new t_function($1, $3, $5, $2);
      y_field_val = 0;
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
      if ($4 < 0) {
        yyerror("Negative value (%d) not allowed as a field key.", $4);
        exit(1);
      }
      $$ = new t_field($1, $2, (uint32_t)$4);
      y_field_val = $4+1;
    }
| FieldType tok_identifier
    {
      pdebug("Field -> FieldType tok_identifier");
      printf("WARNING (%d): No field key specified for %s, resulting protocol may have conflicts or not be backwards compatible!\n", yylineno, $2);
      $$ = new t_field($1, $2, y_field_val++);
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
| tok_byte
    {
      pdebug("BaseType -> tok_byte");
      $$ = g_program->get_byte_type();
    }
| tok_i32
    {
      pdebug("BaseType -> tok_i32");
      $$ = g_program->get_i32_type();
    }
| tok_u32
    {
      pdebug("BaseType -> tok_u32");
      $$ = g_program->get_u32_type();
    }
| tok_i64
    {
      pdebug("BaseType -> tok_i64");
      $$ = g_program->get_i64_type();
    }
| tok_u64
    {
      pdebug("BaseType -> tok_u64");
      $$ = g_program->get_u64_type();
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
  tok_map '<' FieldType ',' FieldType '>'
    {
      pdebug("MapType -> tok_map <FieldType, FieldType>");
      $$ = new t_map($3, $5);
    }

SetType:
  tok_set '<' FieldType '>'
    {
      pdebug("SetType -> tok_set<FieldType>");
      $$ = new t_set($3);
    }

ListType:
  tok_list '<' FieldType '>'
    {
      pdebug("ListType -> tok_list<FieldType>");
      $$ = new t_list($3);
    }

%%
