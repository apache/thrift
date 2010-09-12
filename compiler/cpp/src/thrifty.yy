%{
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

/**
 * Thrift parser.
 *
 * This parser is used on a thrift definition file.
 *
 */

#define __STDC_LIMIT_MACROS
#define __STDC_FORMAT_MACROS
#include <stdio.h>
#include <inttypes.h>
#include <limits.h>
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
int g_arglist = 0;
const int struct_is_struct = 0;
const int struct_is_union = 1;

%}

/**
 * This structure is used by the parser to hold the data types associated with
 * various parse nodes.
 */
%union {
  char*          id;
  int64_t        iconst;
  double         dconst;
  bool           tbool;
  t_doc*         tdoc;
  t_type*        ttype;
  t_base_type*   tbase;
  t_typedef*     ttypedef;
  t_enum*        tenum;
  t_enum_value*  tenumv;
  t_const*       tconst;
  t_const_value* tconstv;
  t_struct*      tstruct;
  t_service*     tservice;
  t_function*    tfunction;
  t_field*       tfield;
  char*          dtext;
  t_field::e_req ereq;
  t_annotation*  tannot;
}

/**
 * Strings identifier
 */
%token<id>     tok_identifier
%token<id>     tok_literal
%token<dtext>  tok_doctext
%token<id>     tok_st_identifier

/**
 * Constant values
 */
%token<iconst> tok_int_constant
%token<dconst> tok_dub_constant

/**
 * Header keywords
 */
%token tok_include
%token tok_namespace
%token tok_cpp_namespace
%token tok_cpp_include
%token tok_cpp_type
%token tok_php_namespace
%token tok_py_module
%token tok_perl_package
%token tok_java_package
%token tok_xsd_all
%token tok_xsd_optional
%token tok_xsd_nillable
%token tok_xsd_namespace
%token tok_xsd_attrs
%token tok_ruby_namespace
%token tok_smalltalk_category
%token tok_smalltalk_prefix
%token tok_cocoa_prefix
%token tok_csharp_namespace

/**
 * Base datatype keywords
 */
%token tok_void
%token tok_bool
%token tok_byte
%token tok_string
%token tok_binary
%token tok_slist
%token tok_senum
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
%token tok_oneway

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
%token tok_const
%token tok_required
%token tok_optional
%token tok_union

/**
 * Grammar nodes
 */

%type<ttype>     BaseType
%type<ttype>     SimpleBaseType
%type<ttype>     ContainerType
%type<ttype>     SimpleContainerType
%type<ttype>     MapType
%type<ttype>     SetType
%type<ttype>     ListType

%type<tdoc>      Definition
%type<ttype>     TypeDefinition

%type<ttypedef>  Typedef

%type<ttype>     TypeAnnotations
%type<ttype>     TypeAnnotationList
%type<tannot>    TypeAnnotation

%type<tfield>    Field
%type<iconst>    FieldIdentifier
%type<ereq>      FieldRequiredness
%type<ttype>     FieldType
%type<tconstv>   FieldValue
%type<tstruct>   FieldList

%type<tenum>     Enum
%type<tenum>     EnumDefList
%type<tenumv>    EnumDef

%type<ttypedef>  Senum
%type<tbase>     SenumDefList
%type<id>        SenumDef

%type<tconst>    Const
%type<tconstv>   ConstValue
%type<tconstv>   ConstList
%type<tconstv>   ConstListContents
%type<tconstv>   ConstMap
%type<tconstv>   ConstMapContents

%type<iconst>    StructHead
%type<tstruct>   Struct
%type<tstruct>   Xception
%type<tservice>  Service

%type<tfunction> Function
%type<ttype>     FunctionType
%type<tservice>  FunctionList

%type<tstruct>   Throws
%type<tservice>  Extends
%type<tbool>     Oneway
%type<tbool>     XsdAll
%type<tbool>     XsdOptional
%type<tbool>     XsdNillable
%type<tstruct>   XsdAttributes
%type<id>        CppType

%type<dtext>     CaptureDocText

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
      /*
      TODO(dreiss): Decide whether full-program doctext is worth the trouble.
      if ($1 != NULL) {
        g_program->set_doc($1);
      }
      */
      clear_doctext();
    }

CaptureDocText:
    {
      if (g_parse_mode == PROGRAM) {
        $$ = g_doctext;
        g_doctext = NULL;
      } else {
        $$ = NULL;
      }
    }

/* TODO(dreiss): Try to DestroyDocText in all sorts or random places. */
DestroyDocText:
    {
      if (g_parse_mode == PROGRAM) {
        clear_doctext();
      }
    }

/* We have to DestroyDocText here, otherwise it catches the doctext
   on the first real element. */
HeaderList:
  HeaderList DestroyDocText Header
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
| tok_namespace tok_identifier tok_identifier
    {
      pdebug("Header -> tok_namespace tok_identifier tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace($2, $3);
      }
    }
| tok_namespace '*' tok_identifier
    {
      pdebug("Header -> tok_namespace * tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("*", $3);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_cpp_namespace tok_identifier
    {
      pwarning(1, "'cpp_namespace' is deprecated. Use 'namespace cpp' instead");
      pdebug("Header -> tok_cpp_namespace tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("cpp", $2);
      }
    }
| tok_cpp_include tok_literal
    {
      pdebug("Header -> tok_cpp_include tok_literal");
      if (g_parse_mode == PROGRAM) {
        g_program->add_cpp_include($2);
      }
    }
| tok_php_namespace tok_identifier
    {
      pwarning(1, "'php_namespace' is deprecated. Use 'namespace php' instead");
      pdebug("Header -> tok_php_namespace tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("php", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_py_module tok_identifier
    {
      pwarning(1, "'py_module' is deprecated. Use 'namespace py' instead");
      pdebug("Header -> tok_py_module tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("py", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_perl_package tok_identifier
    {
      pwarning(1, "'perl_package' is deprecated. Use 'namespace perl' instead");
      pdebug("Header -> tok_perl_namespace tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("perl", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_ruby_namespace tok_identifier
    {
      pwarning(1, "'ruby_namespace' is deprecated. Use 'namespace rb' instead");
      pdebug("Header -> tok_ruby_namespace tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("rb", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_smalltalk_category tok_st_identifier
    {
      pwarning(1, "'smalltalk_category' is deprecated. Use 'namespace smalltalk.category' instead");
      pdebug("Header -> tok_smalltalk_category tok_st_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("smalltalk.category", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_smalltalk_prefix tok_identifier
    {
      pwarning(1, "'smalltalk_prefix' is deprecated. Use 'namespace smalltalk.prefix' instead");
      pdebug("Header -> tok_smalltalk_prefix tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("smalltalk.prefix", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_java_package tok_identifier
    {
      pwarning(1, "'java_package' is deprecated. Use 'namespace java' instead");
      pdebug("Header -> tok_java_package tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("java", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_cocoa_prefix tok_identifier
    {
      pwarning(1, "'cocoa_prefix' is deprecated. Use 'namespace cocoa' instead");
      pdebug("Header -> tok_cocoa_prefix tok_identifier");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("cocoa", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_xsd_namespace tok_literal
    {
      pwarning(1, "'xsd_namespace' is deprecated. Use 'namespace xsd' instead");
      pdebug("Header -> tok_xsd_namespace tok_literal");
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("cocoa", $2);
      }
    }
/* TODO(dreiss): Get rid of this once everyone is using the new hotness. */
| tok_csharp_namespace tok_identifier
   {
     pwarning(1, "'csharp_namespace' is deprecated. Use 'namespace csharp' instead");
     pdebug("Header -> tok_csharp_namespace tok_identifier");
     if (g_parse_mode == PROGRAM) {
       g_program->set_namespace("csharp", $2);
     }
   }

Include:
  tok_include tok_literal
    {
      pdebug("Include -> tok_include tok_literal");
      if (g_parse_mode == INCLUDES) {
        std::string path = include_file(std::string($2));
        if (!path.empty()) {
          g_program->add_include(path, std::string($2));
        }
      }
    }

DefinitionList:
  DefinitionList CaptureDocText Definition
    {
      pdebug("DefinitionList -> DefinitionList Definition");
      if ($2 != NULL && $3 != NULL) {
        $3->set_doc($2);
      }
    }
|
    {
      pdebug("DefinitionList -> ");
    }

Definition:
  Const
    {
      pdebug("Definition -> Const");
      if (g_parse_mode == PROGRAM) {
        g_program->add_const($1);
      }
      $$ = $1;
    }
| TypeDefinition
    {
      pdebug("Definition -> TypeDefinition");
      if (g_parse_mode == PROGRAM) {
        g_scope->add_type($1->get_name(), $1);
        if (g_parent_scope != NULL) {
          g_parent_scope->add_type(g_parent_prefix + $1->get_name(), $1);
        }
      }
      $$ = $1;
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
      $$ = $1;
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
| Senum
    {
      pdebug("TypeDefinition -> Senum");
      if (g_parse_mode == PROGRAM) {
        g_program->add_typedef($1);
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
  tok_typedef FieldType tok_identifier
    {
      pdebug("TypeDef -> tok_typedef FieldType tok_identifier");
      t_typedef *td = new t_typedef(g_program, $2, $3);
      $$ = td;
    }

CommaOrSemicolonOptional:
  ','
    {}
| ';'
    {}
|
    {}

Enum:
  tok_enum tok_identifier '{' EnumDefList '}'
    {
      pdebug("Enum -> tok_enum tok_identifier { EnumDefList }");
      $$ = $4;
      $$->set_name($2);
      $$->resolve_values();
      // make constants for all the enum values
      if (g_parse_mode == PROGRAM) {
        const std::vector<t_enum_value*>& enum_values = $$->get_constants();
        std::vector<t_enum_value*>::const_iterator c_iter;
        for (c_iter = enum_values.begin(); c_iter != enum_values.end(); ++c_iter) {
          std::string const_name = $$->get_name() + "." + (*c_iter)->get_name();
          t_const_value* const_val = new t_const_value((*c_iter)->get_value());
          const_val->set_enum($$);
          g_scope->add_constant(const_name, new t_const(g_type_i32, (*c_iter)->get_name(), const_val));
          if (g_parent_scope != NULL) {
            g_parent_scope->add_constant(g_parent_prefix + const_name, new t_const(g_type_i32, (*c_iter)->get_name(), const_val));
          }
        }
      }
    }

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
  CaptureDocText tok_identifier '=' tok_int_constant CommaOrSemicolonOptional
    {
      pdebug("EnumDef -> tok_identifier = tok_int_constant");
      if ($4 < 0) {
        pwarning(1, "Negative value supplied for enum %s.\n", $2);
      }
      if ($4 > INT_MAX) {
        pwarning(1, "64-bit value supplied for enum %s.\n", $2);
      }
      $$ = new t_enum_value($2, $4);
      if ($1 != NULL) {
        $$->set_doc($1);
      }
    }
|
  CaptureDocText tok_identifier CommaOrSemicolonOptional
    {
      pdebug("EnumDef -> tok_identifier");
      $$ = new t_enum_value($2);
      if ($1 != NULL) {
        $$->set_doc($1);
      }
    }

Senum:
  tok_senum tok_identifier '{' SenumDefList '}'
    {
      pdebug("Senum -> tok_senum tok_identifier { SenumDefList }");
      $$ = new t_typedef(g_program, $4, $2);
    }

SenumDefList:
  SenumDefList SenumDef
    {
      pdebug("SenumDefList -> SenumDefList SenumDef");
      $$ = $1;
      $$->add_string_enum_val($2);
    }
|
    {
      pdebug("SenumDefList -> ");
      $$ = new t_base_type("string", t_base_type::TYPE_STRING);
      $$->set_string_enum(true);
    }

SenumDef:
  tok_literal CommaOrSemicolonOptional
    {
      pdebug("SenumDef -> tok_literal");
      $$ = $1;
    }

Const:
  tok_const FieldType tok_identifier '=' ConstValue CommaOrSemicolonOptional
    {
      pdebug("Const -> tok_const FieldType tok_identifier = ConstValue");
      if (g_parse_mode == PROGRAM) {
        g_scope->resolve_const_value($5, $2);
        $$ = new t_const($2, $3, $5);
        validate_const_type($$);

        g_scope->add_constant($3, $$);
        if (g_parent_scope != NULL) {
          g_parent_scope->add_constant(g_parent_prefix + $3, $$);
        }
      } else {
        $$ = NULL;
      }
    }

ConstValue:
  tok_int_constant
    {
      pdebug("ConstValue => tok_int_constant");
      $$ = new t_const_value();
      $$->set_integer($1);
      if ($1 < INT32_MIN || $1 > INT32_MAX) {
        pwarning(1, "64-bit constant \"%"PRIi64"\" may not work in all languages.\n", $1);
      }
    }
| tok_dub_constant
    {
      pdebug("ConstValue => tok_dub_constant");
      $$ = new t_const_value();
      $$->set_double($1);
    }
| tok_literal
    {
      pdebug("ConstValue => tok_literal");
      $$ = new t_const_value($1);
    }
| tok_identifier
    {
      pdebug("ConstValue => tok_identifier");
      $$ = new t_const_value();
      $$->set_identifier($1);
    }
| ConstList
    {
      pdebug("ConstValue => ConstList");
      $$ = $1;
    }
| ConstMap
    {
      pdebug("ConstValue => ConstMap");
      $$ = $1;
    }

ConstList:
  '[' ConstListContents ']'
    {
      pdebug("ConstList => [ ConstListContents ]");
      $$ = $2;
    }

ConstListContents:
  ConstListContents ConstValue CommaOrSemicolonOptional
    {
      pdebug("ConstListContents => ConstListContents ConstValue CommaOrSemicolonOptional");
      $$ = $1;
      $$->add_list($2);
    }
|
    {
      pdebug("ConstListContents =>");
      $$ = new t_const_value();
      $$->set_list();
    }

ConstMap:
  '{' ConstMapContents '}'
    {
      pdebug("ConstMap => { ConstMapContents }");
      $$ = $2;
    }

ConstMapContents:
  ConstMapContents ConstValue ':' ConstValue CommaOrSemicolonOptional
    {
      pdebug("ConstMapContents => ConstMapContents ConstValue CommaOrSemicolonOptional");
      $$ = $1;
      $$->add_map($2, $4);
    }
|
    {
      pdebug("ConstMapContents =>");
      $$ = new t_const_value();
      $$->set_map();
    }

StructHead:
  tok_struct
    {
      $$ = struct_is_struct;
    }
| tok_union
    {
      $$ = struct_is_union;
    }

Struct:
  StructHead tok_identifier XsdAll '{' FieldList '}' TypeAnnotations
    {
      pdebug("Struct -> tok_struct tok_identifier { FieldList }");
      $5->set_xsd_all($3);
      $5->set_union($1 == struct_is_union);
      $$ = $5;
      $$->set_name($2);
      if ($7 != NULL) {
        $$->annotations_ = $7->annotations_;
        delete $7;
      }
    }
    
XsdAll:
  tok_xsd_all
    {
      $$ = true;
    }
|
    {
      $$ = false;
    }

XsdOptional:
  tok_xsd_optional
    {
      $$ = true;
    }
|
    {
      $$ = false;
    }

XsdNillable:
  tok_xsd_nillable
    {
      $$ = true;
    }
|
    {
      $$ = false;
    }

XsdAttributes:
  tok_xsd_attrs '{' FieldList '}'
    {
      $$ = $3;
    }
|
    {
      $$ = NULL;
    }

Xception:
  tok_xception tok_identifier '{' FieldList '}'
    {
      pdebug("Xception -> tok_xception tok_identifier { FieldList }");
      $4->set_name($2);
      $4->set_xception(true);
      $$ = $4;
    }

Service:
  tok_service tok_identifier Extends '{' FlagArgs FunctionList UnflagArgs '}'
    {
      pdebug("Service -> tok_service tok_identifier { FunctionList }");
      $$ = $6;
      $$->set_name($2);
      $$->set_extends($3);
    }

FlagArgs:
    {
       g_arglist = 1;
    }

UnflagArgs:
    {
       g_arglist = 0;
    }

Extends:
  tok_extends tok_identifier
    {
      pdebug("Extends -> tok_extends tok_identifier");
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
  CaptureDocText Oneway FunctionType tok_identifier '(' FieldList ')' Throws CommaOrSemicolonOptional
    {
      $6->set_name(std::string($4) + "_args");
      $$ = new t_function($3, $4, $6, $8, $2);
      if ($1 != NULL) {
        $$->set_doc($1);
      }
    }

Oneway:
  tok_oneway
    {
      $$ = true;
    }
|
    {
      $$ = false;
    }

Throws:
  tok_throws '(' FieldList ')'
    {
      pdebug("Throws -> tok_throws ( FieldList )");
      $$ = $3;
      if (g_parse_mode == PROGRAM && !validate_throws($$)) {
        yyerror("Throws clause may not contain non-exception types");
        exit(1);
      }
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
      if (!($$->append($2))) {
        yyerror("Field identifier %d for \"%s\" has already been used", $2->get_key(), $2->get_name().c_str());
        exit(1);
      }
    }
|
    {
      pdebug("FieldList -> ");
      y_field_val = -1;
      $$ = new t_struct(g_program);
    }

Field:
  CaptureDocText FieldIdentifier FieldRequiredness FieldType tok_identifier FieldValue XsdOptional XsdNillable XsdAttributes TypeAnnotations CommaOrSemicolonOptional
    {
      pdebug("tok_int_constant : Field -> FieldType tok_identifier");
      if ($2 < 0) {
        pwarning(1, "No field key specified for %s, resulting protocol may have conflicts or not be backwards compatible!\n", $5);
        if (g_strict >= 192) {
          yyerror("Implicit field keys are deprecated and not allowed with -strict");
          exit(1);
        }
      }
      $$ = new t_field($4, $5, $2);
      $$->set_req($3);
      if ($6 != NULL) {
        g_scope->resolve_const_value($6, $4);
        validate_field_value($$, $6);
        $$->set_value($6);
      }
      $$->set_xsd_optional($7);
      $$->set_xsd_nillable($8);
      if ($1 != NULL) {
        $$->set_doc($1);
      }
      if ($9 != NULL) {
        $$->set_xsd_attrs($9);
      }
      if ($10 != NULL) {
        $$->annotations_ = $10->annotations_;
        delete $10;
      }
    }

FieldIdentifier:
  tok_int_constant ':'
    {
      if ($1 <= 0) {
        pwarning(1, "Nonpositive value (%d) not allowed as a field key.\n", $1);
        $1 = y_field_val--;
      }
      $$ = $1;
    }
|
    {
      $$ = y_field_val--;
    }

FieldRequiredness:
  tok_required
    {
      $$ = t_field::T_REQUIRED;
    }
| tok_optional
    {
      if (g_arglist) {
        if (g_parse_mode == PROGRAM) {
          pwarning(1, "optional keyword is ignored in argument lists.\n");
        }
        $$ = t_field::T_OPT_IN_REQ_OUT;
      } else {
        $$ = t_field::T_OPTIONAL;
      }
    }
|
    {
      $$ = t_field::T_OPT_IN_REQ_OUT;
    }

FieldValue:
  '=' ConstValue
    {
      if (g_parse_mode == PROGRAM) {
        $$ = $2;
      } else {
        $$ = NULL;
      }
    }
|
    {
      $$ = NULL;
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

BaseType: SimpleBaseType TypeAnnotations
    {
      pdebug("BaseType -> SimpleBaseType TypeAnnotations");
      if ($2 != NULL) {
        $$ = new t_base_type(*static_cast<t_base_type*>($1));
        $$->annotations_ = $2->annotations_;
        delete $2;
      } else {
        $$ = $1;
      }
    }

SimpleBaseType:
  tok_string
    {
      pdebug("BaseType -> tok_string");
      $$ = g_type_string;
    }
| tok_binary
    {
      pdebug("BaseType -> tok_binary");
      $$ = g_type_binary;
    }
| tok_slist
    {
      pdebug("BaseType -> tok_slist");
      $$ = g_type_slist;
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

ContainerType: SimpleContainerType TypeAnnotations
    {
      pdebug("ContainerType -> SimpleContainerType TypeAnnotations");
      $$ = $1;
      if ($2 != NULL) {
        $$->annotations_ = $2->annotations_;
        delete $2;
      }
    }

SimpleContainerType:
  MapType
    {
      pdebug("SimpleContainerType -> MapType");
      $$ = $1;
    }
| SetType
    {
      pdebug("SimpleContainerType -> SetType");
      $$ = $1;
    }
| ListType
    {
      pdebug("SimpleContainerType -> ListType");
      $$ = $1;
    }

MapType:
  tok_map CppType '<' FieldType ',' FieldType '>'
    {
      pdebug("MapType -> tok_map <FieldType, FieldType>");
      $$ = new t_map($4, $6);
      if ($2 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

SetType:
  tok_set CppType '<' FieldType '>'
    {
      pdebug("SetType -> tok_set<FieldType>");
      $$ = new t_set($4);
      if ($2 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

ListType:
  tok_list '<' FieldType '>' CppType
    {
      pdebug("ListType -> tok_list<FieldType>");
      $$ = new t_list($3);
      if ($5 != NULL) {
        ((t_container*)$$)->set_cpp_name(std::string($5));
      }
    }

CppType:
  tok_cpp_type tok_literal
    {
      $$ = $2;
    }
|
    {
      $$ = NULL;
    }

TypeAnnotations:
  '(' TypeAnnotationList ')'
    {
      pdebug("TypeAnnotations -> ( TypeAnnotationList )");
      $$ = $2;
    }
|
    {
      $$ = NULL;
    }

TypeAnnotationList:
  TypeAnnotationList TypeAnnotation
    {
      pdebug("TypeAnnotationList -> TypeAnnotationList , TypeAnnotation");
      $$ = $1;
      $$->annotations_[$2->key] = $2->val;
      delete $2;
    }
|
    {
      /* Just use a dummy structure to hold the annotations. */
      $$ = new t_struct(g_program);
    }

TypeAnnotation:
  tok_identifier '=' tok_literal CommaOrSemicolonOptional
    {
      pdebug("TypeAnnotation -> tok_identifier = tok_literal");
      $$ = new t_annotation;
      $$->key = $1;
      $$->val = $3;
    }

%%
