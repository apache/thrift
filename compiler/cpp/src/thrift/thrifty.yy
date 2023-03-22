%code requires {
#include "thrift/parse/t_program.h"
}
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

#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS
#endif
#ifndef __STDC_FORMAT_MACROS
#define __STDC_FORMAT_MACROS
#endif
#include <stdio.h>
#include <string.h>
#ifndef _MSC_VER
#include <inttypes.h>
#else
#include <stdint.h>
#endif
#include <limits.h>
#ifdef _MSC_VER
#include "thrift/windows/config.h"
#endif
#include "thrift/main.h"
#include "thrift/common.h"
#include "thrift/globals.h"
#include "thrift/parse/t_program.h"
#include "thrift/parse/t_scope.h"

#ifdef _MSC_VER
//warning C4065: switch statement contains 'default' but no 'case' labels
#pragma warning(disable:4065)
#endif

/**
 * This global variable is used for automatic numbering of field indices etc.
 * when parsing the members of a struct. Field values are automatically
 * assigned starting from -1 and working their way down.
 */
int y_field_val = -1;
/**
 * This global variable is used for automatic numbering of enum values.
 * y_enum_val is the last value assigned; the next auto-assigned value will be
 * y_enum_val+1, and then it continues working upwards.  Explicitly specified
 * enum values reset y_enum_val to that value.
 */
int32_t y_enum_val = -1;
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
  char*          keyword;
  t_field::e_req ereq;
  t_annotation*  tannot;
  t_field_id     tfieldid;
}

/**
 * Strings identifier
 */
%token<id>     tok_identifier
%token<id>     tok_literal
%token<dtext>  tok_doctext

/**
 * Constant values
 */
%token<iconst> tok_int_constant
%token<dconst> tok_dub_constant

/**
 * Header keywords
 */
%token<keyword> tok_include
%token<keyword> tok_namespace
%token<keyword> tok_cpp_include
%token<keyword> tok_cpp_type
%token<keyword> tok_xsd_all
%token<keyword> tok_xsd_optional
%token<keyword> tok_xsd_nillable
%token<keyword> tok_xsd_attrs

/**
 * Base datatype keywords
 */
%token<keyword> tok_void
%token<keyword> tok_bool
%token<keyword> tok_string
%token<keyword> tok_binary
%token<keyword> tok_uuid
%token<keyword> tok_byte
%token<keyword> tok_i8
%token<keyword> tok_i16
%token<keyword> tok_i32
%token<keyword> tok_i64
%token<keyword> tok_double

/**
 * Complex type keywords
 */
%token<keyword> tok_map
%token<keyword> tok_list
%token<keyword> tok_set

/**
 * Function modifiers
 */
%token<keyword> tok_oneway
%token<keyword> tok_async

/**
 * Thrift language keywords
 */
%token<keyword> tok_typedef
%token<keyword> tok_struct
%token<keyword> tok_xception
%token<keyword> tok_throws
%token<keyword> tok_extends
%token<keyword> tok_service
%token<keyword> tok_enum
%token<keyword> tok_const
%token<keyword> tok_required
%token<keyword> tok_optional
%token<keyword> tok_union
%token<keyword> tok_reference

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
%type<id>        TypeAnnotationValue

%type<tfield>    Field
%type<tfieldid>  FieldIdentifier
%type<id>        FieldName
%type<ereq>      FieldRequiredness
%type<ttype>     FieldType
%type<tconstv>   FieldValue
%type<tstruct>   FieldList
%type<tbool>     FieldReference

%type<tenum>     Enum
%type<tenum>     EnumDefList
%type<tenumv>    EnumDef
%type<tenumv>    EnumValue

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
      if((g_program_doctext_candidate != nullptr) && (g_program_doctext_status != ALREADY_PROCESSED))
      {
        g_program->set_doc(g_program_doctext_candidate);
        g_program_doctext_status = ALREADY_PROCESSED;
      }
      clear_doctext();
    }

CaptureDocText:
    {
      if (g_parse_mode == PROGRAM) {
        $$ = g_doctext;
        g_doctext = nullptr;
      } else {
        $$ = nullptr;
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
| tok_namespace tok_identifier tok_identifier TypeAnnotations
    {
      pdebug("Header -> tok_namespace tok_identifier tok_identifier");
      declare_valid_program_doctext();
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace($2, $3);
      }
      if ($4 != nullptr) {
        g_program->set_namespace_annotations($2, $4->annotations_);
        delete $4;
      }
    }
| tok_namespace '*' tok_identifier
    {
      pdebug("Header -> tok_namespace * tok_identifier");
      declare_valid_program_doctext();
      if (g_parse_mode == PROGRAM) {
        g_program->set_namespace("*", $3);
      }
    }
| tok_cpp_include tok_literal
    {
      pdebug("Header -> tok_cpp_include tok_literal");
      declare_valid_program_doctext();
      if (g_parse_mode == PROGRAM) {
        g_program->add_cpp_include($2);
      }
    }

Include:
  tok_include tok_literal
    {
      pdebug("Include -> tok_include tok_literal");
      declare_valid_program_doctext();
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
      if ($2 != nullptr && $3 != nullptr) {
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
        if (g_parent_scope != nullptr) {
          g_parent_scope->add_type(g_parent_prefix + $1->get_name(), $1);
        }
        if (! g_program->is_unique_typename($1)) {
          yyerror("Type \"%s\" is already defined.", $1->get_name().c_str());
          exit(1);
        }
      }
      $$ = $1;
    }
| Service
    {
      pdebug("Definition -> Service");
      if (g_parse_mode == PROGRAM) {
        g_scope->add_service($1->get_name(), $1);
        if (g_parent_scope != nullptr) {
          g_parent_scope->add_service(g_parent_prefix + $1->get_name(), $1);
        }
        g_program->add_service($1);
        if (! g_program->is_unique_typename($1)) {
          yyerror("Type \"%s\" is already defined.", $1->get_name().c_str());
          exit(1);
        }
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

CommaOrSemicolonOptional:
  ','
    {}
| ';'
    {}
|
    {}

Typedef:
  tok_typedef FieldType tok_identifier TypeAnnotations CommaOrSemicolonOptional
    {
      pdebug("TypeDef -> tok_typedef FieldType tok_identifier");
      validate_simple_identifier( $3);
      t_typedef *td = new t_typedef(g_program, $2, $3);
      $$ = td;
      if ($4 != nullptr) {
        $$->annotations_ = $4->annotations_;
        delete $4;
      }
    }

Enum:
  tok_enum tok_identifier '{' EnumDefList '}' TypeAnnotations
    {
      pdebug("Enum -> tok_enum tok_identifier { EnumDefList }");
      $$ = $4;
      validate_simple_identifier( $2);
      $$->set_name($2);
      if ($6 != nullptr) {
        $$->annotations_ = $6->annotations_;
        delete $6;
      }

      // make constants for all the enum values
      if (g_parse_mode == PROGRAM) {
        const std::vector<t_enum_value*>& enum_values = $$->get_constants();
        std::vector<t_enum_value*>::const_iterator c_iter;
        for (c_iter = enum_values.begin(); c_iter != enum_values.end(); ++c_iter) {
          std::string const_name = $$->get_name() + "." + (*c_iter)->get_name();
          t_const_value* const_val = new t_const_value((*c_iter)->get_value());
          const_val->set_enum($$);
          g_scope->add_constant(const_name, new t_const(g_type_i32, (*c_iter)->get_name(), const_val));
          if (g_parent_scope != nullptr) {
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
      y_enum_val = -1;
    }

EnumDef:
  CaptureDocText EnumValue TypeAnnotations CommaOrSemicolonOptional
    {
      pdebug("EnumDef -> EnumValue");
      $$ = $2;
      if ($1 != nullptr) {
        $$->set_doc($1);
      }
	  if ($3 != nullptr) {
        $$->annotations_ = $3->annotations_;
        delete $3;
      }
    }

EnumValue:
  tok_identifier '=' tok_int_constant
    {
      pdebug("EnumValue -> tok_identifier = tok_int_constant");
      if ($3 < INT32_MIN || $3 > INT32_MAX) {
        // Note: this used to be just a warning.  However, since thrift always
        // treats enums as i32 values, I'm changing it to a fatal error.
        // I doubt this will affect many people, but users who run into this
        // will have to update their thrift files to manually specify the
        // truncated i32 value that thrift has always been using anyway.
        failure("64-bit value supplied for enum %s will be truncated.", $1);
      }
      y_enum_val = static_cast<int32_t>($3);
      $$ = new t_enum_value($1, y_enum_val);
    }
 |
  tok_identifier
    {
      pdebug("EnumValue -> tok_identifier");
      validate_simple_identifier( $1);
      if (y_enum_val == INT32_MAX) {
        failure("enum value overflow at enum %s", $1);
      }
      ++y_enum_val;
      $$ = new t_enum_value($1, y_enum_val);
    }

Const:
  tok_const FieldType tok_identifier '=' ConstValue CommaOrSemicolonOptional
    {
      pdebug("Const -> tok_const FieldType tok_identifier = ConstValue");
      if (g_parse_mode == PROGRAM) {
        validate_simple_identifier( $3);
        g_scope->resolve_const_value($5, $2);
        $$ = new t_const($2, $3, $5);
        validate_const_type($$);

        g_scope->add_constant($3, $$);
        if (g_parent_scope != nullptr) {
          g_parent_scope->add_constant(g_parent_prefix + $3, $$);
        }
      } else {
        $$ = nullptr;
      }
    }

ConstValue:
  tok_int_constant
    {
      pdebug("ConstValue => tok_int_constant");
      $$ = new t_const_value();
      $$->set_integer($1);
      if (!g_allow_64bit_consts && ($1 < INT32_MIN || $1 > INT32_MAX)) {
        pwarning(1, "64-bit constant \"%" PRIi64"\" may not work in all languages.\n", $1);
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
      validate_simple_identifier( $2);
      $5->set_xsd_all($3);
      $5->set_union($1 == struct_is_union);
      $$ = $5;
      $$->set_name($2);
      if ($7 != nullptr) {
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
      $$ = nullptr;
    }

Xception:
  tok_xception tok_identifier '{' FieldList '}' TypeAnnotations
    {
      pdebug("Xception -> tok_xception tok_identifier { FieldList }");
      validate_simple_identifier( $2);
      $4->set_name($2);
      $4->set_xception(true);
      $$ = $4;
      if ($6 != nullptr) {
        $$->annotations_ = $6->annotations_;
        delete $6;
      }
    }

Service:
  tok_service tok_identifier Extends '{' FlagArgs FunctionList UnflagArgs '}' TypeAnnotations
    {
      pdebug("Service -> tok_service tok_identifier { FunctionList }");
      validate_simple_identifier( $2);
      $$ = $6;
      $$->set_name($2);
      $$->set_extends($3);
      if ($9 != nullptr) {
        $$->annotations_ = $9->annotations_;
        delete $9;
      }
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
      $$ = nullptr;
      if (g_parse_mode == PROGRAM) {
        $$ = g_scope->get_service($2);
        if ($$ == nullptr) {
          yyerror("Service \"%s\" has not been defined.", $2);
          exit(1);
        }
      }
    }
|
    {
      $$ = nullptr;
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
  CaptureDocText Oneway FunctionType tok_identifier '(' FieldList ')' Throws TypeAnnotations CommaOrSemicolonOptional
    {
      validate_simple_identifier( $4);
      $6->set_name(std::string($4) + "_args");
      $$ = new t_function($3, $4, $6, $8, $2);
      if ($1 != nullptr) {
        $$->set_doc($1);
      }
      if ($9 != nullptr) {
        $$->annotations_ = $9->annotations_;
        delete $9;
      }
    }

Oneway:
  tok_oneway
    {
      $$ = true;
    }
|  tok_async  // deprecated
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
        yyerror("\"%d: %s\" - field identifier/name has already been used", $2->get_key(), $2->get_name().c_str());
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
  CaptureDocText FieldIdentifier FieldRequiredness FieldType FieldReference FieldName FieldValue XsdOptional XsdNillable XsdAttributes TypeAnnotations CommaOrSemicolonOptional
    {
      pdebug("tok_int_constant : Field -> FieldType FieldName");
      if ($2.auto_assigned) {
        pwarning(1, "No field key specified for %s, resulting protocol may have conflicts or not be backwards compatible!\n", $6);
        if (g_strict >= 192) {
          yyerror("Implicit field keys are deprecated and not allowed with -strict");
          exit(1);
        }
      }
      validate_simple_identifier($6);
      $$ = new t_field($4, $6, $2.value);
      $$->set_reference($5);
      $$->set_req($3);
      if ($7 != nullptr) {
        g_scope->resolve_const_value($7, $4);
        validate_field_value($$, $7);
        $$->set_value($7);
      }
      $$->set_xsd_optional($8);
      $$->set_xsd_nillable($9);
      if ($1 != nullptr) {
        $$->set_doc($1);
      }
      if ($10 != nullptr) {
        $$->set_xsd_attrs($10);
      }
      if ($11 != nullptr) {
        $$->annotations_ = $11->annotations_;
        delete $11;
      }
    }

FieldName:  // identifiers and everything that could be one if it would not be identified as a different token already and excluding the "xsd*" keywords to follow a FieldName
  tok_identifier
    {
      pdebug("FieldName -> tok_identifier");
      $$ = $1;
    }
| tok_namespace
    {
      pdebug("FieldName -> tok_namespace");
      $$ = strdup("namespace");
    }
| tok_cpp_include
    {
      pdebug("FieldName -> tok_cpp_include");
      $$ = strdup("cpp_include");
    }
/* see THRIFT-5627 "More consistent syntax for cpp_type" -> activate when this issue is resolved
| tok_cpp_type
    {
      pdebug("FieldName -> tok_cpp_type");
      $$ = $strdup("cpp_type");
    }
*/
| tok_include
    {
      pdebug("FieldName -> tok_include");
      $$ = strdup("include");
    }
| tok_void
    {
      pdebug("FieldName -> tok_void");
      $$ = strdup("void");
    }
| tok_bool
    {
      pdebug("FieldName -> tok_bool");
      $$ = strdup("bool");
    }
| tok_byte
    {
      pdebug("FieldName -> tok_byte");
      $$ = strdup("byte");
    }
| tok_i8
    {
      pdebug("FieldName -> tok_i8");
      $$ = strdup("i8");
    }
| tok_i16
    {
      pdebug("FieldName -> tok_i16");
      $$ = strdup("i16");
    }
| tok_i32
    {
      pdebug("FieldName -> tok_i32");
      $$ = strdup("i32");
    }
| tok_i64
    {
      pdebug("FieldName -> tok_i64");
      $$ = strdup("i64");
    }
| tok_double
    {
      pdebug("FieldName -> tok_double");
      $$ = strdup("double");
    }
| tok_string
    {
      pdebug("FieldName -> tok_string");
      $$ = strdup("string");
    }
| tok_binary
    {
      pdebug("FieldName -> tok_binary");
      $$ = strdup("binary");
    }
| tok_uuid
    {
      pdebug("FieldName -> tok_uuid");
      $$ = strdup("uuid");
    }
| tok_map
    {
      pdebug("FieldName -> tok_map");
      $$ = strdup("map");
    }
| tok_list
    {
      pdebug("FieldName -> tok_list");
      $$ = strdup("list");
    }
| tok_set
    {
      pdebug("FieldName -> tok_set");
      $$ = strdup("set");
    }
| tok_oneway
    {
      pdebug("FieldName -> tok_oneway");
      $$ = strdup("oneway");
    }
| tok_async
    {
      pdebug("FieldName -> tok_async");
      $$ = strdup("async");
    }
| tok_typedef
    {
      pdebug("FieldName -> tok_typedef");
      $$ = strdup("typedef");
    }
| tok_struct
    {
      pdebug("FieldName -> tok_struct");
      $$ = strdup("struct");
    }
| tok_union
    {
      pdebug("FieldName -> tok_union");
      $$ = strdup("union");
    }
| tok_xception
    {
      pdebug("FieldName -> tok_xception");
      $$ = strdup("exception");
    }
| tok_extends
    {
      pdebug("FieldName -> tok_extends");
      $$ = strdup("extends");
    }
| tok_throws
    {
      pdebug("FieldName -> tok_throws");
      $$ = strdup("throws");
    }
| tok_service
    {
      pdebug("FieldName -> tok_service");
      $$ = strdup("service");
    }
| tok_enum
    {
      pdebug("FieldName -> tok_enum");
      $$ = strdup("enum");
    }
| tok_const
    {
      pdebug("FieldName -> tok_const");
      $$ = strdup("const");
    }
| tok_required
    {
      pdebug("FieldName -> tok_required");
      $$ = strdup("required");
    }
| tok_optional
    {
      pdebug("FieldName -> tok_optional");
      $$ = strdup("optional");
    }
  
  
FieldIdentifier:
  tok_int_constant ':'
    {
      if ($1 <= 0) {
        if (g_allow_neg_field_keys) {
          /*
           * g_allow_neg_field_keys exists to allow users to add explicitly
           * specified key values to old .thrift files without breaking
           * protocol compatibility.
           */
          if ($1 != y_field_val) {
            /*
             * warn if the user-specified negative value isn't what
             * thrift would have auto-assigned.
             */
            pwarning(1, "Nonpositive field key (%" PRIi64") differs from what would be "
                     "auto-assigned by thrift (%d).\n", $1, y_field_val);
          }
          /*
           * Leave $1 as-is, and update y_field_val to be one less than $1.
           * The FieldList parsing will catch any duplicate key values.
           */
          y_field_val = static_cast<int32_t>($1 - 1);
          $$.value = static_cast<int32_t>($1);
          $$.auto_assigned = false;
        } else {
          pwarning(1, "Nonpositive value (%d) not allowed as a field key.\n",
                   $1);
          $$.value = y_field_val--;
          $$.auto_assigned = true;
        }
      } else {
        $$.value = static_cast<int32_t>($1);
        $$.auto_assigned = false;
      }
      if( (SHRT_MIN > $$.value) || ($$.value > SHRT_MAX)) {
        pwarning(1, "Field key (%d) exceeds allowed range (%d..%d).\n",
                 $$.value, SHRT_MIN, SHRT_MAX);
      }
    }
|
    {
      $$.value = y_field_val--;
      $$.auto_assigned = true;
      if( (SHRT_MIN > $$.value) || ($$.value > SHRT_MAX)) {
        pwarning(1, "Field key (%d) exceeds allowed range (%d..%d).\n",
                 $$.value, SHRT_MIN, SHRT_MAX);
      }
    }

FieldReference:
  tok_reference
    {
      $$ = true;
    }
|
   {
     $$ = false;
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
        $$ = nullptr;
      }
    }
|
    {
      $$ = nullptr;
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
        $$ = nullptr;
      } else {
        // Lookup the identifier in the current scope
        $$ = g_scope->get_type($1);
        if ($$ == nullptr) {
          /*
           * Either this type isn't yet declared, or it's never
             declared.  Either way allow it and we'll figure it out
             during generation.
           */
          $$ = new t_typedef(g_program, $1, true);
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
      if ($2 != nullptr) {
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
| tok_uuid
    {
      pdebug("BaseType -> tok_uuid");
      $$ = g_type_uuid;
    }
| tok_bool
    {
      pdebug("BaseType -> tok_bool");
      $$ = g_type_bool;
    }
| tok_byte
    {
      pdebug("BaseType -> tok_byte");
      $$ = g_type_i8;  // byte is signed in Thrift, just an alias for i8
    }
| tok_i8
    {
      pdebug("BaseType -> tok_i8");
      $$ = g_type_i8;
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
      if ($2 != nullptr) {
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
      if ($2 != nullptr) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

SetType:
  tok_set CppType '<' FieldType '>'
    {
      pdebug("SetType -> tok_set<FieldType>");
      $$ = new t_set($4);
      if ($2 != nullptr) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
    }

ListType:
  tok_list CppType '<' FieldType '>' CppType   // the second CppType is for compatibility reasons = deprecated
    {
      pdebug("ListType -> tok_list<FieldType>");
      check_for_list_of_bytes($4);
      $$ = new t_list($4);
      if ($2 != nullptr) {
        ((t_container*)$$)->set_cpp_name(std::string($2));
      }
      if ($6 != nullptr) {
        ((t_container*)$$)->set_cpp_name(std::string($6));
        pwarning(1, "The syntax 'list<type> cpp_type \"c++ type\"' is deprecated. Use 'list cpp_type \"c++ type\" <type>' instead.\n");
      }
      if (($2 != nullptr) && ($6 != nullptr)) {
        pwarning(1, "Two cpp_types clauses at list<%>\n", $2);
      }
    }

CppType:
  tok_cpp_type tok_literal
    {
      $$ = $2;
    }
|
    {
      $$ = nullptr;
    }

TypeAnnotations:
  '(' TypeAnnotationList ')'
    {
      pdebug("TypeAnnotations -> ( TypeAnnotationList )");
      $$ = $2;
    }
|
    {
      $$ = nullptr;
    }

TypeAnnotationList:
  TypeAnnotationList TypeAnnotation
    {
      pdebug("TypeAnnotationList -> TypeAnnotationList , TypeAnnotation");
      $$ = $1;
      $$->annotations_[$2->key].push_back($2->val);
      delete $2;
    }
|
    {
      /* Just use a dummy structure to hold the annotations. */
      $$ = new t_struct(g_program);
    }

TypeAnnotation:
  tok_identifier TypeAnnotationValue CommaOrSemicolonOptional
    {
      pdebug("TypeAnnotation -> TypeAnnotationValue");
      $$ = new t_annotation;
      $$->key = $1;
      $$->val = $2;
    }

TypeAnnotationValue:
  '=' tok_literal
    {
      pdebug("TypeAnnotationValue -> = tok_literal");
      $$ = $2;
    }
|
    {
      pdebug("TypeAnnotationValue ->");
      $$ = strdup("1");
    }

%%
