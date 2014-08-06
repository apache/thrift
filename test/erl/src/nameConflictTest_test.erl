%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(nameConflictTest_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("nameConflictTest_constants.hrl").

record_generation_test_() ->
  [
    {"using record", ?_assertMatch(
      {using, _, _},
      #using{single=null,integer=null}
    )},
    {"delegate record", ?_assertMatch(
      {delegate, _, _},
      #delegate{partial=null,delegate=null}
    )},
    {"get record", ?_assertMatch(
      {get, _},
      #get{sbyte=null}
    )},
    {"partial record", ?_assertMatch(
      {partial, _},
      #partial{using=null}
    )},
    {"ClassAndProp record", ?_assertMatch(
      {classAndProp, _, _, _, _},
      #classAndProp{
        'ClassAndProp'=null,
        'ClassAndProp_'=null,
        'ClassAndProp__'=null,
        'ClassAndProper'=null
      }
    )},
    {"second_chance record", ?_assertMatch(
      {second_chance, _, _, _, _},
      #second_chance{
        'SECOND_CHANCE'=null,
        'SECOND_CHANCE_'=null,
        'SECOND_CHANCE__'=null,
        'SECOND_CHANCES'=null
      }
    )},
    {"NOW_EAT_THIS record", ?_assertMatch(
      {nOW_EAT_THIS, _, _, _, _},
      #nOW_EAT_THIS{
        now_eat_this=null,
        now_eat_this_=null,
        now_eat_this__=null,
        now_eat_this_and_this=null
      }
    )},
    {"TheEdgeCase record", ?_assertMatch(
      {theEdgeCase, _, _, _, _, _, _},
      #theEdgeCase{
        theEdgeCase=null,
        theEdgeCase_=null,
        theEdgeCase__=null,
        'TheEdgeCase'=null,
        'TheEdgeCase_'=null,
        'TheEdgeCase__'=null
      }
    )},
    {"Tricky_ record", ?_assertMatch(
      {tricky_, _, _},
      #tricky_{tricky=null,'Tricky'=null}
    )},
    {"Nested record", ?_assertMatch(
      {nested, _, _, _, _, _, _},
      #nested{
        'ClassAndProp'=null,
        second_chance=null,
        'NOW_EAT_THIS'=null,
        'TheEdgeCase'=null,
        'Tricky_'=null,
        'Nested'=null
      }
    )},
    {"Problem_ record", ?_assertMatch(
      {problem_, _, _},
      #problem_{problem=null,'Problem'=null}
    )}
  ].

struct_info_test_() ->
  [
    {"using definition", ?_assertEqual(
      {struct, [{1, double},{2, double}]},
      nameConflictTest_types:struct_info(using)
    )},
    {"delegate definition", ?_assertEqual(
      {struct, [
        {1, string},
        {2, {struct, {nameConflictTest_types, delegate}}}
      ]},
      nameConflictTest_types:struct_info(delegate)
    )},
    {"get definition", ?_assertEqual(
      {struct, [{1, bool}]},
      nameConflictTest_types:struct_info(get)
    )},
    {"partial definition", ?_assertEqual(
      {struct, [{1, {struct, {nameConflictTest_types, using}}}]},
      nameConflictTest_types:struct_info(partial)
    )},
    {"ClassAndProp definition", ?_assertEqual(
      {struct, [{1, bool},{2, bool},{3, bool},{4, bool}]},
      nameConflictTest_types:struct_info(classAndProp)
    )},
    {"second_chance definition", ?_assertEqual(
      {struct, [{1, bool},{2, bool},{3, bool},{4, bool}]},
      nameConflictTest_types:struct_info(second_chance)
    )},
    {"NOW_EAT_THIS definition", ?_assertEqual(
      {struct, [{1, bool},{2, bool},{3, bool},{4, bool}]},
      nameConflictTest_types:struct_info(nOW_EAT_THIS)
    )},
    {"TheEdgeCase definition", ?_assertEqual(
      {struct, [{1, bool},{2, bool},{3, bool},{4, bool},{5, bool},{6, bool}]},
      nameConflictTest_types:struct_info(theEdgeCase)
    )},
    {"Tricky_ definition", ?_assertEqual(
      {struct, [{1, bool},{2, bool}]},
      nameConflictTest_types:struct_info(tricky_)
    )},
    {"Nested definition", ?_assertEqual(
      {struct, [
        {1, {struct, {nameConflictTest_types, classAndProp}}},
        {2, {struct, {nameConflictTest_types, second_chance}}},
        {3, {struct, {nameConflictTest_types, nOW_EAT_THIS}}},
        {4, {struct, {nameConflictTest_types, theEdgeCase}}},
        {5, {struct, {nameConflictTest_types, tricky_}}},
        {6, {struct, {nameConflictTest_types, nested}}}
      ]},
      nameConflictTest_types:struct_info(nested)
    )},
    {"Problem_ definition", ?_assertEqual(
      {struct, [{1, bool},{2, bool}]},
      nameConflictTest_types:struct_info(problem_)
    )},
    {"using extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, double, single, undefined},
        {2, undefined, double, integer, undefined}
      ]},
      nameConflictTest_types:struct_info_ext(using)
    )},
    {"delegate extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, string, partial, undefined},
        {2, undefined, {struct, {nameConflictTest_types, delegate}}, delegate, undefined}
      ]},
      nameConflictTest_types:struct_info_ext(delegate)
    )},
    {"get extended definition", ?_assertEqual(
      {struct, [{1, undefined, bool, sbyte, undefined}]},
      nameConflictTest_types:struct_info_ext(get)
    )},
    {"partial extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, {struct, {nameConflictTest_types, using}}, using, #using{}}
      ]},
      nameConflictTest_types:struct_info_ext(partial)
    )},
    {"ClassAndProp extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, bool, 'ClassAndProp', undefined},
        {2, undefined, bool, 'ClassAndProp_', undefined},
        {3, undefined, bool, 'ClassAndProp__', undefined},
        {4, undefined, bool, 'ClassAndProper', undefined}
      ]},
      nameConflictTest_types:struct_info_ext(classAndProp)
    )},
    {"second_chance extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, bool, 'SECOND_CHANCE', undefined},
        {2, undefined, bool, 'SECOND_CHANCE_', undefined},
        {3, undefined, bool, 'SECOND_CHANCE__', undefined},
        {4, undefined, bool, 'SECOND_CHANCES', undefined}
      ]},
      nameConflictTest_types:struct_info_ext(second_chance)
    )},
    {"NOW_EAT_THIS extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, bool, now_eat_this, undefined},
        {2, undefined, bool, now_eat_this_, undefined},
        {3, undefined, bool, now_eat_this__, undefined},
        {4, undefined, bool, now_eat_this_and_this, undefined}
      ]},
      nameConflictTest_types:struct_info_ext(nOW_EAT_THIS)
    )},
    {"TheEdgeCase extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, bool, theEdgeCase, undefined},
        {2, undefined, bool, theEdgeCase_, undefined},
        {3, undefined, bool, theEdgeCase__, undefined},
        {4, undefined, bool, 'TheEdgeCase', undefined},
        {5, undefined, bool, 'TheEdgeCase_', undefined},
        {6, undefined, bool, 'TheEdgeCase__', undefined}
      ]},
      nameConflictTest_types:struct_info_ext(theEdgeCase)
    )},
    {"Tricky_ extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, bool, tricky, undefined},
        {2, undefined, bool, 'Tricky', undefined}
      ]},
      nameConflictTest_types:struct_info_ext(tricky_)
    )},
    {"Nested extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, {struct, {
          nameConflictTest_types,
          classAndProp
        }}, 'ClassAndProp', #classAndProp{}},
        {2, undefined, {struct, {
          nameConflictTest_types,
          second_chance
        }}, second_chance, #second_chance{}},
        {3, undefined, {struct, {
          nameConflictTest_types,
          nOW_EAT_THIS
        }}, 'NOW_EAT_THIS', #nOW_EAT_THIS{}},
        {4, undefined, {struct, {
          nameConflictTest_types,
          theEdgeCase
        }}, 'TheEdgeCase', #theEdgeCase{}},
        {5, undefined, {struct, {
          nameConflictTest_types,
          tricky_
        }}, 'Tricky_', #tricky_{}},
        {6, undefined, {struct, {
          nameConflictTest_types,
          nested
        }}, 'Nested', undefined}
      ]},
      nameConflictTest_types:struct_info_ext(nested)
    )},
    {"Problem_ extended definition", ?_assertEqual(
      {struct, [
        {1, undefined, bool, problem, undefined},
        {2, undefined, bool, 'Problem', undefined}
      ]},
      nameConflictTest_types:struct_info_ext(problem_)
    )}
  ].

service_info_test_() ->
  [
    {"event params", ?_assertEqual(
      {struct, [{1, {struct, {nameConflictTest_types, partial}}}]},
      extern_thrift:function_info(event, params_type)
    )},
    {"event reply", ?_assertEqual(
      {struct, {nameConflictTest_types, delegate}},
      extern_thrift:function_info(event, reply_type)
    )},
    {"event exceptions", ?_assertEqual(
      {struct, []},
      extern_thrift:function_info(event, exceptions)
    )},
    {"Foo params", ?_assertEqual(
      {struct, [{1, {struct, {nameConflictTest_types, nested}}}]},
      extern_thrift:function_info('Foo', params_type)
    )},
    {"Foo reply", ?_assertEqual(
      {struct, []},
      extern_thrift:function_info('Foo', reply_type)
    )},
    {"Foo exceptions", ?_assertEqual(
      {struct, [{1, {struct, {nameConflictTest_types, problem_}}}]},
      extern_thrift:function_info('Foo', exceptions)
    )}
  ].