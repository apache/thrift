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

% don't rename this thrift_test, it clobbers generated files
-module(thrift_test_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("gen-erlang/thrift_test_thrift_test_types.hrl").

constant_test_() ->
  [
    {"myNumberz equals 1", ?_assertEqual(1, ?THRIFT_TEST_THRIFT_TEST_MYNUMBERZ)}
  ].

record_generation_test_() ->
  [
    {"Bonk record", ?_assertMatch(
      {'Bonk', _, _},
      #'Bonk'{message=null,type=null}
    )},
    {"Bools record", ?_assertMatch(
      {'Bools', _, _},
      #'Bools'{im_true=null,im_false=null}
    )},
    {"Xtruct record", ?_assertMatch(
      {'Xtruct', _, _, _, _},
      #'Xtruct'{string_thing=null,byte_thing=null,i32_thing=null,i64_thing=null}
    )},
    {"Xtruct2 record", ?_assertMatch(
      {'Xtruct2', _, _, _},
      #'Xtruct2'{byte_thing=null,struct_thing=null,i32_thing=null}
    )},
    {"Xtruct3 record", ?_assertMatch(
      {'Xtruct3', _, _, _, _},
      #'Xtruct3'{string_thing=null,changed=null,i32_thing=null,i64_thing=null}
    )},
    {"Insanity record", ?_assertMatch(
      {'Insanity', _, _},
      #'Insanity'{userMap=null,xtructs=null}
    )},
    {"CrazyNesting record", ?_assertMatch(
      {'CrazyNesting', _, _, _, _},
      #'CrazyNesting'{
        string_field=null,
        set_field=null,
        list_field=null,
        binary_field=null
      }
    )},
    {"Xception record", ?_assertMatch(
      {'Xception', _, _},
      #'Xception'{errorCode=null,message=null}
    )},
    {"Xception2 record", ?_assertMatch(
      {'Xception2', _, _},
      #'Xception2'{errorCode=null,struct_thing=null}
    )},
    {"EmptyStruct record", ?_assertMatch({'EmptyStruct'}, #'EmptyStruct'{})},
    {"OneField record", ?_assertMatch({'OneField', _}, #'OneField'{field=null})},
    {"VersioningTestV1 record", ?_assertMatch(
      {'VersioningTestV1', _, _, _},
      #'VersioningTestV1'{begin_in_both=null,old_string=null,end_in_both=null}
    )},
    {"VersioningTestV2 record", ?_assertMatch(
      {'VersioningTestV2', _, _, _, _, _, _, _, _, _, _, _, _},
      #'VersioningTestV2'{
        begin_in_both=null,
        newint=null,
        newbyte=null,
        newshort=null,
        newlong=null,
        newdouble=null,
        newstruct=null,
        newlist=null,
        newset=null,
        newmap=null,
        newstring=null,
        end_in_both=null
      }
    )},
    {"ListTypeVersioningV1 record", ?_assertMatch(
      {'ListTypeVersioningV1', _, _},
      #'ListTypeVersioningV1'{myints=null,hello=null}
    )},
    {"ListTypeVersioningV2 record", ?_assertMatch(
      {'ListTypeVersioningV2', _, _},
      #'ListTypeVersioningV2'{strings=null,hello=null}
    )},
    {"GuessProtocolStruct record", ?_assertMatch(
      {'GuessProtocolStruct', _},
      #'GuessProtocolStruct'{map_field=null}
    )},
    {"LargeDeltas record", ?_assertMatch(
      {'LargeDeltas', _, _, _, _, _, _, _, _, _, _},
      #'LargeDeltas'{
        b1=null,
        b10=null,
        b100=null,
        check_true=null,
        b1000=null,
        check_false=null,
        vertwo2000=null,
        a_set2500=null,
        vertwo3000=null,
        big_numbers=null
      }
    )},
    {"NestedListsI32x2 record", ?_assertMatch(
      {'NestedListsI32x2', _},
      #'NestedListsI32x2'{integerlist=null}
    )},
    {"NestedListsI32x3 record", ?_assertMatch(
      {'NestedListsI32x3', _},
      #'NestedListsI32x3'{integerlist=null}
    )},
    {"NestedMixedx2 record", ?_assertMatch(
      {'NestedMixedx2', _, _, _},
      #'NestedMixedx2'{
        int_set_list=null,
        map_int_strset=null,
        map_int_strset_list=null
      }
    )},
    {"ListBonks record", ?_assertMatch({'ListBonks', _}, #'ListBonks'{bonk=null})},
    {"NestedListsBonk record", ?_assertMatch(
      {'NestedListsBonk', _},
      #'NestedListsBonk'{bonk=null}
    )},
    {"BoolTest record", ?_assertMatch(
      {'BoolTest', _, _},
      #'BoolTest'{b=null,s=null}
    )},
    {"StructA record", ?_assertMatch({'StructA', _}, #'StructA'{s=null})},
    {"StructB record", ?_assertMatch(
      {'StructB', _, _},
      #'StructB'{aa=null,ab=null}
    )}
  ].

struct_info_test_() ->
  [
    {"Bonk definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, string, message, undefined},
        {2, undefined, i32, type, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('Bonk')
    )},
    {"Bools definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, bool, im_true, undefined},
        {2, undefined, bool, im_false, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('Bools')
    )},
    {"Xtruct definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, string, string_thing, undefined},
        {4, undefined, byte, byte_thing, undefined},
        {9, undefined, i32, i32_thing, undefined},
        {11, undefined, i64, i64_thing, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('Xtruct')
    )},
    {"Xtruct2 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, byte, byte_thing, undefined},
        {2, undefined, {struct, struct, {'thrift_test_thrift_test_types', 'Xtruct'}}, struct_thing, #'Xtruct'{}},
        {3, undefined, i32, i32_thing, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('Xtruct2')
    )},
    {"Xtruct3 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, string, string_thing, undefined},
        {4, undefined, i32, changed, undefined},
        {9, undefined, i32, i32_thing, undefined},
        {11, undefined, i64, i64_thing, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('Xtruct3')
    )},
    {"Insanity definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {map, {enum, {'thrift_test_thrift_test_types', 'Numberz'}}, i64}, userMap, #{}},
        {2, undefined, {list, {struct, struct, {'thrift_test_thrift_test_types', 'Xtruct'}}}, xtructs, []}
      ]},
      thrift_test_thrift_test_types:struct_info('Insanity')
    )},
    {"CrazyNesting definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, string, string_field, undefined},
        {2, optional, {set, {struct, struct, {'thrift_test_thrift_test_types', 'Insanity'}}}, set_field, ordsets:new()},
        {3, required, {list, {map,
          {set, i32},
          {map, i32, {set, {list, {map, {struct, struct, {'thrift_test_thrift_test_types', 'Insanity'}}, string}}}}
        }}, list_field, []},
        {4, undefined, string, binary_field, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('CrazyNesting')
    )},
    {"Xception definition", ?_assertEqual(
      {struct, exception, [
        {1, undefined, i32, errorCode, undefined},
        {2, undefined, string, message, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('Xception')
    )},
    {"Xception2 definition", ?_assertEqual(
      {struct, exception, [
        {1, undefined, i32, errorCode, undefined},
        {2, undefined, {struct, struct, {'thrift_test_thrift_test_types', 'Xtruct'}}, struct_thing, #'Xtruct'{}}
      ]},
      thrift_test_thrift_test_types:struct_info('Xception2')
    )},
    {"EmptyStruct definition", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:struct_info('EmptyStruct')
    )},
    {"OneField definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, struct, {'thrift_test_thrift_test_types', 'EmptyStruct'}}, field, #'EmptyStruct'{}}
      ]},
      thrift_test_thrift_test_types:struct_info('OneField')
    )},
    {"VersioningTestV1 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, i32, begin_in_both, undefined},
        {3, undefined, string, old_string, undefined},
        {12, undefined, i32, end_in_both, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('VersioningTestV1')
    )},
    {"VersioningTestV2 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, i32, begin_in_both, undefined},
        {2, undefined, i32, newint, undefined},
        {3, undefined, byte, newbyte, undefined},
        {4, undefined, i16, newshort, undefined},
        {5, undefined, i64, newlong, undefined},
        {6, undefined, double, newdouble, undefined},
        {7, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Bonk'}}, newstruct, #'Bonk'{}},
        {8, undefined, {list, i32}, newlist, []},
        {9, undefined, {set, i32}, newset, ordsets:new()},
        {10, undefined, {map, i32, i32}, newmap, #{}},
        {11, undefined, string, newstring, undefined},
        {12, undefined, i32, end_in_both, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('VersioningTestV2')
    )},
    {"ListTypeVersioningV1 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, i32}, myints, []},
        {2, undefined, string, hello, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('ListTypeVersioningV1')
    )},
    {"ListTypeVersioningV2 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, string}, strings, []},
        {2, undefined, string, hello, undefined}
      ]},
      thrift_test_thrift_test_types:struct_info('ListTypeVersioningV2')
    )},
    {"GuessProtocolStruct definition", ?_assertEqual(
      {struct, struct, [
        {7, undefined, {map, string, string}, map_field, #{}}
      ]},
      thrift_test_thrift_test_types:struct_info('GuessProtocolStruct')
    )},
    {"LargeDeltas definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Bools'}}, b1, #'Bools'{}},
        {10, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Bools'}}, b10, #'Bools'{}},
        {100, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Bools'}}, b100, #'Bools'{}},
        {500, undefined, bool, check_true, undefined},
        {1000, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Bools'}}, b1000, #'Bools'{}},
        {1500, undefined, bool, check_false, undefined},
        {2000, undefined, {struct, struct, {thrift_test_thrift_test_types, 'VersioningTestV2'}}, vertwo2000, #'VersioningTestV2'{}},
        {2500, undefined, {set, string}, a_set2500, ordsets:new()},
        {3000, undefined, {struct, struct, {thrift_test_thrift_test_types, 'VersioningTestV2'}}, vertwo3000, #'VersioningTestV2'{}},
        {4000, undefined, {list, i32}, big_numbers, []}
      ]},
      thrift_test_thrift_test_types:struct_info('LargeDeltas')
    )},
    {"NestedListsI32x2 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, {list, i32}}, integerlist, []}
      ]},
      thrift_test_thrift_test_types:struct_info('NestedListsI32x2')
    )},
    {"NestedListsI32x3 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, {list, {list, i32}}}, integerlist, []}
      ]},
      thrift_test_thrift_test_types:struct_info('NestedListsI32x3')
    )},
    {"NestedMixedx2 definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, {set, i32}}, int_set_list, []},
        {2, undefined, {map, i32, {set, string}}, map_int_strset, #{}},
        {3, undefined, {list, {map, i32, {set, string}}}, map_int_strset_list, []}
      ]},
      thrift_test_thrift_test_types:struct_info('NestedMixedx2')
    )},
    {"ListBonks definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, {struct, struct, {thrift_test_thrift_test_types, 'Bonk'}}}, bonk, []}
      ]},
      thrift_test_thrift_test_types:struct_info('ListBonks')
    )},
    {"NestedListsBonk definition", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, {list, {list, {struct, struct, {thrift_test_thrift_test_types, 'Bonk'}}}}}, bonk, []}
      ]},
      thrift_test_thrift_test_types:struct_info('NestedListsBonk')
    )},
    {"BoolTest definition", ?_assertEqual(
      {struct, struct, [
        {1, optional, bool, b, true},
        {2, optional, string, s, "true"}
      ]},
      thrift_test_thrift_test_types:struct_info('BoolTest')
    )},
    {"StructA definition", ?_assertEqual(
      {struct, struct, [{1, required, string, s, undefined}]},
      thrift_test_thrift_test_types:struct_info('StructA')
    )},
    {"StructB definition", ?_assertEqual(
      {struct, struct, [
        {1, optional, {struct, struct, {thrift_test_thrift_test_types, 'StructA'}}, aa, #'StructA'{}},
        {2, required, {struct, struct, {thrift_test_thrift_test_types, 'StructA'}}, ab, #'StructA'{}}
      ]},
      thrift_test_thrift_test_types:struct_info('StructB')
    )}
  ].

service_info_test_() ->
  [
    {"testVoid params", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testVoid, params_type)
    )},
    {"testVoid reply", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testVoid, reply_type)
    )},
    {"testVoid exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testVoid, exceptions)
    )},
    {"testString params", ?_assertEqual(
      {struct, struct, [{1, undefined, string, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testString, params_type)
    )},
    {"testString reply", ?_assertEqual(
      string,
      thrift_test_thrift_test_types:function_info('ThriftTest', testString, reply_type)
    )},
    {"testString exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testString, exceptions)
    )},
    {"testByte params", ?_assertEqual(
      {struct, struct, [{1, undefined, byte, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testByte, params_type)
    )},
    {"testByte reply", ?_assertEqual(
      byte,
      thrift_test_thrift_test_types:function_info('ThriftTest', testByte, reply_type)
    )},
    {"testByte exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testByte, exceptions)
    )},
    {"testI32 params", ?_assertEqual(
      {struct, struct, [{1, undefined, i32, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testI32, params_type)
    )},
    {"testI32 reply", ?_assertEqual(
      i32,
      thrift_test_thrift_test_types:function_info('ThriftTest', testI32, reply_type)
    )},
    {"testI32 exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testI32, exceptions)
    )},
    {"testI64 params", ?_assertEqual(
      {struct, struct, [{1, undefined, i64, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testI64, params_type)
    )},
    {"testI64 reply", ?_assertEqual(
      i64,
      thrift_test_thrift_test_types:function_info('ThriftTest', testI64, reply_type)
    )},
    {"testI64 exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testI64, exceptions)
    )},
    {"testDouble params", ?_assertEqual(
      {struct, struct, [{1, undefined, double, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testDouble, params_type)
    )},
    {"testDouble reply", ?_assertEqual(
      double,
      thrift_test_thrift_test_types:function_info('ThriftTest', testDouble, reply_type)
    )},
    {"testDouble exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testDouble, exceptions)
    )},
    {"testStruct params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Xtruct'}}, 'thing', #'Xtruct'{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testStruct, params_type)
    )},
    {"testStruct reply", ?_assertEqual(
      {struct, struct, {thrift_test_thrift_test_types, 'Xtruct'}},
      thrift_test_thrift_test_types:function_info('ThriftTest', testStruct, reply_type)
    )},
    {"testStruct exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testStruct, exceptions)
    )},
    {"testNest params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Xtruct2'}}, 'thing', #'Xtruct2'{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testNest, params_type)
    )},
    {"testNest reply", ?_assertEqual(
      {struct, struct, {thrift_test_thrift_test_types, 'Xtruct2'}},
      thrift_test_thrift_test_types:function_info('ThriftTest', testNest, reply_type)
    )},
    {"testNest exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testNest, exceptions)
    )},
    {"testMap params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {map, i32, i32}, 'thing', #{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMap, params_type)
    )},
    {"testMap reply", ?_assertEqual(
      {map, i32, i32},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMap, reply_type)
    )},
    {"testMap exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMap, exceptions)
    )},
    {"testStringMap params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {map, string, string}, 'thing', #{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testStringMap, params_type)
    )},
    {"testStringMap reply", ?_assertEqual(
      {map, string, string},
      thrift_test_thrift_test_types:function_info('ThriftTest', testStringMap, reply_type)
    )},
    {"testStringMap exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testStringMap, exceptions)
    )},
    {"testSet params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {set, i32}, 'thing', ordsets:new()}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testSet, params_type)
    )},
    {"testSet reply", ?_assertEqual(
      {set, i32},
      thrift_test_thrift_test_types:function_info('ThriftTest', testSet, reply_type)
    )},
    {"testSet exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testSet, exceptions)
    )},
    {"testList params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {list, i32}, 'thing', []}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testList, params_type)
    )},
    {"testList reply", ?_assertEqual(
      {list, i32},
      thrift_test_thrift_test_types:function_info('ThriftTest', testList, reply_type)
    )},
    {"testList exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testList, exceptions)
    )},
    {"testEnum params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {enum, {thrift_test_thrift_test_types, 'Numberz'}}, 'thing', undefined}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testEnum, params_type)
    )},
    {"testEnum reply", ?_assertEqual(
      {enum, {thrift_test_thrift_test_types, 'Numberz'}},
      thrift_test_thrift_test_types:function_info('ThriftTest', testEnum, reply_type)
    )},
    {"testEnum exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testEnum, exceptions)
    )},
    {"testTypedef params", ?_assertEqual(
      {struct, struct, [{1, undefined, i64, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testTypedef, params_type)
    )},
    {"testTypedef reply", ?_assertEqual(
      i64,
      thrift_test_thrift_test_types:function_info('ThriftTest', testTypedef, reply_type)
    )},
    {"testTypedef exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testTypedef, exceptions)
    )},
    {"testMapMap params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, i32, 'hello', undefined}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMapMap, params_type)
    )},
    {"testMapMap reply", ?_assertEqual(
      {map, i32, {map, i32,i32}},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMapMap, reply_type)
    )},
    {"testMapMap exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMapMap, exceptions)
    )},
    {"testInsanity params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, struct, {thrift_test_thrift_test_types, 'Insanity'}}, 'argument', #'Insanity'{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testInsanity, params_type)
    )},
    {"testInsanity reply", ?_assertEqual(
      {map, i64, {map,
        {enum, {thrift_test_thrift_test_types, 'Numberz'}},
        {struct, struct, {'thrift_test_thrift_test_types', 'Insanity'}}
      }},
      thrift_test_thrift_test_types:function_info('ThriftTest', testInsanity, reply_type)
    )},
    {"testInsanity exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testInsanity, exceptions)
    )},
    {"testMulti params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, byte, 'arg0', undefined},
        {2, undefined, i32, 'arg1', undefined},
        {3, undefined, i64, 'arg2', undefined},
        {4, undefined, {map, i16, string}, 'arg3', #{}},
        {5, undefined, {enum, {thrift_test_thrift_test_types, 'Numberz'}}, 'arg4', undefined},
        {6, undefined, i64, 'arg5', undefined}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMulti, params_type)
    )},
    {"testMulti reply", ?_assertEqual(
      {struct, struct, {thrift_test_thrift_test_types, 'Xtruct'}},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMulti, reply_type)
    )},
    {"testMulti exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMulti, exceptions)
    )},
    {"testException params", ?_assertEqual(
      {struct, struct, [{1, undefined, string, 'arg', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testException, params_type)
    )},
    {"testException reply", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testException, reply_type)
    )},
    {"testException exceptions", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, exception, {thrift_test_thrift_test_types, 'Xception'}}, 'err1', #'Xception'{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testException, exceptions)
    )},
    {"testMultiException params", ?_assertEqual(
      {struct, struct, [
        {1, undefined, string, 'arg0', undefined},
        {2, undefined, string, 'arg1', undefined}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMultiException, params_type)
    )},
    {"testMultiException reply", ?_assertEqual(
      {struct, struct, {thrift_test_thrift_test_types, 'Xtruct'}},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMultiException, reply_type)
    )},
    {"testMultiException exceptions", ?_assertEqual(
      {struct, struct, [
        {1, undefined, {struct, exception, {thrift_test_thrift_test_types, 'Xception'}}, 'err1', #'Xception'{}},
        {2, undefined, {struct, exception, {thrift_test_thrift_test_types, 'Xception2'}}, 'err2', #'Xception2'{}}
      ]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testMultiException, exceptions)
    )},
    {"testOneway params", ?_assertEqual(
      {struct, struct, [{1, undefined, i32, 'secondsToSleep', undefined}]},
      thrift_test_thrift_test_types:function_info('ThriftTest', testOneway, params_type)
    )},
    {"testOneway reply", ?_assertEqual(
      oneway_void,
      thrift_test_thrift_test_types:function_info('ThriftTest', testOneway, reply_type)
    )},
    {"testOneway exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('ThriftTest', testOneway, exceptions)
    )},
    {"blahBlah params", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('SecondService', blahBlah, params_type)
    )},
    {"blahBlah reply", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('SecondService', blahBlah, reply_type)
    )},
    {"blahBlah exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('SecondService', blahBlah, exceptions)
    )},
    {"secondtestString params", ?_assertEqual(
      {struct, struct, [{1, undefined, string, 'thing', undefined}]},
      thrift_test_thrift_test_types:function_info('SecondService', secondtestString, params_type)
    )},
    {"secondtestString reply", ?_assertEqual(
      string,
      thrift_test_thrift_test_types:function_info('SecondService', secondtestString, reply_type)
    )},
    {"secondtestString exceptions", ?_assertEqual(
      {struct, struct, []},
      thrift_test_thrift_test_types:function_info('SecondService', secondtestString, exceptions)
    )}
  ].
