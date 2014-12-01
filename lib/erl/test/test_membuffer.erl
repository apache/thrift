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

-module(test_membuffer).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("gen-erl/thrift_test_types.hrl").

test_data() ->
  #'Xtruct'{
    string_thing = <<"foobar">>,
    byte_thing = 123,
    i32_thing = 1234567,
    i64_thing = 12345678900
  }.

encode_decode_1_test() ->
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol0} = thrift_binary_protocol:new(Transport),
  TestData = test_data(),
  {Protocol1, ok} = thrift_protocol:write(Protocol0,
    {{struct, element(2, thrift_test_types:struct_info('Xtruct'))},
      TestData}),
  {_Protocol2, {ok, Result}} = thrift_protocol:read(Protocol1,
    {struct, element(2, thrift_test_types:struct_info('Xtruct'))},
    'Xtruct'),
  Result = TestData.

encode_decode_2_test() ->
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol0} = thrift_binary_protocol:new(Transport),
  TestData = test_data(),
  {Protocol1, ok} = thrift_protocol:write(Protocol0,
    {{struct, element(2, thrift_test_types:struct_info('Xtruct'))},
      TestData}),
  {_Protocol2, {ok, Result}} = thrift_protocol:read(Protocol1,
    {struct, element(2, thrift_test_types:struct_info('Xtruct3'))},
    'Xtruct3'),

  Result = #'Xtruct3'{string_thing = TestData#'Xtruct'.string_thing,
    changed = undefined,
    i32_thing = TestData#'Xtruct'.i32_thing,
    i64_thing = TestData#'Xtruct'.i64_thing}.


encode_decode_3_test() ->
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol0} = thrift_binary_protocol:new(Transport),
  TestData = #'Bools'{im_true = true, im_false = false},
  {Protocol1, ok} = thrift_protocol:write(Protocol0,
    {{struct, element(2, thrift_test_types:struct_info('Bools'))},
      TestData}),
  {_Protocol2, {ok, Result}} = thrift_protocol:read(Protocol1,
    {struct, element(2, thrift_test_types:struct_info('Bools'))},
    'Bools'),

  true = TestData#'Bools'.im_true  =:= Result#'Bools'.im_true,
  true = TestData#'Bools'.im_false =:= Result#'Bools'.im_false.


encode_decode_4_test() ->
  {ok, Transport} = thrift_memory_buffer:new(),
  {ok, Protocol0} = thrift_binary_protocol:new(Transport),
  TestData = #'Insanity'{xtructs=[]},
  {Protocol1, ok} = thrift_protocol:write(Protocol0,
    {{struct, element(2, thrift_test_types:struct_info('Insanity'))},
      TestData}),
  {_Protocol2, {ok, Result}} = thrift_protocol:read(Protocol1,
    {struct, element(2, thrift_test_types:struct_info('Insanity'))},
    'Insanity'),

  TestData = Result.

encode_decode_5_test() ->
  % test writing to a buffer, getting the bytes out, putting them
  % in a new buffer and reading them

  % here's the writing part
  {ok, Transport0} = thrift_memory_buffer:new(),
  {ok, Protocol0} = thrift_binary_protocol:new(Transport0),
  TestData = test_data(),
  {Protocol1, ok} = thrift_protocol:write(Protocol0,
    {{struct, element(2, thrift_test_types:struct_info('Xtruct'))},
      TestData}),
  % flush now returns the buffer
  {_Protocol2, Buf} = thrift_protocol:flush_transport(Protocol1),

  % now the reading part
  {ok, T2} = thrift_memory_buffer:new (Buf),
  {ok, P2} = thrift_binary_protocol:new(T2),
  {_, {ok, Result}} = thrift_protocol:read(P2,
    {struct, element(2, thrift_test_types:struct_info('Xtruct'))},
    'Xtruct'),

  Result = TestData.

-endif.
