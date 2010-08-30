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
-export([t/0]).

-include("thriftTest_types.hrl").

test_data() ->
    #xtruct{string_thing = <<"foobar">>,
	    byte_thing = 123,
	    i32_thing = 1234567,
	    i64_thing = 12345678900}.

t1() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    TestData = test_data(),
    ok = thrift_protocol:write(Protocol,
			       {{struct, element(2, thriftTest_types:struct_info('xtruct'))},
				TestData}),
    {ok, Result} = thrift_protocol:read(Protocol,
					{struct, element(2, thriftTest_types:struct_info('xtruct'))},
					'xtruct'),

    Result = TestData.


t2() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    TestData = test_data(),
    ok = thrift_protocol:write(Protocol,
			       {{struct, element(2, thriftTest_types:struct_info('xtruct'))},
				TestData}),
    {ok, Result} = thrift_protocol:read(Protocol,
					{struct, element(2, thriftTest_types:struct_info('xtruct3'))},
					'xtruct3'),

    Result = #xtruct3{string_thing = TestData#xtruct.string_thing,
                      changed = undefined,
                      i32_thing = TestData#xtruct.i32_thing,
                      i64_thing = TestData#xtruct.i64_thing}.


t3() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    TestData = #bools{im_true = true, im_false = false},
    ok = thrift_protocol:write(Protocol,
			       {{struct, element(2, thriftTest_types:struct_info('bools'))},
				TestData}),
    {ok, Result} = thrift_protocol:read(Protocol,
					{struct, element(2, thriftTest_types:struct_info('bools'))},
					'bools'),

    true = TestData#bools.im_true  =:= Result#bools.im_true,
    true = TestData#bools.im_false =:= Result#bools.im_false.


t() ->
    t1(),
    t2(),
    t3().

