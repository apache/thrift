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

-module(test_thrift_1151).

-include("gen-erl/thrift1151_types.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

unmatched_struct_test() ->
    S1 = #'StructC'{x = #'StructB'{x = 1}},
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    ?assertException(
        error,
        struct_unmatched,
        thrift_protocol:write(
            Protocol,
            {{struct, element(2, thrift1151_types:struct_info('StructC'))}, S1}
        )
    ).

badarg_test() ->
    S2 = #'StructC'{x = #'StructA'{x = "1"}},
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    ?assertException(
        error,
        badarg,
        thrift_protocol:write(
            Protocol,
            {{struct, element(2, thrift1151_types:struct_info('StructC'))}, S2}
        )
    ).

-endif.
