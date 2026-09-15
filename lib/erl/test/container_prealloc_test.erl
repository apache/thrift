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

-module(container_prealloc_test).
-include_lib("eunit/include/eunit.hrl").

%% A container carries its element count in its header, read before any element,
%% so the decoder reads elements incrementally rather than materialising a driver
%% list from the wire count. A container larger than any internal cap must still
%% round-trip in full: the incremental read bounds only the up-front cost, not the
%% number of elements read.

list_larger_than_prealloc_cap_round_trips_test() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, P0} = thrift_binary_protocol:new(Transport),
    List = lists:seq(1, 3000),
    {P1, ok} = thrift_protocol:write(P0, {{list, i32}, List}),
    {_P2, {ok, ReadList}} = thrift_protocol:read(P1, {list, i32}),
    ?assertEqual(3000, length(ReadList)),
    ?assertEqual(List, ReadList).

set_larger_than_prealloc_cap_round_trips_test() ->
    {ok, Transport} = thrift_memory_buffer:new(),
    {ok, P0} = thrift_binary_protocol:new(Transport),
    Elements = lists:seq(1, 2000),
    {P1, ok} = thrift_protocol:write(P0, {{set, i32}, sets:from_list(Elements)}),
    {_P2, {ok, ReadSet}} = thrift_protocol:read(P1, {set, i32}),
    ?assertEqual(2000, sets:size(ReadSet)).
