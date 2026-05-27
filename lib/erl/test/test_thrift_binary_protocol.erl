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

-module(test_thrift_binary_protocol).
-include_lib("eunit/include/eunit.hrl").
-include("thrift_protocol.hrl").

new(Buf) ->
    {ok, Transport} = thrift_membuffer_transport:new(Buf),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    Protocol.

read(This, Type) -> thrift_protocol:read(This, Type).

negative_map_size_test() ->
    %% ktype=8 (I32), vtype=11 (STRING), size=-1 (0xffffffff big-endian)
    P = new(<<8, 11, 255, 255, 255, 255>>),
    ?assertError({protocol_error, negative_size}, read(P, map_begin)).

negative_list_size_test() ->
    %% etype=11 (STRING), size=-1
    P = new(<<11, 255, 255, 255, 255>>),
    ?assertError({protocol_error, negative_size}, read(P, list_begin)).

negative_set_size_test() ->
    %% etype=11 (STRING), size=-1
    P = new(<<11, 255, 255, 255, 255>>),
    ?assertError({protocol_error, negative_size}, read(P, set_begin)).

negative_string_size_test() ->
    %% size=-1
    P = new(<<255, 255, 255, 255>>),
    ?assertError({protocol_error, negative_size}, read(P, string)).
