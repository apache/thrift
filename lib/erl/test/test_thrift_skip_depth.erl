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

%% How deep thrift_protocol:skip/2 will follow a peer's nesting.
%%
%% skip/2 is driven by type ids taken off the wire, not by the IDL, so the
%% peer picks the shape and the depth.  Three bytes buy a struct level and
%% five buy a list level, which is the whole of the cost to the sender.
%%
%% The depth these tests use is far past anything a schema produces: the
%% generated readers only recurse as deep as the declared types, so nothing
%% legitimate comes near the ceiling.  The last two tests say so from the
%% other side -- a reasonable nesting still skips, and a caller who wants a
%% different ceiling can pass one.

-module(test_thrift_skip_depth).

-include_lib("eunit/include/eunit.hrl").
-include("thrift_constants.hrl").

%% Comfortably past the ceiling, and still under half a kilobyte on the wire.
-define(TOO_DEEP, 200).

new(Buf) ->
    {ok, Transport} = thrift_membuffer_transport:new(Buf),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    Protocol.

%% ---------------------------------------------------------------------------
%% Each container type nests through its own skip loop, so each gets a test.
%% ---------------------------------------------------------------------------

deeply_nested_structs_are_refused_test() ->
    P = new(nested_structs(?TOO_DEEP)),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:skip(P, struct)
    ).

deeply_nested_lists_are_refused_test() ->
    P = new(nested_lists(?TOO_DEEP)),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:skip(P, list)
    ).

deeply_nested_sets_are_refused_test() ->
    P = new(nested_sets(?TOO_DEEP)),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:skip(P, set)
    ).

deeply_nested_maps_are_refused_test() ->
    P = new(nested_maps(?TOO_DEEP)),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:skip(P, map)
    ).

%% The wire cost of getting there, stated as a number so that a later change
%% to the ceiling has to look at it: 200 struct levels is 801 bytes.
nesting_is_cheap_to_send_test() ->
    ?assertEqual(801, byte_size(nested_structs(?TOO_DEEP))).

%% ---------------------------------------------------------------------------
%% ...and the ceiling has to stay out of the way of real messages.
%% ---------------------------------------------------------------------------

ordinary_nesting_still_skips_test() ->
    P = new(nested_structs(16)),
    ?assertMatch({_, ok}, thrift_protocol:skip(P, struct)).

%% skip/3 takes the ceiling as an argument, so a caller with a stricter idea
%% of what its schema can contain can say so.
an_explicit_ceiling_is_honoured_test() ->
    P = new(nested_structs(16)),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:skip(P, struct, 4)
    ),
    Q = new(nested_structs(16)),
    ?assertMatch({_, ok}, thrift_protocol:skip(Q, struct, 32)).

%% ---------------------------------------------------------------------------
%% Wire shapes.  Each level holds exactly one thing, so the depth is the
%% only dimension that grows.
%% ---------------------------------------------------------------------------

%% Level 0 is the empty struct; every level above it is one struct-typed
%% field holding the level below.  3 bytes for the field header, 1 for the
%% stop.
nested_structs(0) ->
    <<?tType_STOP>>;
nested_structs(N) ->
    Inner = nested_structs(N - 1),
    <<?tType_STRUCT, 0, 1, Inner/binary, ?tType_STOP>>.

%% Level 0 is an empty list of bool; every level above it is a one-element
%% list of list.  5 bytes a level.
nested_lists(0) ->
    <<?tType_BOOL, 0, 0, 0, 0>>;
nested_lists(N) ->
    Inner = nested_lists(N - 1),
    <<?tType_LIST, 0, 0, 0, 1, Inner/binary>>.

nested_sets(0) ->
    <<?tType_BOOL, 0, 0, 0, 0>>;
nested_sets(N) ->
    Inner = nested_sets(N - 1),
    <<?tType_SET, 0, 0, 0, 1, Inner/binary>>.

%% Maps carry a key as well, so each level is a bool key followed by the map
%% below it.  7 bytes a level.
nested_maps(0) ->
    <<?tType_BOOL, ?tType_BOOL, 0, 0, 0, 0>>;
nested_maps(N) ->
    Inner = nested_maps(N - 1),
    <<?tType_BOOL, ?tType_MAP, 0, 0, 0, 1, 0, Inner/binary>>.
