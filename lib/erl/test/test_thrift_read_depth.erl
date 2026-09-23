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

%% How deep thrift_protocol:read/2 will follow the nesting a peer sends.
%%
%% read/2 walks the types the generated struct_info/1 declares, and a
%% {struct, {Module, Name}} reference is resolved lazily, so a schema whose
%% types refer back to themselves recurses one frame per wire nesting level.
%% The peer, not the schema author, decides how many levels arrive: a struct
%% level is three bytes on the wire.  read/2 spends the same allowance skip/2
%% does -- ?DEFAULT_RECURSION_DEPTH, exceeded the same way -- so the two agree
%% on where nesting stops.  This module hosts its own recursive struct_info/1
%% rather than generate one, so the wire shapes below are the only input.

-module(test_thrift_read_depth).

-include_lib("eunit/include/eunit.hrl").
-include("thrift_constants.hrl").

-export([struct_info/1]).

%% A struct that holds one field of its own type, and one that holds a list of
%% its own type.  The list case charges twice per wire level (the list and the
%% element it contains), the same as skip/2, so it stops at roughly half the
%% depth -- the ceiling is per composite level, not per struct.
struct_info('Node') ->
    {struct, [{1, {struct, {?MODULE, 'Node'}}}, {2, i32}]};
struct_info('ListNode') ->
    {struct, [{1, {list, {struct, {?MODULE, 'ListNode'}}}}]}.

new(Buf) ->
    {ok, Transport} = thrift_membuffer_transport:new(Buf),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    Protocol.

read_node(Proto) ->
    thrift_protocol:read(Proto, {struct, {?MODULE, 'Node'}}).

%% ---------------------------------------------------------------------------
%% Wire shapes.  Each level holds exactly one thing, so depth is the only
%% dimension that grows.
%% ---------------------------------------------------------------------------

%% N nested Node structs.  Level 1 is the empty struct (just STOP); every level
%% above it is one struct-typed field (id 1) holding the level below.  Reading
%% it enters read/2's struct clause exactly N times.
node_chain(1) ->
    <<?tType_STOP>>;
node_chain(N) when N > 1 ->
    Inner = node_chain(N - 1),
    <<?tType_STRUCT, 0, 1, Inner/binary, ?tType_STOP>>.

%% N nested ListNode structs: field id 1 is a one-element list whose element is
%% the level below.  Field header (3 bytes) + list header etype=STRUCT size=1
%% (5 bytes) per level.
listnode_chain(1) ->
    <<?tType_STOP>>;
listnode_chain(N) when N > 1 ->
    Inner = listnode_chain(N - 1),
    <<?tType_LIST, 0, 1, ?tType_STRUCT, 0, 0, 0, 1, Inner/binary, ?tType_STOP>>.

%% ---------------------------------------------------------------------------
%% read/2 stops at the same depth skip/2 does, on the very same bytes.
%% ---------------------------------------------------------------------------

reading_to_the_limit_succeeds_test() ->
    ?assertMatch({_, {ok, _}}, read_node(new(node_chain(?DEFAULT_RECURSION_DEPTH)))).

reading_past_the_limit_is_refused_test() ->
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        read_node(new(node_chain(?DEFAULT_RECURSION_DEPTH + 1)))
    ).

%% The point of the change: before it, read/2 accepted this depth that skip/2
%% has always refused.  Both must now agree, at the limit and one past it.
read_and_skip_agree_at_the_limit_test() ->
    AtLimit = node_chain(?DEFAULT_RECURSION_DEPTH),
    ?assertMatch({_, {ok, _}}, read_node(new(AtLimit))),
    ?assertMatch({_, ok}, thrift_protocol:skip(new(AtLimit), struct)).

read_and_skip_agree_past_the_limit_test() ->
    PastLimit = node_chain(?DEFAULT_RECURSION_DEPTH + 1),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        read_node(new(PastLimit))
    ),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:skip(new(PastLimit), struct)
    ).

%% A comfortable, realistic nesting still reads.
ordinary_nesting_still_reads_test() ->
    ?assertMatch({_, {ok, _}}, read_node(new(node_chain(16)))).

%% ---------------------------------------------------------------------------
%% Nesting reached through a container is charged too.
%% ---------------------------------------------------------------------------

deeply_nested_lists_are_refused_test() ->
    P = new(listnode_chain(?DEFAULT_RECURSION_DEPTH + 1)),
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        thrift_protocol:read(P, {struct, {?MODULE, 'ListNode'}})
    ).

ordinary_list_nesting_still_reads_test() ->
    P = new(listnode_chain(8)),
    ?assertMatch(
        {_, {ok, _}},
        thrift_protocol:read(P, {struct, {?MODULE, 'ListNode'}})
    ).

%% ---------------------------------------------------------------------------
%% The ceiling holds through a protocol decorator: it lives in the
%% type-directed layer, above whichever protocol module does the leaf reads.
%% ---------------------------------------------------------------------------

multiplexed_read_is_refused_past_the_limit_test() ->
    ?assertError(
        {protocol_error, max_skip_depth_exceeded},
        read_node(multiplexed(node_chain(?DEFAULT_RECURSION_DEPTH + 1)))
    ).

multiplexed_read_to_the_limit_succeeds_test() ->
    ?assertMatch(
        {_, {ok, _}},
        read_node(multiplexed(node_chain(?DEFAULT_RECURSION_DEPTH)))
    ).

multiplexed(Buf) ->
    {ok, Transport} = thrift_membuffer_transport:new(Buf),
    {ok, Protocol} = thrift_binary_protocol:new(Transport),
    {ok, Multiplexed} = thrift_multiplexed_protocol:new(Protocol, "svc"),
    Multiplexed.
