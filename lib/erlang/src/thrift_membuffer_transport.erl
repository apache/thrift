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

-module(thrift_membuffer_transport).

-behaviour(thrift_transport).

%% constructors
-export([new/0, new/1]).
%% protocol callbacks
-export([read/2, read_exact/2, write/2, flush/1, close/1]).

-record(t_membuffer, {
    buffer = []
}).

-type state() :: #t_membuffer{}.

-spec new() -> thrift_transport:t_transport().

new() -> new(<<>>).

-spec new(Buf :: iodata()) -> thrift_transport:t_transport().

new(Buf) when is_list(Buf) ->
    State = #t_membuffer{buffer = iolist_to_binary(Buf)},
    thrift_transport:new(?MODULE, State);
new(Buf) when is_binary(Buf) ->
    State = #t_membuffer{buffer = Buf},
    thrift_transport:new(?MODULE, State).

-include("thrift_transport_behaviour.hrl").

read(State = #t_membuffer{buffer = Buf}, Len) when
    is_integer(Len), Len >= 0
->
    Give = min(byte_size(Buf), Len),
    {Result, Remaining} = split_binary(Buf, Give),
    {State#t_membuffer{buffer = Remaining}, {ok, Result}}.

read_exact(State = #t_membuffer{buffer = Buf}, Len) when
    is_integer(Len), Len >= 0
->
    case byte_size(Buf) of
        X when X >= Len ->
            {Result, Remaining} = split_binary(Buf, Len),
            {State#t_membuffer{buffer = Remaining}, {ok, Result}};
        _ ->
            {State, {error, eof}}
    end.

write(State = #t_membuffer{buffer = Buf}, Data) when
    is_list(Data); is_binary(Data)
->
    {State#t_membuffer{buffer = <<Buf/binary, (iolist_to_binary(Data))/binary>>}, ok}.

flush(State = #t_membuffer{}) ->
    {State, ok}.

close(State = #t_membuffer{buffer = Buf}) ->
    {State, {ok, Buf}}.
