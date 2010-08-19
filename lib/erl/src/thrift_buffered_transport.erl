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

-module(thrift_buffered_transport).

-behaviour(thrift_transport).

%% API
-export([new/1, new_transport_factory/1]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

-record(buffered_transport, {wrapped, % a thrift_transport
                             write_buffer % iolist()
                            }).
-type state() :: #buffered_transport{}.
-include("thrift_transport_behaviour.hrl").


new(WrappedTransport) ->
    State = #buffered_transport{wrapped = WrappedTransport,
                                write_buffer = []},
    thrift_transport:new(?MODULE, State).


%% Writes data into the buffer
write(State = #buffered_transport{write_buffer = WBuf}, Data) ->
    {State#buffered_transport{write_buffer = [WBuf, Data]}, ok}.

%% Flushes the buffer through to the wrapped transport
flush(State = #buffered_transport{write_buffer = WBuf,
                                  wrapped = Wrapped0}) ->
    {Wrapped1, Response} = thrift_transport:write(Wrapped0, WBuf),
    {Wrapped2, _} = thrift_transport:flush(Wrapped1),
    NewState = State#buffered_transport{write_buffer = [],
                                        wrapped = Wrapped2},
    {NewState, Response}.

%% Closes the transport and the wrapped transport
close(State = #buffered_transport{wrapped = Wrapped0}) ->
    {Wrapped1, Result} = thrift_transport:close(Wrapped0),
    NewState = State#buffered_transport{wrapped = Wrapped1},
    {NewState, Result}.

%% Reads data through from the wrapped transport
read(State = #buffered_transport{wrapped = Wrapped0}, Len) when is_integer(Len) ->
    {Wrapped1, Response} = thrift_transport:read(Wrapped0, Len),
    NewState = State#buffered_transport{wrapped = Wrapped1},
    {NewState, Response}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
new_transport_factory(WrapFactory) ->
    F = fun() ->
                {ok, Wrapped} = WrapFactory(),
                new(Wrapped)
        end,
    {ok, F}.
