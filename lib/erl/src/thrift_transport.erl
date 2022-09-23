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

-module(thrift_transport).

%% constructors
-export([new/1, new/2]).
%% transport callbacks
-export([read/2, read_exact/2, write/2, flush/1, close/1]).

-record(t_transport, {
    module :: module(),
    state :: term()
}).
-type t_transport() :: #t_transport{}.
-export_type([t_transport/0]).

%%%=========================================================================
%%%  API
%%%=========================================================================
-type state() :: term().
-export_type([state/0]).
-type reason() :: term().
-export_type([reason/0]).

-callback write(state(), iolist() | binary()) -> {state(), ok | {error, reason()}}.
-callback read(state(), non_neg_integer()) -> {state(), {ok, binary()} | {error, reason()}}.
-callback flush(state()) -> {state(), ok | {error, reason()}}.
-callback close(state()) -> {state(), ok | {error, reason()}}.

-ifdef(transport_wrapper_module).
-define(debug_wrap(Transport),
    case Transport#t_transport.module of
        ?transport_wrapper_module ->
            Transport;
        _Else ->
            {ok, Result} = ?transport_wrapper_module:new(Transport),
            Result
    end
).
-else.
-define(debug_wrap(Transport), Transport).
-endif.

-type wrappable() ::
    binary()
    | list()
    | {membuffer, binary() | list()}
    | {membuffer, binary() | list(), list()}
    | {tcp, port()}
    | {tcp, port(), list()}
    | {file, file:io_device()}
    | {file, file:io_device(), list()}
    | {file, file:filename()}
    | {file, file:filename(), list()}.

-spec new(wrappable()) -> {ok, #t_transport{}}.

new({membuffer, Membuffer}) ->
    new({membuffer, Membuffer, []});
new({membuffer, Membuffer, Opts}) when is_binary(Membuffer); is_list(Membuffer) ->
    thrift_membuffer_transport:new(Membuffer, Opts);
new({tcp, Socket}) when is_port(Socket) ->
    new({tcp, Socket, []});
new({tcp, Socket, Opts}) when is_port(Socket) ->
    thrift_socket_transport:new(Socket, Opts);
new({file, Filename}) when is_list(Filename); is_binary(Filename) ->
    new({file, Filename, []});
new({file, Filename, Opts}) when is_list(Filename); is_binary(Filename) ->
    {ok, File} = file:open(Filename, [raw, binary]),
    new({file, File, Opts});
new({file, File, Opts}) ->
    thrift_file_transport:new(File, Opts).

-spec new(Module :: module(), State :: any()) -> {ok, t_transport()}.

new(Module, State) when is_atom(Module) ->
    {ok, ?debug_wrap(#t_transport{module = Module, state = State})}.

read(Transport = #t_transport{module = Module}, Len) when
    is_integer(Len), Len >= 0
->
    {NewState, Result} = Module:read(Transport#t_transport.state, Len),
    {Transport#t_transport{state = NewState}, Result}.

read_exact(Transport = #t_transport{module = Module}, Len) when
    is_integer(Len), Len >= 0
->
    case lists:keyfind(read_exact, 1, Module:module_info(exports)) of
        {read_exact, 2} ->
            {NewState, Result} = Module:read_exact(Transport#t_transport.state, Len),
            {Transport#t_transport{state = NewState}, Result};
        _ ->
            read(Transport, Len)
    end.

write(Transport = #t_transport{module = Module}, Data) ->
    {NewState, Result} = Module:write(Transport#t_transport.state, Data),
    {Transport#t_transport{state = NewState}, Result}.

flush(Transport = #t_transport{module = Module}) ->
    {NewState, Result} = Module:flush(Transport#t_transport.state),
    {Transport#t_transport{state = NewState}, Result}.

close(Transport = #t_transport{module = Module}) ->
    {NewState, Result} = Module:close(Transport#t_transport.state),
    {Transport#t_transport{state = NewState}, Result}.
