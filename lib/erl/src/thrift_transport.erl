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

-export([behaviour_info/1]).

-export([new/2,
         write/2,
         read/2,
         flush/1,
         close/1
        ]).

behaviour_info(callbacks) ->
    [{read, 2},
     {write, 2},
     {flush, 1},
     {close, 1}
    ].

-record(transport, {module, data}).

-ifdef(transport_wrapper_module).
-define(debug_wrap(Transport),
        case Transport#transport.module of
            ?transport_wrapper_module ->
                Transport;
            _Else ->
                {ok, Result} = ?transport_wrapper_module:new(Transport),
                Result
        end).
-else.
-define(debug_wrap(Transport), Transport).
-endif.

new(Module, Data) when is_atom(Module) ->
    Transport0 = #transport{module = Module, data = Data},
    Transport1 = ?debug_wrap(Transport0),
    {ok, Transport1}.

-spec write(#transport{}, iolist() | binary()) -> {#transport{}, ok | {error, _Reason}}.
write(Transport, Data) ->
    Module = Transport#transport.module,
    {NewTransData, Result} = Module:write(Transport#transport.data, Data),
    {Transport#transport{data = NewTransData}, Result}.

-spec read(#transport{}, non_neg_integer()) -> {#transport{}, {ok, binary()} | {error, _Reason}}.
read(Transport, Len) when is_integer(Len) ->
    Module = Transport#transport.module,
    {NewTransData, Result} = Module:read(Transport#transport.data, Len),
    {Transport#transport{data = NewTransData}, Result}.

-spec flush(#transport{}) -> {#transport{}, ok | {error, _Reason}}.
flush(Transport = #transport{module = Module, data = Data}) ->
    {NewTransData, Result} = Module:flush(Data),
    {Transport#transport{data = NewTransData}, Result}.

-spec close(#transport{}) -> {#transport{}, ok | {error, _Reason}}.
close(Transport = #transport{module = Module, data = Data}) ->
    {NewTransData, Result} = Module:close(Data),
    {Transport#transport{data = NewTransData}, Result}.
