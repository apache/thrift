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

new(Module, Data) when is_atom(Module) ->
    {ok, #transport{module = Module,
                    data = Data}}.

%% Data :: iolist()
write(Transport, Data) ->
    Module = Transport#transport.module,
    Module:write(Transport#transport.data, Data).

read(Transport, Len) when is_integer(Len) ->
    Module = Transport#transport.module,
    Module:read(Transport#transport.data, Len).

flush(#transport{module = Module, data = Data}) ->
    Module:flush(Data).

close(#transport{module = Module, data = Data}) ->
    Module:close(Data).
