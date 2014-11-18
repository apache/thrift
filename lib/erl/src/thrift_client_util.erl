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

-module(thrift_client_util).

-export([new/4]).

%%
%% Splits client options into client, protocol, and transport options
%%
%% split_options([Options...]) -> {ProtocolOptions, TransportOptions}
%%
split_options(Options) ->
    split_options(Options, [], []).

split_options([], ProtoIn, TransIn) ->
    {ProtoIn, TransIn};

split_options([Opt = {OptKey, _} | Rest], ProtoIn, TransIn)
  when OptKey =:= strict_read;
       OptKey =:= strict_write ->
    split_options(Rest, [Opt | ProtoIn], TransIn);

split_options([Opt = {OptKey, _} | Rest], ProtoIn, TransIn)
  when OptKey =:= framed;
       OptKey =:= connect_timeout;
       OptKey =:= recv_timeout;
       OptKey =:= sockopts ->
    split_options(Rest, ProtoIn, [Opt | TransIn]).


%% Client constructor for the common-case of socket transports
%% with the binary protocol
new(Host, Port, Service, Options)
  when is_integer(Port), is_atom(Service), is_list(Options) ->
    {ProtoOpts, TransOpts} = split_options(Options),

    {ok, TransportFactory} =
        thrift_socket_transport:new_transport_factory(Host, Port, TransOpts),

    {ok, ProtocolFactory} = thrift_binary_protocol:new_protocol_factory(
                              TransportFactory, ProtoOpts),

    case ProtocolFactory() of
        {ok, Protocol} ->
            thrift_client:new(Protocol, Service);
        {error, Error} ->
            {error, Error}
    end.
