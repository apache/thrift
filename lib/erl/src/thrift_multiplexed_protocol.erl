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
%% The JSON protocol implementation was created by
%% Peter Neumark <neumark.peter@gmail.com> based on
%% the binary protocol implementation.

-module(thrift_multiplexed_protocol).

-behaviour(thrift_protocol).

-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-export([new/2,
         read/2,
         write/2,
         flush_transport/1,
         close_transport/1
        ]).

-record(protocol, {module, data}).
-type protocol() :: #protocol{}.

-record (multiplexed_protocol, {protocol_module_to_decorate,
								protocol_data_to_decorate,
								service_name}).

-type state() :: #multiplexed_protocol{}.

-include("thrift_protocol_behaviour.hrl").

-spec new(ProtocolToDecorate :: protocol(), ServiceName :: list()) -> {ok, protocol()}.
new(ProtocolToDecorate, ServiceName) when is_record(ProtocolToDecorate, protocol),
										  is_list(ServiceName) ->
	State = #multiplexed_protocol{protocol_module_to_decorate = ProtocolToDecorate#protocol.module,
								  protocol_data_to_decorate = ProtocolToDecorate#protocol.data,
						  		  service_name = ServiceName},
	thrift_protocol:new(?MODULE, State).

flush_transport(State = #multiplexed_protocol{protocol_module_to_decorate = ProtocolModuleToDecorate,
								  	  		  protocol_data_to_decorate   = ProtocolDataToDecorate}) ->
	{NewState, ok} = ProtocolModuleToDecorate:flush_transport(ProtocolDataToDecorate),
	{State#multiplexed_protocol{protocol_data_to_decorate = NewState}, ok}.

close_transport(State = #multiplexed_protocol{protocol_module_to_decorate = ProtocolModuleToDecorate,
								  	  		  protocol_data_to_decorate   = ProtocolDataToDecorate}) ->
	{NewState, ok} = ProtocolModuleToDecorate:close_transport(ProtocolDataToDecorate),
	{State#multiplexed_protocol{protocol_data_to_decorate = NewState}, ok}.

%%%
%%% instance methods
%%%

write(State = #multiplexed_protocol{protocol_module_to_decorate = ProtocolModuleToDecorate,
								 protocol_data_to_decorate   = ProtocolDataToDecorate,
								 service_name = ServiceName},
	  Message = #protocol_message_begin{name = Name}) ->
	{NewState, ok} = ProtocolModuleToDecorate:write(ProtocolDataToDecorate,
								   					Message#protocol_message_begin{name=ServiceName ++ ?MULTIPLEXED_SERVICE_SEPARATOR ++ Name}),
	{State#multiplexed_protocol{protocol_data_to_decorate = NewState}, ok};

write(State = #multiplexed_protocol{protocol_module_to_decorate = ProtocolModuleToDecorate,
							     protocol_data_to_decorate   = ProtocolDataToDecorate},
	  Message) ->
	{NewState, ok} = ProtocolModuleToDecorate:write(ProtocolDataToDecorate, Message),
	{State#multiplexed_protocol{protocol_data_to_decorate = NewState}, ok}.

read(State = #multiplexed_protocol{protocol_module_to_decorate = ProtocolModuleToDecorate,
						   protocol_data_to_decorate   = ProtocolDataToDecorate},
	 Message) ->
	{NewState, Result} = ProtocolModuleToDecorate:read(ProtocolDataToDecorate, Message),
	{State#multiplexed_protocol{protocol_data_to_decorate = NewState}, Result}.
