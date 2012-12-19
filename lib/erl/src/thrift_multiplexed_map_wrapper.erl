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

-module(thrift_multiplexed_map_wrapper).

-export([new/0,
		 store/3,
		 find/2,
		 fetch/2]).

-type key()   :: term().
-type value() :: term().
-type map()   :: [{Key :: key(), Value :: value()}].

-spec new() -> map().
new() ->
	orddict:new().

-spec store(Key, Value, Map) -> NewMap when
	Key 		:: key(),
	Value 		:: value(),
	Map    		:: map(),
	NewMap		:: map().
store(Key, Value, Map) ->
	orddict:store(Key, Value, Map).

-spec find(Key, Map) -> {ok, Value} | error when
	Key 		:: key(),
	Value 		:: value(),
	Map    		:: map().
find(Key, Map) ->
	orddict:find(Key, Map).

-spec fetch(Key, Map) -> Value when
	Key 		:: key(),
	Value 		:: value(),
	Map    		:: map().
fetch(Key, Map) ->
	orddict:fetch(Key, Map).
