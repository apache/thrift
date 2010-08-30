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

-module(thrift_file_transport).

-behaviour(thrift_transport).

-export([new_reader/1,
         new/1,
         new/2,
         write/2, read/2, flush/1, close/1]).

-record(t_file_transport, {device,
                           should_close = true,
                           mode = write}).
-type state() :: #t_file_transport{}.
-include("thrift_transport_behaviour.hrl").

%%%% CONSTRUCTION   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

new_reader(Filename) ->
    case file:open(Filename, [read, binary, {read_ahead, 1024*1024}]) of
        {ok, IODevice} ->
            new(IODevice, [{should_close, true}, {mode, read}]);
        Error -> Error
    end.

new(Device) ->
    new(Device, []).

%% Device :: io_device()
%%
%% Device should be opened in raw and binary mode.
new(Device, Opts) when is_list(Opts) ->
    State = parse_opts(Opts, #t_file_transport{device = Device}),
    thrift_transport:new(?MODULE, State).


%% Parse options
parse_opts([{should_close, Bool} | Rest], State) when is_boolean(Bool) ->
    parse_opts(Rest, State#t_file_transport{should_close = Bool});
parse_opts([{mode, Mode} | Rest], State)
  when Mode =:= write;
       Mode =:= read ->
    parse_opts(Rest, State#t_file_transport{mode = Mode});
parse_opts([], State) ->
     State.


%%%% TRANSPORT IMPL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write(This = #t_file_transport{device = Device, mode = write}, Data) ->
    {This, file:write(Device, Data)};
write(This, _D) ->
    {This, {error, read_mode}}.


read(This = #t_file_transport{device = Device, mode = read}, Len)
  when is_integer(Len), Len >= 0 ->
    {This, file:read(Device, Len)};
read(This, _D) ->
    {This, {error, read_mode}}.

flush(This = #t_file_transport{device = Device, mode = write}) ->
    {This, file:sync(Device)}.

close(This = #t_file_transport{device = Device, should_close = SC}) ->
    case SC of
        true ->
            {This, file:close(Device)};
        false ->
            {This, ok}
    end.
