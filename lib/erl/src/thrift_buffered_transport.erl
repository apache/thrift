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

-behaviour(gen_server).
-behaviour(thrift_transport).

%% API
-export([new/1, new_transport_factory/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

-record(buffered_transport, {wrapped, % a thrift_transport
                             write_buffer % iolist()
                            }).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
new(WrappedTransport) ->
    case gen_server:start_link(?MODULE, [WrappedTransport], []) of
        {ok, Pid} ->
            thrift_transport:new(?MODULE, Pid);
        Else ->
            Else
    end.



%%--------------------------------------------------------------------
%% Function: write(Transport, Data) -> ok
%%
%% Data = iolist()
%%
%% Description: Writes data into the buffer
%%--------------------------------------------------------------------
write(Transport, Data) ->
    gen_server:call(Transport, {write, Data}).

%%--------------------------------------------------------------------
%% Function: flush(Transport) -> ok
%%
%% Description: Flushes the buffer through to the wrapped transport
%%--------------------------------------------------------------------
flush(Transport) ->
    gen_server:call(Transport, flush).

%%--------------------------------------------------------------------
%% Function: close(Transport) -> ok
%%
%% Description: Closes the transport and the wrapped transport
%%--------------------------------------------------------------------
close(Transport) ->
    gen_server:cast(Transport, close).

%%--------------------------------------------------------------------
%% Function: Read(Transport, Len) -> {ok, Data}
%%
%% Data = binary()
%%
%% Description: Reads data through from the wrapped transoprt
%%--------------------------------------------------------------------
read(Transport, Len) when is_integer(Len) ->
    gen_server:call(Transport, {read, Len}, _Timeout=10000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Wrapped]) ->
    {ok, #buffered_transport{wrapped = Wrapped,
                             write_buffer = []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({write, Data}, _From, State = #buffered_transport{write_buffer = WBuf}) ->
    {reply, ok, State#buffered_transport{write_buffer = [WBuf, Data]}};

handle_call({read, Len}, _From, State = #buffered_transport{wrapped = Wrapped}) ->
    Response = thrift_transport:read(Wrapped, Len),
    {reply, Response, State};

handle_call(flush, _From, State = #buffered_transport{write_buffer = WBuf,
                                                      wrapped = Wrapped}) ->
    Response = thrift_transport:write(Wrapped, WBuf),
    thrift_transport:flush(Wrapped),
    {reply, Response, State#buffered_transport{write_buffer = []}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(close, State = #buffered_transport{write_buffer = WBuf,
                                               wrapped = Wrapped}) ->
    thrift_transport:write(Wrapped, WBuf),
    %% Wrapped is closed by terminate/2
    %%  error_logger:info_msg("thrift_buffered_transport ~p: closing", [self()]),
    {stop, normal, State};
handle_cast(Msg, State=#buffered_transport{}) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State = #buffered_transport{wrapped=Wrapped}) ->
    thrift_transport:close(Wrapped),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
