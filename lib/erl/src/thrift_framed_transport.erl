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

-module(thrift_framed_transport).

-behaviour(gen_server).
-behaviour(thrift_transport).

%% API
-export([new/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

-record(framed_transport, {wrapped, % a thrift_transport
                           read_buffer, % iolist()
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
    gen_server:call(Transport, {read, Len}).

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
    {ok, #framed_transport{wrapped = Wrapped,
                           read_buffer = [],
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
handle_call({write, Data}, _From, State = #framed_transport{write_buffer = WBuf}) ->
    {reply, ok, State#framed_transport{write_buffer = [WBuf, Data]}};

handle_call({read, Len}, _From, State = #framed_transport{wrapped = Wrapped,
                                                          read_buffer = RBuf}) ->
    {RBuf1, RBuf1Size} =
        %% if the read buffer is empty, read another frame
        %% otherwise, just read from what's left in the buffer
        case iolist_size(RBuf) of
            0 ->
                %% read the frame length
                {ok, <<FrameLen:32/integer-signed-big, _/binary>>} =
                    thrift_transport:read(Wrapped, 4),
                %% then read the data
                {ok, Bin} =
                    thrift_transport:read(Wrapped, FrameLen),
                {Bin, erlang:byte_size(Bin)};
            Sz ->
                {RBuf, Sz}
        end,

    %% pull off Give bytes, return them to the user, leave the rest in the buffer
    Give = min(RBuf1Size, Len),
    <<Data:Give/binary, RBuf2/binary>> = iolist_to_binary(RBuf1),

    Response = {ok, Data},
    State1 = State#framed_transport{read_buffer=RBuf2},

    {reply, Response, State1};

handle_call(flush, _From, State) ->
    {Response, State1} = do_flush(State),
    {reply, Response, State1}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(close, State) ->
    {_, State1} = do_flush(State),
    %% Wrapped is closed by terminate/2
    %%  error_logger:info_msg("thrift_framed_transport ~p: closing", [self()]),
    {stop, normal, State};
handle_cast(Msg, State=#framed_transport{}) ->
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
terminate(_Reason, State = #framed_transport{wrapped=Wrapped}) ->
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
do_flush(State = #framed_transport{write_buffer = Buffer,
                                   wrapped = Wrapped}) ->
    FrameLen = iolist_size(Buffer),
    Data     = [<<FrameLen:32/integer-signed-big>>, Buffer],

    Response = thrift_transport:write(Wrapped, Data),

    thrift_transport:flush(Wrapped),

    State1 = State#framed_transport{write_buffer = []},
    {Response, State1}.

min(A,B) when A<B -> A;
min(_,B)          -> B.

