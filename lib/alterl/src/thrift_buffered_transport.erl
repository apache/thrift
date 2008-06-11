%%%-------------------------------------------------------------------
%%% File    : thrift_buffered_transport.erl
%%% Author  :  <todd@lipcon.org>
%%% Description : Buffered transport for thrift
%%%
%%% Created : 30 Jan 2008 by  <todd@lipcon.org>
%%%-------------------------------------------------------------------
-module(thrift_buffered_transport).

-behaviour(gen_server).
-behaviour(thrift_transport).

%% API
-export([new/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% thrift_transport callbacks
-export([write/2, read/2, flush/1, close/1]).

-record(buffered_transport, {wrapped, % a thrift_transport
                             buffer
                             %% a list of binaries which will be concatenated and sent during
                             %% a flush.
                             %%
                             %% *** THIS LIST IS STORED IN REVERSE ORDER!!! ***
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
%% Data = binary()
%%
%% Description: Writes data into the buffer
%%--------------------------------------------------------------------
write(Transport, Data) when is_binary(Data) ->
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
    %% TODO(cpiro): need to trap exits here so when transport exits
    %% normally from under our feet we exit normally
    {ok, #buffered_transport{wrapped = Wrapped,
                             buffer = []}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({write, Data}, _From, State = #buffered_transport{buffer = Buffer}) ->
    {reply, ok, State#buffered_transport{buffer = [Data | Buffer]}};

handle_call({read, Len}, _From, State = #buffered_transport{wrapped = Wrapped}) ->
    Response = thrift_transport:read(Wrapped, Len),
    {reply, Response, State};

handle_call(flush, _From, State = #buffered_transport{buffer = Buffer,
                                                      wrapped = Wrapped}) ->
    Concat   = concat_binary(lists:reverse(Buffer)),
    Response = thrift_transport:write(Wrapped, Concat),
    thrift_transport:flush(Wrapped),
    {reply, Response, State#buffered_transport{buffer = []}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(close, State = #buffered_transport{buffer = Buffer,
                                               wrapped = Wrapped}) ->
    thrift_transport:write(Wrapped, concat_binary(lists:reverse(Buffer))),
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
