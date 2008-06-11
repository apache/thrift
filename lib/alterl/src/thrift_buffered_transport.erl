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

-record(state, {
          % The wrapped transport
          wrapped,

          % a list of binaries which will be concatenated and sent during
          % a flush.
          %
          % *** THIS LIST IS STORED IN REVERSE ORDER!!! ***
          %
          buffer}).

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
    gen_server:call(Transport, close).

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
    {ok, #state{wrapped = Wrapped,
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
handle_call({write, Data}, _From, State = #state{buffer = Buffer}) ->
    {reply, ok, State#state{buffer = [Data | Buffer]}};

handle_call({read, Len}, _From, State = #state{wrapped = Wrapped}) ->
    Response = thrift_transport:read(Wrapped, Len),
    {reply, Response, State};

handle_call(flush, _From, State = #state{buffer = Buffer,
                                         wrapped = Wrapped}) ->
    Concat   = concat_binary(lists:reverse(Buffer)),
    Response = thrift_transport:write(Wrapped, Concat),
    thrift_transport:flush(Wrapped),
    {reply, Response, State#state{buffer = []}};

handle_call(close, _From, State = #state{buffer  = Buffer,
                                         wrapped = Wrapped}) ->
    thrift_transport:write(Wrapped, concat_binary(lists:reverse(Buffer))),
    Close=thrift_transport:close(Wrapped),
    {stop, shutdown, Close, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Msg, State=#state{}) ->
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
terminate(_Reason, _State) ->
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
