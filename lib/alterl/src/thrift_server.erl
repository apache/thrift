%%%-------------------------------------------------------------------
%%% File    : thrift_server.erl
%%% Author  :  <todd@lipcon.org>
%%% Description : 
%%%
%%% Created : 28 Jan 2008 by  <todd@lipcon.org>
%%%-------------------------------------------------------------------
-module(thrift_server).

-behaviour(gen_server).

%% API
-export([start_link/3, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {listen_socket, acceptor, service}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Port, Service, HandlerModule) when is_integer(Port), is_atom(HandlerModule) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Port, Service, HandlerModule}, []).


%%--------------------------------------------------------------------
%% Function: stop(Pid) -> ok, {error, Reason}
%% Description: Stops the server.
%%--------------------------------------------------------------------
stop(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, stop).


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
init({Port, Service, Handler}) ->
    {ok, Socket} = gen_tcp:listen(Port,
                                  [binary,
                                   {packet, 0},
                                   {active, false},
                                   {nodelay, true},
                                   {reuseaddr, true}]),
    Acceptor = spawn_link(fun () -> acceptor(Socket, Service, Handler) end),
    {ok, #state{listen_socket = Socket,
                acceptor = Acceptor,
                service = Service}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    State#state.acceptor ! stop,
    {stop, stopped, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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
    State#state.acceptor ! refresh,
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

acceptor(ListenSocket, Service, Handler)
  when is_port(ListenSocket), is_atom(Handler) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    error_logger:info_msg("Accepted client"),

    {ok, SocketTransport} = thrift_socket_transport:new(Socket),
    {ok, BufferedTransport} = thrift_buffered_transport:new(SocketTransport),
    {ok, Protocol} = thrift_binary_protocol:new(BufferedTransport),

    thrift_processor:start(Protocol, Protocol, Service, Handler),
    receive
        refresh ->
            error_logger:info_msg("Acceptor refreshing~n"),
            ?MODULE:acceptor(ListenSocket, Service, Handler);
        stop ->
            ok
    after 0      -> acceptor(ListenSocket, Service, Handler)
    end.
