%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

%%%-------------------------------------------------------------------
%%% @doc  
%%% @end
%%%-------------------------------------------------------------------
-module(thrift_oop_server).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include("oop.hrl").

-include("thrift.hrl").

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 start_link/0,
	 stop/0
	 ]).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% record definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server.
%% @spec start_link() -> {ok, pid()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the server.
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------

init({Class, Args}) ->
    process_flag(trap_exit, true),
    try
	State = apply(Class, new, Args),
	?INFO(oop_new, {Args, Class, State}),
	{ok, State}
    catch
	E -> {stop, {new_failed, E}}
    end;

init(_) ->
    {stop, invalid_params}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call(Request, From, State) ->
    handle_either(call, Request, From, State).

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({Method, Args}, State) ->
    handle_either(cast, {Method, Args}, undefined, State).

-define(REPLY(Value, State),
	case Type of 
	    call -> {reply, Value, State};
	    cast -> {noreply, State} 
	end
). 

handle_either(Type, Request, From, State) ->
    %% error_logger:info_msg("~p: ~p", [?SERVER, oop:inspect(State)]),
    %% error_logger:info_msg("handle_call(Request=~p, From=~p, State)", [Request, From]),

    case Request of 
	{get, [Field]} ->
	    Value = oop:get(State, Field),
	    ?REPLY(Value, State);

	{set, [Field, Value]} ->
	    State1 = oop:set(State, Field, Value),
	    ?REPLY(Value, State1);

	{class, []} ->
	    ?REPLY(?CLASS(State), State);

	{Method, Args} ->
	    handle_method(Type, State, Method, Args);

	_ ->
	    error_logger:format("no match for Request = ~p", [Request]),
	    %% {stop, server_error, State} 
	    {reply, server_error, State}
    end.

handle_method(Type, State, Method, Args) ->
    %% is an effectful call?
    Is_effectful = lists:prefix("effectful_", atom_to_list(Method)),
    Call         = oop:call(State, Method, Args),
    
    %% TODO(cpiro): maybe add error handling here? = catch oop:call?
    
    case {Is_effectful, Call} of 
	{true, {Retval, State1}} ->
	    ?REPLY(Retval, State1);
	
	{true, _MalformedReturn} ->
	    %% TODO(cpiro): bad match -- remove when we're done converting
	    error_logger:format("oop:call(effectful_*,..,..) malformed return value ~p", 
				[_MalformedReturn]),
	    %% {stop, server_error, State} 
	    {noreply, State};
	
	{false, Retval} ->
	    ?REPLY(Retval, State)
    end.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, Except} = All, State) ->
    Result = try
	oop:call(State, catches, [Pid, Except])
    catch
	exit:{missing_method, _} ->
	    unhandled
    end,

    case Result of
	unhandled ->
	    error_logger:format("unhandled exit ~p", [All]),
	    {stop, All, State};
	_WasHandled ->
	    {noreply, State}
    end;

handle_info(Info, State) ->
    error_logger:info_msg("~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    %%error_logger:format("~p terminated!: ~p", [self(), Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
 %%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================
