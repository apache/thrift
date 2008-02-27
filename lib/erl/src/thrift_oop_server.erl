%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift_oop_server).

-behaviour(gen_server).

-include("oop.hrl").

-include("thrift.hrl").

-include("transport/tTransportException.hrl").
-include("protocol/tProtocolException.hrl").

-export([
         start_link/0,
         stop/0
         ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%
%%% api
%%%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%
%%% init
%%%

init({Class, Args}) ->
    process_flag(trap_exit, true),
    try %% TODO use apply_if_defined
        State = apply(Class, new, Args),
        ?INFO("thrift ~p:new(~s) = ~s", [Class, thrift_utils:unbrack(Args), oop:inspect(State)]),
        {ok, State}
    catch
        E -> {stop, {new_failed, E}}
    end;

init(_) ->
    {stop, invalid_params}.

%%%
%%% call and cast
%%%

handle_call(Request, From, State)  -> handle_call_cast(call, Request, From, State).
handle_cast(stop, State)           -> {stop, normal, State};
handle_cast({Method, Args}, State) -> handle_call_cast(cast, {Method, Args}, undefined, State).

reply(call, Value, State)  -> {reply, Value, State};
reply(cast, _Value, State) -> {noreply, State}.

handle_call_cast(Type, Request, From, State) ->
    %% ?INFO("~p: ~p", [?SERVER, oop:inspect(State)]),
    %% ?INFO("handle_call(Request=~p, From=~p, State)", [Request, From]),

    case Request of
        {get, [Field]} ->
            Value = oop:get(State, Field),
            reply(Type, Value, State);

        {set, [Field, Value]} ->
            State1 = oop:set(State, Field, Value),
            reply(Type, Value, State1);

        {class, []} ->
            reply(Type, ?CLASS(State), State);

        {Method, Args} ->
            handle_method(Type, State, Method, Args);

        _ ->
            ?ERROR("thrift no match for Request = ~p", [Request]),
            {stop, server_error, State}
            %% {reply, server_error, State}
    end.

handle_method(Type, State, Method, Args) ->
    Is_effectful = lists:prefix("effectful_", atom_to_list(Method)),

    try {Is_effectful, oop:call(State, Method, Args)} of
        {true, {Retval, State1}} ->
            reply(Type, Retval, State1);

        {true, _MalformedReturn} ->
            %% TODO(cpiro): bad match -- remove when we're done converting
            ?ERROR("oop:call(effectful_*,..,..) malformed return value ~p",
                   [_MalformedReturn]),
            {stop, server_error, State};
            %% {noreply, State};

        {false, Retval} ->
            reply(Type, Retval, State)

    catch
        exit:{thrift_exception, E}          -> handle_exception(E, nothing);
        exit:{{thrift_exception, E}, Stack} -> handle_exception(E, Stack);
        exit:normal                         -> exit(normal);
        exit:(X = {timeout, _})             -> exit(X);
        exit:Other ->
            exit(Other)
    end.

handle_exception(E, Stack) ->
    %% ?ERROR("texception ~p", [E]),
    case {oop:is_a(E, tException), Stack} of
        {true, nothing} -> % good
            exit({thrift_exception, E});
        {true, _} -> % good
            E1 = tException:add_backtrace_element(E, Stack),
            exit({thrift_exception, E1});
        {false, _} -> % shit
            ?ERROR("exception wasn't really a tException ~p", [E]),
            exit(bum)
    end.

%%%
%%% info, terminate, and code_change
%%%

handle_info({'EXIT', Pid, Except} = All, State) ->
    case Except of
        normal ->
            {noreply, State};
        {normal, _} ->
            {noreply, State};
        _unhandled ->
            error_logger:format("unhandled exit ~p", [All]),
            {stop, All, State}
    end;

handle_info(Info, State) ->
    ?INFO("~p", [Info]),
    {noreply, State}.

terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.
