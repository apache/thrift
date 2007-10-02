%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift_logger).

-behaviour(gen_event).

-record(state, {config}).

-define(CONFIG(Item), config(Item, State)).

%% TODO(cpiro): either
%% make exceptions know whether they need to be displayed
%% or not exit with tExecptions for non-errors
%% or "register" tExceptions with the logger (I LIKE!)
%% ... we shouldn't need to build any specifics in here
-include("thrift.hrl").
-include("oop.hrl").
-include("transport/tTransportException.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([install/0, install/1, reconfig/0, reconfig/1]).

%% ensure the regular logger is out and ours is in
install() ->
    install([]).
install(Config) ->
    %% remove loggers
    io:format("starting logger~n"),
    lists:foreach(fun(Logger) ->
      case Logger of
        _ -> gen_event:delete_handler(error_logger, Logger, normal)
      end end,
      gen_event:which_handlers(error_logger)),

    %% TODO(cpiro): sasl someday?
    %% gen_event:add_handler(error_logger, sasl_report_file_h, {LogFile, all}),

    gen_event:add_handler(error_logger, ?MODULE, []),

    reconfig(Config),

    ok.

%% load our config file and amend the given stuff
reconfig() ->
    reconfig([]).

reconfig(Config) ->
    gen_event:call(error_logger, ?MODULE, {reconfig, Config}).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State}.
%% 
%% @doc 
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% @end 
%%--------------------------------------------------------------------
init([]) ->
    BootConfig = [
      {term_width, 80},
      {force_one_line, false},
      {omit, []},
      {gen_server_messages, true},
      {lookup, false}
    ], %% configuration before we try loading from file

    State = #state{config=BootConfig},
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec  handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler.
%% 
%% @doc 
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%% @end 
%%--------------------------------------------------------------------
handle_event2(Symbol, Pid, Type, Message, State) -> % Message must be a string
    {ok, MessageSafe, NL} = regexp:gsub(Message, "[\n]+", " "), % collapse whitespace to one space

    Type1 =
	case Type of
	    "" -> "";
	    _  -> sformat("~p ", [Type])
	end,

    Banner     = sformat("~s ~p ~s", [Symbol, Pid, Type1]),
    BannerLen  = length(Banner),
    {Output, OutputSafe} = 
	try %% there's no way to see if Message is a string? just try
	    {sformat("~s", [Message]),
	     sformat("~s", [MessageSafe])}
	catch X -> why_doesnt_this_work
	end,

    Length =
	case (length(OutputSafe) + BannerLen) < ?CONFIG(term_width) of
	    true  -> short;
	    false -> long
	end,

    OneLine =
	case NL == 0 of
	    true  -> oneliner;
	    false -> multiline
	end,

    case { ?CONFIG(force_one_line), Length, OneLine } of
	%% one line and short ... print as is
	{_, short, oneliner} ->
	    format("~s~s~n", [Banner, OutputSafe]);

	%% too long ... squash to one
	{true, long, _} ->
	    O = Banner ++ OutputSafe,
	    Format = sformat("~~~ps >~n", [?CONFIG(term_width)-2]), % e.g. "~80s >~n"
	    format(Format, [O]);

	%% short but multiline... collapse to one
	{true, short, multiline} ->
	    format("~s~s~n", [Banner, OutputSafe]);

	%% just print it
	_ ->
	    format("~s~n~s~n~n", [Banner, Output])
    end.

%%
handle_event1({What, _Gleader, {Ref, Format, Data}}, State) when is_list(Format) ->
    Symbol = case What of
	error       -> "!!";
	warning_msg -> "**";
	info_msg    -> "..";
	_Else       -> "??"
    end,

    case Format of
	"** Generic server ~p terminating \n** Last message in was ~p~n** When Server state == ~p~n** Reason for termination == ~n** ~p~n" ->
	    %% v- Pid is a pattern match, not a bind
	    [Ref, LastMessage, Obj, Reason] = Data,

	    %% TODO: move as much logic as possible out of thrift_logger
	    Ignore =
		begin
		    is_tuple(Reason) andalso
			size(Reason) >= 1 andalso element(1, Reason) == timeout
		end
		orelse
		begin
		    case thrift_utils:unnest_record(Reason, tTransportException) of
			error -> false;
			{ok, TTE} ->
			    oop:get(TTE, type) == ?tTransportException_NOT_OPEN andalso
				oop:get(TTE, message) == "in tSocket:read/2: gen_tcp:recv"
		    end
		end,

	    case Ignore of
		true ->
		    ok;
		false ->
		    Format1 = "** gen_server terminating in message ~p~n** State  = ~s~n** Reason = ~s~n",
		    Message = sformat(Format1, [LastMessage, oop:inspect(Obj), oop:inspect(Reason)]), %% TODO(cpiro): hope Reason is an object?
		    handle_event2(Symbol, Ref, "", Message, State)
	    end;
	_ ->
	    Message = sformat(Format, Data),
	    handle_event2(Symbol, Ref, "", Message, State)
    end,
    {ok, State};

handle_event1({What, _Gleader, {Pid, Type, Report}}, State) ->
    Symbol = case What of
	error_report   -> "!!";
	warning_report -> "**";
	info_report    -> "..";
	_Else          -> "??"
    end,

    case Type of
	{thrift_info, TI} ->
	    %% should we show it?
	    case not lists:member(TI, ?CONFIG(omit)) of
		true ->
		    Message = handle_thrift_info(TI, Report, State),
		    handle_event2(Symbol, Pid, "", Message, State);
		false ->
		    ok
	    end;
	crash_report ->
	    %% [Cruft|_] = Report,									      %%
	    %% {value, {_, Reason}} = lists:keysearch(error_info, 1, Cruft),				      %%
	    %% {value, {_, {_, _, [_,_,_,_, Obj, []]}}} = lists:keysearch(initial_call, 1, Cruft),	      %%
	    %% sformat("state == ~s~nreason ==~s", [oop:inspect(Obj), oop:inspect(Reason)]),	      %%
	    ok;
	progress ->
	    ok;

	_ ->
	    Message = sformat("|| ~s", [oop:inspect(Report)]),
	    handle_event2(Symbol, Pid, Type, Message, State)
    end,
    {ok, State};

handle_event1(_Event, State) ->
    handle_event2("??", "<?.?.?>", "", _Event, State),
    {ok, State}.

handle_event(Event, State) ->
    try
	handle_event1(Event, State)
    catch
	_:E ->
	    format("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~n error logger error:~n ~p~n Event = ~p~n State = ~p~n ~p~n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~n", 
		   [E, Event, State, erlang:get_stacktrace()]),
	    {ok, State}
    end.

%% thrift info handlers
handle_thrift_info(oop_new, {Args, Class, Object}, State) ->
    %% arg Class can't come first! Then it'd look like a Class object
    L = io_lib:format("~p:new(~s) = ~s", [Class, thrift_utils:unbrack(Args), oop:inspect(Object)]),
    lists:flatten(L);

handle_thrift_info(server_listening, {Port}, State) ->
    sformat("server listening on port ~p", [Port]);

handle_thrift_info(req_processed, {Value}, State) ->
    sformat("request: ~p", [Value]);

handle_thrift_info(conn_accepted, {AddrString}, State) ->
    sformat("connection accepted from ~s", [AddrString]);

handle_thrift_info(conn_timeout, {AddrString}, State) ->
    sformat("connection timed out from ~s", [AddrString]);

handle_thrift_info(conn_closed, {AddrString}, State) ->
    sformat("connection closed from ~s", [AddrString]);

handle_thrift_info(Else, Report, State) ->
    sformat("~p ~s", [Else, oop:inspect(Report)]).

%%--------------------------------------------------------------------
%% @spec handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}.
%% 
%% @doc 
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%% @end 
%%--------------------------------------------------------------------
handle_call({reconfig, Amendments}, State) ->
    {OkOrError, State1} = reconfig1(State, Amendments),
    format(".. reconfig was ~p~n", [OkOrError]),
    {ok, OkOrError, State1};
    
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler.
%% 
%% @doc 
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void().
%% 
%% @doc 
%% Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%% @end 
%%--------------------------------------------------------------------
terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    format("*****************~n~n  frick, error logger terminating: ~p~n~n*****************~n~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}. 
%% 
%% @doc 
%% Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%%% Internal functions
%%====================================================================

%% how to output
format(Format, Data) ->
    io:format(Format, Data).

%% convenience
sformat(Format, Data) ->
    thrift_utils:sformat(Format, Data).

reconfig1(State, Amendments) ->
    case thrift:config(thrift_logger) of
	{value, Config} ->
	    Config1     = lists:keysort(1, Config),
	    Amendments1 = lists:keysort(1, Amendments),
	    Config2     = lists:keymerge(1, Amendments1, Config1),

	    State1 = State#state{config=Config2},
	    {ok, State1};
	_ -> 
	    {error, State}
    end.

config(Item, State) ->
    case thrift:config(Item, State#state.config) of
	{value, V} ->
	    V;
	Else ->
	    ?ERROR("config for ~p is unavailable: ~p", [Item, Else])
    end.
