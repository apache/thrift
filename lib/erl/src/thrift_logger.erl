%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift_logger).

-behaviour(gen_event).

-include("thrift.hrl").
-include("oop.hrl").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-export([install/0]).

%%

-record(state, {}).

-define(GS_TERM_FORMAT, "** Generic server ~p terminating \n** Last message in was ~p~n** When Server state == ~p~n** Reason for termination == ~n** ~p~n").

%%%
%%% ensure the regular logger is out and ours is in
%%%

install() ->
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

    ok.

%%%
%%% init
%%%

init([]) ->
    State = #state{},
    {ok, State}.

%%%
%%% handle_event
%%%

handle_event2(Symbol, Pid, Type, Message, State) -> % Message must be a string
    {ok, MessageSafe, NL} = regexp:gsub(Message, "[\n]+", " "), % collapse whitespace to one space

    Type1 =
        case Type of
            "" -> "";
            _  -> sformat("~p ", [Type])
        end,

    Banner =
        case config(show_pid) of
            true ->
                sformat("~s ~p ~s", [Symbol, Pid, Type1]);
            false ->
                sformat("~s~i ~s", [Symbol, Pid, Type1])
        end,
    BannerLen = length(Banner),

    %% there's no way to see if Message is a string? just try
    Output = sformat("~s", [Message]),
    OutputSafe = sformat("~s", [MessageSafe]),

    Length =
        case (length(OutputSafe) + BannerLen) < config(term_width) of
            true  -> short;
            false -> long
        end,

    OneLine =
        case NL == 0 of
            true  -> oneliner;
            false -> multiline
        end,

    case { config(force_one_line), Length, OneLine } of
        %% one line and short ... print as is
        {_, short, oneliner} ->
            format("~s~s~n", [Banner, OutputSafe]);

        %% too long ... squash to one
        {true, long, _} ->
            O = Banner ++ OutputSafe,
            Format = sformat("~~~ps >~n", [config(term_width)-2]), % e.g. "~80s >~n"
            format(Format, [O]);

        %% short but multiline... collapse to one
        {true, short, multiline} ->
            format("~s~s~n", [Banner, OutputSafe]);

        %% just print it
        _ ->
            format("~s~n~s~n~n", [Banner, Output])
    end.

%%

bin_trim(L) when is_list(L) ->
    lists:map(fun bin_trim/1, L);
bin_trim(T) when is_tuple(T) ->
    list_to_tuple(bin_trim(tuple_to_list(T)));
bin_trim(Bin) when is_binary(Bin), size(Bin) > 100 ->
    {Bin1,Rest} = split_binary(Bin, 100),
    Bin1;
bin_trim(Term) ->
    Term.

handle_event1({What, _Gleader, {Ref, Format, Data}}, State) when is_list(Format) ->
    Symbol =
        case What of
            error       -> "!!";
            warning_msg -> "**";
            info_msg    -> "..";
            _Else       -> "??"
        end,

    case {Format, Data} of
        {?GS_TERM_FORMAT, [Ref, LastMessage, Obj, {Kind, E}]} when Kind == timeout; Kind == thrift_exception ->
            ok;

        {?GS_TERM_FORMAT, [Ref, LastMessage, Obj, Reason]} ->
            Format1 = "** gen_server terminating in message ~p~n** State  = ~s~n** Reason = ~p~n",
            Message = sformat(Format1, [LastMessage, bin_trim(oop:inspect(Obj)), Reason]),
            handle_event2(Symbol, Ref, "", Message, State);

        {?GS_TERM_FORMAT, _Dta} ->
            TrimData = bin_trim(Data),
            Message = sformat("DATA DIDN'T MATCH: ~p~n", [TrimData]) ++ sformat(Format, TrimData),
            handle_event2(Symbol, Ref, "", Message, State);
        {_, _} ->
            case lists:member(Format, config(omit_fmt)) of
                false ->
                    Message = sformat(Format, bin_trim(Data)),
                    handle_event2(Symbol, Ref, "", Message, State);
                true ->
                    ok
            end
    end,
    {ok, State};

handle_event1({What, _Gleader, {Pid, Type, Report}}, State) ->
    Symbol =
        case What of
            error_report   -> "!!";
            warning_report -> "**";
            info_report    -> "..";
            _Else          -> "??"
        end,

    case Type of
        crash_report ->
            print_crash_report(Report);
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

%%%
%%% call, info, terminate, code_change
%%%

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(normal, _State) ->
    ok;
terminate(Reason, _State) ->
    format("*****************~n~n  frick, error logger terminating: ~p~n~n*****************~n~n", [Reason]),
    ok.

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

config(Item) ->
    thrift:config(Item).

print_crash_report(Report) ->
    case Report of
        %% for R12B0
        [[_, _, {error_info, {exit, {thrift_exception, _}, _}} | _] | _] ->
            ok;
        [[_, _, {error_info, {exit, {timeout, _}, _}} | _] | _]  ->
            ok;

        %% for R11B5
        [[_,_,{error_info, {thrift_exception, _}}|_] | _]  ->
            ok;
        [[_,_,{error_info, {timeout, _}}|_] | _]  ->
            ok;

        %% else
        _ ->
            io:format("~~~~ crash report: ~w~n", [Report])
    end.
