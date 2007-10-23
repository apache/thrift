%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift).

-export([start/0, stop/0, config/1, config/2, reconfig/1]).

-include("thrift.hrl").

%%%
%%% behavior definition
%%%

start() ->
    application:start(thrift).

stop() ->
    application:stop(thrift).

%%%
%%% configuration
%%%

config(Item) ->
    config(?MODULE, Item).

config(App, Item) ->
    case application:get_env(App, Item) of
        {ok, Value} ->
            Value;
        undefined ->
            ?ERROR("configuration for ~p is unavailable", [Item]),
            unconfigured_item,
            exit({missing_config, App, Item})
    end.

reconfig(Config) ->
    BFName = filename:basename(Config, ".config"),
    FName  = filename:join(filename:dirname(Config),
                           BFName ++ ".config"),

    case file:consult(FName) of
	{error, R={_,_,_}} ->
	    {error, file_error, file:format_error(R)};
	{error, Posix} ->
	    {error, file_error, Posix};
	{ok, [List]} when is_list(List) ->
            reconfig1(List)
    end.

reconfig1([]) ->
    ok;
reconfig1([{App, List}|Tl]) ->
    reconfig2(List, App, 0),
    reconfig1(Tl).

reconfig2([], App, Count) ->
    ?INFO("application ~p reconfigured: ~p keys updated", [App, Count]),
    ok;
reconfig2([{Par, Val}|Tl], App, Count) ->
    application:set_env(App, Par, Val),
    reconfig2(Tl, App, Count+1).
