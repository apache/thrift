%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift).

-export([start/0, stop/0, config/1, config/2]).

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

%% go to the global file by default
config(Path) ->
    config(Path, {file, ?CONFIG_FILE}).

config(Path, Source) ->
    case config1(Path, Source) of
	{error, file_error, R} ->
	    ?ERROR("error opening config ~p: ~p", [Source, R]),
	    false;
	{error, bad_item, Tuple} ->
	    ?ERROR("malformed config item ~p found at ~p in ~p", [Tuple, Path, Source]),
	    false;
	{error, not_found} ->
	    ?ERROR("config item ~p not found in ~p", [Path, Source]),
	    false;
	{value, V} ->
	    {value, V}
    end.

%% go to a file
config1(Path, {file, File}) ->
    case file:consult(File) of
	{error, R={_,_,_}} ->
	    {error, file_error, file:format_error(R)};
	{error, Posix} ->
	    {error, file_error, Posix};
	{ok, List} when is_list(List) ->
	    config1(Path, List)
    end;

%% go through a list from a file or a sublist
config1([P|Ps], List) when is_list(List) ->
    case lists:keysearch(P, 1, List) of
	{value, Tuple} when size(Tuple) == 2 ->
	    List1 = element(2, Tuple), %% either another list or, if Ps is [], the item itself
	    config1(Ps, List1);
	{value, Tuple} ->
	    {error, bad_item, Tuple};
	false ->
	    {error, not_found}
    end;
config1([], Item)     -> {value, Item};
config1(Item, Source) -> config1([Item], Source).
