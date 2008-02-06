%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift_app).

-export([start/2, stop/1]).
-behaviour(application).

-include("thrift.hrl").

%%%
%%% behavior definition
%%%

start(_Type, _StartArgs) ->
    io:format("starting thrift~n"),
    thrift_logger:install(),
    {ok, Sup} = thrift_app_sup:start_link(),
    {ok, Sup}.

stop(_State) ->
    ok.
