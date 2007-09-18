%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(thrift).

-export([start/2, shutdown/0, stop/1]).
-behaviour(application).

-include("thrift.hrl").

%%%
%%% behavior definition
%%%

start(Type, StartArgs) ->
    ok.

shutdown() ->
    application:stop(?MODULE).

stop(_State) ->
    ok.
