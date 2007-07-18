%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tProtocolFactory).

-include("oop.hrl").

-include("thrift.hrl").
-include("protocol/tProtocolFactory.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, getProtocol/2]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?ATTR_DUMMY.

%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    none.

%%% inspect(This) -> string()

inspect(_This) ->
    "".

%%%
%%% class methods
%%%

new() ->
    #?MODULE{}.

%%%
%%% instance methods
%%%

getProtocol(This, Trans) ->
    nil.
