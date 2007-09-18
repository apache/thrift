%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tBinaryProtocolFactory).

-include("oop.hrl").

-include("thrift.hrl").
-include("protocol/tBinaryProtocol.hrl").
-include("protocol/tBinaryProtocolFactory.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, getProtocol/2]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tProtocolFactory.

%%% inspect(This) -> string()

inspect(_This) ->
    "".

%%%
%%% class methods
%%%

new() ->
    Super = (super()):new(),
    #?MODULE{super=Super}.

%%%
%%% instance methods
%%%

getProtocol(_This, Trans) ->
    oop:start_new(tBinaryProtocol, [Trans]).

