%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tTransportException).

-include("oop.hrl").

-include("thrift.hrl").
-include("transport/tTransportException.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, new/1, new/2]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(type).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tException.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(type).

%%%
%%% class methods
%%%

new(Type, Message) ->
    Super = (super()):new(Message),
    #?MODULE{super=Super, type=Type}.

new() ->
    new(?tTransportException_UNKNOWN, undefined).
new(Type) ->
    new(Type, undefined).

%%%
%%% instance methods
%%%
