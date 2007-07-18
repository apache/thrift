%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tException).

-include("oop.hrl").
-include("tException.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(message).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    none.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(message).

%%%
%%% class methods
%%%

new(Message) ->
    #?MODULE{message=Message}.

%%%
%%% instance methods
%%%

