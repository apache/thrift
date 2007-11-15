%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tBufferedTransportFactory).

-include("oop.hrl").
-include("transport/tBufferedTransport.hrl").
-include("transport/tBufferedTransportFactory.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, getTransport/2]).

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
    tTransportFactory.

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

getTransport(_This, Trans) ->
    oop:start_new(tBufferedTransport, [Trans]).
