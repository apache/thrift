%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tBufferedTransport).

-include("oop.hrl").

-include("thrift.hrl").
-include("transport/tBufferedTransport.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/1, isOpen/1, open/1, close/1, read/2, effectful_write/2, effectful_flush/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(transport);
?DEFINE_ATTR(wbuf).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tTransport.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(transport) ++
    ?FORMAT_ATTR(wbuf).

%%%
%%% class methods
%%%

new(Transport) ->
    Super = (super()):new(),
    #?MODULE{super=Super, transport=Transport, wbuf=""}.

%%%
%%% instance methods
%%%

isOpen(This) ->
    Transport = oop:get(This, transport),
    ?R0(Transport, isOpen).

open(This) ->
    Transport = oop:get(This, transport),
    ?R0(Transport, open).

close(This) ->
    Transport = oop:get(This, transport),
    ?R0(Transport, close).

read(This, Sz) ->
    Transport = oop:get(This, transport),
    ?R1(Transport, read, Sz).

effectful_write(This, Buf) -> % be sure to rebind This to the retval
    Wbuf = oop:get(This, wbuf),
    This1 = oop:set(This, wbuf, Wbuf++Buf), % TODO: ++ efficiency?
    {ok, This1}.

effectful_flush(This) ->
    Wbuf = oop:get(This, wbuf),
    Transport = oop:get(This, transport),    
    ?R1(Transport, effectful_write, Wbuf),
    ?R0(Transport, effectful_flush),
    This1 = oop:set(This, wbuf, ""),
    {ok, This1}.
