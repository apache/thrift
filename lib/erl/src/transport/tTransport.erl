%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tTransport).

-include("oop.hrl").

-include("thrift.hrl").
-include("transport/tTransport.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, isOpen/1, effectful_open/1, effectful_close/1, read/2, readAll/2, effectful_write/2, effectful_flush/1]).

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

e() ->
    exit('tTransport is abstract').

isOpen(_This) ->
    e(),
    nil.

effectful_open(This) ->
    e(),
    {nil, This}.

effectful_close(This) ->
    e(),
    {nil, This}.

read(_This, _Sz) ->
    e(),
    nil.

readAll(This, Sz) ->
    readAll_loop(This, Sz, "", 0).

readAll_loop(This, Sz, Buff, Have) ->
    if
        Have < Sz ->
            Chunk = ?L1(read, Sz - Have),

            %% man gen_tcp:
            %% exactly Length bytes are returned, or an error;
            %% possibly discarding less than Length bytes of data when
            %% the socket gets closed from the other side.

            %% error_logger:info_msg("READ |~p|", [Chunk]),

            Have1 = Have + (Sz-Have), % length(Chunk)
            Buff1 = Buff ++ Chunk, % TODO: ++ efficiency?
            readAll_loop(This, Sz, Buff1, Have1);
        true ->
            Buff
    end.

effectful_write(This, _Buf) ->
    e(),
    {nil, This}.

effectful_flush(This) ->
    {nil, This}.
