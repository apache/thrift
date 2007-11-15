%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tServerSocket).

-include("oop.hrl").
-include("thrift.hrl").
-include("transport/tServerSocket.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/1, effectful_listen/1, accept/1, effectful_close/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(port);
?DEFINE_ATTR(handle).

%%%
%%% behavior callbacks
%%%

%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tServerTransport.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(port) ++ ", " ++
    ?FORMAT_ATTR(handle).

%%%
%%% class methods
%%%

new(Port) ->
    Super = (super()):new(),
    #?MODULE{super = Super, port = Port, handle = nil}.

%%%
%%% instance methods
%%%

effectful_listen(This) ->
    Port = oop:get(This, port),
    Options = [binary, {packet, 0}, {active, false}], % was []

    case gen_tcp:listen(Port, Options) of
        {ok, ListenSocket} ->
            This1 = oop:set(This, handle, ListenSocket),
            {ok, This1}

            %% {error, _} ->
            %% TODO: no error handling in Ruby version?
    end.

accept(This) ->
    case oop:get(This, handle) of
        nil ->
            nil; % cpiro: sic the Ruby code

        Handle ->
            case gen_tcp:accept(Handle) of
                {ok, Sock} ->
                    Trans = oop:start_new(tSocket, []),
                    ?R1(Trans, effectful_setHandle, Sock),
                    Trans
                %% {error, _} ->
                %% TODO: no error handling in Ruby version?
            end
    end.

effectful_close(This) ->
    case oop:get(This, handle) of
        nil ->
            {nil, This};
        Handle ->
            case gen_tcp:close(Handle) of
                ok ->
                    {ok, This} % cpiro: sic the Ruby version: don't set handle to nil
                %% {error, _} ->
                %% TODO: no error handling in Ruby version?
            end
    end.
