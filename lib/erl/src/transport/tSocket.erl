%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tSocket).

-include("oop.hrl").

-include("thrift.hrl").
-include("transport/tTransportException.hrl").
% -include("transport/tTransport.hrl").
-include("transport/tSocket.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/0, new/1, new/2,
	 effectful_setHandle/2, effectful_open/1,
	 isOpen/1, effectful_write/2, read/2, effectful_close/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super);
?DEFINE_ATTR(host);
?DEFINE_ATTR(port);
?DEFINE_ATTR(handle).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tTransport.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(host) ++ ", " ++
    ?FORMAT_ATTR(port) ++ ", " ++
    ?FORMAT_ATTR(handle).

%%%
%%% class methods
%%%

new(Host, Port) ->
    Super = (super()):new(),
    #?MODULE{super=Super, host=Host, port=Port, handle=nil}.

new(Host) ->
    new(Host, 9090).

new() ->
    new("localhost", 9090).
    
%%%
%%% instance methods
%%%

effectful_setHandle(This, Handle) ->
    {ok, oop:set(This, handle, Handle)}.

effectful_open(This) -> 
    Host = oop:get(This, host),
    Port = oop:get(This, port),
    Options = [],

    case gen_tcp:connect(Host, Port, Options) of
	{error, _} ->
	    exit(tTransportException:new(
		   ?tTransportException_NOT_OPEN,
		   "Could not connect to " ++ Host ++ ":" ++ Port)
		 );
	{ok, Socket} ->
	    {ok, oop:set(This, handle, Socket)}
    end.

isOpen(This) ->
    oop:get(This, handle) /= nil.

effectful_write(This, Str) ->
    Handle = oop:get(This, handle),
    Val = gen_tcp:send(Handle, Str),

    %% DEBUG
    %% error_logger:info_msg("tSocket: wrote ~p~n", [Str]),

    %% error_logger:info_msg("WRITE |~p| (~p)", [Str,Val]),
    
    case Val of
	{error, _} ->
	    throw(tTransportException:new(?tTransportException_NOT_OPEN, "in write"));
	ok ->
	    {ok, This}
    end.

read(This, Sz) ->
    Handle = oop:get(This, handle),
    case gen_tcp:recv(Handle, Sz) of
	{ok, []} ->
	    Host = oop:get(This, host),
	    Port = oop:get(This, port),
	    throw(tTransportException:new(?tTransportException_UNKNOWN, "TSocket: Could not read " ++ Sz ++ "bytes from " ++ Host ++ ":" ++ Port));
	{ok, Data} ->
	    %% DEBUG
	    %% io:format("tSocket: read ~p~n", [Data]),
	    Data;
	{error, Error} ->
	    exit(tTransportException:new(?tTransportException_NOT_OPEN, "in tSocket:read/2: gen_tcp:recv"))
	end.
	    
effectful_close(This) ->
    case oop:get(This, handle) of
	nil ->
	    {ok, This};
	Handle -> 
	    gen_tcp:close(Handle),
	    {ok, oop:set(This, handle, nil)}
    end.
