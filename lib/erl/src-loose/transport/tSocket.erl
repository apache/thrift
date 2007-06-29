-module(tSocket).

-include("thrift/thrift.hrl").
-include("thrift/transport/tTransportException.hrl").
% -include("thrift/transport/tTransport.hrl").
-include("thrift/transport/tSocket.hrl").

-export([new/0, new/1, new/2, setHandle_MUTABLE/2, open_MUTABLE/1, isOpen/1, write/2, read/2, close_MUTABLE/1, readAll/2]).

new(Host, Port) ->
    #tSocket{host=Host, port=Port, handle=nil}. % WATCH

new()     -> new("localhost", 9090).
new(Host) -> new(Host, 9090).
    
setHandle_MUTABLE(This, Handle) ->
    This#tSocket{handle=Handle}.

open_MUTABLE(This) -> 
    Host = This#tSocket.host,
    Port = This#tSocket.port,
    Options = [],

    case gen_tcp:connect(Host, Port, Options) of
	{error, _} ->
	    throw(tTransportException:new(
		    ?tTransportException_NOT_OPEN,
		    "Could not connect to " ++ Host ++ ":" ++ Port)
		 ),
	    {error, This}; % cpiro not reached?
	{ok, Socket} ->
	    {ok, This#tSocket{handle=Socket}}
    end.

handle(This) ->
    This#tSocket.handle.

isOpen(This) ->
    handle(This) /= nil.

write(This, Str) ->
    Val = gen_tcp:send(handle(This), Str),

    %% io:format("WRITE |~p|(~p)~n", [Str,Val]),
    
    case Val of
	{error, _} ->
	    throw(tTransportException:new(?tTransportException_NOT_OPEN, "in write"));
	ok ->
	    ok
    end.

read(This, Sz) ->
    case gen_tcp:recv(handle(This), Sz) of
	{ok, []} ->
	    { Host, Port } = { This#tSocket.host, This#tSocket.port },
	    throw(tTransportException:new(?tTransportException_UNKNOWN, "TSocket: Could not read " ++ Sz ++ "bytes from " ++ Host ++ ":" ++ Port));
	{ok, Data} ->
	    Data;
	{error, Error} ->
	    io:format("in tSocket:read/2: gen_tcp:recv(~p, ~p) => {error, ~p}~n",
		      [handle(This), Sz, Error]),
	    throw(tTransportException:new(?tTransportException_NOT_OPEN, "in tSocket:read/2: gen_tcp:recv"))
	end.
	    
close_MUTABLE(This) ->
    if
	This#tSocket.handle == nil ->
	    This;
	true ->
	    gen_tcp:close(handle(This)),
	    This#tSocket{handle=nil}
    end.

readAll(This, Sz) ->
    readAll_loop(This, Sz, "", 0).

readAll_loop(This, Sz, Buff, Have) ->
    if 
	Have < Sz ->
	    Chunk = ?M1(This, read, Sz - Have),

	    %% man gen_tcp:
	    %% exactly Length bytes are returned, or an error; 
	    %% possibly discarding less than Length bytes of data when 
	    %% the socket gets closed from the other side.

	    %% io:format("READ |~p|~n", [Chunk]),

	    Have1 = Have + (Sz-Have), % length(Chunk)
	    Buff1 = Buff ++ Chunk, % TODO: ++ efficiency?
	    readAll_loop(This, Sz, Buff1, Have1);
	true ->
	    Buff
    end.

