-module(tServerSocket).
-include("tServerSocket.hrl").

-export([new/1, listen_MUTABLE/1, accept_MUTABLE/1, close/1]).

new(Port) ->
    #tServerSocket{port=Port, handle=nil}.

listen_MUTABLE(This) ->
    Port = This#tServerSocket.port,
    Options = [binary, {packet, 0}, {active, false}], % was []

    case gen_tcp:listen(Port, Options) of 
	{ok, ListenSocket} ->
	    This#tServerSocket{handle=ListenSocket}
	% {error, _} -> 
	% TODO: no error handling in Ruby version?
    end.

accept_MUTABLE(This) ->
    if 
	This#tServerSocket.handle /= nil ->
	    case gen_tcp:accept(This#tServerSocket.handle) of
		{ok, Socket} ->
		    tSocket:setHandle_MUTABLE( tSocket:new(), Socket )
	        % {error, _} -> 
                % TODO: no error handling in Ruby version?
	    end;
	true ->
	    nil
    end.

close(This) ->
    if 
 	This#tServerSocket.handle /= nil ->
	    case gen_tcp:close(This#tServerSocket.handle) of 
		ok ->
		    ok
	        % {error, _} -> 
                % TODO: no error handling in Ruby version?
	    end;
	true ->
	    ok
    end.
