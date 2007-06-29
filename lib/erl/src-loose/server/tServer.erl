-module(tServer).

-include("thrift/thrift.hrl").
-include("thrift/protocol/tProtocol.hrl").
-include("thrift/protocol/tBinaryProtocol.hrl").
% -include("thrift/transport/tTransport.hrl").
-include("tServer.hrl").

-export([new/3, serve/1]).

% now processor is the module with process_*, not an object

new(ProcessorModule, HandlerModule, ServerTransport) ->
    #tServer{processorModule=ProcessorModule, 
	     handlerModule=HandlerModule,
	     serverTransport=ServerTransport}.

serverTransport(This) ->
    This#tServer.serverTransport.

serve(This) ->
    ST1 = ?M0(serverTransport(This), listen_MUTABLE),
    This1 = This#tServer{serverTransport=ST1},
    serve_loop(This1).

processorModule(This) ->
    This#tServer.processorModule.

handlerModule(This) ->
    This#tServer.handlerModule.

serve_loop(This) ->
    io:format("~nready.~n", []),
    Client = ?M0(serverTransport(This), accept_MUTABLE),
    This1  = This#tServer{serverTransport=Client},

    Trans  = Client, % factory
    Prot   = tBinaryProtocol:new(Trans),
    serve_loop_loop(This1, Prot), % giggle loop?
    ?M0(Trans, close_MUTABLE), % don't "assign" ... discard
    serve_loop(This).
    
serve_loop_loop(This, Prot) ->
    Next = try
	Val = (processorModule(This)):process(handlerModule(This), Prot, Prot),
	io:format("request processed: rv=~p~n", [Val]),
	loop
    catch 
	%% TODO(cpiro) case when is_record(...) to pick out our exception
	%% records vs. normal erlang throws
	{tTransportException,_,_} ->
	    io:format("tTransportException (normal-ish?)~n", []),
	    close;
	E ->
	    io:format("EXCEPTION: ~p~n", [E]),
	    close
    end,
    case Next of 
	loop -> serve_loop_loop(This, Prot);
	close -> ok
    end.

