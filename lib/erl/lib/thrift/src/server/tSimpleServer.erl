%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%% 
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tSimpleServer).

-include("oop.hrl").

-include("thrift.hrl").
-include("transport/tTransportException.hrl").
-include("server/tSimpleServer.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/5, new/4, new/3, serve/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(super).
   
%%%
%%% behavior callbacks
%%%
 
%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    tServer.

%%% inspect(This) -> string()

inspect(_This) ->
    "".

%%%
%%% class methods
%%%

new(Handler, Processor, ServerTransport, TransportFactory, ProtocolFactory) ->
    Super = (super()):new(Handler, Processor, ServerTransport, TransportFactory, ProtocolFactory),
    #?MODULE{super=Super}.

new(Handler, Processor, ServerTransport) ->
    new(Handler, Processor, ServerTransport, nil, nil).

new(Handler, Processor, ServerTransport, TransportFactory) ->
    new(Handler, Processor, ServerTransport, TransportFactory, nil).

%

serve(This) ->
    ST = oop:get(This, serverTransport),
    ?R0(ST, effectful_listen),

    serve_loop(This).

serve_loop(This) ->
    io:format("~nready.~n", []),

    ST     = oop:get(This, serverTransport),
    Client = ?RT0(ST, accept, infinity),

    TF     = oop:get(This, transportFactory),
    Trans  = ?F1(TF, getTransport, Client), %% cpiro: OPAQUE!! Trans = Client

    PF     = oop:get(This, protocolFactory),
    Prot   = ?F1(PF, getProtocol, Trans), %% cpiro: OPAQUE!! Prot = start_new(tBinaryProtocol, [Trans])

    io:format("client accept()ed~n", []),

    serve_loop_loop(This, Prot), % giggle loop?

    ?R0(Trans, effectful_close),

    serve_loop(This).
    
serve_loop_loop(This, Prot) ->
    Next = 
	try
	    Handler   = oop:get(This, handler),
	    Processor = oop:get(This, processor),
	    Val = apply(Processor, process, [Handler, Prot, Prot]), %% TODO(cpiro): make processor a gen_server instance
	    io:format("request processed: rv=~p~n", [Val]),
	    loop
	catch 
	    %% TODO(cpiro) case when is_record(...) to pick out our exception
	    %% records vs. normal erlang throws
	    E when is_record(E, tTransportException) ->
		io:format("tTransportException (normal-ish?)~n", []),
		close;
	    F ->
		io:format("EXCEPTION: ~p~n", [F]),
		close
	end,
    case Next of 
	loop -> serve_loop_loop(This, Prot);
	close -> ok
    end.
