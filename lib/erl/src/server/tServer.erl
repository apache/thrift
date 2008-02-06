%%% Copyright (c) 2007- Facebook
%%% Distributed under the Thrift Software License
%%%
%%% See accompanying file LICENSE or visit the Thrift site at:
%%% http://developers.facebook.com/thrift/

-module(tServer).

-include("oop.hrl").

-include("thrift.hrl").
-include("server/tServer.hrl").
-include("transport/tTransportFactory.hrl").
-include("protocol/tBinaryProtocolFactory.hrl").

-behavior(oop).

-export([attr/4, super/0, inspect/1]).

-export([new/5, new/4, new/3, serve/1]).

%%%
%%% define attributes
%%% 'super' is required unless ?MODULE is a base class
%%%

?DEFINE_ATTR(handler);
?DEFINE_ATTR(processor);
?DEFINE_ATTR(serverTransport);
?DEFINE_ATTR(transportFactory);
?DEFINE_ATTR(protocolFactory).

%%%
%%% behavior callbacks
%%%

%%% super() -> SuperModule = atom()
%%%             |  none

super() ->
    none.

%%% inspect(This) -> string()

inspect(This) ->
    ?FORMAT_ATTR(handler) ++ ", " ++
    ?FORMAT_ATTR(processor) ++ ", " ++
    ?FORMAT_ATTR(serverTransport) ++ ", " ++
    ?FORMAT_ATTR(transportFactory) ++ ", " ++
    ?FORMAT_ATTR(protocolFactory).

%%%
%%% class methods
%%%

new(Handler, Processor, ServerTransport, TransportFactory, ProtocolFactory) ->
    #?MODULE{handler=Handler, processor=Processor, serverTransport=ServerTransport,

	     %% much ado about nothing but
	     %% subclasses pass nil too
	     transportFactory =
	     case TransportFactory of
		 nil -> tTransportFactory:new();
		 _   -> TransportFactory
	     end,
	
	     protocolFactory =
	     case ProtocolFactory of
		 nil -> tBinaryProtocolFactory:new();
		 _   -> ProtocolFactory
	     end
}.

new(Handler, Processor, ServerTransport) ->
    new(Handler, Processor, ServerTransport, nil, nil).

new(Handler, Processor, ServerTransport, TransportFactory) ->
    new(Handler, Processor, ServerTransport, TransportFactory, nil).

serve(_This) ->
    ok.
