-module(tTransportException).

-include("thrift/thrift.hrl").
-include("thrift/transport/tTransportException.hrl").

-export([new/0, new/1, new/2, message/1]).

new(Type, Message) ->
    #tTransportException{type = Type, message = Message}.

new()     -> new(?tTransportException_UNKNOWN, nil). % WATCH
new(Type) -> new(Type, nil). % WATCH

message(This) ->
    ?ATTR(message).
