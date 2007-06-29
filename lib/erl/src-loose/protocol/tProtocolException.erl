-module(tProtocolException).
-include("tProtocolException.hrl").
-export([new/2, new/1, new/0]).

new(Type, Message) ->
    #tProtocolException{type=Type, message=Message}.

new(Type) -> new(Type, nil).
new()     -> new(?tProtocolException_UNKNOWN, nil).
