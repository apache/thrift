-module(tBufferedTransport).

-include("thrift/thrift.hrl").
-include("thrift/transport/tBufferedTransport.hrl").

-export([new/1, isOpen/1, open/1, close/1, read/2, write_MUTABLE/2, flush_MUTABLE/1]).

new(Transport) ->
    #tBufferedTransport{transport=Transport, wbuf=""}.

transport(This) -> % local accessor
    This#tBufferedTransport.transport.

isOpen(This) ->
    ?M0(transport(This), isOpen).

open(This) ->
    ?M0(transport(This), open).

close(This) ->
    ?M0(transport(This), close).

read(This, Sz) ->
    ?M1(transport(This), read, Sz).

write_MUTABLE(This, Buf) -> % be sure to rebind This to the retval
    Wbuf = This#tBufferedTransport.wbuf,
    This#tBufferedTransport{wbuf=Wbuf++Buf}. % TODO: ++ efficiency?

flush_MUTABLE(This) -> % be sure to rebind This to the retval
    Wbuf = This#tBufferedTransport.wbuf,
    ?M1(transport(This), write, Wbuf),
    ?M0(transport(This), flush),
    This#tBufferedTransport{wbuf=""}. % TODO: ++ efficiency?
