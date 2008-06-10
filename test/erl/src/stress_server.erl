-module(stress_server).

-include("thrift.hrl").

-export([start_link/1, old_start_link/1,

         handle_function/2,

         echoVoid/0,
         echoByte/1,
         echoI32/1,
         echoI64/1,
         echoString/1,
         echoList/1,
         echoSet/1,
         echoMap/1
        ]).

start_link(Port) ->
    thrift_server:start_link(Port, service_thrift, ?MODULE).

% Start the server with the old style bindings
old_start_link(Port) ->
    Handler   = ?MODULE,
    Processor = service_thrift,

    TF = tBufferedTransportFactory:new(),
    PF = tBinaryProtocolFactory:new(),

    ServerTransport = tErlAcceptor,
    ServerFlavor    = tErlServer,

    Server = oop:start_new(ServerFlavor, [Port, Handler, Processor, ServerTransport, TF, PF]),

    case ?R0(Server, effectful_serve) of
	ok    -> Server;
	Error -> Error
    end.
    

handle_function(Function, Args) ->
    case apply(?MODULE, Function, tuple_to_list(Args)) of
        ok ->
             ok;
        Else -> {reply, Else}
    end.


echoVoid() ->
    ok.
echoByte(X) ->
    X.
echoI32(X) ->
    X.
echoI64(X) ->
    X.
echoString(X) ->
    X.
echoList(X) ->
    X.
echoSet(X) ->
    X.
echoMap(X) ->
    X.
