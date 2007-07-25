-module(server).

-include("thrift.hrl").
-include("transport/tSocket.hrl").
-include("protocol/tBinaryProtocol.hrl").

-include("server/tErlServer.hrl").
-include("transport/tErlAcceptor.hrl").

-include("calculator.hrl").


-export([start/0, start/1, stop/1, ping/0, add/2, calculate/2, getStruct/1, zip/0]).

ping() ->
    io:format("ping()~n",[]),
    nil.

add(N1, N2) ->
    io:format("add(~p,~p)~n",[N1,N2]),
    N1+N2.

calculate(Logid, Work) ->
    { Op, Num1, Num2 } = { Work#work.op, Work#work.num1, Work#work.num2 },
    io:format("calculate(~p, {~p,~p,~p})~n", [Logid, Op, Num1, Num2]),
    case Op of
        ?tutorial_ADD      -> Num1 + Num2;
	?tutorial_SUBTRACT -> Num1 - Num2;
	?tutorial_MULTIPLY -> Num1 * Num2;

	?tutorial_DIVIDE when Num2 == 0 ->
	    throw(#invalidOperation{what=Op, why="Cannot divide by 0"});
	?tutorial_DIVIDE ->
	    Num1 div Num2;

	_Else ->
	    throw(#invalidOperation{what=Op, why="Invalid operation"})

    end.

getStruct(Key) ->
    io:format("getStruct(~p)~n", [Key]),
    #sharedStruct{key=Key, value="RARG"}.

zip() ->
    io:format("zip~n").

%%

start() ->
    start(9090).

start(Port) ->
    Handler   = ?MODULE,
    Processor = calculator,

    TF = tBufferedTransportFactory:new(),
    PF = tBinaryProtocolFactory:new(),

    ServerTransport = tErlAcceptor,
    ServerFlavor    = tErlServer,

    Server = oop:start_new(ServerFlavor, [Port, Handler, Processor, ServerTransport, TF, PF]),

    ?R0(Server, effectful_serve),

    Server.

stop(Server) ->
    ?C0(Server, stop),
    ok.
