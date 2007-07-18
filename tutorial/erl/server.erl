-module(server).

-include("thrift.hrl").
-include("transport/tSocket.hrl").
-include("protocol/tBinaryProtocol.hrl").

-include("server/tErlServer.hrl").
-include("transport/tErlAcceptor.hrl").

-include("calculator.hrl").

-export([start/0, stop/1, ping/0, add/2, calculate/2, getStruct/1, zip/0]).

ping() ->
    io:format("ping()~n",[]),
    {ok, nil}.

add(N1, N2) ->
    io:format("add(~p,~p)~n",[N1,N2]),
    {ok, N1+N2}.

calculate(Logid, Work) ->
    { Op, Num1, Num2 } = { Work#work.op, Work#work.num1, Work#work.num2 },
    io:format("calculate(~p, {~p,~p,~p})~n", [Logid, Op, Num1, Num2]),
    case Op of
        ?ADD      -> {ok, Num1 + Num2};
	?SUBTRACT -> {ok, Num1 - Num2};
	?MULTIPLY -> {ok, Num1 * Num2};
	?DIVIDE ->
	    if 	Num2 == 0 -> {error, #invalidOperation{what=Op, why="Cannot divide by 0"}};
		true      -> {ok, Num1 / Num2}
	    end;
	true ->
	    {error, #invalidOperation{what=Op, why="Invalid operation"}}
    end.

getStruct(Key) ->
    io:format("getStruct(~p)~n", [Key]),
    {ok, get(Key)}.

zip() ->
    io:format("zip~n").

%%

start() ->
    Handler   = ?MODULE, % cpiro: or generated handler?
    Processor = calculator,
    Port      = 9090,

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

