-module(calculatorHandler).

-include("thrift/thrift.hrl").
-include("thrift/transport/tSocket.hrl").
-include("thrift/protocol/tBinaryProtocol.hrl").
-include("thrift/server/tServer.hrl").
-include("thrift/transport/tServerSocket.hrl").

-include("gen-erl/calculator.hrl").
%-include("gen-erl/shared_types.hrl").

%-include("gen-erl/tutorial_types.hrl"). % TODO(cpiro): o rly?

-export([start/0, ping/0, add/2, calculate/2, getStruct/1, zip/0]).

%%%   def initialize()
%%%     @log = {}
%%%   end

% TODO: voids take only ok as return? 

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

start() ->
    Transport = tServerSocket:new(9090),
    Server = tServer:new(calculator, ?MODULE, Transport),
    io:format("Starting the server...~n", []),
    ?M0(Server, serve),
    io:format("done.~n", []), % won't ever reach, rookie beotch
    ok.

%%% handler = CalculatorHandler.new()
%%% processor = Calculator::Processor.new(handler)
%%% transport = TServerSocket.new(9090)
%%% transportFactory = TBufferedTransportFactory.new()
%%% server = TSimpleServer.new(processor, transport, transportFactory)
%%% 
%%% puts "Starting the server..."
%%% server.serve()
%%% puts "done."
