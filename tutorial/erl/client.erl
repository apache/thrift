-module(client).

-include("thrift.hrl").
-include("transport/tSocket.hrl").
-include("protocol/tBinaryProtocol.hrl").

-include("calculator_thrift.hrl").

-export([go/0]).

p(X) ->
    io:format("~p~n", [X]),
    ok.

t() ->
    thrift:start(),
    Host = "dev020",
    Port = 9999,

    try
        _Sock = oop:start_new(tSocket, [Host, Port]),
        Trans = oop:start_new(tBufferedTransport, [_Sock]),
        Prot  = oop:start_new(tBinaryProtocol, [Trans]),

        ?R0(Trans, effectful_open),

        Client = calculator_thrift:new(Prot),

        calculator_thrift:ping(Client),
        io:format("ping~n", []),

        Sum = calculator_thrift:add(Client, 1, 1),
        io:format("1+1=~p~n", [Sum]),

        Sum1 = calculator_thrift:add(Client, 1, 4),
        io:format("1+4=~p~n", [Sum1]),

        Work = #work{op=?tutorial_SUBTRACT,
                     num1=15,
                     num2=10},
        Diff = calculator_thrift:calculate(Client, 1, Work),
        io:format("15-10=~p~n", [Diff]),

        %% xxx inheritance doesn't work
        %% Log = sharedService_thrift:getStruct(Client, 1),
        %% io:format("Log: ~p~n", [Log]),

        %% xxx neither do exceptions :(
        try
            Work1 = #work{op=?tutorial_DIVIDE,
                          num1=1,
                          num2=0},
            _Quot = (calculator_thrift:calculate(Client, 1, Work1)),

            io:format("LAME: exception handling is broken~n", [])
        catch
            Z ->
                p(Z)
        %%   rescue InvalidOperation => io
        %%     print "InvalidOperation: ", io.why, "\n"
        %%   end
        end,

        calculator_thrift:zip(Client),
        io:format("zip~n", []),

        ?R0(Trans, effectful_close)

    catch
        Y ->
            p(Y)
    end,
    ok.
