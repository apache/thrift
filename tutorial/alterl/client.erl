-module(client).

-include("calculator_thrift.hrl").

-export([t/0]).

p(X) ->
    io:format("~p~n", [X]),
    ok.

t() ->
    Port = 9999,
    
    {ok, Client} = thrift_client:start_link("127.0.0.1",
                                            Port,
                                            calculator_thrift),

    thrift_client:call(Client, ping, []),
    io:format("ping~n", []),

    {ok, Sum} = thrift_client:call(Client, add,  [1, 1]),
    io:format("1+1=~p~n", [Sum]),

    {ok, Sum1} = thrift_client:call(Client, add, [1, 4]),
    io:format("1+4=~p~n", [Sum1]),

    Work = #work{op=?tutorial_SUBTRACT,
                 num1=15,
                 num2=10},
    {ok, Diff} = thrift_client:call(Client, calculate, [1, Work]),
    io:format("15-10=~p~n", [Diff]),

    {ok, Log} = thrift_client:call(Client, getStruct, [1]),
    io:format("Log: ~p~n", [Log]),

    try
        Work1 = #work{op=?tutorial_DIVIDE,
                      num1=1,
                      num2=0},
        {ok, _Quot} = thrift_client:call(Client, calculate, [2, Work1]),

        io:format("LAME: exception handling is broken~n", [])
    catch
        Z ->
            io:format("Got exception where expecting - the " ++
                      "following is NOT a problem!!!~n"),
            p(Z)
    end,


    {ok, ok} = thrift_client:call(Client, zip, []),
    io:format("zip~n", []),

    ok = thrift_client:close(Client),
    ok.
