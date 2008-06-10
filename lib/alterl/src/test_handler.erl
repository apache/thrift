-module(test_handler).

-export([handle_function/2]).

handle_function(add, Params = {A, B}) ->
    io:format("Got params: ~p~n", [Params]),
    {reply, A + B}.
