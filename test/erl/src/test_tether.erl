%% Tests the behavior of clients in the face of transport errors.
%% Makes sure start, start_linked, and start_tethered work as expected.

-module(test_tether).

-compile(export_all).


t() ->
    io:format("Beginning transport error test.~n"),
    Pid1 = erlang:spawn(?MODULE, t_sub, [2]),
    wait_for(Pid1),
    io:format("Beginning protocol error test.~n"),
    Pid2 = erlang:spawn(?MODULE, t_sub, [22]),
    wait_for(Pid2),
    ok.

t_sub(Port) ->
    io:format("Starting.~n", []),
    register(tester, self()),

    Pid1 = erlang:spawn(?MODULE, test_start, [Port]),
    receive after 200 -> ok end,  % Wait for completion.
    case is_up(Pid1) of
        true ->
            io:format("PASS.  Unlinked owner still alive.~n");
        false ->
            io:format("FAIL.  Unlinked owner is dead.~n")
    end,

    Pid2 = erlang:spawn(?MODULE, test_linked, [Port]),
    receive after 200 -> ok end,  % Wait for completion.
    case is_up(Pid2) of
        true ->
            io:format("FAIL.  Linked owner still alive.~n");
        false ->
            io:format("PASS.  Linked owner is dead.~n")
    end,

    Pid3 = erlang:spawn(?MODULE, test_tethered, [Port]),
    receive after 200 -> ok end,  % Wait for completion.
    case is_up(Pid3) of
        true ->
            io:format("PASS.  Tethered owner still alive.~n");
        false ->
            io:format("FAIL.  Tethered owner is dead.~n")
    end,

    check_extras(3).

is_up(Pid) ->
    MonitorRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonitorRef, process, Pid, _Info} ->
            false
    after
        50 ->
            erlang:demonitor(MonitorRef),
            true
    end.

wait_for(Pid) ->
    MonitorRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonitorRef, process, Pid, _Info} ->
            ok
    end.

check_extras(0) -> ok;
check_extras(N) ->
    receive
        {client, Type, Pid} ->
            case {Type, is_up(Pid)} of
                {unlinked, true} ->
                    io:format("PASS.  Unlinked client still alive.~n");
                {unlinked, false} ->
                    io:format("FAIL.  Unlinked client dead.~n");
                {linked, true} ->
                    io:format("FAIL.  Linked client still alive.~n");
                {linked, false} ->
                    io:format("PASS.  Linked client dead.~n");
                {tethered, true} ->
                    io:format("FAIL.  Tethered client still alive.~n");
                {tethered, false} ->
                    io:format("PASS.  Tethered client dead.~n")
            end,
            check_extras(N-1)
    after
        500 ->
            io:format("FAIL.  Expected ~p more clients.~n", [N])
    end.

make_thrift_client(Opts) ->
     thrift_client:start(fun()->ok end, thriftTest_thrift, Opts).

make_protocol_factory(Port) ->
    {ok, TransportFactory} =
        thrift_socket_transport:new_transport_factory(
          "127.0.0.1", Port, []),
    {ok, ProtocolFactory} =
        thrift_binary_protocol:new_protocol_factory(
          TransportFactory, []),
    ProtocolFactory.


test_start(Port) ->
    {ok, Client1} = make_thrift_client([{connect, false}]),
    tester ! {client, unlinked, Client1},
    {ok, Client2} = make_thrift_client([{connect, false}]),
    io:format("PASS.  Unlinked clients created.~n"),
    try
        gen_server:call(Client2, {connect, make_protocol_factory(Port)}),
        thrift_client:call(Client2, testVoid, []),
        io:format("FAIL.  Unlinked client connected and called.~n", [])
    catch
        Kind:Info ->
            io:format("PASS.  Caught unlinked error.  ~p:~p~n", [Kind, Info])
    end,
    receive after 100 ->
                    io:format("PASS.  Still alive after unlinked death.~n"),
                    %% Hang around a little longer so our parent can verify.
                    receive after 200 -> ok end
    end,
    %% Exit abnormally to not kill our unlinked extra client.
    exit(die).

test_linked(Port) ->
    {ok, Client1} = make_thrift_client([{connect, false}, {monitor, link}]),
    tester ! {client, linked, Client1},
    {ok, Client2} = make_thrift_client([{connect, false}, {monitor, link}]),
    io:format("PASS.  Linked clients created.~n"),
    try
        gen_server:call(Client2, {connect, make_protocol_factory(Port)}),
        thrift_client:call(Client2, testVoid, []),
        io:format("FAIL.  Linked client connected and called.~n", [])
    catch
        Kind:Info ->
            io:format("FAIL.  Caught linked error.  ~p:~p~n", [Kind, Info])
    end,
    receive after 100 ->
                    io:format("FAIL.  Still alive after linked death.~n"),
                    % Hang around a little longer so our parent can verify.
                    receive after 200 -> ok end
    end,
    %% Exit abnormally to kill our linked extra client.
    %% But we should never get here.
    exit(die).

test_tethered(Port) ->
    {ok, Client1} = make_thrift_client([{connect, false}, {monitor, tether}]),
    tester ! {client, tethered, Client1},
    {ok, Client2} = make_thrift_client([{connect, false}, {monitor, tether}]),
    io:format("PASS.  Tethered clients created.~n"),
    try
        gen_server:call(Client2, {connect, make_protocol_factory(Port)}),
        thrift_client:call(Client2, testVoid, []),
        io:format("FAIL.  Tethered client connected and called.~n", [])
    catch
        Kind:Info ->
            io:format("PASS.  Caught tethered error.  ~p:~p~n", [Kind, Info])
    end,
    receive after 100 ->
                    io:format("PASS.  Still alive after tethered death.~n"),
                    % Hang around a little longer so our parent can verify.
                    receive after 200 -> ok end
    end,
    %% Exit abnormally to kill our tethered extra client.
    exit(die).
