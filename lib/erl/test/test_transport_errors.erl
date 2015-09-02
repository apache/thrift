-module(test_transport_errors).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

error_on_write_test_() ->
    {setup, local, fun start_client/0, fun teardown_client/1, fun error_on_write/1}.

error_on_write(Client) ->
    ?_assertEqual(
        {Client, {error, ebadf}},
        thrift_client:call(Client, testString, [<<"hello world">>])
    ).

start_client() ->
    TransportFactory =
        fun() ->
            case file:open([], [ram, read]) of
                {ok, IODevice} ->
                    thrift_file_transport:new(IODevice, [{should_close, true}, {mode, write}]);
                Error  ->
                    Error
            end
        end,

    {ok, ProtocolFactory} = thrift_binary_protocol:new_protocol_factory(
        TransportFactory, []),

    {ok, Protocol} = ProtocolFactory(),
    {ok, Client} = thrift_client:new(Protocol, thrift_test_thrift),
    Client.

teardown_client(Client) ->
    thrift_client:close(Client).

-endif.
