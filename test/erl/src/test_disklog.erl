-module(test_disklog).

-compile(export_all).

t() ->
    {ok, TransportFactory} =
        thrift_disk_log_transport:new_transport_factory(
          test_disklog,
          [{file, "/tmp/test_log"},
           {size, {1024*1024, 10}}]),
    {ok, ProtocolFactory} = thrift_binary_protocol:new_protocol_factory(
                              TransportFactory, []),
    {ok, Client} = thrift_client:start_link(ProtocolFactory, thriftTest_thrift),

    io:format("Client started~n"),
    % We have to make async calls into this client only since otherwise it will try
    % to read from the disklog and go boom.
    {ok, ok} = thrift_client:call(Client, testAsync, [16#deadbeef]),
    io:format("Call written~n"),

    ok = thrift_client:close(Client),
    io:format("Client closed~n"),

    ok.

