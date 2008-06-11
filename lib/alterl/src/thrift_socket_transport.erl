-module(thrift_socket_transport).

-behaviour(thrift_transport).

-export([new/1,
         new/2,
         write/2, read/2, flush/1, close/1,

         new_protocol_factory/3]).

-record(data, {socket,
               recv_timeout=infinity}).

new(Socket) ->
    new(Socket, []).

new(Socket, Opts) when is_list(Opts) ->
    State =
        case lists:keysearch(recv_timeout, 1, Opts) of
            {value, {recv_timeout, Timeout}}
            when is_integer(Timeout), Timeout > 0 ->
                #data{socket=Socket, recv_timeout=Timeout};
            _ ->
                #data{socket=Socket}
        end,
    thrift_transport:new(?MODULE, State).

%% Data :: iolist()
write(#data{socket = Socket}, Data) ->
    gen_tcp:send(Socket, Data).

read(#data{socket=Socket, recv_timeout=Timeout}, Len)
  when is_integer(Len), Len >= 0 ->
    case gen_tcp:recv(Socket, Len, Timeout) of
        Err = {error, timeout} ->
            error_logger:info_msg("read timeout: peer conn ~p", [inet:peername(Socket)]),
            gen_tcp:close(Socket),
            Err;
        Data -> Data
    end.

%% We can't really flush - everything is flushed when we write
flush(_) ->
    ok.

close(#data{socket = Socket}) ->
    gen_tcp:close(Socket).


%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Generates a "protocol factory" function - a fun which returns a Protocol instance.
%% This can be passed to thrift_client:start_link in order to connect to a
%% server over a socket.
%%
new_protocol_factory(Host, Port, Options) ->
    ConnectTimeout = proplists:get_value(connect_timeout, Options, infinity),
    InSockOpts     = proplists:get_value(sockopts, Options, []),
    Framed         = proplists:get_value(framed, Options, false),
    StrictRead     = proplists:get_value(strict_read, Options, true),
    StrictWrite    = proplists:get_value(strict_write, Options, true),

    F = fun() ->
                SockOpts = [binary,
                            {packet, 0},
                            {active, false},
                            {nodelay, true} |
                            InSockOpts],
                case catch gen_tcp:connect(Host, Port, SockOpts, ConnectTimeout) of
                    {ok, Sock} ->
                        {ok, Transport} = thrift_socket_transport:new(Sock),
                        {ok, BufTransport} =
                            case Framed of
                                true  -> thrift_framed_transport:new(Transport);
                                false -> thrift_buffered_transport:new(Transport)
                            end,
                        thrift_binary_protocol:new(BufTransport,
                                                   [{strict_read,  StrictRead},
                                                    {strict_write, StrictWrite}]);
                    Error  ->
                        Error
                end
        end,
    {ok, F}.
