-module(thrift_socket_transport).

-behaviour(thrift_transport).

-export([new/1,
         new/2,
         write/2, read/2, flush/1, close/1]).

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
            error_logger:error_msg("read timeout for conn with ~p", [inet:peername(Socket)]),
            gen_tcp:close(Socket),
            Err;
        Data -> Data
    end.

%% We can't really flush - everything is flushed when we write
flush(_) ->
    ok.

close(#data{socket = Socket}) ->
    gen_tcp:close(Socket).
