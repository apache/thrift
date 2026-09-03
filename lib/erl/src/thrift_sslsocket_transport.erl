%%
%% Licensed to the Apache Software Foundation (ASF) under one
%% or more contributor license agreements. See the NOTICE file
%% distributed with this work for additional information
%% regarding copyright ownership. The ASF licenses this file
%% to you under the Apache License, Version 2.0 (the
%% "License"); you may not use this file except in compliance
%% with the License. You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied. See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(thrift_sslsocket_transport).

-behaviour(thrift_transport).

-export([
    new/3,
    write/2,
    read/2,
    flush/1,
    close/1,

    new_transport_factory/3
]).

%% Export only for the transport factory
-export([new/2]).

-record(data, {
    socket :: ssl:sslsocket(),
    recv_timeout = infinity :: infinity | timeout()
}).

-type data() :: #data{}.
-type reason() :: thrift_transport:reason().

%% The following "local" record is filled in by parse_factory_options/2
%% below. These options can be passed to new_protocol_factory/3 in a
%% proplists-style option list. They're parsed like this so it is an O(n)
%% operation instead of O(n^2)
-record(factory_opts, {
    connect_timeout = infinity :: infinity | timeout(),
    sockopts = [] :: [inet:inet_backend() | gen_tcp:connect_option()],
    framed = false :: boolean(),
    ssloptions = [] :: [ssl:tls_client_option()]
}).

parse_factory_options([], Opts) ->
    Opts;
parse_factory_options([{framed, Bool} | Rest], Opts) when is_boolean(Bool) ->
    parse_factory_options(Rest, Opts#factory_opts{framed = Bool});
parse_factory_options([{sockopts, OptList} | Rest], Opts) when is_list(OptList) ->
    parse_factory_options(Rest, Opts#factory_opts{sockopts = OptList});
parse_factory_options([{connect_timeout, TO} | Rest], Opts) when TO =:= infinity; is_integer(TO) ->
    parse_factory_options(Rest, Opts#factory_opts{connect_timeout = TO});
parse_factory_options([{ssloptions, SslOptions} | Rest], Opts) when is_list(SslOptions) ->
    parse_factory_options(Rest, Opts#factory_opts{ssloptions = SslOptions}).

new(Socket, SockOpts, SslOptions) when is_list(SockOpts), is_list(SslOptions) ->
    %% => prevent the ssl handshake messages get lost
    ok = inet:setopts(Socket, [{active, false}]),

    %% upgrade to an ssl socket

    % infinite timeout
    case catch ssl:handshake(Socket, SslOptions) of
        {ok, SslSocket} ->
            new(SslSocket, SockOpts);
        {error, Reason} ->
            exit({error, Reason});
        Other ->
            error_logger:error_report(
                [
                    {application, thrift},
                    "SSL accept failed error",
                    lists:flatten(io_lib:format("~p", [Other]))
                ]
            ),
            exit({error, ssl_accept_failed})
    end.

new(SslSocket, SockOpts) ->
    State =
        case lists:keysearch(recv_timeout, 1, SockOpts) of
            {value, {recv_timeout, Timeout}} when
                is_integer(Timeout), Timeout > 0
            ->
                #data{socket = SslSocket, recv_timeout = Timeout};
            _ ->
                #data{socket = SslSocket}
        end,
    thrift_transport:new(?MODULE, State).

%% Data :: iolist()
-spec write(data(), iolist()) -> {data(), ok | {error, reason()}}.
write(This = #data{socket = Socket}, Data) ->
    {This, ssl:send(Socket, Data)}.

read(This = #data{socket = Socket, recv_timeout = Timeout}, Len) when
    is_integer(Len), Len >= 0
->
    case ssl:recv(Socket, Len, Timeout) of
        Err = {error, timeout} ->
            ok = ssl:close(Socket),
            {This, Err};
        Data ->
            {This, Data}
    end.

%% We can't really flush - everything is flushed when we write
flush(This) ->
    {This, ok}.

close(This = #data{socket = Socket}) ->
    {This, ssl:close(Socket)}.

%%%% FACTORY GENERATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Generates a "transport factory" function - a fun which returns a thrift_transport()
%% instance.
%% This can be passed into a protocol factory to generate a connection to a
%% thrift server over a socket.
%%
%% Options ssl:connect/3 needs before it will validate the server's
%% certificate. They are prepended to the caller's list, not appended, because
%% ssl:connect/3 honours the last occurrence of a duplicated option: a caller
%% passing its own {verify, _} still overrides this one.
%%
%% Both halves are needed. From OTP 26 on, {verify, verify_peer} without CAs is
%% not a weaker setting, it is a rejected one -- ssl:connect/3 answers
%% {options, incompatible, [{verify, verify_peer}, {cacerts, undefined}]}.
default_ssl_options(CallerOptions) ->
    [{verify, verify_peer} | system_cacerts(CallerOptions)].

%% The system trust store, and only when the caller has named no CAs itself.
%% {cacerts, _} and {cacertfile, _} are separate options rather than two
%% spellings of one, so prepending the first is not overridden by a caller's
%% second: it wins over it, and the certificate the caller meant to trust is
%% rejected as unknown_ca. Supplying either one is the caller saying which CAs
%% to use, so leave that alone.
system_cacerts(CallerOptions) ->
    case lists:any(fun is_ca_option/1, CallerOptions) of
        true ->
            [];
        false ->
            %% public_key:cacerts_get/0 raises on a host with no trust store,
            %% and does not exist at all before OTP 25. Leave the option out in
            %% both cases and let ssl report the missing CAs, rather than
            %% failing here or falling back to not verifying at all.
            try public_key:cacerts_get() of
                CaCerts -> [{cacerts, CaCerts}]
            catch
                _:_ -> []
            end
    end.

is_ca_option({cacerts, _}) -> true;
is_ca_option({cacertfile, _}) -> true;
is_ca_option(_) -> false.

new_transport_factory(Host, Port, Options) ->
    ParsedOpts = parse_factory_options(Options, #factory_opts{}),

    F = fun() ->
        SslOptions = ParsedOpts#factory_opts.ssloptions,
        SockOpts = [
            binary,
            {packet, 0},
            {active, false},
            {nodelay, true}
            | ParsedOpts#factory_opts.sockopts
        ],
        case
            catch gen_tcp:connect(
                Host,
                Port,
                SockOpts,
                ParsedOpts#factory_opts.connect_timeout
            )
        of
            {ok, Sock} ->
                SslSock =
                    case
                        catch ssl:connect(
                            Sock,
                            default_ssl_options(SslOptions) ++ SslOptions,
                            ParsedOpts#factory_opts.connect_timeout
                        )
                    of
                        {ok, SslSocket} ->
                            SslSocket;
                        Other ->
                            error_logger:info_msg(
                                "error while connecting over ssl - reason: ~p~n", [Other]
                            ),
                            catch gen_tcp:close(Sock),
                            exit(error)
                    end,
                {ok, Transport} = thrift_sslsocket_transport:new(SslSock, SockOpts),
                {ok, BufTransport} =
                    case ParsedOpts#factory_opts.framed of
                        true -> thrift_framed_transport:new(Transport);
                        false -> thrift_buffered_transport:new(Transport)
                    end,
                {ok, BufTransport};
            Error ->
                Error
        end
    end,
    {ok, F}.
