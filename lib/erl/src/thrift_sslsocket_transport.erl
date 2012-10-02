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

-export([new/1,
         new/2,
         new_with_ssl_upgrade/2,
         write/2, read/2, flush/1, close/1,

         new_transport_factory/3]).

-record(data, {socket,
               recv_timeout=infinity}).
-type state() :: #data{}.
-include("thrift_transport_behaviour.hrl").


%% The following "local" record is filled in by parse_factory_options/2
%% below. These options can be passed to new_protocol_factory/3 in a
%% proplists-style option list. They're parsed like this so it is an O(n)
%% operation instead of O(n^2)
-record(factory_opts, {connect_timeout = infinity,
                       sockopts = [],
                       framed = false,
                       %% TODO: set cacert etc. to a form of sslsockopts = [] this is more flexible with other options like keypassword etc.
                       cacertfile = undefined :: string(), %% Path to file containing PEM encoded CA certificates (trusted certificates used for verifying a peer certificate). May be omitted if you do not want to verify the peer.
                       certfile = undefined :: string(),   %% Path to a file containing the user's certificate.
                       keyfile = undefined :: string()}).  %% Path to file containing user's private PEM encoded key. As PEM-files may contain several entries this option defaults to the same file as given by certfile option.

parse_factory_options([], Opts) ->
    Opts;
parse_factory_options([{cacertfile, Path} | Rest], Opts) when is_list(Path) ->
    parse_factory_options(Rest, Opts#factory_opts{cacertfile=Path});
parse_factory_options([{certfile, Path} | Rest], Opts) when is_list(Path) ->
    parse_factory_options(Rest, Opts#factory_opts{certfile=Path});
parse_factory_options([{keyfile, Path} | Rest], Opts) when is_list(Path) ->
    parse_factory_options(Rest, Opts#factory_opts{keyfile=Path});

parse_factory_options([{framed, Bool} | Rest], Opts) when is_boolean(Bool) ->
    parse_factory_options(Rest, Opts#factory_opts{framed=Bool});
parse_factory_options([{sockopts, OptList} | Rest], Opts) when is_list(OptList) ->
    parse_factory_options(Rest, Opts#factory_opts{sockopts=OptList});
parse_factory_options([{connect_timeout, TO} | Rest], Opts) when TO =:= infinity; is_integer(TO) ->
    parse_factory_options(Rest, Opts#factory_opts{connect_timeout=TO}).

new_with_ssl_upgrade(Socket, Options) ->
    inet:setopts(Socket, [{active, false}]), %% => prevent the ssl handshake messages get lost

    % TODO: remove {recv_timeout, Timeout} from ssl option list
    {ok, SSLSocket} = ssl:ssl_accept(Socket, Options),

    new(SSLSocket, Options).

new(SSLSocket) ->
    new(SSLSocket, []).

new(SSLSocket, Opts) when is_list(Opts) ->
    State =
        case lists:keysearch(recv_timeout, 1, Opts) of
            {value, {recv_timeout, Timeout}}
            when is_integer(Timeout), Timeout > 0 ->
                #data{socket=SSLSocket, recv_timeout=Timeout};
            _ ->
                #data{socket=SSLSocket}
        end,
    thrift_transport:new(?MODULE, State).
    
%% Data :: iolist()
write(This = #data{socket = Socket}, Data) ->
    {This, ssl:send(Socket, Data)}.

read(This = #data{socket=Socket, recv_timeout=Timeout}, Len)
  when is_integer(Len), Len >= 0 ->
    case ssl:recv(Socket, Len, Timeout) of
        Err = {error, timeout} ->
            error_logger:info_msg("read timeout: peer conn ~p", [inet:peername(Socket)]),
            ssl:close(Socket),
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
new_transport_factory(Host, Port, Options) ->
    ParsedOpts = parse_factory_options(Options, #factory_opts{}),

    F = fun() ->
                SockOpts = [binary,
                            {packet, 0},
                            {active, false},
                            {nodelay, true} |
                            ParsedOpts#factory_opts.sockopts],
                case catch gen_tcp:connect(Host, Port, SockOpts,
                                           ParsedOpts#factory_opts.connect_timeout) of
                    {ok, Sock} ->
                        SSLSocket = case ssl:connect(Sock, [{cacertfile, ParsedOpts#factory_opts.cacertfile},
                                                            {certfile, ParsedOpts#factory_opts.certfile},
                                                            {keyfile, ParsedOpts#factory_opts.keyfile}], infinity) of
                                        {ok, Socket} -> error_logger:info_msg("connecting over ssl successfully port:~p...~n", [Socket]), Socket;
                                        Other -> error_logger:info_msg("error while connecting over ssl! reason: ~p~n", [Other]), exit(error)
                                    end,
                        %{ok, SSLSocket} = ssl:connect(Sock, [{cacertfile, ParsedOpts#factory_opts.cacertfile},
                        %                                     {certfile, ParsedOpts#factory_opts.certfile},
                        %                                     {keyfile, ParsedOpts#factory_opts.keyfile}], infinity),
                        {ok, Transport} = thrift_sslsocket_transport:new(SSLSocket),
                        {ok, BufTransport} =
                            case ParsedOpts#factory_opts.framed of
                                true  -> thrift_framed_transport:new(Transport);
                                false -> thrift_buffered_transport:new(Transport)
                            end,
                        {ok, BufTransport};
                    Error  ->
                        Error
                end
        end,
    {ok, F}.