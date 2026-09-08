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

%% What a peer is told when it calls a method the service does not have.
%%
%% The method name is a string the peer chooses. Turning it into the atom the
%% processor dispatches on happens before the handler runs, so a name that is
%% not one of the service's functions used to raise an unhandled badarg and take
%% the connection's process down. The server should instead drain the call and
%% answer it with a TApplicationException(UNKNOWN_METHOD), exactly as the other
%% bindings do, and stay up for the next request.

-module(unknown_method_test).

-include_lib("eunit/include/eunit.hrl").
-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

-export([handle_function/2, handle_error/2]).

-define(SERVICE, multiplexing__calculator_thrift).
%% Binary protocol strict header; not exported from thrift_constants.hrl.
-define(BINARY_VERSION_1, 16#80010000).
%% Almost certainly never created as an atom, so it exercises the badarg path.
-define(UNKNOWN, "no_such_method_zzz").

unknown_method_is_reported_not_crashed_test() ->
    ok = load_thrift_application(),
    Port = 9096,
    {ok, Server} = thrift_socket_server:start([
        {ip, "127.0.0.1"},
        {port, Port},
        {name, ?MODULE},
        {service, ?SERVICE},
        {handler, ?MODULE},
        {framed, true}
    ]),
    try
        {ok, Sock} = gen_tcp:connect(
            {127, 0, 0, 1}, Port, [binary, {packet, 0}, {active, false}]
        ),
        ok = gen_tcp:send(Sock, framed(call_bytes(?UNKNOWN, 7))),
        {Type, Name, ExceptionType} = recv_reply(Sock),
        ?assertEqual(?tMessageType_EXCEPTION, Type),
        ?assertEqual(?UNKNOWN, Name),
        ?assertEqual(?TApplicationException_UNKNOWN_METHOD, ExceptionType),
        ok = gen_tcp:close(Sock)
    after
        thrift_socket_server:stop(Server)
    end.

%% The service never dispatches to these; they satisfy the behaviour.
handle_function(_Function, _Args) -> {reply, 0}.
handle_error(_Function, _Reason) -> ok.

%% A strict binary CALL with an empty argument struct.
call_bytes(Name, SeqId) ->
    NameBin = list_to_binary(Name),
    <<
        (?BINARY_VERSION_1 bor ?tMessageType_CALL):32,
        (byte_size(NameBin)):32,
        NameBin/binary,
        SeqId:32,
        ?tType_STOP
    >>.

framed(Payload) -> <<(byte_size(Payload)):32, Payload/binary>>.

%% Reads one framed reply and returns {MessageType, MethodName, ExceptionType}.
recv_reply(Sock) ->
    {ok, <<Len:32>>} = gen_tcp:recv(Sock, 4, 5000),
    {ok, Payload} = gen_tcp:recv(Sock, Len, 5000),
    {ok, Transport} = thrift_membuffer_transport:new(Payload),
    {ok, Proto0} = thrift_binary_protocol:new(Transport),
    {Proto1, #protocol_message_begin{name = Name, type = Type}} =
        thrift_protocol:read(Proto0, message_begin),
    {_Proto2, {ok, Exception}} =
        thrift_protocol:read(Proto1, ?TApplicationException_Structure),
    {Type, Name, element(2, Exception)}.

load_thrift_application() ->
    case application:load(thrift) of
        ok -> ok;
        {error, {already_loaded, thrift}} -> ok
    end.
