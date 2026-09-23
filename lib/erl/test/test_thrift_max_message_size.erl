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

%% The binary and compact protocols read a message only up to its maximum
%% size: the max_message_size option, or else the thrift application's
%% max_message_size.
-module(test_thrift_max_message_size).
-include_lib("eunit/include/eunit.hrl").
-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").

%% thrift_socket_server handler callbacks
-export([handle_function/2, handle_error/2]).

-define(ARGS, {struct, [{1, string}]}).
-define(MAX, 4096).

%% {Name, Module, Options} of each protocol and header form under test.
protocols() ->
    [
        {"binary", thrift_binary_protocol, []},
        {"binary without version header", thrift_binary_protocol, [
            {strict_read, false}, {strict_write, false}
        ]},
        {"compact", thrift_compact_protocol, []}
    ].

new(Module, Bytes, Options) ->
    {ok, Transport} = thrift_membuffer_transport:new(Bytes),
    {ok, Protocol} = Module:new(Transport, Options),
    Protocol.

%% What is left in the memory buffer under Protocol, or under the protocol's
%% own state, which a failed read inside the protocol raises with.
buffered({protocol, _Module, State}) ->
    buffered(State);
buffered(State) ->
    {t_transport, thrift_membuffer_transport, {t_membuffer, Bytes}} = element(2, State),
    Bytes.

%% The bytes of a call named Name whose one argument is the string Text.
message(Module, Options, Name, Text) ->
    Protocol0 = new(Module, <<>>, Options),
    {Protocol1, ok} = thrift_protocol:write(Protocol0, #protocol_message_begin{
        name = Name, type = ?tMessageType_CALL, seqid = 1
    }),
    {Protocol2, ok} = thrift_protocol:write(Protocol1, {?ARGS, {args, Text}}),
    {Protocol3, ok} = thrift_protocol:write(Protocol2, message_end),
    {Protocol4, ok} = thrift_protocol:flush_transport(Protocol3),
    iolist_to_binary(buffered(Protocol4)).

%% A call of exactly Size bytes, the argument making up the rest. The compact
%% protocol writes the string's length in as few bytes as it needs, so the
%% first guess is corrected once.
message_of_size(Module, Options, Size) ->
    Guess = Size - byte_size(message(Module, Options, "m", <<>>)),
    First = message(Module, Options, "m", binary:copy(<<"a">>, Guess)),
    Bytes = message(Module, Options, "m", binary:copy(<<"a">>, Guess - (byte_size(First) - Size))),
    Size = byte_size(Bytes),
    Bytes.

%% Reads one call as thrift_processor does: {Result, bytes taken from the
%% transport, Protocol}. A refusal inside the arguments surfaces as the
%% exception the struct reader raises for any failed read; where that
%% exception does not carry the protocol, the bytes taken are unknown.
read_call(Protocol0, Bytes) ->
    try read_whole_call(Protocol0) of
        {Outcome, Protocol} ->
            {Outcome, byte_size(Bytes) - iolist_size(buffered(Protocol)), Protocol}
    catch
        error:{badmatch, {Protocol, {error, _} = Error}} ->
            {Error, byte_size(Bytes) - iolist_size(buffered(Protocol)), Protocol};
        error:{case_clause, {error, _} = Error} ->
            {Error, unknown, undefined}
    end.

read_whole_call(Protocol0) ->
    case thrift_protocol:read(Protocol0, message_begin) of
        {Protocol1, #protocol_message_begin{name = Name}} ->
            {Protocol2, {ok, Args}} = thrift_protocol:read(Protocol1, ?ARGS),
            {Protocol3, ok} = thrift_protocol:read(Protocol2, message_end),
            {{ok, Name, Args}, Protocol3};
        {Protocol1, Error} ->
            {Error, Protocol1}
    end.

refused() -> {error, {message_size_exceeds_maximum, ?MAX}}.

%% A call up to the maximum is read, and one a byte longer is refused. A
%% string that would take the call past the maximum is refused from its
%% length, before its bytes are taken from the transport.
limit_test_() ->
    [
        {Name,
            ?_test(begin
                Options = [{max_message_size, ?MAX} | Base],
                AtMax = message_of_size(Module, Base, ?MAX),
                ?assertMatch(
                    {{ok, "m", _}, ?MAX, _}, read_call(new(Module, AtMax, Options), AtMax)
                ),
                ByOne = message_of_size(Module, Base, ?MAX + 1),
                ?assertMatch({{error, _}, _, _}, read_call(new(Module, ByOne, Options), ByOne)),
                Long = message_of_size(Module, Base, 2 * ?MAX),
                {Result, Taken, _} = read_call(new(Module, Long, Options), Long),
                ?assertEqual(refused(), Result),
                ?assert(Taken < ?MAX)
            end)}
     || {Name, Module, Base} <- protocols()
    ].

%% A message name longer than the maximum is refused by message_begin, which
%% reports it as an error rather than raising, and before its bytes are taken.
name_test_() ->
    [
        {Name,
            ?_test(begin
                Bytes = message(Module, Base, binary_to_list(binary:copy(<<"n">>, ?MAX)), <<>>),
                Protocol = new(Module, Bytes, [{max_message_size, ?MAX} | Base]),
                {Protocol1, Result} = thrift_protocol:read(Protocol, message_begin),
                ?assertEqual(refused(), Result),
                ?assert(byte_size(Bytes) - iolist_size(buffered(Protocol1)) =< 8)
            end)}
     || {Name, Module, Base} <- protocols()
    ].

%% Each message has the whole maximum to itself.
one_message_after_another_test_() ->
    [
        {Name,
            ?_test(begin
                One = message_of_size(Module, Base, ?MAX),
                Bytes = <<One/binary, One/binary, One/binary>>,
                Protocol0 = new(Module, Bytes, [{max_message_size, ?MAX} | Base]),
                {{ok, _, _}, _, Protocol1} = read_call(Protocol0, Bytes),
                {{ok, _, _}, _, Protocol2} = read_call(Protocol1, Bytes),
                ?assertMatch({{ok, "m", _}, _, _}, read_call(Protocol2, Bytes))
            end)}
     || {Name, Module, Base} <- protocols()
    ].

%% Outside a message, reads are held to the maximum one at a time and do not
%% add up: many structs read one after another without a message around them.
outside_a_message_test_() ->
    [
        {Name,
            ?_test(begin
                Protocol0 = new(Module, <<>>, Base),
                {Protocol1, ok} = thrift_protocol:write(
                    Protocol0, {?ARGS, {args, binary:copy(<<"s">>, 1000)}}
                ),
                One = iolist_to_binary(buffered(Protocol1)),
                Bytes = binary:copy(One, 10),
                Protocol2 = new(Module, Bytes, [{max_message_size, ?MAX} | Base]),
                Final = lists:foldl(
                    fun(_, P) ->
                        {P1, {ok, {Text}}} = thrift_protocol:read(P, ?ARGS),
                        ?assertEqual(1000, byte_size(Text)),
                        P1
                    end,
                    Protocol2,
                    lists:seq(1, 10)
                ),
                ?assertEqual(0, iolist_size(buffered(Final)))
            end)}
     || {Name, Module, Base} <- protocols()
    ].

%% Without the option, the thrift application's max_message_size applies; the
%% option takes its place.
application_env_test_() ->
    {setup,
        fun() ->
            ok = load_thrift_application(),
            Saved = application:get_env(thrift, max_message_size),
            ok = application:set_env(thrift, max_message_size, ?MAX),
            Saved
        end,
        fun restore_env/1, [
            {Name,
                ?_test(begin
                    Over = message_of_size(Module, Base, ?MAX + 1),
                    ?assertMatch({{error, _}, _, _}, read_call(new(Module, Over, Base), Over)),
                    ?assertMatch(
                        {{ok, "m", _}, _, _},
                        read_call(new(Module, Over, [{max_message_size, 2 * ?MAX} | Base]), Over)
                    )
                end)}
         || {Name, Module, Base} <- protocols()
        ]}.

%%%% Over a socket %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(_Function, Args) -> {reply, element(1, Args)}.

handle_error(_Function, _Reason) -> ok.

%% The server refuses a call over the application's maximum and closes that
%% connection, and goes on serving others. A client given max_message_size
%% refuses a reply over it.
socket_test_() ->
    {setup,
        fun() ->
            ok = load_thrift_application(),
            Saved = application:get_env(thrift, max_message_size),
            ok = application:set_env(thrift, max_message_size, 64 * 1024),
            Saved
        end,
        fun restore_env/1, [
            {atom_to_list(Protocol), ?_test(socket_calls(Port, Protocol))}
         || {Port, Protocol} <- [{9110, binary}, {9111, compact}]
        ]}.

socket_calls(Port, Protocol) ->
    {ok, Server} = thrift_socket_server:start([
        {ip, "127.0.0.1"},
        {port, Port},
        {name, list_to_atom("max_message_size_server_" ++ integer_to_list(Port))},
        {service, thrift_test_thrift},
        {handler, ?MODULE},
        {protocol, Protocol}
    ]),
    try
        Options = [{protocol, Protocol}, {recv_timeout, 2000}],
        {ok, Client0} = thrift_client_util:new("127.0.0.1", Port, thrift_test_thrift, Options),
        {Client1, Small} = thrift_client:call(Client0, testString, [<<"small">>]),
        ?assertEqual({ok, <<"small">>}, Small),
        Large = binary:copy(<<"L">>, 128 * 1024),
        ?assertMatch({_, {error, _}}, catch_call(Client1, testString, [Large])),
        {ok, Client2} = thrift_client_util:new("127.0.0.1", Port, thrift_test_thrift, Options),
        ?assertMatch(
            {_, {ok, <<"again">>}}, thrift_client:call(Client2, testString, [<<"again">>])
        ),
        {ok, Client3} = thrift_client_util:new(
            "127.0.0.1", Port, thrift_test_thrift, [{max_message_size, 1024} | Options]
        ),
        ?assertMatch({_, {error, _}}, catch_call(Client3, testString, [binary:copy(<<"r">>, 2048)]))
    after
        thrift_socket_server:stop(Server)
    end.

%% A call whose connection fails or whose reply is refused, as {error, _}.
catch_call(Client, Function, Args) ->
    try thrift_client:call(Client, Function, Args) of
        {Client1, {ok, Reply}} -> {Client1, {ok, Reply}};
        {Client1, Other} -> {Client1, {error, Other}}
    catch
        Class:Reason -> {Client, {error, {Class, Reason}}}
    end.

load_thrift_application() ->
    case application:load(thrift) of
        ok -> ok;
        {error, {already_loaded, thrift}} -> ok
    end.

restore_env(undefined) -> application:unset_env(thrift, max_message_size);
restore_env({ok, Value}) -> application:set_env(thrift, max_message_size, Value).
