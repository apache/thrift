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

-module(test_thrift_json_protocol).
-include_lib("eunit/include/eunit.hrl").
-include("thrift_constants.hrl").
-include("thrift_protocol.hrl").
-include("gen-erl/thrift_test_types.hrl").

%% thrift_socket_server handler callbacks
-export([handle_function/2, handle_error/2]).

%% A struct with a field of each type the protocol writes, and maps keyed by
%% each kind of scalar, which the protocol writes as quoted object keys.
-define(EVERY_TYPE,
    {struct, [
        {1, bool},
        {2, byte},
        {3, i16},
        {4, i32},
        {5, i64},
        {6, double},
        {7, string},
        {8, {list, string}},
        {9, {set, i32}},
        {10, {map, i32, string}},
        {11, {map, string, double}},
        {12, {map, bool, i64}},
        {13, {map, double, bool}},
        {14, {map, i64, i16}},
        {15, {struct, [{1, i16}, {2, {list, {struct, [{1, string}]}}}]}}
    ]}
).

%% Characters a JSON string has to escape, characters JSON itself is made of,
%% and some that are not ASCII.
-define(AWKWARD, [
    $a,
    $",
    $\\,
    $/,
    $\b,
    $\f,
    $\n,
    $\r,
    $\t,
    0,
    1,
    31,
    $],
    $},
    $[,
    ${,
    $,,
    $:,
    $\s
    | binary_to_list(<<"é€😀"/utf8>>)
]).

new_protocol(Bytes) ->
    {ok, Transport} = thrift_membuffer_transport:new(Bytes),
    {ok, Protocol} = thrift_json_protocol:new(Transport),
    Protocol.

%% The bytes of a message of Type and Value, called Name, as the protocol
%% writes them.
message(Name, Type, Value) ->
    Protocol0 = new_protocol(<<>>),
    {Protocol1, ok} = thrift_protocol:write(Protocol0, #protocol_message_begin{
        name = Name, type = ?tMessageType_CALL, seqid = 42
    }),
    {Protocol2, ok} = thrift_protocol:write(Protocol1, {Type, Value}),
    {Protocol3, ok} = thrift_protocol:write(Protocol2, message_end),
    {Protocol4, ok} = thrift_protocol:flush_transport(Protocol3),
    iolist_to_binary(buffered(Protocol4)).

%% What is left in the memory buffer under Protocol.
buffered({protocol, thrift_json_protocol, State}) ->
    {t_transport, thrift_membuffer_transport, {t_membuffer, Bytes}} = element(2, State),
    Bytes.

%% Reads a message of Type from Protocol: {Protocol, {Name, Value}}.
read_message(Protocol0, Type) ->
    {Protocol1, #protocol_message_begin{name = Name, type = ?tMessageType_CALL, seqid = 42}} =
        thrift_protocol:read(Protocol0, message_begin),
    {Protocol2, {ok, Value}} = thrift_protocol:read(Protocol1, Type),
    {Protocol3, ok} = thrift_protocol:read(Protocol2, message_end),
    {Protocol3, {Name, Value}}.

every_type_value() ->
    {every_type, true, -128, -32768, -2147483648, -9223372036854775808, -1.5, "text",
        ["one", <<"two">>], sets:from_list([3, 1, 2]), dict:from_list([{7, "seven"}]),
        dict:from_list([{<<"pi">>, 3.25}]), dict:from_list([{true, 1}, {false, -1}]),
        dict:from_list([{0.5, true}, {-2.0, false}]),
        dict:from_list([{9223372036854775807, 32767}]),
        {nested, 7, [{inner, "x"}, {inner, <<"y">>}]}}.

%% Everything the protocol writes, it reads back.
every_type_test() ->
    Bytes = message("every", ?EVERY_TYPE, every_type_value()),
    {_, {Name, Value}} = read_message(new_protocol(Bytes), ?EVERY_TYPE),
    ?assertEqual("every", Name),
    Expected = [
        true,
        -128,
        -32768,
        -2147483648,
        -9223372036854775808,
        -1.5,
        <<"text">>,
        [<<"one">>, <<"two">>],
        [1, 2, 3],
        [{7, <<"seven">>}],
        [{<<"pi">>, 3.25}],
        [{false, -1}, {true, 1}],
        [{-2.0, false}, {0.5, true}],
        [{9223372036854775807, 32767}],
        {7, [{<<"x">>}, {<<"y">>}]}
    ],
    ?assertEqual(Expected, [sorted(Field) || Field <- tuple_to_list(Value)]).

%% A set or a map as a sorted list, anything else as it is.
sorted(Field) ->
    case {sets:is_set(Field), is_dict(Field)} of
        {true, _} -> lists:sort(sets:to_list(Field));
        {_, true} -> lists:sort(dict:to_list(Field));
        _ -> Field
    end.

is_dict(Field) ->
    try dict:to_list(Field) of
        _ -> true
    catch
        error:_ -> false
    end.

%% A string is written as a JSON string, whether given as a list or as a
%% binary, and reads back as the same bytes.
string_test_() ->
    [
        {Title,
            ?_test(begin
                Bytes = message(Name, {struct, [{1, string}]}, {args, String}),
                ?assertEqual([], [B || <<B>> <= Bytes, B < 16#20]),
                {_, Read} = read_message(new_protocol(Bytes), {struct, [{1, string}]}),
                ?assertEqual({Name, {list_to_binary(?AWKWARD)}}, Read)
            end)}
     || {Title, Name, String} <- [
            {"list", ?AWKWARD, ?AWKWARD},
            {"binary", ?AWKWARD, list_to_binary(?AWKWARD)}
        ]
    ].

%% Each character that has to be escaped is written as JSON writes it.
string_escapes_test() ->
    Escaped = [$", $\\, $\b, $\f, $\n, $\r, $\t, 1, 31],
    Bytes = message("m", {struct, [{1, string}]}, {args, Escaped}),
    ?assertNotEqual(nomatch, binary:match(Bytes, <<"\"\\\"\\\\\\b\\f\\n\\r\\t\\u0001\\u001f\"">>)).

%% A read ends where its message does, so the message after it is still
%% there to be read.
messages_one_after_another_test() ->
    Type = {struct, [{1, string}, {2, i32}]},
    First = message("first", Type, {args, "]}\"[{", 1}),
    Second = message("second", Type, {args, "\\", 2}),
    {Protocol1, Read1} = read_message(new_protocol(<<First/binary, Second/binary>>), Type),
    ?assertEqual({"first", {<<"]}\"[{">>, 1}}, Read1),
    {_, Read2} = read_message(Protocol1, Type),
    ?assertEqual({"second", {<<"\\">>, 2}}, Read2).

%% A message that stops short is an error, not a message.
truncated_message_test() ->
    Bytes = message("m", {struct, [{1, string}]}, {args, "text"}),
    Short = binary:part(Bytes, 0, byte_size(Bytes) - 1),
    ?assertMatch({_, {error, _}}, thrift_protocol:read(new_protocol(Short), message_begin)).

%% What the transport reports instead of data is what the read returns.
transport_error_test_() ->
    {setup, fun() -> meck:new(thrift_membuffer_transport, [passthrough]) end,
        fun(_) -> meck:unload(thrift_membuffer_transport) end,
        ?_test(begin
            meck:expect(thrift_membuffer_transport, read, fun(State, _Len) ->
                {State, {error, closed}}
            end),
            ?assertMatch(
                {_, {error, closed}}, thrift_protocol:read(new_protocol(<<"[">>), message_begin)
            )
        end)}.

%% The protocol takes more than one byte of a message per transport call.
%% It never asks for more than the message can still have, because a socket
%% transport waits until it has as many bytes as it was asked for.
read_calls_test_() ->
    {setup, fun() -> meck:new(thrift_membuffer_transport, [passthrough]) end,
        fun(_) -> meck:unload(thrift_membuffer_transport) end,
        ?_test(begin
            Size = 100000,
            Type = {struct, [{1, string}]},
            Bytes = message("m", Type, {args, binary:copy(<<"a">>, Size)}),
            meck:reset(thrift_membuffer_transport),
            {_, {"m", {Text}}} = read_message(new_protocol(Bytes), Type),
            ?assertEqual(Size, byte_size(Text)),
            Calls = meck:num_calls(thrift_membuffer_transport, read, '_'),
            ?assert(Calls < Size div 3)
        end)}.

%%%% Message size %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% A message of exactly Size bytes, a string field making up the rest.
message_of_size(Size) ->
    Type = {struct, [{1, string}]},
    Empty = byte_size(message("m", Type, {args, ""})),
    message("m", Type, {args, binary:copy(<<"a">>, Size - Empty)}).

%% Reads message_begin from Bytes: what it returned, and how many bytes it
%% took from the transport.
read_begin(Bytes, Options) ->
    {ok, Transport} = thrift_membuffer_transport:new(Bytes),
    {ok, Protocol0} = thrift_json_protocol:new(Transport, Options),
    {Protocol1, Result} = thrift_protocol:read(Protocol0, message_begin),
    {Result, byte_size(Bytes) - iolist_size(buffered(Protocol1))}.

%% A message up to the max_message_size option is read. One a byte longer is
%% refused before more than the maximum is taken from the transport.
max_message_size_option_test() ->
    Max = 4096,
    ?assertEqual(Max, byte_size(message_of_size(Max))),
    ?assertMatch(
        {#protocol_message_begin{name = "m"}, Max},
        read_begin(message_of_size(Max), [{max_message_size, Max}])
    ),
    {Result, Taken} = read_begin(message_of_size(Max + 1), [{max_message_size, Max}]),
    ?assertEqual({error, {message_size_exceeds_maximum, Max}}, Result),
    ?assert(Taken =< Max).

%% Without the option, the thrift application's max_message_size applies.
max_message_size_env_test_() ->
    {setup,
        fun() ->
            ok = load_thrift_application(),
            Saved = application:get_env(thrift, max_message_size),
            ok = application:set_env(thrift, max_message_size, 4096),
            Saved
        end,
        fun
            (undefined) -> application:unset_env(thrift, max_message_size);
            ({ok, Value}) -> application:set_env(thrift, max_message_size, Value)
        end,
        ?_test(begin
            ?assertMatch(
                {#protocol_message_begin{name = "m"}, 4096}, read_begin(message_of_size(4096), [])
            ),
            {Result, Taken} = read_begin(message_of_size(4097), []),
            ?assertEqual({error, {message_size_exceeds_maximum, 4096}}, Result),
            ?assert(Taken =< 4096),
            %% The option takes the place of the application's setting.
            ?assertMatch(
                {#protocol_message_begin{name = "m"}, 8192},
                read_begin(message_of_size(8192), [{max_message_size, 8192}])
            )
        end)}.

%% A long message reads back the same, including the characters that decide
%% where it ends, and the message after it is still there to be read.
long_message_test() ->
    Type = {struct, [{1, string}, {2, i32}]},
    Text = binary:copy(list_to_binary(?AWKWARD), 20000),
    First = message("first", Type, {args, Text, 1}),
    Second = message("second", Type, {args, "]}\"[{", 2}),
    {Protocol1, Read1} = read_message(new_protocol(<<First/binary, Second/binary>>), Type),
    ?assertEqual({"first", {Text, 1}}, Read1),
    {_, Read2} = read_message(Protocol1, Type),
    ?assertEqual({"second", {<<"]}\"[{">>, 2}}, Read2).

%%%% Memory %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The largest heap, in words, a reader process gets for a message with a
%% string of about two million characters: less than one list cell each.
-define(READ_HEAP_WORDS, 4000000).

%% A message with a long string, of every kind of character the reader
%% handles, is read within ?READ_HEAP_WORDS.
long_string_heap_test() ->
    Type = {struct, [{1, string}]},
    Text = binary:copy(list_to_binary(?AWKWARD), 80000),
    Bytes = message("m", Type, {args, Text}),
    Parent = self(),
    {Pid, Ref} = spawn_opt(
        fun() ->
            {_, {"m", {Read}}} = read_message(new_protocol(Bytes), Type),
            Parent ! {read, self(), Read =:= Text}
        end,
        [monitor, {max_heap_size, #{size => ?READ_HEAP_WORDS, kill => true, error_logger => false}}]
    ),
    receive
        {read, Pid, Same} ->
            erlang:demonitor(Ref, [flush]),
            ?assert(Same);
        {'DOWN', Ref, process, Pid, Reason} ->
            ?assertEqual(read, {not_read, Reason})
    after 60000 ->
        exit(Pid, kill),
        ?assert(false)
    end.

%% A number of more than 1024 characters is refused; one of 1024 is read.
long_number_test() ->
    Type = {struct, [{1, i64}]},
    Message = fun(Digits) ->
        iolist_to_binary([<<"[1,\"m\",1,42,{\"1\":{\"i64\":">>, Digits, <<"}}]">>])
    end,
    Longest = binary:copy(<<"1">>, 1024),
    {_, {"m", {Value}}} = read_message(new_protocol(Message(Longest)), Type),
    ?assertEqual(binary_to_integer(Longest), Value),
    ?assertError(badarg, read_message(new_protocol(Message(<<Longest/binary, "1">>)), Type)).

%% A message name up to ?MAX_MESSAGE_NAME_SIZE bytes is read; a longer one is
%% refused by message_begin.
message_name_test() ->
    Type = {struct, [{1, string}]},
    AtMax = lists:duplicate(?MAX_MESSAGE_NAME_SIZE, $n),
    {_, {Name, _}} = read_message(new_protocol(message(AtMax, Type, {args, "x"})), Type),
    ?assertEqual(AtMax, Name),
    Over = message([$n | AtMax], Type, {args, "x"}),
    ?assertMatch(
        {_, {error, {message_name_exceeds_maximum, ?MAX_MESSAGE_NAME_SIZE}}},
        thrift_protocol:read(new_protocol(Over), message_begin)
    ).

%%%% Over a socket %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_function(_Function, Args) -> {reply, element(1, Args)}.

handle_error(_Function, _Reason) -> ok.

%% Two calls of each kind over one connection to a server that speaks JSON.
socket_test_() ->
    [
        {atom_to_list(Framing), ?_test(socket_calls(Port, Framing =:= framed))}
     || {Port, Framing} <- [{9098, buffered}, {9099, framed}]
    ].

socket_calls(Port, Framed) ->
    ok = load_thrift_application(),
    {ok, Server} = thrift_socket_server:start([
        {ip, "127.0.0.1"},
        {port, Port},
        {name, list_to_atom("json_server_" ++ integer_to_list(Port))},
        {service, thrift_test_thrift},
        {handler, ?MODULE},
        {protocol, json},
        {framed, Framed}
    ]),
    try
        {ok, Client0} = thrift_client_util:new(
            "127.0.0.1",
            Port,
            thrift_test_thrift,
            [{protocol, json}, {framed, Framed}, {recv_timeout, 2000}]
        ),
        Xtruct = #'thrift.test.Xtruct'{
            string_thing = <<"]\"">>, byte_thing = 1, i32_thing = 2, i64_thing = 3
        },
        Calls = [
            {testString, [list_to_binary(?AWKWARD)]},
            %% Ends right after a string opens, with no byte to spare: a read
            %% of more than the least the message needs waits here.
            {testString, [<<>>]},
            {testI32, [-7]},
            {testDouble, [0.25]},
            {testBool, [false]},
            {testList, [[1, 2, 3]]},
            {testStruct, [Xtruct]},
            {testStruct, [Xtruct]}
        ],
        lists:foldl(
            fun({Function, [Arg]}, Client) ->
                {Client1, Result} = thrift_client:call(Client, Function, [Arg]),
                ?assertEqual({Function, {ok, Arg}}, {Function, Result}),
                Client1
            end,
            Client0,
            Calls
        )
    after
        thrift_socket_server:stop(Server)
    end.

load_thrift_application() ->
    case application:load(thrift) of
        ok -> ok;
        {error, {already_loaded, thrift}} -> ok
    end.
