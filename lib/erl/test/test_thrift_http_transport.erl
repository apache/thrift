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

-module(test_thrift_http_transport).
-include_lib("eunit/include/eunit.hrl").

%% logger handler callback, see capture_log/1
-export([log/2]).

%% The limit most tests below set; their replies are sized in multiples of it.
-define(LIMIT, 1000).

%% The default limit: the default maximum message size of the other bindings.
-define(DEFAULT_LIMIT, (100 * 1024 * 1024)).

%% The ways a reply can say where its body ends.
-define(FRAMINGS, [length, close, chunked]).

%% A local HTTP server that answers the requests it receives with the given
%% replies in turn, repeating the last one, and sends each request body it
%% receives to the test process.
start_server(Replies) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Listen),
    Parent = self(),
    spawn_link(fun() -> serve(Listen, Parent, Replies) end),
    {Listen, "127.0.0.1:" ++ integer_to_list(Port)}.

stop_server({Listen, _Host}) ->
    gen_tcp:close(Listen).

serve(Listen, Parent, [{Status, Body} | Rest] = Replies) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Parent ! {request_body, read_request(Socket)},
            ok = gen_tcp:send(Socket, [
                <<"HTTP/1.1 ">>,
                Status,
                <<"\r\nContent-Type: application/x-thrift\r\nContent-Length: ">>,
                integer_to_list(byte_size(Body)),
                <<"\r\nConnection: close\r\n\r\n">>,
                Body
            ]),
            gen_tcp:close(Socket),
            case Rest of
                [] -> serve(Listen, Parent, Replies);
                _ -> serve(Listen, Parent, Rest)
            end;
        %% The test is over: closed, or einval if the listening socket was
        %% closed before accept/1 was called.
        {error, _} ->
            ok
    end.

%% Reads the request head and returns the body.
read_request(Socket) ->
    ok = inet:setopts(Socket, [{packet, http_bin}]),
    {ok, {http_request, 'POST', _, _}} = gen_tcp:recv(Socket, 0),
    Length = read_headers(Socket, 0),
    ok = inet:setopts(Socket, [{packet, raw}]),
    case Length of
        0 ->
            <<>>;
        _ ->
            {ok, Body} = gen_tcp:recv(Socket, Length),
            Body
    end.

read_headers(Socket, Length) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_header, _, 'Content-Length', _, Value}} ->
            read_headers(Socket, binary_to_integer(Value));
        {ok, {http_header, _, _, _, _}} ->
            read_headers(Socket, Length);
        {ok, http_eoh} ->
            Length
    end.

next_request_body() ->
    receive
        {request_body, Body} -> Body
    after 5000 -> timeout
    end.

new_transport(Host) ->
    {ok, Transport} = thrift_http_transport:new(Host, "/"),
    Transport.

flush(Transport, Data) ->
    {Transport1, ok} = thrift_transport:write(Transport, Data),
    thrift_transport:flush(Transport1).

with_inets(Tests) ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(inets) end, Tests}.

%% Runs Test(Host) in the test process against a server started by that
%% process, so that the server's messages reach it.
with_server(Replies, Test) ->
    ?_test(begin
        Server = start_server(Replies),
        try
            Test(element(2, Server))
        after
            stop_server(Server)
        end
    end).

%% A local HTTP server that answers the requests it receives with the given
%% {Status, Framing, Body} replies in turn, sending each body in slices a
%% millisecond apart, as a server slow to produce it would. It sends each
%% request body it receives to the test process, and then how much of the
%% reply body it sent: {Bytes, complete}, or {Bytes, stopped} if the client
%% went away first.
start_slow_server(Replies) ->
    {ok, Listen} = gen_tcp:listen(0, [binary, {active, false}, {ip, {127, 0, 0, 1}}]),
    {ok, Port} = inet:port(Listen),
    Parent = self(),
    spawn_link(fun() -> serve_slowly(Listen, Parent, Replies) end),
    {Listen, "127.0.0.1:" ++ integer_to_list(Port)}.

serve_slowly(_Listen, _Parent, []) ->
    ok;
serve_slowly(Listen, Parent, [{Status, Framing, Body} | Rest]) ->
    case gen_tcp:accept(Listen) of
        {ok, Socket} ->
            Parent ! {request_body, read_request(Socket)},
            ok = gen_tcp:send(Socket, [
                <<"HTTP/1.1 ">>,
                Status,
                <<"\r\n">>,
                framing_header(Framing, Body),
                <<"Connection: close\r\n\r\n">>
            ]),
            Parent ! {sent, send_body(Socket, Framing, Body, 0)},
            %% A body shorter than declared stalls until the client gives up.
            _ =
                case Framing of
                    {length, _} -> gen_tcp:recv(Socket, 0, 10000);
                    _ -> ok
                end,
            gen_tcp:close(Socket),
            serve_slowly(Listen, Parent, Rest);
        {error, _} ->
            ok
    end.

framing_header(length, Body) ->
    framing_header({length, byte_size(Body)}, Body);
%% A length other than the body's own.
framing_header({length, Length}, _Body) ->
    [<<"Content-Length: ">>, integer_to_list(Length), <<"\r\n">>];
framing_header(chunked, _Body) ->
    <<"Transfer-Encoding: chunked\r\n">>;
%% The body ends where the connection does.
framing_header(close, _Body) ->
    <<>>.

send_body(Socket, Framing, <<>>, Sent) ->
    case send_slice(Socket, Framing, <<>>) of
        ok -> {Sent, complete};
        {error, _} -> {Sent, stopped}
    end;
send_body(Socket, Framing, Body, Sent) ->
    Size = min(250, byte_size(Body)),
    <<Slice:Size/binary, Rest/binary>> = Body,
    timer:sleep(1),
    case send_slice(Socket, Framing, Slice) of
        ok -> send_body(Socket, Framing, Rest, Sent + Size);
        {error, _} -> {Sent, stopped}
    end.

%% An empty slice ends the body, which only a chunked one has to say.
send_slice(Socket, chunked, Slice) ->
    gen_tcp:send(Socket, [integer_to_list(byte_size(Slice), 16), "\r\n", Slice, "\r\n"]);
send_slice(_Socket, _Framing, <<>>) ->
    ok;
send_slice(Socket, _Framing, Slice) ->
    gen_tcp:send(Socket, Slice).

next_sent() ->
    receive
        {sent, Sent} -> Sent
    after 10000 -> timeout
    end.

body(Size) ->
    binary:copy(<<"x">>, Size).

%% Sends a request over a transport made with Options to a slow server that
%% answers it with the given reply. Returns what the flush returned, what a
%% read of the whole reply body then returned, and how much of that body
%% the server sent.
exchange(Options, Status, Framing, Body) ->
    {_, Host} = Server = start_slow_server([{Status, Framing, Body}]),
    try
        {ok, Transport} = thrift_http_transport:new(Host, "/", Options),
        {Transport1, Flushed} = flush(Transport, <<"request">>),
        ?assertEqual(<<"request">>, next_request_body()),
        {_, Read} = thrift_transport:read(Transport1, byte_size(Body)),
        {Flushed, Read, next_sent()}
    after
        stop_server(Server)
    end.

%% One test of Test(Framing) for each of Framings, titled Name(Framing), with
%% the thrift application's max_message_size set to ?LIMIT.
per_framing(Name, Framings, Test) ->
    with_inets([
        {
            lists:flatten(io_lib:format("~s(~s)", [Name, Framing])),
            ?_test(with_limit(?LIMIT, fun() -> Test(Framing) end))
        }
     || Framing <- Framings
    ]).

%% Runs Fun with the thrift application's max_message_size set to Limit.
with_limit(Limit, Fun) ->
    ok = application:set_env(thrift, max_message_size, Limit),
    try
        Fun()
    after
        application:unset_env(thrift, max_message_size)
    end.

httpc_takes_max_body_size() ->
    {ok, Vsn} = application:get_key(inets, vsn),
    thrift_http_transport:takes_max_body_size(Vsn).

%% Runs Fun and returns what was logged meanwhile, at level info or above,
%% by the processes that ran on its behalf and have finished.
capture_log(Fun) ->
    #{level := Level} = logger:get_primary_config(),
    ok = logger:add_handler(?MODULE, ?MODULE, #{level => all, config => #{pid => self()}}),
    ok = logger:set_primary_config(level, info),
    try
        Fun()
    after
        ok = logger:set_primary_config(level, Level),
        ok = logger:remove_handler(?MODULE)
    end,
    collect_logged().

log(Event, #{config := #{pid := Pid}}) ->
    Pid ! {logged, Event}.

collect_logged() ->
    receive
        {logged, Event} -> [Event | collect_logged()]
    after 0 -> []
    end.

%% How much a process that has run Fun once grows by running it Times more:
%% {grew_by, Bytes}, or why it could not.
memory_growth(Fun, Times) ->
    {Pid, Monitor} = spawn_monitor(fun() ->
        Fun(),
        Before = memory_after_gc(),
        lists:foreach(fun(_) -> Fun() end, lists:seq(1, Times)),
        exit({grew_by, memory_after_gc() - Before})
    end),
    receive
        {'DOWN', Monitor, process, Pid, Result} -> Result
    end.

memory_after_gc() ->
    true = erlang:garbage_collect(),
    {memory, Memory} = process_info(self(), memory),
    Memory.

ok_reply_test_() ->
    with_inets(
        with_server([{<<"200 OK">>, <<"reply">>}], fun(Host) ->
            {Transport1, ok} = flush(new_transport(Host), <<"request">>),
            ?assertEqual(<<"request">>, next_request_body()),
            ?assertMatch({_, {ok, <<"reply">>}}, thrift_transport:read(Transport1, 5))
        end)
    ).

error_reply_test_() ->
    with_inets([
        with_server([{Status, <<"not a thrift reply">>}], fun(Host) ->
            {_, Result} = flush(new_transport(Host), <<"request">>),
            ?assertEqual({error, {http_status, Code, Reason}}, Result),
            ?assertEqual(<<"request">>, next_request_body())
        end)
     || {Status, Code, Reason} <- [
            {<<"500 Internal Server Error">>, 500, "Internal Server Error"},
            {<<"404 Not Found">>, 404, "Not Found"},
            {<<"302 Found">>, 302, "Found"}
        ]
    ]).

unreachable_server_test_() ->
    with_inets(
        ?_test(begin
            %% A port that was just released has nothing listening on it.
            {ok, Listen} = gen_tcp:listen(0, [{ip, {127, 0, 0, 1}}]),
            {ok, Port} = inet:port(Listen),
            ok = gen_tcp:close(Listen),
            Host = "127.0.0.1:" ++ integer_to_list(Port),
            ?assertMatch({_, {error, _}}, flush(new_transport(Host), <<"request">>))
        end)
    ).

%% What was written for a request that failed is not sent again with the next one.
failed_request_is_not_resent_test_() ->
    with_inets(
        with_server([{<<"503 Service Unavailable">>, <<>>}, {<<"200 OK">>, <<"reply">>}], fun(Host) ->
            {Transport1, {error, _}} = flush(new_transport(Host), <<"first">>),
            ?assertEqual(<<"first">>, next_request_body()),
            {_, ok} = flush(Transport1, <<"second">>),
            ?assertEqual(<<"second">>, next_request_body())
        end)
    ).

%% A client call over HTTP returns the error instead of crashing the caller.
client_call_test_() ->
    with_inets(
        with_server([{<<"500 Internal Server Error">>, <<>>}], fun(Host) ->
            {ok, Protocol} = thrift_binary_protocol:new(new_transport(Host)),
            {ok, Client} = thrift_client:new(Protocol, thrift_test_thrift),
            ?assertMatch(
                {_, {error, {http_status, 500, _}}},
                thrift_client:call(Client, testVoid, [])
            ),
            ?assertNotEqual(timeout, next_request_body())
        end)
    ).

%% A reply as long as the limit is read whole, however its end is marked.
reply_at_limit_test_() ->
    per_framing(?FUNCTION_NAME, ?FRAMINGS, fun(Framing) ->
        Body = body(?LIMIT),
        ?assertEqual(
            {ok, {ok, Body}, {?LIMIT, complete}},
            exchange([], <<"200 OK">>, Framing, Body)
        )
    end).

%% A reply one byte longer is refused, and nothing of it is left to read.
reply_over_limit_test_() ->
    per_framing(?FUNCTION_NAME, ?FRAMINGS, fun(Framing) ->
        ?assertMatch(
            {{error, {message_size_exceeds_maximum, ?LIMIT}}, {ok, <<>>}, _},
            exchange([], <<"200 OK">>, Framing, body(?LIMIT + 1))
        )
    end).

%% A long reply is given up once it has passed the limit, long before the
%% server has sent all of it, and nothing of it reaches the caller.
long_reply_test_() ->
    per_framing(?FUNCTION_NAME, ?FRAMINGS, fun(Framing) ->
        {Flushed, Read, {Sent, Outcome}} =
            exchange([], <<"200 OK">>, Framing, body(100 * ?LIMIT)),
        ?assertEqual({error, {message_size_exceeds_maximum, ?LIMIT}}, Flushed),
        ?assertEqual({ok, <<>>}, Read),
        ?assertEqual(stopped, Outcome),
        ?assert(Sent < 20 * ?LIMIT),
        ?assertEqual({messages, []}, process_info(self(), messages))
    end).

%% Without a limit set, a reply declared one byte longer than the default
%% is refused before its body is read, rather than waited for.
default_limit_test_() ->
    with_inets(
        ?_test(begin
            Options = [{http_options, [{timeout, 2000}]}],
            {Flushed, _, {_, Outcome}} =
                exchange(Options, <<"200 OK">>, {length, ?DEFAULT_LIMIT + 1}, body(100 * ?LIMIT)),
            ?assertEqual({error, {message_size_exceeds_maximum, ?DEFAULT_LIMIT}}, Flushed),
            ?assertEqual(stopped, Outcome)
        end)
    ).

%% httpc reads a reply other than 200 whole before handing it over. Where it
%% takes max_body_size, that bounds a reply which declares its length or is
%% chunked.
long_error_reply_test_() ->
    per_framing(?FUNCTION_NAME, [length, chunked], fun(Framing) ->
        {Flushed, _, {_, Outcome}} =
            exchange([], <<"500 Internal Server Error">>, Framing, body(100 * ?LIMIT)),
        case httpc_takes_max_body_size() of
            true ->
                ?assertEqual({error, {message_size_exceeds_maximum, ?LIMIT}}, Flushed),
                ?assertEqual(stopped, Outcome);
            false ->
                ?assertEqual({error, {http_status, 500, "Internal Server Error"}}, Flushed),
                ?assertEqual(complete, Outcome)
        end
    end).

%% A request that times out still returns, also while its reply is under way.
timeout_test_() ->
    with_inets(
        ?_test(begin
            {_, Host} =
                Server = start_slow_server([{<<"200 OK">>, {length, 2 * ?LIMIT}, body(?LIMIT)}]),
            try
                Options = [{http_options, [{timeout, 500}]}],
                {ok, Transport} = thrift_http_transport:new(Host, "/", Options),
                ?assertMatch({_, {error, timeout}}, flush(Transport, <<"request">>)),
                ?assertEqual(<<"request">>, next_request_body()),
                ?assertEqual({?LIMIT, complete}, next_sent())
            after
                stop_server(Server)
            end
        end)
    ).

%% A transport's own limit takes the place of the application's.
transport_limit_test_() ->
    with_inets([
        {"higher",
            ?_test(
                with_limit(?LIMIT, fun() ->
                    Body = body(2 * ?LIMIT),
                    ?assertEqual(
                        {ok, {ok, Body}, {2 * ?LIMIT, complete}},
                        exchange([{max_message_size, 2 * ?LIMIT}], <<"200 OK">>, length, Body)
                    )
                end)
            )},
        {"lower",
            ?_test(
                with_limit(2 * ?LIMIT, fun() ->
                    ?assertMatch(
                        {{error, {message_size_exceeds_maximum, ?LIMIT}}, {ok, <<>>}, _},
                        exchange(
                            [{max_message_size, ?LIMIT}], <<"200 OK">>, length, body(2 * ?LIMIT)
                        )
                    )
                end)
            )}
    ]).

%% httpc reports each request option it does not take as ignored, on every
%% request, so the transport passes max_body_size only to an httpc that
%% takes it.
no_ignored_option_test_() ->
    with_inets(
        with_server([{<<"200 OK">>, <<"reply">>}], fun(Host) ->
            Logged = capture_log(fun() -> {_, ok} = flush(new_transport(Host), <<"request">>) end),
            ?assertEqual(<<"request">>, next_request_body()),
            Texts = [unicode:characters_to_list(logger_formatter:format(E, #{})) || E <- Logged],
            Ignored = [Text || Text <- Texts, string:find(Text, "Invalid option") =/= nomatch],
            ?assertEqual([], Ignored)
        end)
    ).

%% httpc makes each asynchronous request an alias of the process that made
%% it, for as long as that process lives: some hundred bytes a request. The
%% transport's requests leave nothing of the kind behind in the caller.
caller_does_not_grow_test_() ->
    with_inets(
        with_server([{<<"200 OK">>, <<"reply">>}], fun(Host) ->
            Transport = new_transport(Host),
            Result = memory_growth(fun() -> {_, ok} = flush(Transport, <<"request">>) end, 100),
            ?assertEqual(
                lists:duplicate(101, <<"request">>),
                [next_request_body() || _ <- lists:seq(0, 100)]
            ),
            ?assertMatch({grew_by, Bytes} when Bytes < 100 * 16, Result)
        end)
    ).

invalid_limit_test_() ->
    [
        ?_assertEqual(
            {error, {invalid_option, {max_message_size, Limit}}},
            thrift_http_transport:new("localhost", "/", [{max_message_size, Limit}])
        )
     || Limit <- [0, -1, 1.5, infinity]
    ].

%% What was written for a request whose reply was refused is not sent again
%% with the next one, and the transport goes on working.
refused_request_is_not_resent_test_() ->
    with_inets(
        ?_test(
            with_limit(?LIMIT, fun() ->
                {_, Host} =
                    Server = start_slow_server([
                        {<<"200 OK">>, length, body(2 * ?LIMIT)},
                        {<<"200 OK">>, length, <<"reply">>}
                    ]),
                try
                    {Transport1, Refused} = flush(new_transport(Host), <<"first">>),
                    ?assertEqual({error, {message_size_exceeds_maximum, ?LIMIT}}, Refused),
                    ?assertEqual(<<"first">>, next_request_body()),
                    ?assertMatch({_, _}, next_sent()),
                    {Transport2, ok} = flush(Transport1, <<"second">>),
                    ?assertEqual(<<"second">>, next_request_body()),
                    ?assertEqual({5, complete}, next_sent()),
                    ?assertMatch({_, {ok, <<"reply">>}}, thrift_transport:read(Transport2, 5))
                after
                    stop_server(Server)
                end
            end)
        )
    ).

%% The inets releases whose httpc takes max_body_size, from the OTP patches
%% of 2026-09-01 on.
takes_max_body_size_test_() ->
    [
        ?_assertEqual(Expected, thrift_http_transport:takes_max_body_size(Vsn))
     || {Vsn, Expected} <- [
            % OTP 25.3.2.21
            {"8.3.1.5", false},
            % OTP 26.2.5.21
            {"9.1.0.7", false},
            % OTP 27.3.4.16
            {"9.3.2.6", false},
            % OTP 27.3.4.17
            {"9.3.2.7", true},
            {"9.3.2.10", true},
            % OTP 28.1
            {"9.4.2", false},
            % OTP 28.5.0.5
            {"9.6.2.2", false},
            % OTP 28.5.0.6
            {"9.6.2.3", true},
            {"9.6.2.10", true},
            % OTP 29.0.5
            {"9.7.1", false},
            % OTP 29.0.6
            {"9.7.2", true},
            % OTP 29.1
            {"9.8", true},
            {"10.0", true},
            {"9.3.2.7-rc1", false},
            {"", false}
        ]
    ].
