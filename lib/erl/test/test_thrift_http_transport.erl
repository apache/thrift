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
        {error, closed} ->
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
