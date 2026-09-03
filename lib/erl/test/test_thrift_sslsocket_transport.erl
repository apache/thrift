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

-module(test_thrift_sslsocket_transport).
-include_lib("eunit/include/eunit.hrl").

%%%% What the client hands to ssl:connect/3 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Build a transport factory with the given options and capture the option
%% list it passes to ssl:connect/3, without opening a socket.
capture_ssl_options(FactoryOptions) ->
    Self = self(),
    meck:new(gen_tcp, [unstick, passthrough]),
    meck:new(ssl, [unstick, passthrough]),
    try
        meck:expect(gen_tcp, connect, fun(_, _, _, _) -> {ok, a_fake_socket} end),
        meck:expect(gen_tcp, close, fun(_) -> ok end),
        meck:expect(ssl, connect, fun(_Socket, Options, _Timeout) ->
            Self ! {captured, Options},
            {error, not_connecting_in_a_unit_test}
        end),
        {ok, Factory} = thrift_sslsocket_transport:new_transport_factory(
            "localhost", 9999, FactoryOptions
        ),
        %% The factory exits when the handshake does not complete; that is the
        %% path being exercised, so swallow it and read what was captured.
        catch Factory(),
        receive
            {captured, Options} -> Options
        after 1000 -> erlang:error(ssl_connect_was_never_called)
        end
    after
        meck:unload(ssl),
        meck:unload(gen_tcp)
    end.

default_verifies_the_peer_test() ->
    Options = capture_ssl_options([]),
    ?assertEqual(
        verify_peer,
        proplists:get_value(verify, Options),
        "the default client must ask ssl to validate the server certificate"
    ).

default_supplies_cacerts_test() ->
    %% Without CAs, {verify, verify_peer} is not merely weaker -- ssl refuses
    %% to connect at all from OTP 26 on. The default has to carry both.
    case has_system_cacerts() of
        false ->
            ?debugMsg("no system trust store on this host, skipping"),
            ok;
        true ->
            Options = capture_ssl_options([]),
            ?assertMatch([_ | _], proplists:get_value(cacerts, Options))
    end.

caller_options_win_over_the_defaults_test() ->
    Options = capture_ssl_options([{ssloptions, [{verify, verify_none}]}]),
    %% Both occurrences are present and the order is the contract: ours first,
    %% so proplists:get_value/2 sees it, and the caller's last, because that is
    %% the one ssl:connect/3 honours.
    ?assertEqual(verify_peer, proplists:get_value(verify, Options)),
    ?assertEqual(
        {verify, verify_none},
        lists:last([KV || KV = {verify, _} <- Options]),
        "a caller that asks for verify_none must still get it"
    ).

caller_cacertfile_is_not_shadowed_by_the_system_store_test() ->
    %% {cacerts, _} and {cacertfile, _} are separate options, so "the caller's
    %% comes last" does not save a caller here: an injected system store would
    %% win over the CA file it named, and its server would be unknown_ca.
    %% Asserting the cacertfile survives is not enough -- it survived the bug.
    Options = capture_ssl_options([{ssloptions, [{cacertfile, "/dev/null"}]}]),
    ?assertEqual("/dev/null", proplists:get_value(cacertfile, Options)),
    ?assertEqual(
        undefined,
        proplists:get_value(cacerts, Options),
        "naming a cacertfile must suppress the system trust store"
    ).

caller_cacerts_are_not_shadowed_by_the_system_store_test() ->
    Options = capture_ssl_options([{ssloptions, [{cacerts, [a_caller_supplied_der]}]}]),
    ?assertEqual([a_caller_supplied_der], proplists:get_value(cacerts, Options)),
    ?assertEqual(
        1,
        length([KV || KV = {cacerts, _} <- Options]),
        "the system trust store must not be prepended alongside the caller's"
    ).

missing_trust_store_does_not_crash_test() ->
    %% public_key:cacerts_get/0 raises when the host has no trust store -- the
    %% official erlang:*-slim images are such hosts. The client must still be
    %% constructible there and must not quietly drop back to no verification.
    meck:new(public_key, [unstick, passthrough]),
    try
        meck:expect(public_key, cacerts_get, fun() -> erlang:error(enoent) end),
        Options = capture_ssl_options([]),
        ?assertEqual(verify_peer, proplists:get_value(verify, Options)),
        ?assertEqual(undefined, proplists:get_value(cacerts, Options))
    after
        meck:unload(public_key)
    end.

has_system_cacerts() ->
    try public_key:cacerts_get() of
        [_ | _] -> true;
        _ -> false
    catch
        _:_ -> false
    end.

%%%% Against a real TLS listener %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% test/keys/server.crt is self-signed, so no system trust store validates it.
keys_dir() ->
    Candidates = ["../../test/keys", "../../../test/keys", "test/keys"],
    case [D || D <- Candidates, filelib:is_regular(filename:join(D, "server.crt"))] of
        [Dir | _] -> {ok, Dir};
        [] -> error
    end.

with_tls_server(Fun) ->
    {ok, Dir} = keys_dir(),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, LSock} = ssl:listen(0, [
        {certfile, filename:join(Dir, "server.crt")},
        {keyfile, filename:join(Dir, "server.key")},
        binary,
        {active, false},
        {reuseaddr, true}
    ]),
    {ok, {_, Port}} = ssl:sockname(LSock),
    Acceptor = spawn(fun() -> accept_loop(LSock) end),
    try
        Fun(Port)
    after
        exit(Acceptor, kill),
        ssl:close(LSock)
    end.

accept_loop(LSock) ->
    case ssl:transport_accept(LSock, 5000) of
        {ok, Socket} ->
            spawn(fun() -> catch ssl:handshake(Socket, 5000) end),
            accept_loop(LSock);
        _ ->
            ok
    end.

connect(Port, FactoryOptions) ->
    {ok, Factory} = thrift_sslsocket_transport:new_transport_factory(
        "localhost", Port, FactoryOptions
    ),
    catch Factory().

self_signed_server_is_refused_by_default_test() ->
    case keys_dir() of
        error ->
            ?debugMsg("test/keys not reachable from here, skipping"),
            ok;
        {ok, _} ->
            with_tls_server(fun(Port) ->
                ?assertMatch({'EXIT', _}, connect(Port, []))
            end)
    end.

verify_none_still_connects_test() ->
    %% The escape hatch for anyone who was relying on the old behaviour, and
    %% the half of this that has been broken since OTP 26.
    case keys_dir() of
        error ->
            ok;
        {ok, _} ->
            with_tls_server(fun(Port) ->
                ?assertMatch(
                    {ok, _}, connect(Port, [{ssloptions, [{verify, verify_none}]}])
                )
            end)
    end.
