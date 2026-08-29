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

%% What a peer is told when a handler fails.
%%
%% thrift_processor:handle_error/5 can put the crash term and the Erlang stack
%% trace into the TApplicationException it sends back, and whether it does is
%% the `exceptions_include_traces' application variable.  These tests pin the
%% value that variable has when nobody sets it, because that is the value the
%% overwhelming majority of deployments run with, and they pin it through a
%% real call over a real socket rather than by reading the .app file.
%%
%% The switch itself is deliberately kept working: the last test sets it and
%% checks the trace comes back, so this stays a change of default rather than a
%% removal of the feature.

-module(exception_traces_test).

-include_lib("eunit/include/eunit.hrl").

-export([handle_function/2, handle_error/2]).

-define(SERVICE, multiplexing__calculator_thrift).

%% Planted in the crash so it can be looked for in what comes back.  Nothing
%% else in the system says this word.
-define(CANARY, "canary_from_the_handlers_own_state").

%% ---------------------------------------------------------------------------
%% Tests
%% ---------------------------------------------------------------------------

%% erlang:error/1 in a handler reaches handle_error/5 directly.  By default the
%% peer must learn that the call failed and nothing else: not the crash term,
%% and not the stack, which names our internal modules and the absolute source
%% paths of the machine that built the release.
error_in_handler_does_not_reach_the_peer_test() ->
    Message = message_from_failing_call(9092, crash),
    ?assertEqual(nomatch, string:find(Message, ?CANARY)),
    ?assertEqual(nomatch, string:find(Message, "thrift_processor")),
    ?assertNotEqual("", Message).

%% A handler that throws something the IDL does not declare takes the other
%% route -- handle_exception/5 finds no matching declared exception and calls
%% handle_unknown_exception/5, which forwards the thrown term itself to
%% handle_error/5 wrapped in exception_not_declared_as_thrown.  So this path
%% discloses application data even when the stack is uninteresting.
undeclared_exception_does_not_reach_the_peer_test() ->
    Message = message_from_failing_call(9093, undeclared_throw),
    ?assertEqual(nomatch, string:find(Message, ?CANARY)),
    ?assertEqual(nomatch, string:find(Message, "exception_not_declared_as_thrown")),
    ?assertNotEqual("", Message).

%% Opting in still works.  Anyone who wants traces on the wire -- a closed
%% network, a debugging session -- sets the variable and gets them.
enabling_traces_still_sends_them_test() ->
    Restore = set_traces(true),
    try
        Message = message_from_failing_call(9094, crash),
        ?assertNotEqual(nomatch, string:find(Message, ?CANARY))
    after
        Restore()
    end.

%% ---------------------------------------------------------------------------
%% Handlers
%% ---------------------------------------------------------------------------

handle_function(add, {1, _Y}) ->
    erlang:error({handler_blew_up, ?CANARY});
handle_function(add, {2, _Y}) ->
    throw({some_undeclared_problem, ?CANARY});
handle_function(add, {X, Y}) ->
    {reply, X + Y}.

handle_error(_Function, _Reason) ->
    ok.

%% ---------------------------------------------------------------------------
%% Harness
%% ---------------------------------------------------------------------------

%% Runs one add/2 against a server on Port whose handler fails in the requested
%% way, and returns the message field of the TApplicationException the client
%% is handed.
message_from_failing_call(Port, How) ->
    ok = load_thrift_application(),
    {ok, Server} = thrift_socket_server:start([
        {ip, "127.0.0.1"},
        {port, Port},
        {name, list_to_atom(?MODULE_STRING ++ "_" ++ integer_to_list(Port))},
        {service, ?SERVICE},
        {handler, ?MODULE}
    ]),
    try
        {ok, Client} = thrift_client_util:new("127.0.0.1", Port, ?SERVICE, []),
        Arg =
            case How of
                crash -> 1;
                undeclared_throw -> 2
            end,
        try
            thrift_client:call(Client, add, [Arg, 1]),
            erlang:error(expected_the_call_to_fail)
        catch
            throw:{_Client, {exception, Exception}} ->
                'TApplicationException' = element(1, Exception),
                element(2, Exception)
        end
    after
        thrift_socket_server:stop(Server)
    end.

%% handle_error/5 reads the variable out of the thrift application, so the
%% application has to be loaded for the shipped default to be the one in
%% force.  Merely having the code path on the code path is not enough.
load_thrift_application() ->
    case application:load(thrift) of
        ok -> ok;
        {error, {already_loaded, thrift}} -> ok
    end.

%% Sets exceptions_include_traces and returns a fun that puts back whatever was
%% there before, including nothing.
set_traces(Value) ->
    ok = load_thrift_application(),
    Previous = application:get_env(thrift, exceptions_include_traces),
    application:set_env(thrift, exceptions_include_traces, Value),
    fun() ->
        case Previous of
            {ok, Old} -> application:set_env(thrift, exceptions_include_traces, Old);
            undefined -> application:unset_env(thrift, exceptions_include_traces)
        end
    end.
