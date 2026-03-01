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

-module(thrift_socket_server).

-behaviour(gen_server).

-include("thrift_constants.hrl").

-export([start/1, stop/1]).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3,
    handle_info/2
]).
-export([acceptor_loop/1]).

-ifdef(TEST).
-export([parse_options/1]).
-endif.

-type protocol() ::
    compact
    | {compact, term()}
    | json
    | {json, term()}
    | binary
    | {binary | term()}
    | {custom, module(), term()}.

-type socket_opts() :: [inet:inet_backend() | gen_tcp:listen_option()].
-type ssl_opts() :: [ssl:tls_server_option()].

-record(thrift_socket_server, {
    port :: inet:port_number(),
    service :: thrift_multiplexed_map_wrapper:service_handler_map(),
    handler :: thrift_multiplexed_map_wrapper:service_handler_map(),
    acceptors_left :: non_neg_integer(),
    listen :: gen_tcp:socket(),
    acceptor :: undefined | pid(),
    socket_opts :: socket_opts(),
    protocol :: protocol(),
    framed :: boolean(),
    ssltransport :: boolean(),
    ssloptions :: ssl_opts()
}).

start(Options) ->
    start_server(parse_options(Options)).

stop(Name) when is_atom(Name) ->
    gen_server:cast(Name, stop);
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop);
stop({local, Name}) ->
    stop(Name);
stop({global, Name}) ->
    stop(Name);
stop(Options) ->
    #{name := Name} = parse_options(Options),
    stop(Name).

%% Internal API

parse_options(Options) ->
    parse_options(Options, #{}).

parse_options([], State) ->
    State;
parse_options([{name, L} | Rest], State) when is_list(L) ->
    Name = {local, list_to_atom(L)},
    parse_options(Rest, State#{name => Name});
parse_options([{name, A} | Rest], State) when is_atom(A) ->
    Name = {local, A},
    parse_options(Rest, State#{name => Name});
parse_options([{name, Name} | Rest], State) ->
    parse_options(Rest, State#{name => Name});
parse_options([{port, L} | Rest], State) when is_list(L) ->
    Port = list_to_integer(L),
    parse_options(Rest, State#{port => Port});
parse_options([{port, Port} | Rest], State) when is_integer(Port), Port >= 0, Port =< 65535 ->
    parse_options(Rest, State#{port => Port});
parse_options([{ip, Ip} | Rest], State) ->
    case Ip of
        any ->
            parse_options(Rest, State);
        Ip when is_tuple(Ip) ->
            parse_options(Rest, State#{ip => Ip});
        Ip when is_list(Ip) ->
            {ok, IpTuple} = inet_parse:address(Ip),
            parse_options(Rest, State#{ip => IpTuple})
    end;
parse_options([{socket_opts, L} | Rest], State) when is_list(L), length(L) > 0 ->
    parse_options(Rest, State#{socket_opts => L});
parse_options([{handler, []} | _Rest], _State) ->
    throw("At least an error handler must be defined.");
parse_options([{handler, ServiceHandlerPropertyList} | Rest], State) when
    is_list(ServiceHandlerPropertyList)
->
    ServiceHandlerMap =
        case maps:get(handler, State, undefined) of
            undefined ->
                lists:foldl(
                    fun
                        ({ServiceName, ServiceHandler}, Acc) when
                            is_list(ServiceName), is_atom(ServiceHandler)
                        ->
                            thrift_multiplexed_map_wrapper:store(ServiceName, ServiceHandler, Acc);
                        (_, _Acc) ->
                            throw(
                                "The handler option is not properly configured for multiplexed services. It should be a kind of [{\"error_handler\", Module::atom()}, {SericeName::list(), Module::atom()}, ...]"
                            )
                    end,
                    thrift_multiplexed_map_wrapper:new(),
                    ServiceHandlerPropertyList
                );
            _ ->
                throw("Error while parsing the handler option.")
        end,
    case thrift_multiplexed_map_wrapper:find(?MULTIPLEXED_ERROR_HANDLER_KEY, ServiceHandlerMap) of
        {ok, _ErrorHandler} ->
            parse_options(Rest, State#{handler => ServiceHandlerMap});
        error ->
            throw(
                "The handler option is not properly configured for multiplexed services. It should be a kind of [{\"error_handler\", Module::atom()}, {SericeName::list(), Module::atom()}, ...]"
            )
    end;
parse_options([{handler, Handler} | Rest], State) when
    not is_map_key(handler, State), is_atom(Handler)
->
    parse_options(Rest, State#{handler => Handler});
parse_options([{service, []} | _Rest], _State) ->
    throw("At least one service module must be defined.");
parse_options([{service, ServiceModulePropertyList} | Rest], State) when
    is_list(ServiceModulePropertyList)
->
    ServiceModuleMap =
        case maps:get(service, State, undefined) of
            undefined ->
                lists:foldl(
                    fun
                        ({ServiceName, ServiceModule}, Acc) when
                            is_list(ServiceName), is_atom(ServiceModule)
                        ->
                            thrift_multiplexed_map_wrapper:store(ServiceName, ServiceModule, Acc);
                        (_, _Acc) ->
                            throw(
                                "The service option is not properly configured for multiplexed services. It should be a kind of [{SericeName::list(), ServiceModule::atom()}, ...]"
                            )
                    end,
                    thrift_multiplexed_map_wrapper:new(),
                    ServiceModulePropertyList
                );
            _ ->
                throw("Error while parsing the service option.")
        end,
    parse_options(Rest, State#{service => ServiceModuleMap});
parse_options([{service, Service} | Rest], State) when
    not is_map_key(service, State), is_atom(Service)
->
    parse_options(Rest, State#{service => Service});
parse_options([{max, Max} | Rest], State) when is_integer(Max), Max > 0 ->
    parse_options(Rest, State#{max => Max});
parse_options([{protocol, Proto} | Rest], State) when is_atom(Proto) ->
    parse_options(Rest, State#{protocol => Proto});
parse_options([{framed, Framed} | Rest], State) when is_boolean(Framed) ->
    parse_options(Rest, State#{framed => Framed});
parse_options([{ssltransport, SSLTransport} | Rest], State) when is_boolean(SSLTransport) ->
    parse_options(Rest, State#{ssltransport => SSLTransport});
parse_options([{ssloptions, SSLOptions} | Rest], State) when is_list(SSLOptions) ->
    parse_options(Rest, State#{ssloptions => SSLOptions}).

start_server(Options = #{name := Name}) ->
    case Name of
        undefined ->
            gen_server:start_link(?MODULE, Options, []);
        _ ->
            gen_server:start_link(Name, ?MODULE, Options, [])
    end.

init(State = #{port := Port}) ->
    process_flag(trap_exit, true),
    BaseOpts = [
        binary,
        {reuseaddr, true},
        {packet, 0},
        {backlog, 4096},
        {recbuf, 8192},
        {active, false}
    ],
    Opts =
        case maps:get(ip, State, undefined) of
            undefined ->
                BaseOpts;
            Ip ->
                [{ip, Ip} | BaseOpts]
        end,
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen} ->
            {ok, ListenPort} = inet:port(Listen),
            {ok,
                new_acceptor(#thrift_socket_server{
                    acceptors_left = maps:get(max, State, 2048),
                    listen = Listen,
                    port = ListenPort,
                    service = maps:get(service, State),
                    handler = maps:get(handler, State),
                    socket_opts = maps:get(socket_opts, State, [{recv_timeout, 500}]),
                    framed = maps:get(framed, State, false),
                    protocol = maps:get(protocol, State, binary),
                    ssltransport = maps:get(ssltransport, State, false),
                    ssloptions = maps:get(ssloptions, State, [])
                })};
        {error, Reason} ->
            {stop, Reason}
    end.

new_acceptor(State = #thrift_socket_server{acceptors_left = 0}) ->
    error_logger:error_msg("Not accepting new connections"),
    State#thrift_socket_server{acceptor = undefined};
new_acceptor(
    State = #thrift_socket_server{
        listen = Listen,
        service = Service,
        handler = Handler,
        socket_opts = Opts,
        framed = Framed,
        protocol = Proto,
        ssltransport = SslTransport,
        ssloptions = SslOptions
    }
) ->
    Pid = proc_lib:spawn_link(
        ?MODULE,
        acceptor_loop,
        [{self(), Listen, Service, Handler, Opts, Framed, SslTransport, SslOptions, Proto}]
    ),
    State#thrift_socket_server{acceptor = Pid}.

acceptor_loop(
    {Server, Listen, Service, Handler, SocketOpts, Framed, SslTransport, SslOptions, Proto}
) when
    is_pid(Server), is_list(SocketOpts)
->
    % infinite timeout
    case catch gen_tcp:accept(Listen) of
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self()}),
            ProtoGen = fun() ->
                {ok, SocketTransport} =
                    case SslTransport of
                        true -> thrift_sslsocket_transport:new(Socket, SocketOpts, SslOptions);
                        false -> thrift_socket_transport:new(Socket, SocketOpts)
                    end,
                {ok, Transport} =
                    case Framed of
                        true -> thrift_framed_transport:new(SocketTransport);
                        false -> thrift_buffered_transport:new(SocketTransport)
                    end,
                {ok, Protocol} =
                    case Proto of
                        compact ->
                            thrift_compact_protocol:new(Transport);
                        {compact, Options} ->
                            thrift_compact_protocol:new(Transport, Options);
                        json ->
                            thrift_json_protocol:new(Transport);
                        {json, Options} ->
                            thrift_json_protocol:new(Transport, Options);
                        binary ->
                            thrift_binary_protocol:new(Transport);
                        {binary, Options} ->
                            thrift_binary_protocol:new(Transport, Options);
                        {custom, Module, Options} ->
                            case erlang:function_exported(Module, new, 2) of
                                true -> Module:new(Transport, Options);
                                false -> throw("Could not instantiate custom protocol")
                            end
                    end,
                {ok, Protocol}
            end,
            thrift_processor:init({Server, ProtoGen, Service, Handler});
        {error, closed} ->
            exit({error, closed});
        Other ->
            error_logger:error_report(
                [
                    {application, thrift},
                    "Accept failed error",
                    lists:flatten(io_lib:format("~p", [Other]))
                ]
            ),
            exit({error, accept_failed})
    end.

handle_call({get, port}, _From, State = #thrift_socket_server{port = Port}) ->
    {reply, Port, State};
handle_call(_Message, _From, State) ->
    Res = error,
    {reply, Res, State}.

handle_cast(
    {accepted, Pid},
    State = #thrift_socket_server{acceptor = Pid, acceptors_left = Max}
) ->
    % io:format("accepted ~p~n", [Pid]),
    State1 = State#thrift_socket_server{acceptors_left = Max - 1},
    {noreply, new_acceptor(State1)};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(Reason, #thrift_socket_server{listen = Listen}) ->
    gen_tcp:close(Listen),
    case Reason of
        normal ->
            ok;
        shutdown ->
            ok;
        _ ->
            {backtrace, Bt} = erlang:process_info(self(), backtrace),
            error_logger:error_report({?MODULE, ?LINE, {child_error, Reason, Bt}})
    end.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_info(
    {'EXIT', Pid, normal},
    State = #thrift_socket_server{acceptor = Pid}
) ->
    {noreply, new_acceptor(State)};
handle_info(
    {'EXIT', Pid, Reason},
    State = #thrift_socket_server{acceptor = Pid}
) ->
    error_logger:error_report({?MODULE, ?LINE, {acceptor_error, Reason}}),
    timer:sleep(100),
    {noreply, new_acceptor(State)};
handle_info(
    {'EXIT', _LoopPid, Reason},
    State = #thrift_socket_server{acceptor = Pid, acceptors_left = AcceptorsLeft}
) ->
    case Reason of
        normal -> ok;
        shutdown -> ok;
        _ -> error_logger:error_report({?MODULE, ?LINE, {child_error, Reason}})
    end,
    State1 = State#thrift_socket_server{acceptors_left = AcceptorsLeft + 1},
    State2 =
        case Pid of
            undefined -> new_acceptor(State1);
            _ -> State1
        end,
    {noreply, State2};
handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.
