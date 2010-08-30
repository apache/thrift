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

-export([start/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
         handle_info/2]).

-export([acceptor_loop/1]).

-record(thrift_socket_server,
        {port,
         service,
         handler,
         name,
         max=2048,
         ip=any,
         listen=null,
         acceptor=null,
         socket_opts=[{recv_timeout, 500}],
         framed=false
        }).

start(State=#thrift_socket_server{}) ->
    start_server(State);
start(Options) ->
    start(parse_options(Options)).

stop(Name) when is_atom(Name) ->
    gen_server:cast(Name, stop);
stop(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, stop);
stop({local, Name}) ->
    stop(Name);
stop({global, Name}) ->
    stop(Name);
stop(Options) ->
    State = parse_options(Options),
    stop(State#thrift_socket_server.name).

%% Internal API

parse_options(Options) ->
    parse_options(Options, #thrift_socket_server{}).

parse_options([], State) ->
    State;
parse_options([{name, L} | Rest], State) when is_list(L) ->
    Name = {local, list_to_atom(L)},
    parse_options(Rest, State#thrift_socket_server{name=Name});
parse_options([{name, A} | Rest], State) when is_atom(A) ->
    Name = {local, A},
    parse_options(Rest, State#thrift_socket_server{name=Name});
parse_options([{name, Name} | Rest], State) ->
    parse_options(Rest, State#thrift_socket_server{name=Name});
parse_options([{port, L} | Rest], State) when is_list(L) ->
    Port = list_to_integer(L),
    parse_options(Rest, State#thrift_socket_server{port=Port});
parse_options([{port, Port} | Rest], State) ->
    parse_options(Rest, State#thrift_socket_server{port=Port});
parse_options([{ip, Ip} | Rest], State) ->
    ParsedIp = case Ip of
                   any ->
                       any;
                   Ip when is_tuple(Ip) ->
                       Ip;
                   Ip when is_list(Ip) ->
                       {ok, IpTuple} = inet_parse:address(Ip),
                       IpTuple
               end,
    parse_options(Rest, State#thrift_socket_server{ip=ParsedIp});
parse_options([{socket_opts, L} | Rest], State) when is_list(L), length(L) > 0 ->
    parse_options(Rest, State#thrift_socket_server{socket_opts=L});
parse_options([{handler, Handler} | Rest], State) ->
    parse_options(Rest, State#thrift_socket_server{handler=Handler});
parse_options([{service, Service} | Rest], State) ->
    parse_options(Rest, State#thrift_socket_server{service=Service});
parse_options([{max, Max} | Rest], State) ->
    MaxInt = case Max of
                 Max when is_list(Max) ->
                     list_to_integer(Max);
                 Max when is_integer(Max) ->
                     Max
             end,
    parse_options(Rest, State#thrift_socket_server{max=MaxInt});
parse_options([{framed, Framed} | Rest], State) when is_boolean(Framed) ->
    parse_options(Rest, State#thrift_socket_server{framed=Framed}).

start_server(State=#thrift_socket_server{name=Name}) ->
    case Name of
        undefined ->
            gen_server:start_link(?MODULE, State, []);
        _ ->
            gen_server:start_link(Name, ?MODULE, State, [])
    end.

init(State=#thrift_socket_server{ip=Ip, port=Port}) ->
    process_flag(trap_exit, true),
    BaseOpts = [binary,
                {reuseaddr, true},
                {packet, 0},
                {backlog, 4096},
                {recbuf, 8192},
                {active, false}],
    Opts = case Ip of
               any ->
                   BaseOpts;
               Ip ->
                   [{ip, Ip} | BaseOpts]
           end,
    case gen_tcp_listen(Port, Opts, State) of
        {stop, eacces} ->
            %% fdsrv module allows another shot to bind
            %% ports which require root access
            case Port < 1024 of
                true ->
                    case fdsrv:start() of
                        {ok, _} ->
                            case fdsrv:bind_socket(tcp, Port) of
                                {ok, Fd} ->
                                    gen_tcp_listen(Port, [{fd, Fd} | Opts], State);
                                _ ->
                                    {stop, fdsrv_bind_failed}
                            end;
                        _ ->
                            {stop, fdsrv_start_failed}
                    end;
                false ->
                    {stop, eacces}
            end;
        Other ->
            error_logger:info_msg("thrift service listening on port ~p", [Port]),
            Other
    end.

gen_tcp_listen(Port, Opts, State) ->
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen} ->
            {ok, ListenPort} = inet:port(Listen),
            {ok, new_acceptor(State#thrift_socket_server{listen=Listen,
                                                         port=ListenPort})};
        {error, Reason} ->
            {stop, Reason}
    end.

new_acceptor(State=#thrift_socket_server{max=0}) ->
    error_logger:error_msg("Not accepting new connections"),
    State#thrift_socket_server{acceptor=null};
new_acceptor(State=#thrift_socket_server{listen=Listen,
                                         service=Service, handler=Handler,
                                         socket_opts=Opts, framed=Framed
                                        }) ->
    Pid = proc_lib:spawn_link(?MODULE, acceptor_loop,
                              [{self(), Listen, Service, Handler, Opts, Framed}]),
    State#thrift_socket_server{acceptor=Pid}.

acceptor_loop({Server, Listen, Service, Handler, SocketOpts, Framed})
  when is_pid(Server), is_list(SocketOpts) ->
    case catch gen_tcp:accept(Listen) of % infinite timeout
        {ok, Socket} ->
            gen_server:cast(Server, {accepted, self()}),
            ProtoGen = fun() ->
                               {ok, SocketTransport}   = thrift_socket_transport:new(Socket, SocketOpts),
                               {ok, Transport}         =
                                   case Framed of
                                       true  -> thrift_framed_transport:new(SocketTransport);
                                       false -> thrift_buffered_transport:new(SocketTransport)
                                   end,
                               {ok, Protocol}          = thrift_binary_protocol:new(Transport),
                               {ok, Protocol}
                       end,
            thrift_processor:init({Server, ProtoGen, Service, Handler});
        {error, closed} ->
            exit({error, closed});
        Other ->
            error_logger:error_report(
              [{application, thrift},
               "Accept failed error",
               lists:flatten(io_lib:format("~p", [Other]))]),
            exit({error, accept_failed})
    end.

handle_call({get, port}, _From, State=#thrift_socket_server{port=Port}) ->
    {reply, Port, State};
handle_call(_Message, _From, State) ->
    Res = error,
    {reply, Res, State}.

handle_cast({accepted, Pid},
            State=#thrift_socket_server{acceptor=Pid, max=Max}) ->
    % io:format("accepted ~p~n", [Pid]),
    State1 = State#thrift_socket_server{max=Max - 1},
    {noreply, new_acceptor(State1)};
handle_cast(stop, State) ->
    {stop, normal, State}.

terminate(_Reason, #thrift_socket_server{listen=Listen, port=Port}) ->
    gen_tcp:close(Listen),
    case Port < 1024 of
        true ->
            catch fdsrv:stop(),
            ok;
        false ->
            ok
    end.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_info({'EXIT', Pid, normal},
            State=#thrift_socket_server{acceptor=Pid}) ->
    {noreply, new_acceptor(State)};
handle_info({'EXIT', Pid, Reason},
            State=#thrift_socket_server{acceptor=Pid}) ->
    error_logger:error_report({?MODULE, ?LINE,
                               {acceptor_error, Reason}}),
    timer:sleep(100),
    {noreply, new_acceptor(State)};
handle_info({'EXIT', _LoopPid, Reason},
            State=#thrift_socket_server{acceptor=Pid, max=Max}) ->
    case Reason of
        normal -> ok;
        shutdown -> ok;
        _ -> error_logger:error_report({?MODULE, ?LINE,
                                        {child_error, Reason, erlang:get_stacktrace()}})
    end,
    State1 = State#thrift_socket_server{max=Max + 1},
    State2 = case Pid of
                 null -> new_acceptor(State1);
                 _ -> State1
             end,
    {noreply, State2};
handle_info(Info, State) ->
    error_logger:info_report([{'INFO', Info}, {'State', State}]),
    {noreply, State}.
